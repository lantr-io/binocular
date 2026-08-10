package binocular

import binocular.watchtower.*

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry as MPF
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry.ProofStep
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF
import scalus.uplc.builtin.Builtins.integerToByteString
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.hex

/** Unit tests for the off-chain swept-peg-ins (SPI) trie helpers.
  *
  * The SPI trie answers "did this deposit reach the treasury". Its entries come from one place: the
  * inputs of a confirmed Treasury Movement.
  *
  *   - [SPI-1] every input EXCEPT input 0 becomes a key. Input 0 is the treasury outpoint the TM
  *     spends, not a deposit, so an entry for it would let a forged PegInRequest prove a sweep.
  *   - [SPI-3] every entry a TM adds carries that TM's OWN input-0 outpoint as its value, so one
  *     TM's entries all share one value. The TM's txid cannot be the value: the root rides in the
  *     same transaction's commitment output, so a txid value would need a hash fixed point.
  *   - [SPI-4] a membership proof must be servable to any caller, which the later [CPI-9] peg-in
  *     completion verifies on-chain with `mpf.has`.
  */
class SweptPegInsTrieTest extends AnyFunSuite {

    private def filled(v: Int, n: Int): ByteString =
        ByteString.fromArray(Array.fill[Byte](n)(v.toByte))

    /** A Bitcoin outpoint as the trie encodes it: `prev_txid`(32) ++ `prev_vout`(4, LE) = 36. */
    private def outpoint(txidByte: Int, vout: Long): ByteString =
        filled(txidByte, 32) ++ integerToByteString(false, 4, BigInt(vout))

    // --- a 3-input raw TM -----------------------------------------------------------------------
    // Input 0 is the treasury outpoint. Inputs 1 and 2 are swept peg-in deposits. Outputs are the
    // treasury change, one peg-out payment, and the single 71-byte "BTMR1" root commitment.

    private val treasuryIn = outpoint(0xa0, 7)
    private val deposit1 = outpoint(0xb1, 0)
    private val deposit2 = outpoint(0xb2, 3)

    private val changeSpk = hex"5120" ++ filled(0x11, 32) // P2TR treasury change
    private val paySpk = hex"0014" ++ filled(0x22, 20) // P2WPKH peg-out payment
    private val commitmentSpk = hex"6a4542544d5231" ++ filled(0xc1, 32) ++ filled(0xc2, 32)

    /** Serialize a segwit transaction: version, marker+flag, inputs, outputs, witnesses, locktime.
      * Each input carries an empty scriptSig and a one-item 64-byte witness (a Schnorr signature),
      * which is what a key-path treasury spend really looks like.
      */
    private def rawTxWith(
        inputs: Seq[ByteString],
        outputs: Seq[(ByteString, BigInt)]
    ): ByteString = {
        val insHex = inputs.map(o => o.toHex + "00" + "ffffffff").mkString
        val outsHex = outputs.map { case (spk, amt) =>
            integerToByteString(false, 8, amt).toHex + f"${spk.size}%02x" + spk.toHex
        }.mkString
        val witnessHex = inputs.map(_ => "01" + "40" + ("77" * 64)).mkString
        ByteString.fromHex(
          "02000000" + "0001" +
              f"${inputs.size}%02x" + insHex +
              f"${outputs.size}%02x" + outsHex +
              witnessHex +
              "00000000"
        )
    }

    private val rawTm: ByteString = rawTxWith(
      Seq(treasuryIn, deposit1, deposit2),
      Seq(
        (changeSpk, BigInt(500_000)),
        (paySpk, BigInt(120_000)),
        (commitmentSpk, BigInt(0))
      )
    )

    // --- entries --------------------------------------------------------------------------------

    test("entriesOf a 3-input TM yields 2 entries, one per non-treasury input ([SPI-1])") {
        val entries = SweptPegInsTrie.entriesOf(rawTm)
        assert(entries.size == 2)
        assert(entries.map(_._1) == Seq(deposit1, deposit2))
        assert(entries.forall(_._1.size == 36))
    }

    test("entriesOf never keys an entry by the TM's input-0 outpoint ([SPI-1])") {
        assert(!SweptPegInsTrie.entriesOf(rawTm).exists(_._1 == treasuryIn))
    }

    test("every entry of one TM carries that TM's input-0 outpoint as its value ([SPI-3])") {
        val values = SweptPegInsTrie.entriesOf(rawTm).map(_._2)
        assert(values == Seq(treasuryIn, treasuryIn))
    }

    test("entriesOf drops exactly the first of allInputOutpoints, in input order") {
        val parsed = TreasuryMovementValidator.allInputOutpoints(rawTm).asScala.toSeq
        assert(parsed == Seq(treasuryIn, deposit1, deposit2))
        assert(SweptPegInsTrie.entriesOf(rawTm).map(_._1) == parsed.drop(1))
    }

    test("a TM that sweeps nothing (treasury input only) yields no entries") {
        val raw = rawTxWith(Seq(treasuryIn), Seq((changeSpk, BigInt(400_000))))
        assert(SweptPegInsTrie.entriesOf(raw).isEmpty)
    }

    // --- set-to-root builder --------------------------------------------------------------------

    test("trieFrom of no entries is the empty trie, whose root is 32 zero bytes") {
        assert(SweptPegInsTrie.trieFrom(Seq.empty).toOption.get.rootHash == filled(0, 32))
    }

    test("trieFrom records every entry under its peg-in UTxO id, and nothing under input 0") {
        val trie = SweptPegInsTrie.trieFrom(SweptPegInsTrie.entriesOf(rawTm)).toOption.get
        assert(trie.get(deposit1).contains(treasuryIn))
        assert(trie.get(deposit2).contains(treasuryIn))
        assert(trie.get(treasuryIn).isEmpty)
    }

    test("trieFrom is independent of the order the entries are passed in") {
        val entries = SweptPegInsTrie.entriesOf(rawTm)
        assert(
          SweptPegInsTrie.trieFrom(entries).toOption.get.rootHash ==
              SweptPegInsTrie.trieFrom(entries.reverse).toOption.get.rootHash
        )
        assert(
          SweptPegInsTrie.trieFrom(entries).toOption.get.rootHash ==
              entries
                  .foldLeft(OffChainMPF.empty)((t, kv) => t.insert(kv._1, kv._2))
                  .rootHash
        )
    }

    test("trieFrom rejects one peg-in UTxO id swept by two different TMs") {
        // Two TMs claiming the same deposit means one of the two sources is wrong. Picking either
        // yields a root no TM ever committed, so it is reported, never resolved.
        val res = SweptPegInsTrie.trieFrom(
          Seq((deposit1, treasuryIn), (deposit1, outpoint(0xa9, 1)))
        )
        assert(res.isLeft)
        assert(res.left.toOption.get.contains("different"))
    }

    // --- membership proof ([SPI-4], verified the way [CPI-9] will) -------------------------------

    test("a membership proof of a swept deposit verifies against the computed root") {
        val entries = SweptPegInsTrie.entriesOf(rawTm)
        val trie = SweptPegInsTrie.trieFrom(entries).toOption.get
        val proof: PList[ProofStep] =
            SweptPegInsTrie.membershipProof(trie, deposit1).fold(e => fail(e), identity)
        assert(MPF(trie.rootHash).has(deposit1, treasuryIn, proof))
        // The value is part of what is proved: the wrong sweeping TM must not verify.
        assert(!MPF(trie.rootHash).has(deposit1, outpoint(0xa9, 1), proof))
    }

    test("no membership proof exists for the TM's own input-0 outpoint") {
        val trie = SweptPegInsTrie.trieFrom(SweptPegInsTrie.entriesOf(rawTm)).toOption.get
        assert(SweptPegInsTrie.membershipProof(trie, treasuryIn).isLeft)
    }
}
