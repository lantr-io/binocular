package binocular

import binocular.bitcoin.BitcoinHelpers
import binocular.oracle.reverse
import binocular.watchtower.*
import binocular.watchtower.SweptPegInsProofService.*

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry as MPF
import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF
import scalus.uplc.builtin.Builtins.integerToByteString
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.hex

/** Unit tests for the swept-peg-ins proof server ([SPI-4], [SPI-6]).
  *
  * The fixture is a synthetic Bitcoin treasury chain:
  *
  * {{{
  *   genesis funding tx (no "BTMR1" output — not a TM)
  *     └─ TM1: spends genesis:0, sweeps d1 + d2, commits spiRoot1
  *          └─ TM2: spends TM1:0, sweeps d3, commits spiRoot2
  *               └─ TM3: spends TM2:0, sweeps d4, commits spiRoot3
  * }}}
  *
  * All four transactions are "mined" (present in the fetcher), but the singleton's head is TM2's
  * outpoint: TM3 is swept on Bitcoin and NOT yet confirmed on Cardano. That is the [SPI-6] boundary
  * this suite pins: d4 MUST NOT be served until the head advances.
  */
class SweptPegInsProofServiceTest extends AnyFunSuite {

    private def filled(v: Int, n: Int): ByteString =
        ByteString.fromArray(Array.fill[Byte](n)(v.toByte))

    /** A Bitcoin outpoint as the trie encodes it: `prev_txid`(32) ++ `prev_vout`(4, LE) = 36. */
    private def outpoint(txidByte: Int, vout: Long): ByteString =
        filled(txidByte, 32) ++ integerToByteString(false, 4, BigInt(vout))

    private val changeSpk = hex"5120" ++ filled(0x11, 32) // P2TR treasury change
    private val paySpk = hex"0014" ++ filled(0x22, 20) // P2WPKH peg-out payment

    private def commitmentSpk(spiRoot: ByteString, cpoRoot: ByteString): ByteString =
        hex"6a4542544d5231" ++ spiRoot ++ cpoRoot

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

    private def txidOf(raw: ByteString): ByteString = BitcoinHelpers.getTxHash(raw)

    private def outpoint0(raw: ByteString): ByteString =
        txidOf(raw) ++ hex"00000000"

    private def rootOf(entries: Seq[(ByteString, ByteString)]): ByteString =
        entries.foldLeft(OffChainMPF.empty)((t, kv) => t.insert(kv._1, kv._2)).rootHash

    // --- the chain -----------------------------------------------------------------------------

    private val d1 = outpoint(0xb1, 0)
    private val d2 = outpoint(0xb2, 3)
    private val d3 = outpoint(0xb3, 1)
    private val d4 = outpoint(0xb4, 0)

    private val cpoRoot = filled(0xc2, 32) // opaque here: this service only reconciles spi_root

    // The genesis funding tx: pays the treasury, carries NO commitment output. Its own input is an
    // arbitrary wallet outpoint that must never appear in the swept set.
    private val genesisFundingInput = outpoint(0xee, 5)
    private val genesisTx = rawTxWith(
      Seq(genesisFundingInput),
      Seq((changeSpk, BigInt(1_000_000)))
    )

    // TM1: spends genesis:0, sweeps d1 + d2.
    private val tm1Entries = Seq(d1 -> outpoint0(genesisTx), d2 -> outpoint0(genesisTx))
    private val spiRoot1 = rootOf(tm1Entries)
    private val tm1 = rawTxWith(
      Seq(outpoint0(genesisTx), d1, d2),
      Seq((changeSpk, BigInt(900_000)), (commitmentSpk(spiRoot1, cpoRoot), BigInt(0)))
    )

    // TM2: spends TM1:0, sweeps d3, pays one peg-out.
    private val tm2Entries = tm1Entries :+ (d3 -> outpoint0(tm1))
    private val spiRoot2 = rootOf(tm2Entries)
    private val tm2 = rawTxWith(
      Seq(outpoint0(tm1), d3),
      Seq(
        (changeSpk, BigInt(800_000)),
        (paySpk, BigInt(50_000)),
        (commitmentSpk(spiRoot2, cpoRoot), BigInt(0))
      )
    )

    // TM3: spends TM2:0, sweeps d4 — MINED ON BITCOIN but not yet confirmed on Cardano.
    private val tm3Entries = tm2Entries :+ (d4 -> outpoint0(tm2))
    private val spiRoot3 = rootOf(tm3Entries)
    private val tm3 = rawTxWith(
      Seq(outpoint0(tm2), d4),
      Seq((changeSpk, BigInt(700_000)), (commitmentSpk(spiRoot3, cpoRoot), BigInt(0)))
    )

    private val allTxs: Map[String, ByteString] =
        Seq(genesisTx, tm1, tm2, tm3).map(raw => txidOf(raw).toHex -> raw).toMap

    private def fetcher(txs: Map[String, ByteString]): ByteString => Option[ByteString] =
        txidLE => txs.get(txidLE.toHex)

    /** The singleton after TM2's Confirm: head = TM2's outpoint, spi_root = spiRoot2. TM3 exists on
      * Bitcoin but the singleton does not know it yet.
      */
    private val stateAfterTm2 =
        BridgeState(spiRoot2, cpoRoot, outpoint0(tm2), BigInt(800_000))

    /** The singleton after TM3's Confirm. */
    private val stateAfterTm3 =
        BridgeState(spiRoot3, cpoRoot, outpoint0(tm3), BigInt(700_000))

    // --- the walk ([SPI-6]: anchored at the singleton's head) ----------------------------------

    test("walkConfirmedChain returns the confirmed TMs oldest first, stopping at genesis") {
        val tms = walkConfirmedChain(outpoint0(tm2), fetcher(allTxs)).toOption.get
        assert(tms.map(_.txidLE) == Seq(txidOf(tm1), txidOf(tm2)))
        assert(tms.map(_.committedSpiRoot) == Seq(spiRoot1, spiRoot2))
        assert(tms.flatMap(_.entries) == tm2Entries)
    }

    test("the walk never visits a TM that spends the head (mined but unconfirmed)") {
        // TM3 is fetchable, but the walk starts at the CONFIRMED head and goes backward, so its
        // entries are structurally out of reach. This is where the [SPI-6] boundary lives.
        val tms = walkConfirmedChain(outpoint0(tm2), fetcher(allTxs)).toOption.get
        assert(!tms.exists(_.txidLE == txidOf(tm3)))
        assert(!tms.flatMap(_.entries).exists(_._1 == d4))
    }

    test("the genesis funding tx's own inputs never become swept entries") {
        val tms = walkConfirmedChain(outpoint0(tm2), fetcher(allTxs)).toOption.get
        assert(!tms.flatMap(_.entries).exists(_._1 == genesisFundingInput))
    }

    test("a gap in the chain (missing ancestor tx) aborts the walk") {
        val gapped = allTxs - txidOf(tm1).toHex
        val res = walkConfirmedChain(outpoint0(tm2), fetcher(gapped))
        assert(res.isLeft)
        assert(res.left.toOption.get.contains("not retrievable"))
    }

    test("a malformed head outpoint is rejected") {
        assert(walkConfirmedChain(hex"aabb", fetcher(allTxs)).isLeft)
    }

    test("a tx with several \"BTMR1\" outputs aborts the walk ([CTM-26])") {
        val bad = rawTxWith(
          Seq(outpoint0(tm2), d4),
          Seq(
            (changeSpk, BigInt(1)),
            (commitmentSpk(spiRoot3, cpoRoot), BigInt(0)),
            (commitmentSpk(spiRoot3, cpoRoot), BigInt(0))
          )
        )
        val txs = allTxs + (txidOf(bad).toHex -> bad)
        val res = walkConfirmedChain(outpoint0(bad), fetcher(txs))
        assert(res.isLeft)
        assert(res.left.toOption.get.contains("commitment"))
    }

    test("the walk is depth-bounded") {
        val res = walkConfirmedChain(outpoint0(tm2), fetcher(allTxs), maxDepth = 1)
        assert(res.isLeft)
        assert(res.left.toOption.get.contains("exceeded"))
    }

    test("a fetcher returning bytes that hash to a different txid is rejected") {
        val lying = allTxs + (txidOf(tm1).toHex -> genesisTx)
        val res = walkConfirmedChain(outpoint0(tm2), fetcher(lying))
        assert(res.isLeft)
        assert(res.left.toOption.get.contains("do not hash"))
    }

    // --- reconciliation ([SPI-6]) --------------------------------------------------------------

    test("confirmedTrie reproduces the singleton's attested spi_root") {
        val trie = confirmedTrie(spiRoot2, outpoint0(tm2), fetcher(allTxs)).toOption.get
        assert(trie.rootHash == spiRoot2)
        assert(trie.get(d1).contains(outpoint0(genesisTx)))
        assert(trie.get(d3).contains(outpoint0(tm1)))
        assert(trie.get(d4).isEmpty)
    }

    test("confirmedTrie refuses to serve when the derived root mismatches the attested one") {
        val res = confirmedTrie(filled(0x99, 32), outpoint0(tm2), fetcher(allTxs))
        assert(res == Left(RootMismatch(spiRoot2, filled(0x99, 32))))
    }

    test("confirmedTrie names a TM whose replayed root mismatches its own commitment") {
        // A TM that commits garbage as its spi_root: the per-TM assertion must name it instead of
        // surfacing later as an unexplained final mismatch.
        val badRoot = filled(0x77, 32)
        val tmBad = rawTxWith(
          Seq(outpoint0(tm2), d4),
          Seq((changeSpk, BigInt(1)), (commitmentSpk(badRoot, cpoRoot), BigInt(0)))
        )
        val txs = allTxs + (txidOf(tmBad).toHex -> tmBad)
        val res = confirmedTrie(badRoot, outpoint0(tmBad), fetcher(txs))
        assert(res.isLeft)
        val msg = res.left.toOption.get.message
        assert(msg.contains(txidOf(tmBad).reverse.toHex))
        assert(msg.contains(badRoot.toHex))
    }

    // --- serving ([SPI-4], verified the way [CPI-9] will) --------------------------------------

    test("serve returns a proof [CPI-9] verifies on-chain, with the proven value") {
        val proof = serve(stateAfterTm2, fetcher(allTxs), d1).toOption.get
        assert(proof.pegInUtxoId == d1)
        assert(proof.sweepingTmInput0 == outpoint0(genesisTx))
        assert(proof.spiRoot == spiRoot2)
        assert(MPF(spiRoot2).has(d1, proof.sweepingTmInput0, proof.proof))
        // The value is part of what is proved: the wrong sweeping TM must not verify.
        assert(!MPF(spiRoot2).has(d1, outpoint0(tm1), proof.proof))
    }

    test("serve proves an entry of the head TM itself") {
        val proof = serve(stateAfterTm2, fetcher(allTxs), d3).toOption.get
        assert(proof.sweepingTmInput0 == outpoint0(tm1))
        assert(MPF(spiRoot2).has(d3, proof.sweepingTmInput0, proof.proof))
    }

    test("[SPI-6] boundary: an entry swept on Bitcoin but not yet confirmed is NOT served") {
        // d4 is swept by TM3, which is mined (fetchable) but not confirmed: the singleton's head
        // is still TM2. A Bitcoin-only view would serve a proof that fails [CPI-9] on-chain.
        val res = serve(stateAfterTm2, fetcher(allTxs), d4)
        assert(res == Left(NotInConfirmedSet(d4, spiRoot2)))
        assert(res.left.toOption.get.message.contains("not yet confirmed"))
    }

    test("[SPI-6] boundary: the same entry IS served once its TM confirms") {
        val proof = serve(stateAfterTm3, fetcher(allTxs), d4).toOption.get
        assert(proof.sweepingTmInput0 == outpoint0(tm2))
        assert(MPF(spiRoot3).has(d4, proof.sweepingTmInput0, proof.proof))
    }

    test("serve never proves a never-swept deposit") {
        val res = serve(stateAfterTm2, fetcher(allTxs), outpoint(0xdd, 0))
        assert(res == Left(NotInConfirmedSet(outpoint(0xdd, 0), spiRoot2)))
    }

    test("serve rejects a malformed peg_in_utxo_id without walking anything") {
        val res = serve(stateAfterTm2, fetcher(Map.empty), hex"0102")
        assert(res.isLeft)
        assert(res.left.toOption.get.isInstanceOf[InvalidRequest])
    }

    test("serve refuses EVERY key when the attested root cannot be reproduced") {
        // Even a genuinely swept deposit is not served against a root the chain does not hold.
        val doctored = BridgeState(filled(0x99, 32), cpoRoot, outpoint0(tm2), BigInt(1))
        val res = serve(doctored, fetcher(allTxs), d1)
        assert(res.left.toOption.get.isInstanceOf[RootMismatch])
    }

    test("a genesis-only bridge (no TM confirmed yet) serves nothing and reconciles empty") {
        val genesisState =
            BridgeState(
              OffChainMPF.empty.rootHash,
              cpoRoot,
              outpoint0(genesisTx),
              BigInt(1_000_000)
            )
        val res = serve(genesisState, fetcher(allTxs), d1)
        assert(res == Left(NotInConfirmedSet(d1, OffChainMPF.empty.rootHash)))
    }
}
