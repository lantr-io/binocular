package binocular

import binocular.watchtower.*

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude.List as ScalusList
import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.hex

/** Unit tests for the off-chain completed-peg-outs trie helpers.
  *
  * The important ones are the CROSS-CHECKS: they feed the same parsed TM outputs to
  * [[CompletedPegOutsTrie.committedRoot]] and to the on-chain
  * [[TreasuryMovementValidator.committedRoot]] and require the two to agree — accept the same
  * outputs, reject the same outputs, and return the same 32 bytes. That equality is what makes the
  * confirm builder's trie output datum the one the validator will accept (the on-chain reader is
  * plain Scala here; only the UPLC compilation is skipped).
  */
class CompletedPegOutsTrieTest extends AnyFunSuite {

    private val CommitmentPrefix = hex"6a2543504f5231"

    private def root(b: Int): ByteString =
        ByteString.fromArray(Array.fill[Byte](32)(b.toByte))

    private def porId(b: Int): ByteString =
        ByteString.fromArray(Array.fill[Byte](32)(b.toByte))

    private def commitment(r: ByteString): PegOutEntry = PegOutEntry(CommitmentPrefix ++ r, 0)

    private def payment(spkByte: Int, sats: Long): PegOutEntry =
        PegOutEntry(ByteString.fromArray(Array.fill[Byte](22)(spkByte.toByte)), BigInt(sats))

    private val change = PegOutEntry(hex"0014aabbccddeeff00112233445566778899aabbcc", BigInt(999))

    /** `[change, payment*, commitment]` — the rev-5.1 TM output layout. */
    private def tm(r: ByteString, payments: (Int, Long)*): Seq[PegOutEntry] =
        (change +: payments.map { case (spk, sats) => payment(spk, sats) }) :+ commitment(r)

    /** The ON-CHAIN reader over the same outputs: `Right(root)`, or `Left` for a rejection. */
    private def onChainRoot(outs: Seq[PegOutEntry]): Either[String, ByteString] =
        try Right(TreasuryMovementValidator.committedRoot(ScalusList.from(outs)))
        catch { case t: Throwable => Left(Option(t.getMessage).getOrElse(t.toString)) }

    /** Assert both readers agree, and return what they read. */
    private def bothAgree(outs: Seq[PegOutEntry]): Either[String, ByteString] = {
        val off = CompletedPegOutsTrie.committedRoot(outs)
        val on = onChainRoot(outs)
        assert(
          off.isRight == on.isRight,
          s"off-chain said $off but the on-chain reader said $on"
        )
        assert(off.toOption == on.toOption, s"root mismatch: off-chain $off, on-chain $on")
        off
    }

    // --- root extraction, cross-checked against the on-chain reader ---

    test("committedRoot reads the root of the single \"CPOR1\" output") {
        assert(bothAgree(tm(root(0x5a), (0xaa, 2000L))) == Right(root(0x5a)))
    }

    test("committedRoot accepts a zero-peg-out TM (change + commitment only)") {
        assert(bothAgree(Seq(change, commitment(root(0x11)))) == Right(root(0x11)))
    }

    test("committedRoot finds the commitment at any position, including output 0") {
        assert(bothAgree(Seq(commitment(root(0x22)), change)) == Right(root(0x22)))
    }

    test("committedRoot rejects a TM with no commitment output") {
        assert(bothAgree(Seq(change, payment(0xaa, 2000L))).isLeft)
    }

    test("committedRoot rejects a TM with no outputs at all") {
        assert(bothAgree(Seq.empty).isLeft)
    }

    test("committedRoot rejects two commitment outputs, even of the same root") {
        assert(bothAgree(Seq(change, commitment(root(1)), commitment(root(2)))).isLeft)
        assert(bothAgree(Seq(change, commitment(root(1)), commitment(root(1)))).isLeft)
    }

    test("committedRoot ignores an output with the wrong prefix") {
        // "CPOR2": right length, right OP_RETURN push, wrong tag.
        val wrongTag = PegOutEntry(hex"6a2543504f5232" ++ root(3), 0)
        assert(bothAgree(Seq(change, wrongTag)).isLeft)
        assert(bothAgree(Seq(change, wrongTag, commitment(root(4)))) == Right(root(4)))
    }

    test("committedRoot ignores a right-prefix output of the wrong length") {
        // 38 bytes: without the length check the slice would read past the payload.
        val short = PegOutEntry(commitment(root(5)).scriptPubKey.slice(0, 38), 0)
        assert(bothAgree(Seq(change, short)).isLeft)
    }

    test("committedRoot ignores a peg-in \"BFR\" OP_RETURN") {
        val bfr = PegOutEntry(hex"6a23424652" ++ porId(1), 0)
        assert(bothAgree(Seq(change, bfr)).isLeft)
    }

    test("committedRoot ignores a 39-byte payment script") {
        val payment39 = PegOutEntry(ByteString.fromArray(Array.fill[Byte](39)(0x51)), BigInt(1))
        assert(bothAgree(Seq(change, payment39)).isLeft)
    }

    // --- trie value + set-to-root builder ---

    test("trieValue is scriptPubKey ++ the amount as 8 little-endian bytes") {
        // Byte-for-byte what peg-out.ak rebuilds from the request datum; a change here silently
        // breaks peg-out completion, so the literal bytes are pinned.
        assert(
          CompletedPegOutsTrie.trieValue(payment(0xaa, 2000L)) ==
              payment(0xaa, 2000L).scriptPubKey ++ hex"d007000000000000"
        )
    }

    test("trieFrom of no entries is the empty trie, whose root is 32 zero bytes") {
        // The Aiken bootstrap mint pins exactly this root in the genesis trie datum.
        val tree = CompletedPegOutsTrie.trieFrom(Seq.empty).toOption.get
        assert(tree.rootHash == ByteString.fromArray(Array.fill[Byte](32)(0)))
    }

    test("trieFrom is independent of the order the entries are passed in") {
        val a = (porId(1), CompletedPegOutsTrie.trieValue(payment(0xa1, 1000L)))
        val b = (porId(2), CompletedPegOutsTrie.trieValue(payment(0xa2, 2000L)))
        val c = (porId(3), CompletedPegOutsTrie.trieValue(payment(0xa3, 3000L)))
        val forward = CompletedPegOutsTrie.trieFrom(Seq(a, b, c)).toOption.get.rootHash
        val backward = CompletedPegOutsTrie.trieFrom(Seq(c, b, a)).toOption.get.rootHash
        assert(forward == backward)
        assert(
          forward == OffChainMPF.empty
              .insert(a._1, a._2)
              .insert(b._1, b._2)
              .insert(c._1, c._2)
              .rootHash
        )
    }

    test("trieFrom records every entry under its POR id") {
        val entries = Seq(
          (porId(1), CompletedPegOutsTrie.trieValue(payment(0xa1, 1000L))),
          (porId(2), CompletedPegOutsTrie.trieValue(payment(0xa2, 2000L)))
        )
        val tree = CompletedPegOutsTrie.trieFrom(entries).toOption.get
        assert(tree.get(porId(1)).contains(entries.head._2))
        assert(tree.get(porId(2)).contains(entries(1)._2))
    }

    test("trieFrom inserts a repeated POR id with the same value once") {
        val e = (porId(1), CompletedPegOutsTrie.trieValue(payment(0xa1, 1000L)))
        assert(
          CompletedPegOutsTrie.trieFrom(Seq(e, e)).toOption.get.rootHash ==
              CompletedPegOutsTrie.trieFrom(Seq(e)).toOption.get.rootHash
        )
    }

    test("trieFrom rejects the same POR id with two different values") {
        val res = CompletedPegOutsTrie.trieFrom(
          Seq(
            (porId(1), CompletedPegOutsTrie.trieValue(payment(0xa1, 1000L))),
            (porId(1), CompletedPegOutsTrie.trieValue(payment(0xa2, 1000L)))
          )
        )
        assert(res.isLeft)
        assert(res.left.toOption.get.contains("different"))
    }
}
