package binocular

import binocular.watchtower.*

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry as OnChainMPF
import scalus.cardano.onchain.plutus.prelude.List as ScalusList
import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.hex

/** Unit tests for the off-chain completed-peg-outs trie helpers.
  *
  * The important ones are the CROSS-CHECKS: they feed the steps produced by
  * [[CompletedPegOutsTrie.buildSteps]] straight into the on-chain
  * [[TreasuryMovementValidator.foldCompletedPegOuts]] and require the folded root to equal the
  * off-chain root the confirm builder writes into the trie output datum. That is exactly the
  * equality the TM validator enforces, so a passing cross-check means the built Confirm tx would be
  * accepted (the on-chain fold is plain Scala; only the UPLC compilation is skipped).
  */
class CompletedPegOutsTrieTest extends AnyFunSuite {

    private val MarkerPrefix = hex"6a23504f52"

    private def porId(b: Int): ByteString =
        ByteString.fromArray(Array.fill[Byte](32)(b.toByte))

    private def marker(id: ByteString): PegOutEntry = PegOutEntry(MarkerPrefix ++ id, 0)

    private def payment(spkByte: Int, sats: Long): PegOutEntry =
        PegOutEntry(ByteString.fromArray(Array.fill[Byte](22)(spkByte.toByte)), BigInt(sats))

    private val change = PegOutEntry(hex"0014aabbccddeeff00112233445566778899aabbcc", BigInt(999))

    /** `[change, payment, marker, payment, marker, ...]` from `(spkByte, sats, porIdByte)` triples.
      */
    private def tm(pegOuts: (Int, Long, Int)*): Seq[PegOutEntry] =
        change +: pegOuts.flatMap { case (spk, sats, id) =>
            Seq(payment(spk, sats), marker(porId(id)))
        }

    private def pairsOrFail(outs: Seq[PegOutEntry]): Vector[CompletedPegOutsTrie.PorPair] =
        CompletedPegOutsTrie.pairsOf(outs).fold(err => fail(s"unexpected Left: $err"), identity)

    /** Run the ON-CHAIN fold over the same inputs the confirm builder would submit. */
    private def onChainFold(
        start: OffChainMPF,
        outs: Seq[PegOutEntry],
        steps: List[PegOutTrieStep]
    ): ByteString =
        TreasuryMovementValidator
            .foldCompletedPegOuts(
              OnChainMPF(start.rootHash),
              ScalusList.from(outs.drop(1)),
              ScalusList.from(steps)
            )
            .root

    // --- pair extraction ---

    test("pairsOf drops the treasury change output and yields one pair per (payment, marker)") {
        val pairs = pairsOrFail(tm((0xaa, 2000L, 1)))
        assert(pairs.size == 1)
        assert(pairs.head.porId == porId(1))
        assert(pairs.head.value == payment(0xaa, 2000L).scriptPubKey ++ hex"d007000000000000")
    }

    test("trieValue is scriptPubKey ++ the amount as 8 little-endian bytes") {
        // Byte-for-byte what peg-out.ak rebuilds from the request datum; a change here silently
        // breaks peg-out completion, so the literal bytes are pinned.
        assert(
          CompletedPegOutsTrie.trieValue(payment(0xaa, 2000L)) ==
              payment(0xaa, 2000L).scriptPubKey ++ hex"d007000000000000"
        )
    }

    test("pairsOf keeps output order across several peg-outs") {
        val pairs = pairsOrFail(tm((0xa1, 1000L, 1), (0xa2, 2000L, 2), (0xa3, 3000L, 3)))
        assert(pairs.map(_.porId) == Vector(porId(1), porId(2), porId(3)))
    }

    test("pairsOf on a TM with only the change output yields no pairs") {
        assert(pairsOrFail(Seq(change)).isEmpty)
    }

    test("pairsOf rejects a TM with no outputs at all") {
        assert(CompletedPegOutsTrie.pairsOf(Seq.empty).isLeft)
    }

    test("pairsOf rejects an odd output count after the change output") {
        val outs = tm((0xaa, 2000L, 1)) :+ payment(0xbb, 500L)
        assert(
          CompletedPegOutsTrie.pairsOf(outs) == Left("odd output count after the change output")
        )
    }

    test("pairsOf rejects a marker sitting in a payment position") {
        val outs = Seq(change, marker(porId(1)), marker(porId(2)))
        assert(CompletedPegOutsTrie.pairsOf(outs) == Left("POR marker in a payment position"))
    }

    test("pairsOf rejects a payment whose marker position holds a non-marker") {
        val outs = Seq(change, payment(0xaa, 2000L), payment(0xbb, 1L))
        assert(CompletedPegOutsTrie.pairsOf(outs) == Left("payment output without a POR marker"))
    }

    test("pairsOf rejects a peg-in \"BFR\" marker in the marker position") {
        val bfr = PegOutEntry(hex"6a23424652" ++ porId(1), 0)
        val outs = Seq(change, payment(0xaa, 2000L), bfr)
        assert(CompletedPegOutsTrie.pairsOf(outs) == Left("payment output without a POR marker"))
    }

    // --- replay ---

    test("replay of no Confirmed records is the empty trie, whose root is 32 zero bytes") {
        // The Aiken bootstrap mint pins exactly this root in the genesis trie datum.
        val tree = CompletedPegOutsTrie.replay(Seq.empty).toOption.get
        assert(tree.rootHash == ByteString.fromArray(Array.fill[Byte](32)(0)))
    }

    test("replay is independent of the order the Confirmed records are passed in") {
        val a = tm((0xa1, 1000L, 1))
        val b = tm((0xa2, 2000L, 2), (0xa3, 3000L, 3))
        val forward = CompletedPegOutsTrie.replay(Seq(a, b)).toOption.get.rootHash
        val backward = CompletedPegOutsTrie.replay(Seq(b, a)).toOption.get.rootHash
        assert(forward == backward)
    }

    test("replay records every pair of every Confirmed record") {
        val tree =
            CompletedPegOutsTrie
                .replay(Seq(tm((0xa1, 1000L, 1)), tm((0xa2, 2000L, 2))))
                .toOption
                .get
        assert(tree.get(porId(1)).contains(CompletedPegOutsTrie.trieValue(payment(0xa1, 1000L))))
        assert(tree.get(porId(2)).contains(CompletedPegOutsTrie.trieValue(payment(0xa2, 2000L))))
    }

    test("replay tolerates the same POR id with the same value in two records") {
        // The on-chain AlreadyPresent tolerance: a double-fulfillment inserted the key once.
        val dup = tm((0xa1, 1000L, 1))
        val tree = CompletedPegOutsTrie.replay(Seq(dup, dup)).toOption.get
        assert(tree.rootHash == CompletedPegOutsTrie.replay(Seq(dup)).toOption.get.rootHash)
    }

    test("replay rejects the same POR id recorded with two different values") {
        val res = CompletedPegOutsTrie.replay(Seq(tm((0xa1, 1000L, 1)), tm((0xa2, 1000L, 1))))
        assert(res.isLeft)
        assert(res.left.toOption.get.contains("different"))
    }

    test("replay reports a malformed Confirmed record rather than skipping it") {
        val res = CompletedPegOutsTrie.replay(Seq(Seq(change, payment(0xaa, 1L))))
        assert(res.left.toOption.get.contains("odd output count"))
    }

    // --- step building, cross-checked against the on-chain fold ---

    test("buildSteps on an empty trie: the on-chain fold accepts and reaches the same root") {
        val outs = tm((0xa1, 1000L, 1), (0xa2, 2000L, 2))
        val start = OffChainMPF.empty
        val (steps, end) = CompletedPegOutsTrie.buildSteps(start, pairsOrFail(outs)).toOption.get
        assert(steps.size == 2)
        assert(steps.forall(_.isInstanceOf[PegOutTrieStep.Insert]))
        assert(onChainFold(start, outs, steps) == end.rootHash)
    }

    test("buildSteps against a non-empty replayed trie is accepted by the on-chain fold") {
        // The realistic case: proofs must be produced against the CURRENT root, and each step
        // against the intermediate root the previous one produced.
        val history = Seq(tm((0xb1, 111L, 11)), tm((0xb2, 222L, 12), (0xb3, 333L, 13)))
        val start = CompletedPegOutsTrie.replay(history).toOption.get
        val outs = tm((0xa1, 1000L, 1), (0xa2, 2000L, 2), (0xa3, 3000L, 3))
        val (steps, end) = CompletedPegOutsTrie.buildSteps(start, pairsOrFail(outs)).toOption.get
        assert(onChainFold(start, outs, steps) == end.rootHash)
        assert(end.rootHash != start.rootHash)
    }

    test("buildSteps for a marker-free TM yields no steps and leaves the root unchanged") {
        val start = CompletedPegOutsTrie.replay(Seq(tm((0xb1, 111L, 11)))).toOption.get
        val (steps, end) =
            CompletedPegOutsTrie.buildSteps(start, pairsOrFail(Seq(change))).toOption.get
        assert(steps.isEmpty)
        assert(end.rootHash == start.rootHash)
        assert(onChainFold(start, Seq(change), steps) == start.rootHash)
    }

    test("buildSteps emits AlreadyPresent for a duplicate pair, and the fold keeps the root") {
        val outs = tm((0xa1, 1000L, 1))
        val start = CompletedPegOutsTrie.replay(Seq(outs)).toOption.get
        val (steps, end) = CompletedPegOutsTrie.buildSteps(start, pairsOrFail(outs)).toOption.get
        assert(steps == List(steps.head))
        assert(steps.head.isInstanceOf[PegOutTrieStep.AlreadyPresent])
        assert(end.rootHash == start.rootHash)
        assert(onChainFold(start, outs, steps) == start.rootHash)
    }

    test("buildSteps mixes AlreadyPresent and Insert in one TM") {
        val known = tm((0xa1, 1000L, 1))
        val start = CompletedPegOutsTrie.replay(Seq(known)).toOption.get
        val outs = tm((0xa1, 1000L, 1), (0xa2, 2000L, 2))
        val (steps, end) = CompletedPegOutsTrie.buildSteps(start, pairsOrFail(outs)).toOption.get
        assert(steps.head.isInstanceOf[PegOutTrieStep.AlreadyPresent])
        assert(steps(1).isInstanceOf[PegOutTrieStep.Insert])
        assert(onChainFold(start, outs, steps) == end.rootHash)
    }

    test("buildSteps refuses a POR id already recorded under a different value") {
        // Neither step variant accepts this, so the TM is permanently unconfirmable. Reporting it
        // is the point: heimdall's trie-dedup filter is what must stop such a TM being signed.
        val start = CompletedPegOutsTrie.replay(Seq(tm((0xa1, 1000L, 1)))).toOption.get
        val res = CompletedPegOutsTrie.buildSteps(start, pairsOrFail(tm((0xa2, 1000L, 1))))
        assert(res.isLeft)
        assert(res.left.toOption.get.contains("can never be confirmed"))
    }
}
