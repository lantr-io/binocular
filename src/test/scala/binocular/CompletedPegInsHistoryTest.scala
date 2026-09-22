package binocular

import binocular.watchtower.*
import org.scalatest.funsuite.AnyFunSuite
import scalus.crypto.trie.MerklePatriciaForestry as MPF
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Data.toData

class CompletedPegInsHistoryTest extends AnyFunSuite {
    private def id(n: Int) = ByteString.fromHex(f"$n%02x" * 32 + "00000000")
    private val a = id(1)
    private val b = id(2)
    private val c = id(3)
    private val va = id(11)
    private val vb = id(12)
    private val vc = id(13)
    private val swept = MPF.empty.insert(a, va).insert(b, vb).insert(c, vc)
    private val first = MPF.empty.insert(a, va)
    private val second = first.insert(b, vb)

    test("reconstructs completions without treating all swept deposits as completed") {
        val result = CompletedPegInsHistory
            .replay(
              second.rootHash,
              Seq(second.rootHash, first.rootHash),
              Seq(c, b, a),
              swept
            )
            .toOption
            .get
        assert(result.tree.rootHash == second.rootHash)
        assert(result.tree.get(c).isEmpty)
        assert(result.tree.get(a).contains(va))
        assert(result.tree.get(b).contains(vb))
    }

    test("an empty live root needs no history") {
        val result = CompletedPegInsHistory.replay(MPF.empty.rootHash, Nil, Nil, swept)
        assert(result.toOption.get.tree.rootHash == MPF.empty.rootHash)
    }

    test("missing intermediate roots fail closed") {
        assert(CompletedPegInsHistory.replay(second.rootHash, Nil, Seq(a, b), swept).isLeft)
    }

    test("missing prior requests fail closed") {
        assert(
          CompletedPegInsHistory
              .replay(
                second.rootHash,
                Seq(first.rootHash),
                Seq(b, c),
                swept
              )
              .isLeft
        )
    }

    test("a closed but unswept request is not a completion") {
        val result = CompletedPegInsHistory
            .replay(
              second.rootHash,
              Seq(first.rootHash),
              Seq(a, b, id(99)),
              swept
            )
            .toOption
            .get
        assert(result.tree.get(a).contains(va))
        assert(result.tree.get(b).contains(vb))
        assert(result.tree.get(id(99)).isEmpty)
    }

    test("duplicate pages and out-of-order history are harmless") {
        val result = CompletedPegInsHistory
            .replay(
              second.rootHash,
              Seq(first.rootHash, first.rootHash),
              Seq(b, a, a, b),
              swept
            )
            .toOption
            .get
        assert(result.tree.rootHash == second.rootHash)
        assert(result.tree.get(a).contains(va))
        assert(result.tree.get(b).contains(vb))
    }

    test("a reorg branch cannot hide the path to the live root") {
        val other = MPF.empty.insert(c, vc)
        val result = CompletedPegInsHistory
            .replay(
              second.rootHash,
              Seq(other.rootHash, first.rootHash),
              Seq(c, a, b),
              swept
            )
            .toOption
            .get
        assert(result.tree.rootHash == second.rootHash)
        assert(result.tree.get(a).contains(va))
        assert(result.tree.get(b).contains(vb))
        assert(result.tree.get(c).isEmpty)
    }

    test("values come from the confirmed swept trie, never from the deposit id") {
        val wrong = MPF.empty.insert(a, a)
        assert(CompletedPegInsHistory.replay(wrong.rootHash, Nil, Seq(a), swept).isLeft)
    }

    test("a root not reachable from the available history is rejected") {
        assert(
          CompletedPegInsHistory
              .replay(
                MPF.empty.insert(id(88), id(89)).rootHash,
                Seq(first.rootHash),
                Seq(a, b, c),
                swept
              )
              .isLeft
        )
    }

    private val pirPolicy = "ab" * 28
    private val cpiPolicy = "cd" * 28
    private val cpiAsset = "434950"
    private def request(key: ByteString) = ChainOutput(
      key.take(32),
      0,
      Some(
        PegInDatum(
          AuthorizationMethod.CardanoSignature(ByteString.empty),
          ByteString.empty,
          0,
          key,
          30000,
          ByteString.fromHex("12" * 32),
          0
        ).toData
      ),
      Map((pirPolicy + "ff" * 32) -> BigInt(1))
    )
    private def singleton(root: ByteString) = ChainOutput(
      id(20).take(32),
      0,
      Some(CompletedPegInsMerkleTreeDatum(root).toData),
      Map((cpiPolicy + cpiAsset) -> BigInt(1))
    )
    private def reconstruct(requests: Seq[ChainOutput], roots: Seq[ChainOutput]) = {
        val history = new CpoHistorySource {
            def backend = "test"
            def addressHistory(address: String) = Right(
              if address == "pir" then requests else roots
            )
        }
        CompletedPegInsHistory.reconstruct(
          history,
          "pir",
          "cpi",
          pirPolicy,
          cpiPolicy,
          cpiAsset,
          second.rootHash,
          swept
        )
    }

    test("history adapter authenticates requests and singleton roots by their tokens") {
        assert(reconstruct(Seq(request(a), request(b)), Seq(singleton(first.rootHash))).isRight)
        assert(
          reconstruct(
            Seq(request(a).copy(assets = Map.empty), request(b)),
            Seq(singleton(first.rootHash))
          ).isLeft
        )
        assert(
          reconstruct(
            Seq(request(a), request(b)),
            Seq(singleton(first.rootHash).copy(assets = Map.empty))
          ).isLeft
        )
    }

    test("missing or unreadable required datums cannot become an empty completed set") {
        assert(
          reconstruct(
            Seq(request(a).copy(inlineDatum = None), request(b)),
            Seq(singleton(first.rootHash))
          ).isLeft
        )
        assert(
          reconstruct(
            Seq(request(a), request(b)),
            Seq(singleton(first.rootHash).copy(inlineDatum = None))
          ).isLeft
        )
    }

    test("history failures propagate without guessing a completion") {
        val history = new CpoHistorySource {
            def backend = "test"
            def addressHistory(address: String) = Left(HistoryError.transient("unavailable"))
        }
        assert(
          CompletedPegInsHistory.reconstruct(
            history,
            "pir",
            "cpi",
            pirPolicy,
            cpiPolicy,
            cpiAsset,
            first.rootHash,
            swept
          ) == Left("unavailable")
        )
    }
}
