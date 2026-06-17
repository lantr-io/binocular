package binocular

import binocular.attack.ForkPoint
import binocular.bitcoin.BitcoinHelpers.*
import binocular.oracle.*
import binocular.oracle.ForkTree.*
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.uplc.builtin.ByteString

class ForkPointTest extends AnyFunSuite {

    // 32-byte hash from a short hex tag (e.g. "a1") padded with zero bytes.
    private def hash32(tag: String): ByteString =
        ByteString.fromHex(tag + "00" * (32 - tag.length / 2))

    private def summary(tag: String, ts: BigInt): BlockSummary =
        BlockSummary(hash = hash32(tag), timestamp = ts, addedTimeDelta = 0)

    // A single linear branch A,B,C above the confirmed tip.
    private def linearTree: ForkTree = {
        val a = summary("a1", 100)
        val b = summary("b2", 200)
        val c = summary("c3", 300)
        Blocks(PList.from(List(a, b, c)), chainwork = 30, next = End)
    }

    test("parent=0 resolves to the best-chain tip") {
        val (path, _) = ForkPoint.resolve(linearTree, "0")
        assert(path == PList.from(List(BigInt(2)))) // tip is index 2 in the single Blocks node
    }

    test("parent=1 resolves to one block back from the tip") {
        val (path, _) = ForkPoint.resolve(linearTree, "1")
        assert(path == PList.from(List(BigInt(1))))
    }

    test("parent depth past the branch falls back to the confirmed tip (empty path)") {
        val (path, hashOpt) = ForkPoint.resolve(linearTree, "99")
        assert(path == PList.Nil)
        assert(hashOpt.isEmpty)
    }

    test("parent by explicit hash resolves to that block") {
        val target = hash32("b2")
        val (path, _) = ForkPoint.resolve(linearTree, target.toHex)
        assert(path == PList.from(List(BigInt(1))))
    }

    test("empty tree always resolves to the confirmed tip") {
        val (path, hashOpt) = ForkPoint.resolve(ForkTree.End, "0")
        assert(path == PList.Nil)
        assert(hashOpt.isEmpty)
    }

    test("ctxAtPath at the tip equals accumulating the whole best chain") {
        val p = BitcoinValidatorParams.makeRegtest(
          scalus.cardano.onchain.plutus.v3
              .TxOutRef(scalus.cardano.onchain.plutus.v3.TxId(ByteString.fromHex("00" * 32)), 0),
          scalus.cardano.onchain.plutus.v1.PubKeyHash(ByteString.fromHex("00" * 28))
        )
        val root = TraversalCtx(
          timestamps = PList.from((0 until 11).map(_ => BigInt(50)).toList),
          height = BigInt(10),
          currentBits = targetToCompactByteString(p.powLimit),
          prevDiffAdjTimestamp = 50,
          lastBlockHash = ByteString.fromHex("00" * 32)
        )
        val (path, _) = ForkPoint.resolve(linearTree, "0")
        val ctx = ForkPoint.ctxAtPath(linearTree, path, root, p.powLimit)
        assert(ctx.height == BigInt(13)) // 10 + 3 blocks
        assert(ctx.lastBlockHash == hash32("c3"))
    }
}
