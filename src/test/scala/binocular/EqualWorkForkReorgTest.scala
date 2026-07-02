package binocular

import binocular.bitcoin.*
import binocular.cli.CommandHelpers
import binocular.oracle.*
import binocular.oracle.ForkTree.*
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF
import scalus.uplc.builtin.ByteString

import java.io.{ByteArrayOutputStream, PrintStream}
import scala.concurrent.{ExecutionContext, Future}

/** Regression test for the "Reorg detected at height N … depth=0" log-spam loop.
  *
  * When two blocks exist at the tip height with equal chainwork (a natural Bitcoin fork), the
  * oracle's deterministic tie-break can pick a different tip than bitcoind's first-seen choice. The
  * canonical block is nonetheless already in the fork tree, so this is NOT an actionable reorg —
  * there is nothing to submit until Bitcoin extends one branch. It must be reported once (not the
  * full multi-line reorg report on every poll) and must return the path to the canonical sibling so
  * a later block builds on it.
  */
class EqualWorkForkReorgTest extends AnyFunSuite {

    private given ExecutionContext = ExecutionContext.parasitic

    // 32-byte hash from a short hex tag padded with zero bytes.
    private def hash32(tag: String): ByteString =
        ByteString.fromHex(tag + "00" * (32 - tag.length / 2))

    private def block(tag: String): BlockSummary =
        BlockSummary(hash = hash32(tag), timestamp = 100, addedTimeDelta = 0)

    /** RPC whose canonical hash at `height` is `canonicalTip`, returned big-endian (display) so the
      * caller's `.hexToBytes.reverse` yields the internal little-endian hash back.
      */
    private class SiblingRpc(height: Int, canonicalTip: ByteString) extends BitcoinRpc {
        def getBlockHash(h: Int): Future[String] =
            if h == height then
                Future.successful(ByteString.fromArray(canonicalTip.bytes.reverse).toHex)
            else Future.failed(new RuntimeException(s"unexpected getBlockHash($h)"))
        def getBlockHeader(hash: String): Future[BlockHeaderInfo] = ???
        def getBlock(hash: String): Future[BlockInfo] = ???
        def getBlockchainInfo(): Future[BlockchainInfo] = ???
        def getRawTransaction(txid: String): Future[RawTransactionInfo] = ???
        def sendRawTransaction(hexString: String): Future[String] = ???
    }

    private def state(forkTree: ForkTree): ChainState =
        ChainState(
          confirmedBlocksRoot = ByteString.fromHex("00" * 32),
          ctx = TraversalCtx(
            timestamps = PList.from((0 until 11).map(_ => BigInt(100)).toList),
            height = BigInt(100),
            currentBits = ByteString.fromHex("1d00ffff"),
            prevDiffAdjTimestamp = BigInt(100),
            lastBlockHash = hash32("ab")
          ),
          forkTree = forkTree
        )

    private def capture(body: => Unit): String = {
        val baos = new ByteArrayOutputStream()
        scala.Console.withOut(new PrintStream(baos))(body)
        baos.toString("UTF-8")
    }

    test("equal-work sibling is not a reorg: quiet (once), correct path, no spam loop") {
        // Two competing single-block branches at height 101, equal chainwork. Oracle best tie-breaks
        // to the LEFT branch (a1); bitcoind's canonical tip is the RIGHT branch (b1).
        val left = Blocks(PList.from(List(block("a1"))), chainwork = 100, next = End)
        val right = Blocks(PList.from(List(block("b1"))), chainwork = 100, next = End)
        val tree: ForkTree = Fork(left, right)
        val cs = state(tree)

        assert(tree.bestChainTipHash.contains(block("a1").hash), "oracle best must be the left tip")

        val rpc = new SiblingRpc(height = 101, canonicalTip = block("b1").hash)

        // Two consecutive polls of the identical fork.
        val out = capture {
            val (p1, s1) = CommandHelpers.detectReorgAndComputePath(rpc, cs, OffChainMPF.empty)
            val (p2, s2) = CommandHelpers.detectReorgAndComputePath(rpc, cs, OffChainMPF.empty)

            // Path points at the canonical sibling (right branch → [1, 0]); start is above the tie.
            assert(p1.toScalaList == List(BigInt(1), BigInt(0)), s"path1=$p1")
            assert(p2.toScalaList == List(BigInt(1), BigInt(0)), s"path2=$p2")
            assert(s1 == BigInt(102) && s2 == BigInt(102))
        }

        // The scary reorg report must NOT appear for a mere equal-work tie.
        assert(
          !out.contains("Reorg detected at height"),
          s"equal-work sibling must not be logged as a reorg:\n$out"
        )
        // The benign note is emitted once, not on every poll.
        val notes = "Equal-work fork at height 101".r.findAllIn(out).size
        assert(notes == 1, s"expected exactly one equal-work-fork note, got $notes in:\n$out")
    }
}
