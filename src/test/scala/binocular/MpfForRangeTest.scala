package binocular

import binocular.oracle.*
import binocular.cli.CommandHelpers

import org.scalatest.funsuite.AnyFunSuite
import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF
import scalus.uplc.builtin.ByteString
import scalus.utils.await
import scalus.utils.Hex.hexToBytes

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*

class MpfForRangeTest extends AnyFunSuite {
    private given ec: ExecutionContext = ExecutionContext.global
    private val rpc = new MockBitcoinRpc()

    // Independent reference: internal-order canonical hash, keyed by and valued as itself.
    private def refRoot(start: Long, end: Long): ByteString = {
        val mpf = (start to end).foldLeft(OffChainMPF.empty) { (acc, h) =>
            val hex = rpc.getBlockHash(h.toInt).await(30.seconds)
            val hash = ByteString.fromArray(hex.hexToBytes.reverse)
            acc.insert(hash, hash)
        }
        mpf.rootHash
    }

    test("mpfForRange matches an independent walk over the same range") {
        val start = 864800L
        val end = 864805L
        val root = BitcoinChainState.mpfForRange(rpc, start, end).await(30.seconds).rootHash
        assert(root == refRoot(start, end))
    }

    test("mpfForRange with start == end equals mpfRootForSingleBlock") {
        val h = 864800L
        val hex = rpc.getBlockHash(h.toInt).await(30.seconds)
        val hash = ByteString.fromArray(hex.hexToBytes.reverse)
        val root = BitcoinChainState.mpfForRange(rpc, h, h).await(30.seconds).rootHash
        assert(root == BitcoinChainState.mpfRootForSingleBlock(hash))
    }

    test("rebuildMpf returns Right when expectedRoot equals the range root, Left otherwise") {
        val start = 864800L
        val end = 864805L
        val seedRoot = BitcoinChainState.mpfForRange(rpc, start, end).await(30.seconds).rootHash

        assert(CommandHelpers.rebuildMpf(rpc, start, end, seedRoot).isRight)

        val wrong = BitcoinChainState.mpfRootForSingleBlock(
          ByteString.fromArray(Array.fill[Byte](32)(0))
        )
        assert(CommandHelpers.rebuildMpf(rpc, start, end, wrong).isLeft)
    }
}
