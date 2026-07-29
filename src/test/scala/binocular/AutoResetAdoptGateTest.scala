package binocular

import binocular.bitcoin.*
import binocular.cli.CommandHelpers
import binocular.oracle.*

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude
import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF
import scalus.uplc.builtin.ByteString

import scala.concurrent.{ExecutionContext, Future}

/** [[CommandHelpers.autoResetAdoptableMpf]] is the daemon's auto-reset adopt gate: it must return
  * `None` (reset required) whenever the oracle's confirmed tip is no longer canonical-by-height —
  * even when bitcoind still serves the orphaned headers by hash, which lets the committed MPF
  * reconstruct cleanly against the oracle's own root. Adopting such a state loops the daemon
  * forever: detect deep reorg -> adopt stale state -> detect again (observed on preprod 2026-07-28,
  * one orphaned confirmed block at height 146093).
  */
class AutoResetAdoptGateTest extends AnyFunSuite {

    private given ExecutionContext = ExecutionContext.global

    /** Little-endian (internal order) 32-byte hash with a distinctive first byte. */
    private def leHash(id: Int): ByteString =
        ByteString.unsafeFromArray(id.toByte +: Array.fill(31)(0: Byte))

    /** bitcoind display order: byte-reversed hex of the internal little-endian hash. */
    private def displayHex(le: ByteString): String =
        le.toHex.grouped(2).toList.reverse.mkString

    private def header(hash: String, height: Int, prev: Option[String]): BlockHeaderInfo =
        BlockHeaderInfo(hash, height, 4, "00" * 32, 1000000L, 0L, "1d00ffff", 1.0, prev)

    /** Canonical chain by height + headers by display hash (covers orphans bitcoind retains). */
    private class StubRpc(
        canonicalByHeight: Map[Int, ByteString],
        headers: Map[String, BlockHeaderInfo]
    ) extends BitcoinRpc {
        def getBlockHash(height: Int): Future[String] =
            canonicalByHeight.get(height) match {
                case Some(le) => Future.successful(displayHex(le))
                case scala.None =>
                    Future.failed(new RuntimeException(s"no block at height $height"))
            }
        def getBlockHeader(hash: String): Future[BlockHeaderInfo] =
            headers.get(hash) match {
                case Some(h)    => Future.successful(h)
                case scala.None => Future.failed(new RuntimeException(s"unknown block $hash"))
            }
        def getBlock(hash: String): Future[BlockInfo] =
            Future.failed(new UnsupportedOperationException)
        def getBlockchainInfo(): Future[BlockchainInfo] =
            Future.failed(new UnsupportedOperationException)
        def getRawTransaction(txid: String): Future[RawTransactionInfo] =
            Future.failed(new UnsupportedOperationException)
        def sendRawTransaction(hexString: String): Future[String] =
            Future.failed(new UnsupportedOperationException)
    }

    private def stateWith(tip: ByteString, height: Int, root: ByteString): ChainState =
        ChainState(
          confirmedBlocksRoot = root,
          ctx = TraversalCtx(
            timestamps = prelude.List.from((0 until 11).map(i => BigInt(1000000 - i * 600)).toList),
            height = height,
            currentBits = ByteString.unsafeFromArray(Array.fill(4)(0xff.toByte)),
            prevDiffAdjTimestamp = 1000000,
            lastBlockHash = tip
          ),
          forkTree = ForkTree.End
        )

    private def mpfOf(hashes: ByteString*): OffChainMPF =
        hashes.foldLeft(OffChainMPF.empty)((m, h) => m.insert(h, h))

    // Oracle confirmed 98..100; a reorg orphaned block 100 (o100 -> c100), like preprod 2026-07-28.
    private val o97 = leHash(0x97)
    private val o98 = leHash(0x98)
    private val o99 = leHash(0x99)
    private val o100 = leHash(0xa0)
    private val c100 = leHash(0xb0)

    test("does not adopt a state whose confirmed tip was orphaned even if headers are walkable") {
        val rpc = new StubRpc(
          canonicalByHeight = Map(98 -> o98, 99 -> o99, 100 -> c100),
          headers = Map(
            displayHex(o100) -> header(displayHex(o100), 100, Some(displayHex(o99))),
            displayHex(o99) -> header(displayHex(o99), 99, Some(displayHex(o98))),
            displayHex(o98) -> header(displayHex(o98), 98, Some(displayHex(o97)))
          )
        )
        val state = stateWith(o100, 100, mpfOf(o100, o99, o98).rootHash)
        assert(CommandHelpers.autoResetAdoptableMpf(rpc, state, Some(98L)).isEmpty)
    }

    test("does not adopt a single-block state whose only confirmed block was orphaned") {
        val rpc = new StubRpc(
          canonicalByHeight = Map(100 -> c100),
          headers = Map(displayHex(o100) -> header(displayHex(o100), 100, Some(displayHex(o99))))
        )
        val state = stateWith(o100, 100, mpfOf(o100).rootHash)
        assert(CommandHelpers.autoResetAdoptableMpf(rpc, state, Some(100L)).isEmpty)
    }

    test("adopts a state whose confirmed tip is canonical") {
        val rpc = new StubRpc(
          canonicalByHeight = Map(98 -> o98, 99 -> o99, 100 -> o100),
          headers = Map.empty
        )
        val state = stateWith(o100, 100, mpfOf(o98, o99, o100).rootHash)
        val result = CommandHelpers.autoResetAdoptableMpf(rpc, state, Some(98L))
        assert(result.exists(_.exists(_.rootHash == state.confirmedBlocksRoot)))
    }
}
