package binocular.oracle

import binocular.*
import binocular.bitcoin.*
import binocular.watchtower.*

import scalus.uplc.builtin.ByteString
import scalus.cardano.onchain.plutus.prelude
import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF
import scalus.utils.Hex.hexToBytes

import scala.concurrent.{ExecutionContext, Future}

/** Helper utilities for creating initial ChainState from Bitcoin node */
object BitcoinChainState {

    /** Convert Bitcoin RPC/display-order compact bits hex into internal little-endian bytes. */
    def rpcBitsToCompactBits(bitsHex: String): ByteString =
        ByteString.fromArray(bitsHex.hexToBytes.reverse)

    /** Compute MPF root hash for a single block (used for initial state creation) */
    def mpfRootForSingleBlock(blockHash: ByteString): ByteString =
        OffChainMPF.empty.insert(blockHash, blockHash).rootHash

    /** Build the confirmed-blocks MPF over the canonical hashes in `[startHeight, endHeight]`.
      *
      * Single source of the range->root walk: both oracle init (seeding `confirmedBlocksRoot`) and
      * [[binocular.cli.Command.rebuildMpf]] call this, so a seeded root can never diverge from a
      * later rebuild. Each block hash is stored in internal (little-endian) order, keyed by and
      * valued as itself – identical to `mpfRootForSingleBlock` for a one-element range.
      */
    def mpfForRange(
        rpc: BitcoinRpc,
        startHeight: Long,
        endHeight: Long
    )(using ec: ExecutionContext): Future[OffChainMPF] = {
        def loop(heights: scala.List[Long], mpf: OffChainMPF): Future[OffChainMPF] =
            heights match {
                case Nil => Future.successful(mpf)
                case h :: tail =>
                    for {
                        hashHex <- rpc.getBlockHash(h.toInt)
                        blockHash = ByteString.fromArray(hashHex.hexToBytes.reverse)
                        rest <- loop(tail, mpf.insert(blockHash, blockHash))
                    } yield rest
            }
        loop((startHeight to endHeight).toList, OffChainMPF.empty)
    }

    /** Build 80-byte raw header from block header info */
    private def buildRawHeader(header: BlockHeaderInfo): Array[Byte] = {
        val buffer = new Array[Byte](80)
        var offset = 0

        // Version (4 bytes, little-endian) - Use actual version from RPC
        val version = header.version.toInt
        buffer(offset) = (version & 0xff).toByte
        buffer(offset + 1) = ((version >> 8) & 0xff).toByte
        buffer(offset + 2) = ((version >> 16) & 0xff).toByte
        buffer(offset + 3) = ((version >> 24) & 0xff).toByte
        offset += 4

        // Previous block hash (32 bytes, reversed from hex)
        val prevHash = header.previousblockhash
            .map(h => h.hexToBytes.reverse)
            .getOrElse(new Array[Byte](32))
        System.arraycopy(prevHash, 0, buffer, offset, 32)
        offset += 32

        // Merkle root (32 bytes, reversed from hex)
        val merkleRoot = header.merkleroot.hexToBytes.reverse
        System.arraycopy(merkleRoot, 0, buffer, offset, 32)
        offset += 32

        // Timestamp (4 bytes, little-endian)
        val time = header.time.toInt
        buffer(offset) = (time & 0xff).toByte
        buffer(offset + 1) = ((time >> 8) & 0xff).toByte
        buffer(offset + 2) = ((time >> 16) & 0xff).toByte
        buffer(offset + 3) = ((time >> 24) & 0xff).toByte
        offset += 4

        // Bits (4 bytes, little-endian - must reverse from display hex to internal byte order)
        val bits = header.bits.hexToBytes.reverse
        System.arraycopy(bits, 0, buffer, offset, 4)
        offset += 4

        // Nonce (4 bytes, little-endian)
        val nonce = header.nonce.toInt
        buffer(offset) = (nonce & 0xff).toByte
        buffer(offset + 1) = ((nonce >> 8) & 0xff).toByte
        buffer(offset + 2) = ((nonce >> 16) & 0xff).toByte
        buffer(offset + 3) = ((nonce >> 24) & 0xff).toByte

        buffer
    }

    /** Convert BlockHeaderInfo to BlockHeader */
    def convertHeader(header: BlockHeaderInfo): BlockHeader =
        BlockHeader(ByteString.fromArray(buildRawHeader(header)))

    /** Fetch initial ChainState from Bitcoin RPC
      *
      * This creates a genesis ChainState for the oracle, starting from a specific block height. It
      * fetches the block header and the difficulty adjustment block to construct a valid initial
      * state.
      *
      * @param rpc
      *   BitcoinRpc client
      * @param blockHeight
      *   Bitcoin block height to start from
      * @param ec
      *   ExecutionContext for async operations
      * @return
      *   Future[ChainState] with initial oracle state
      */
    def getInitialChainState(
        rpc: BitcoinRpc,
        blockHeight: Int
    )(using ec: ExecutionContext): Future[ChainState] =
        getInitialChainState(rpc, blockHeight, blockHeight)

    /** Range-seeded init: ctx anchored at `confirmedTip`, `confirmedBlocksRoot` = MPF over the
      * canonical hashes `[startHeight, confirmedTip]`. When `startHeight == confirmedTip` this is
      * byte-identical to the previous single-block behavior.
      */
    def getInitialChainState(
        rpc: BitcoinRpc,
        startHeight: Int,
        confirmedTip: Int
    )(using ec: ExecutionContext): Future[ChainState] = {
        val blockHeight = confirmedTip
        val interval = BitcoinHelpers.DifficultyAdjustmentInterval.toInt
        val adjustmentBlockHeight = blockHeight - (blockHeight % interval)
        val medianTimeSpan = BitcoinHelpers.MedianTimeSpan.toInt // 11 blocks

        for {
            blockHashHex <- rpc.getBlockHash(blockHeight)
            header <- rpc.getBlockHeader(blockHashHex)
            adjustmentBlockHashHex <- rpc.getBlockHash(adjustmentBlockHeight)
            adjustmentHeader <- rpc.getBlockHeader(adjustmentBlockHashHex)

            // Fetch timestamps for the previous 11 blocks (for median-time-past validation)
            // Fetch sequentially to avoid rate limiting from RPC providers
            recentTimestampsSeq <- {
                def fetchTimestamps(
                    remaining: scala.List[Int],
                    acc: scala.List[BigInt]
                ): Future[scala.List[BigInt]] = {
                    remaining match {
                        case Nil => Future.successful(acc.reverse)
                        case h :: tail if h >= 0 =>
                            for {
                                hash <- rpc.getBlockHash(h)
                                hdr <- rpc.getBlockHeader(hash)
                                rest <- fetchTimestamps(tail, BigInt(hdr.time) :: acc)
                            } yield rest
                        case _ :: tail =>
                            fetchTimestamps(tail, BigInt(0) :: acc)
                    }
                }
                val heights = (0 until medianTimeSpan).map(i => blockHeight - i).toList
                fetchTimestamps(heights, Nil)
            }

            // Use the difficulty bits from the block at the most recent retarget boundary,
            // not the starting block. On testnet3/testnet4/regtest, the starting block could
            // itself be a min-difficulty block (powLimit), in which case its `bits` would no
            // longer represent the period's "real" difficulty needed to validate subsequent
            // blocks. The retarget block's bits always carry the period's true target.
            // Bits from RPC is in big-endian (display order); reverse to little-endian.
            bits = rpcBitsToCompactBits(adjustmentHeader.bits)
            // Block hash from RPC is in display order (big-endian), but we store it in internal order (little-endian)
            blockHash = ByteString.fromArray(header.hash.hexToBytes.reverse)
            // Confirmed set seeded over [startHeight, confirmedTip] (single-source range->root
            // walk); for startHeight == confirmedTip this reduces to the single-block root.
            confirmedRoot <- mpfForRange(rpc, startHeight, confirmedTip)
            // Timestamps are in block order (newest first) — matching how the validator
            // prepends each new block's timestamp during accumulation.
            recentTimestamps = recentTimestampsSeq.foldRight(
              prelude.List.Nil: prelude.List[BigInt]
            )((ts, acc) => prelude.List.Cons(ts, acc))
        } yield ChainState(
          confirmedBlocksRoot = confirmedRoot.rootHash, // MPF over [startHeight, confirmedTip]
          ctx = TraversalCtx(
            timestamps = recentTimestamps,
            height = blockHeight,
            currentBits = bits,
            prevDiffAdjTimestamp = BigInt(adjustmentHeader.time),
            lastBlockHash = blockHash
          ),
          forkTree = ForkTree.End // Initialize with empty forks tree
        )
    }
}
