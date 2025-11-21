package binocular

import binocular.BitcoinValidator.*
import scalus.builtin.ByteString
import scalus.prelude
import scalus.utils.Hex.hexToBytes

import scala.concurrent.{ExecutionContext, Future}

/** Helper utilities for creating initial ChainState from Bitcoin node */
object BitcoinChainState {

    /** Build 80-byte raw header from block header info */
    private def buildRawHeader(header: BlockHeaderInfo): Array[Byte] = {
        println(s"[DEBUG buildRawHeader] Building header for block ${header.height}")
        println(s"  Version: ${header.version} (0x${header.version.toHexString})")
        println(s"  Prev hash: ${header.previousblockhash.getOrElse("GENESIS")}")
        println(s"  Merkle root: ${header.merkleroot}")
        println(s"  Timestamp: ${header.time}")
        println(s"  Bits: ${header.bits}")
        println(s"  Nonce: ${header.nonce}")

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

        val headerHex = buffer.map(b => f"${b & 0xff}%02x").mkString
        println(s"  Built header (80 bytes): $headerHex")

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
      *   SimpleBitcoinRpc client
      * @param blockHeight
      *   Bitcoin block height to start from
      * @param ec
      *   ExecutionContext for async operations
      * @return
      *   Future[ChainState] with initial oracle state
      */
    def getInitialChainState(
        rpc: SimpleBitcoinRpc,
        blockHeight: Int
    )(using ec: ExecutionContext): Future[ChainState] = {
        val interval = BitcoinValidator.DifficultyAdjustmentInterval.toInt
        val adjustmentBlockHeight = blockHeight - (blockHeight % interval)
        val medianTimeSpan = BitcoinValidator.MedianTimeSpan.toInt // 11 blocks

        for {
            blockHashHex <- rpc.getBlockHash(blockHeight)
            header <- rpc.getBlockHeader(blockHashHex)
            adjustmentBlockHashHex <- rpc.getBlockHash(adjustmentBlockHeight)
            adjustmentHeader <- rpc.getBlockHeader(adjustmentBlockHashHex)

            // Fetch timestamps for the previous 11 blocks (for median-time-past validation)
            // Fetch sequentially to avoid rate limiting from RPC providers
            recentTimestampsSeq <- {
                def fetchTimestamps(remaining: scala.List[Int], acc: scala.List[BigInt]): Future[scala.List[BigInt]] = {
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

            // Bits from RPC is in big-endian (display order), but Bitcoin headers use little-endian
            // Reverse the bytes to match the format in the block header
            bits = ByteString.fromArray(header.bits.hexToBytes.reverse)
            // Block hash from RPC is in display order (big-endian), but we store it in internal order (little-endian)
            blockHash = ByteString.fromArray(header.hash.hexToBytes.reverse)
            // Sort timestamps by value (descending) - required for median-time-past calculation
            // Bitcoin allows out-of-order timestamps, so block height order != timestamp order
            sortedTimestamps = recentTimestampsSeq.sortBy(-_) // Sort descending by timestamp value
            // Convert to scalus List
            recentTimestamps = sortedTimestamps.foldRight(prelude.List.Nil: prelude.List[BigInt])((ts, acc) =>
                prelude.List.Cons(ts, acc)
            )
        } yield ChainState(
          blockHeight = blockHeight,
          blockHash = blockHash,
          currentTarget = bits,
          blockTimestamp = BigInt(header.time),
          recentTimestamps = recentTimestamps,
          previousDifficultyAdjustmentTimestamp = BigInt(adjustmentHeader.time),
          confirmedBlocksTree = prelude.List(blockHash), // Single-element Merkle tree: [blockHash]
          forksTree = prelude.List.Nil // Initialize with empty forks tree
        )
    }
}
