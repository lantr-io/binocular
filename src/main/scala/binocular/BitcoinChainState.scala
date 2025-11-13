package binocular

import binocular.BitcoinValidator.*
import scalus.builtin.ByteString
import scalus.prelude

import scala.concurrent.{ExecutionContext, Future}

/** Helper utilities for creating initial ChainState from Bitcoin node */
object BitcoinChainState {

    /** Convert hex string to ByteString */
    private def hexToByteString(hex: String): ByteString =
        ByteString.fromArray(hex.sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte))

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
        buffer(offset) = (version & 0xFF).toByte
        buffer(offset + 1) = ((version >> 8) & 0xFF).toByte
        buffer(offset + 2) = ((version >> 16) & 0xFF).toByte
        buffer(offset + 3) = ((version >> 24) & 0xFF).toByte
        offset += 4

        // Previous block hash (32 bytes, reversed from hex)
        val prevHash = header.previousblockhash
          .map(h => hexToByteString(h).bytes.reverse.toArray)
          .getOrElse(new Array[Byte](32))
        System.arraycopy(prevHash, 0, buffer, offset, 32)
        offset += 32

        // Merkle root (32 bytes, reversed from hex)
        val merkleRoot = hexToByteString(header.merkleroot).bytes.reverse.toArray
        System.arraycopy(merkleRoot, 0, buffer, offset, 32)
        offset += 32

        // Timestamp (4 bytes, little-endian)
        val time = header.time.toInt
        buffer(offset) = (time & 0xFF).toByte
        buffer(offset + 1) = ((time >> 8) & 0xFF).toByte
        buffer(offset + 2) = ((time >> 16) & 0xFF).toByte
        buffer(offset + 3) = ((time >> 24) & 0xFF).toByte
        offset += 4

        // Bits (4 bytes, little-endian - must reverse from display hex to internal byte order)
        val bits = hexToByteString(header.bits).bytes.reverse.toArray
        System.arraycopy(bits, 0, buffer, offset, 4)
        offset += 4

        // Nonce (4 bytes, little-endian)
        val nonce = header.nonce.toInt
        buffer(offset) = (nonce & 0xFF).toByte
        buffer(offset + 1) = ((nonce >> 8) & 0xFF).toByte
        buffer(offset + 2) = ((nonce >> 16) & 0xFF).toByte
        buffer(offset + 3) = ((nonce >> 24) & 0xFF).toByte

        val headerHex = buffer.map(b => f"${b & 0xFF}%02x").mkString
        println(s"  Built header (80 bytes): $headerHex")

        buffer
    }

    /** Convert BlockHeaderInfo to BitcoinValidator.BlockHeader */
    def convertHeader(header: BlockHeaderInfo): BitcoinValidator.BlockHeader =
        BitcoinValidator.BlockHeader(ByteString.fromArray(buildRawHeader(header)))

    /** Fetch initial ChainState from Bitcoin RPC
      *
      * This creates a genesis ChainState for the oracle, starting from a specific block height.
      * It fetches the block header and the difficulty adjustment block to construct a valid
      * initial state.
      *
      * @param rpc SimpleBitcoinRpc client
      * @param blockHeight Bitcoin block height to start from
      * @param ec ExecutionContext for async operations
      * @return Future[ChainState] with initial oracle state
      */
    def getInitialChainState(
        rpc: SimpleBitcoinRpc,
        blockHeight: Int
    )(using ec: ExecutionContext): Future[ChainState] = {
        val interval = BitcoinValidator.DifficultyAdjustmentInterval.toInt
        val adjustmentBlockHeight = blockHeight - (blockHeight % interval)

        for {
            blockHashHex <- rpc.getBlockHash(blockHeight)
            header <- rpc.getBlockHeader(blockHashHex)
            adjustmentBlockHashHex <- rpc.getBlockHash(adjustmentBlockHeight)
            adjustmentHeader <- rpc.getBlockHeader(adjustmentBlockHashHex)
            // Bits from RPC is in big-endian (display order), but Bitcoin headers use little-endian
            // Reverse the bytes to match the format in the block header
            bits = ByteString.fromArray(hexToByteString(header.bits).bytes.reverse.toArray)
            // Block hash from RPC is in display order (big-endian), but we store it in internal order (little-endian)
            blockHash = ByteString.fromArray(hexToByteString(header.hash).bytes.reverse.toArray)
        } yield ChainState(
          blockHeight = blockHeight,
          blockHash = blockHash,
          currentTarget = bits,
          blockTimestamp = BigInt(header.time),
          recentTimestamps = prelude.List(BigInt(header.time)),
          previousDifficultyAdjustmentTimestamp = BigInt(adjustmentHeader.time),
          confirmedBlocksTree = prelude.List(blockHash), // Single-element Merkle tree: [blockHash]
          forksTree = prelude.AssocMap.empty // Initialize with empty forks tree
        )
    }
}
