package binocular
import binocular.BitcoinValidator.*
import org.apache.pekko.actor.ActorSystem
import scalus.bloxbean.Interop.??
import scalus.builtin.Builtins.*
import scalus.builtin.ByteString
import scalus.ledger.api.v2.TxOutRef
import scalus.prelude

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._
import scala.concurrent.Await

object Bitcoin {
    def isWitnessTransaction(rawTx: ByteString): Boolean =
        rawTx.at(4) == BigInt(0) && indexByteString(rawTx, 5) == BigInt(1)

    def makeCoinbaseTxFromByteString(rawTx: ByteString): CoinbaseTx = {
        val version = rawTx.slice(0, 4)
        if isWitnessTransaction(rawTx) then
            val (inputScriptSigAndSequence, txOutsOffset) =
                val scriptSigStart =
                    4 + 2 + 1 + 36 // version [4] + [marker][flag] 2 + txInCount [01] + txhash [32] + txindex [4]
                val (scriptLength, newOffset) = readVarInt(rawTx, scriptSigStart)
                val end = newOffset + scriptLength + 4
                val len = end - scriptSigStart
                (sliceByteString(scriptSigStart, len, rawTx), end)
            val txOutsAndLockTime =
                val outsEnd = skipTxOuts(rawTx, txOutsOffset)
                val lockTimeOffset = outsEnd + 1 + 1 + 32 // Skip witness data
                val txOutsLen = outsEnd - txOutsOffset
                val txOuts = sliceByteString(txOutsOffset, txOutsLen, rawTx)
                val lockTime = sliceByteString(lockTimeOffset, 4, rawTx)
                appendByteString(txOuts, lockTime)
            CoinbaseTx(
              version = version,
              inputScriptSigAndSequence = inputScriptSigAndSequence,
              txOutsAndLockTime
            )
        else
            val (inputScriptSigAndSequence, txOutsOffset) =
                val scriptSigStart =
                    4 + 1 + 36 // version [4] + txInCount [01] + txhash [32] + txindex [4]
                val (scriptLength, newOffset) = readVarInt(rawTx, scriptSigStart)
                val end = newOffset + scriptLength + 4
                val len = end - scriptSigStart
                (sliceByteString(scriptSigStart, len, rawTx), end)
            val txOutsAndLockTime =
                val outsEnd = skipTxOuts(rawTx, txOutsOffset)
                val lockTimeOffset = outsEnd
                val txOutsLen = outsEnd - txOutsOffset
                val len = txOutsLen + 4
                sliceByteString(txOutsOffset, len, rawTx)
            CoinbaseTx(
              version = version,
              inputScriptSigAndSequence = inputScriptSigAndSequence,
              txOutsAndLockTime
            )
    }
}

class HeaderSyncWithRpc(config: BitcoinNodeConfig)(using system: ActorSystem) {
    private given ec: ExecutionContext = system.dispatcher

    // Use simple RPC client (no bitcoin-s dependencies)
    private val rpc = new SimpleBitcoinRpc(config)

    // Convert hex string to ByteString  
    private def hexToByteString(hex: String): ByteString =
        ByteString.fromArray(hex.sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte))
    
    // Build 80-byte raw header from block header info
    private def buildRawHeader(header: BlockHeaderInfo): Array[Byte] = {
        val buffer = new Array[Byte](80)
        var offset = 0
        
        // Version (4 bytes, little-endian)
        buffer(offset) = (header.version & 0xFF).toByte
        buffer(offset + 1) = ((header.version >> 8) & 0xFF).toByte
        buffer(offset + 2) = ((header.version >> 16) & 0xFF).toByte
        buffer(offset + 3) = ((header.version >> 24) & 0xFF).toByte
        offset += 4
        
        // Previous block hash (32 bytes, reversed from hex)
        val prevHash = header.previousblockhash.map(h => hexToByteString(h).bytes.reverse.toArray).getOrElse(new Array[Byte](32))
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
        
        // Bits (4 bytes, reversed from hex)
        val bits = hexToByteString(header.bits).bytes.reverse.toArray
        System.arraycopy(bits, 0, buffer, offset, 4)
        offset += 4
        
        // Nonce (4 bytes, little-endian)
        val nonce = header.nonce.toInt
        buffer(offset) = (nonce & 0xFF).toByte
        buffer(offset + 1) = ((nonce >> 8) & 0xFF).toByte
        buffer(offset + 2) = ((nonce >> 16) & 0xFF).toByte
        buffer(offset + 3) = ((nonce >> 24) & 0xFF).toByte
        
        buffer
    }
    
    private def convertHeader(header: BlockHeaderInfo): BitcoinValidator.BlockHeader =
        BitcoinValidator.BlockHeader(ByteString.fromArray(buildRawHeader(header)))

    private def getInitialChainState(blockHeight: Int): Future[ChainState] = {
        val interval = BitcoinValidator.DifficultyAdjustmentInterval.toInt
        val adjustmentBlockHeight = blockHeight - (blockHeight % interval)
        for
            blockHashHex <- rpc.getBlockHash(blockHeight)
            header <- rpc.getBlockHeader(blockHashHex)
            adjustmentBlockHashHex <- rpc.getBlockHash(adjustmentBlockHeight)
            adjustmentHeader <- rpc.getBlockHeader(adjustmentBlockHashHex)
            bits = hexToByteString(header.bits)
            blockHash = hexToByteString(header.hash)
        yield ChainState(
          blockHeight = blockHeight,
          blockHash = blockHash,
          currentTarget = bits,
          blockTimestamp = BigInt(header.time),
          recentTimestamps = prelude.List(BigInt(header.time)),
          previousDifficultyAdjustmentTimestamp = BigInt(adjustmentHeader.time),
          confirmedBlocksTree = prelude.List(blockHash), // Single-element Merkle tree
          forksTree = prelude.AssocMap.empty // Initialize with empty forks tree
        )
    }

    private def processHeader(currentState: ChainState, headerInfo: BlockHeaderInfo): ChainState =
        val header = convertHeader(headerInfo)
        val currentTime = BigInt(System.currentTimeMillis() / 1000)
        BitcoinValidator.updateTip(currentState, header, currentTime)

    private def syncLoop(height: Int, targetHeight: Int, currentState: ChainState): Future[Unit] =
        if height > targetHeight then Future.successful(())
        else
            for
                blockHashHex <- rpc.getBlockHash(height)
                header <- rpc.getBlockHeader(blockHashHex)
                newState =
                    try
                        val newState = processHeader(currentState, header)
                        print(
                          s"\rValidated header at height $height: ${header.hash}, bits: ${header.bits}"
                        )
                        newState
                    catch
                        case e: Exception =>
                            println(s"Validation failed at height $height: ${e.getMessage}")
                            throw e
                _ <- syncLoop(height + 1, targetHeight, newState)
            yield ()

    def syncHeadersFrom(startingBlockHeight: Int): Future[Unit] =
        for
            info <- rpc.getBlockchainInfo()
            initialState <- getInitialChainState(startingBlockHeight)
            latestHeight = info.blocks
            _ <- syncLoop(startingBlockHeight + 1, latestHeight, initialState)
        yield ()

    def printNodeInfo(): Future[Unit] =
        for
            info <- rpc.getBlockchainInfo()
            _ = println(s"""
                           |Bitcoin Node Info:
                           |Chain: ${info.chain}
                           |Blocks: ${info.blocks}
                           |Headers: ${info.headers}
                           |""".stripMargin)
        yield ()
}

object HeaderSyncWithRpc {
    def main(args: Array[String]): Unit =

        // Load Bitcoin node configuration
        val config = BitcoinNodeConfig.load() match {
            case Right(cfg) =>
                println(s"✓ Loaded Bitcoin node config: $cfg")
                cfg
            case Left(error) =>
                System.err.println(s"✗ Failed to load Bitcoin node configuration: $error")
                System.err.println("\nPlease configure Bitcoin node connection via:")
                System.err.println("  1. Environment variables: BITCOIN_NODE_URL, BITCOIN_NODE_USER, BITCOIN_NODE_PASSWORD")
                System.err.println("  2. application.conf: binocular.bitcoin-node.*")
                sys.exit(1)
        }

        // Validate configuration
        config.validate() match {
            case Left(error) =>
                System.err.println(s"✗ Invalid configuration: $error")
                sys.exit(1)
            case Right(_) => ()
        }

        given system: ActorSystem = ActorSystem("bitcoin-s")
        given ec: ExecutionContext = system.dispatcher

        val syncer = HeaderSyncWithRpc(config)

        // Main execution
        val program =
            for
                _ <- syncer.printNodeInfo()
                _ <- syncer.syncHeadersFrom(866870)
            yield ()

        program
            .recover:
                case e: Exception =>
                    println(s"Sync failed: ${e.getMessage}")
            .onComplete: _ =>
                println("Terminating actor system...")
                system.terminate()
                sys.exit(0)
}


def useBinocular() = {
    def getBinocularUtxo: TxOutRef = ???
    def selectBitcoinBlock() = ??? // random previous block which exists in Binocular `blocks`
    def selectBitcoinTxInThatBlock() = ??? // random previous block which exists in Binocular `blocks`
    // separate DApp, like DEX or some other onchain application    
    def makeProofTx = {
        val bincocularRefInput = getBinocularUtxo
        val binocularDatum: ChainState  = ???
        // onchain validation of Bitcoin tx inclusion:
        
        // block header exists in binocularDatum.blocks
        // merkle proof of block header hash inclusion in binocularDatum.blocksMerkleRoot
        // tx hash exists in block's tx merkle root
    }

}