package binocular
import binocular.BitcoinHelpers.*
import org.apache.pekko.actor.ActorSystem
import scalus.cardano.onchain.plutus.prelude
import scalus.cardano.onchain.plutus.v1.TxOutRef
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString

import scala.concurrent.{ExecutionContext, Future}

object Bitcoin {
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

        // Version (4 bytes, little-endian) - Use actual version from RPC
        val version = header.version.toInt
        buffer(offset) = (version & 0xff).toByte
        buffer(offset + 1) = ((version >> 8) & 0xff).toByte
        buffer(offset + 2) = ((version >> 16) & 0xff).toByte
        buffer(offset + 3) = ((version >> 24) & 0xff).toByte
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
        buffer(offset) = (time & 0xff).toByte
        buffer(offset + 1) = ((time >> 8) & 0xff).toByte
        buffer(offset + 2) = ((time >> 16) & 0xff).toByte
        buffer(offset + 3) = ((time >> 24) & 0xff).toByte
        offset += 4

        // Bits (4 bytes, little-endian - must reverse from display hex to internal byte order)
        val bits = hexToByteString(header.bits).bytes.reverse.toArray
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

    private def convertHeader(header: BlockHeaderInfo): BlockHeader =
        BlockHeader(ByteString.fromArray(buildRawHeader(header)))

    private def getInitialChainState(blockHeight: Int): Future[ChainState] = {
        val interval = BitcoinHelpers.DifficultyAdjustmentInterval.toInt
        val medianTimeSpan = BitcoinHelpers.MedianTimeSpan.toInt
        val adjustmentBlockHeight = blockHeight - (blockHeight % interval)
        for
            blockHashHex <- rpc.getBlockHash(blockHeight)
            header <- rpc.getBlockHeader(blockHashHex)
            adjustmentBlockHashHex <- rpc.getBlockHash(adjustmentBlockHeight)
            adjustmentHeader <- rpc.getBlockHeader(adjustmentBlockHashHex)
            // Fetch timestamps for the previous 11 blocks (for median-time-past validation)
            recentTimestamps <- {
                val heights = (0 until medianTimeSpan).map(i => blockHeight - i).toList
                Future.sequence(heights.map { h =>
                    if h >= 0 then
                        rpc.getBlockHash(h).flatMap(rpc.getBlockHeader).map(hdr => BigInt(hdr.time))
                    else Future.successful(BigInt(0))
                })
            }
            bits = BitcoinChainState.rpcBitsToCompactBits(header.bits)
            // Block hash from RPC is in display order (big-endian), but we store it in internal order (little-endian)
            blockHash = ByteString.fromArray(hexToByteString(header.hash).bytes.reverse.toArray)
        yield ChainState(
          confirmedBlocksRoot =
              BitcoinChainState.mpfRootForSingleBlock(blockHash), // MPF trie with single block
          ctx = TraversalCtx(
            timestamps = prelude.List.from(recentTimestamps),
            height = blockHeight,
            currentBits = bits,
            prevDiffAdjTimestamp = BigInt(adjustmentHeader.time),
            lastBlockHash = blockHash
          ),
          forkTree = ForkTree.End // Initialize with empty forks tree
        )
    }

    private val mainnetValidationParams = BitcoinValidatorParams(
      maturationConfirmations = 100,
      challengeAging = 200 * 60,
      oneShotTxOutRef = scalus.cardano.onchain.plutus.v3.TxOutRef(
        scalus.cardano.onchain.plutus.v3.TxId(ByteString.unsafeFromArray(Array.fill(32)(0: Byte))),
        0
      ),
      closureTimeout = 30 * 24 * 60 * 60,
      owner = scalus.cardano.onchain.plutus.v1.PubKeyHash(
        ByteString.unsafeFromArray(Array.fill(28)(0: Byte))
      ),
      powLimit = PowLimit,
      maxBlocksInForkTree = BitcoinContract.DefaultMaxBlocksInForkTree
    )

    private def processHeader(currentState: ChainState, headerInfo: BlockHeaderInfo): ChainState =
        val header = convertHeader(headerInfo)
        val currentTime = BigInt(System.currentTimeMillis() / 1000)
        val ctx = currentState.ctx
        val (summary, newCtx, _) =
            BitcoinValidator.validateBlock(header, ctx, currentTime, mainnetValidationParams)
        ChainState(
          confirmedBlocksRoot = currentState.confirmedBlocksRoot,
          ctx = newCtx.copy(timestamps = newCtx.timestamps.take(BitcoinHelpers.MedianTimeSpan)),
          forkTree = currentState.forkTree
        )

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

        // Load Bitcoin node configuration via PureConfig
        val fullConfig = BinocularConfig.load()
        val config = fullConfig.bitcoinNode
        println(s"✓ Loaded Bitcoin node config: $config")

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
    def selectBitcoinTxInThatBlock() =
        ??? // random previous block which exists in Binocular `blocks`
    // separate DApp, like DEX or some other onchain application
    def makeProofTx = {
        val bincocularRefInput = getBinocularUtxo
        val binocularDatum: ChainState = ???
        // onchain validation of Bitcoin tx inclusion:

        // block header exists in binocularDatum.blocks
        // merkle proof of block header hash inclusion in binocularDatum.blocksMerkleRoot
        // tx hash exists in block's tx merkle root
    }

}
// trigger $(date)
