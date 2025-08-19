package binocular

import binocular.BitcoinValidator.*
import org.apache.pekko.actor.ActorSystem
import org.bitcoins.core.config.MainNet
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.rpc.client.v27.BitcoindV27RpcClient
import org.bitcoins.rpc.config.BitcoindAuthCredentials
import org.bitcoins.rpc.config.BitcoindInstanceRemote
import scalus.bloxbean.Interop.??
import scalus.builtin.Builtins.*
import scalus.builtin.ByteString
import scalus.prelude

import java.net.URI
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

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

class HeaderSyncWithRpc(bitcoindUri: URI, bitcoindUser: String, bitcoindPassword: String)(using system: ActorSystem) {
    private given ec: ExecutionContext = system.dispatcher

    private val instance = BitcoindInstanceRemote(
      network = MainNet,
      uri = bitcoindUri,
      rpcUri = bitcoindUri,
      authCredentials = BitcoindAuthCredentials.PasswordBased(bitcoindUser, bitcoindPassword)
    )

    private val client = new BitcoindV27RpcClient(instance)(using system, null)

    private def convertHeader(btcHeader: BlockHeader): BitcoinValidator.BlockHeader =
        BitcoinValidator.BlockHeader(ByteString.fromArray(btcHeader.bytes.toArray))

    private def getInitialChainState(blockHeight: Int): Future[ChainState] = {
        for
            blockHash <- client.getBlockHash(blockHeight)
            header <- client.getBlockHeader(blockHash)
            bits = ByteString.fromArray(header.blockHeader.nBits.bytes.reverse.toArray)
        yield ChainState(
          blockHeight = blockHeight,
          blockHash = ByteString.fromArray(header.blockHeader.hash.bytes.toArray),
          currentDifficulty = BitcoinValidator.bitsToBigInt(bits),
          cumulativeDifficulty = 0,
          recentTimestamps = prelude.List(header.blockHeader.time.toBigInt),
          previousDifficultyAdjustmentTimestamp = header.blockHeader.time.toBigInt
        )
    }

    private def processHeader(currentState: ChainState, btcHeader: BlockHeader): ChainState =
        val header = convertHeader(btcHeader)
        val currentTime = BigInt(System.currentTimeMillis() / 1000)
        BitcoinValidator.updateTip(currentState, header, currentTime)

    private def syncLoop(height: Int, targetHeight: Int, currentState: ChainState): Future[Unit] =
        if height > targetHeight then Future.successful(())
        else
            for
                blockHash <- client.getBlockHash(height)
                header <- client.getBlockHeader(blockHash)
                newState =
                    try
                        val newState = processHeader(currentState, header.blockHeader)
                        println(
                          s"Validated header at height $height: ${header.blockHeader}, new state: ${newState}"
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
            info <- client.getBlockChainInfo
            initialState <- getInitialChainState(startingBlockHeight)
            latestHeight = info.blocks
            _ <- syncLoop(startingBlockHeight + 1, latestHeight, initialState)
        yield ()

    def printNodeInfo(): Future[Unit] =
        for
            info <- client.getBlockChainInfo
            _ = println(s"""
                           |Bitcoin Node Info:
                           |Chain: ${info.chain}
                           |Blocks: ${info.blocks}
                           |Headers: ${info.headers}
                           |Verification Progress: ${info.verificationprogress}
                           |""".stripMargin)
        yield ()
}

object HeaderSyncWithRpc {
    def main(args: Array[String]): Unit =

        val bitcoindUri = new URI(
          System.getenv("bitcoind_rpc_url") ?? sys.error("bitcoind_rpc_url not set")
        )
        val bitcoindUser =
            System.getenv("bitcoind_rpc_user") ?? sys.error("bitcoind_rpc_user not set")
        val bitcoindPassword =
            System.getenv("bitcoind_rpc_password") ?? sys.error("bitcoind_rpc_password not set")

        given system: ActorSystem = ActorSystem("bitcoin-s")
        given ec: ExecutionContext = system.dispatcher

        val syncer = HeaderSyncWithRpc(bitcoindUri, bitcoindUser, bitcoindPassword)

        // Main execution
        val program =
            for
                _ <- syncer.printNodeInfo()
                _ <- syncer.syncHeadersFrom(866868)
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
