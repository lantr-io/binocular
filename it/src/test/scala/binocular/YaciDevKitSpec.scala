package binocular

import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.{TransactionHash, TransactionInput, Utxo}
import scalus.cardano.node.{BlockchainProvider, BlockfrostProvider}
import scalus.cardano.txbuilder.TransactionSigner
import scalus.cardano.wallet.hd.HdAccount
import scalus.crypto.ed25519.given
import com.bloxbean.cardano.yaci.test.YaciCardanoContainer
import org.scalatest.funsuite.AnyFunSuite

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration.*

/** Base trait for integration tests using Yaci DevKit
  *
  * Provides a local Cardano devnet via Docker container for testing smart contracts and
  * transactions in a controlled environment.
  */
trait YaciDevKitSpec extends AnyFunSuite {

    /** Configuration for Yaci DevKit container */
    case class YaciDevKitConfig(
        initialFundingLovelace: Long = 100_000_000_000L, // 100,000 ADA
        enableLogs: Boolean = false
    )

    /** Wrapper for Yaci DevKit container with Scalus provider */
    class YaciDevKit(
        val container: YaciCardanoContainer,
        val hdAccount: HdAccount,
        val config: YaciDevKitConfig
    ) {
        given ec: ExecutionContext = ExecutionContext.global

        private val yaciUrl =
            s"http://${container.getHost}:${container.getMappedPort(8080)}/api/v1"

        private val yaciAdminUrl =
            s"http://${container.getHost}:${container.getMappedPort(10000)}/local-cluster/api"

        /** Get BlockchainProvider connected to Yaci DevKit.
          *
          * Uses BlockfrostProvider.localYaci which auto-fetches slot config from the Yaci admin API.
          */
        lazy val provider: BlockchainProvider = {
            println(s"[YaciDevKit] Connecting to Yaci DevKit at $yaciUrl (admin: $yaciAdminUrl)")
            Await.result(
              BlockfrostProvider.localYaci(baseUrl = yaciUrl, adminUrl = yaciAdminUrl),
              30.seconds
            )
        }

        /** Get TransactionSigner */
        lazy val signer: TransactionSigner = new TransactionSigner(Set(hdAccount.paymentKeyPair))

        /** Get sponsor address */
        lazy val sponsorAddress: Address = hdAccount.baseAddress(Network.Testnet)

        /** Get address as bech32 string */
        def baseAddress: String =
            sponsorAddress.asInstanceOf[scalus.cardano.address.ShelleyAddress].toBech32.get

        /** Get UTXOs for an address */
        def getUtxos(address: Address): List[Utxo] = {
            val result = Await.result(provider.findUtxos(address), 30.seconds)
            result match {
                case Right(u) => u.map { case (input, output) => Utxo(input, output) }.toList
                case Left(_)  => List.empty
            }
        }

        /** Get total lovelace balance for an address */
        def getLovelaceBalance(address: Address): BigInt = {
            val utxos = getUtxos(address)
            utxos.map(u => BigInt(u.output.value.coin.value)).sum
        }

        /** Wait for transaction to be confirmed
          *
          * @param txHash
          *   transaction hash to wait for
          * @param maxAttempts
          *   maximum number of attempts (default: 30)
          * @param delayMs
          *   delay between attempts in milliseconds (default: 1000)
          * @return
          *   true if transaction was confirmed, false if timeout
          */
        def waitForTransaction(
            txHash: String,
            maxAttempts: Int = 30,
            delayMs: Long = 1000
        ): Boolean = {
            val input = TransactionInput(TransactionHash.fromHex(txHash), 0)
            var attempts = 0
            while attempts < maxAttempts do {
                try {
                    val result = Await.result(provider.findUtxo(input), 10.seconds)
                    result match {
                        case Right(_) => return true
                        case Left(_) =>
                            Thread.sleep(delayMs)
                            attempts += 1
                    }
                } catch {
                    case _: Exception =>
                        Thread.sleep(delayMs)
                        attempts += 1
                }
            }
            false
        }

        /** Stop the container */
        def stop(): Unit = container.stop()
    }

    /** Create and start a Yaci DevKit container */
    def createYaciDevKit(config: YaciDevKitConfig = YaciDevKitConfig()): YaciDevKit = {
        val mnemonic =
            "test test test test test test test test test test test test test test test test test test test test test test test sauce"
        val hdAccount = HdAccount.fromMnemonic(mnemonic, "", accountIndex = 0)

        val container = new YaciCardanoContainer()
        container.withCreateContainerCmdModifier(cmd => cmd.withName("binocular-yaci-devkit"))
        container.withReuse(true)

        if config.enableLogs then {
            container.withLogConsumer(frame => println(s"[Yaci] ${frame.getUtf8String}"))
        }

        container.start()

        new YaciDevKit(container, hdAccount, config)
    }

    /** Execute test with Yaci DevKit container */
    def withYaciDevKit[T](
        config: YaciDevKitConfig = YaciDevKitConfig()
    )(testFn: YaciDevKit => T): T = {
        val devKit = createYaciDevKit(config)
        try {
            testFn(devKit)
        } finally {
            devKit.stop()
        }
    }
}

