package binocular

import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.api.UtxoSupplier
import com.bloxbean.cardano.client.api.model.{ProtocolParams, Utxo}
import com.bloxbean.cardano.client.backend.api.{BackendService, TransactionService}
import com.bloxbean.cardano.client.common.model.Networks
import com.bloxbean.cardano.yaci.test.{Funding, YaciCardanoContainer}
import munit.FunSuite

import scala.jdk.CollectionConverters.*
import scala.util.Using

/** Base trait for integration tests using Yaci DevKit
  *
  * Provides a local Cardano devnet via Docker container for testing smart contracts
  * and transactions in a controlled environment.
  *
  * Usage:
  * {{{
  * class MyIntegrationTest extends YaciDevKitSpec {
  *   test("submit transaction to devnet") {
  *     withYaciDevKit { devKit =>
  *       // Use devKit.container, devKit.account, etc.
  *       val utxos = devKit.getUtxos(devKit.account.baseAddress())
  *       assert(utxos.nonEmpty)
  *     }
  *   }
  * }
  * }}}
  */
trait YaciDevKitSpec extends FunSuite {

    /** Configuration for Yaci DevKit container */
    case class YaciDevKitConfig(
        initialFundingLovelace: Long = 100_000_000_000L, // 100,000 ADA
        enableLogs: Boolean = false
    )

    /** Wrapper for Yaci DevKit container with helper methods */
    class YaciDevKit(
        val container: YaciCardanoContainer,
        val account: Account,
        val config: YaciDevKitConfig
    ) {

        /** Get backend service for transaction operations */
        def getBackendService: BackendService = container.getBackendService

        /** Get UTXO supplier for querying available UTXOs */
        def getUtxoSupplier: UtxoSupplier = container.getUtxoSupplier

        /** Get protocol parameters supplier */
        def getProtocolParams: ProtocolParams =
            container.getBackendService.getEpochService.getProtocolParameters().getValue

        /** Get transaction service for submitting transactions */
        def getTransactionService: TransactionService = container.getBackendService.getTransactionService

        /** Get UTXOs for an address */
        def getUtxos(address: String): List[Utxo] = {
            val utxos = getUtxoSupplier.getAll(address)
            utxos.asScala.toList
        }

        /** Get total lovelace balance for an address */
        def getLovelaceBalance(address: String): BigInt = {
            val utxos = getUtxos(address)
            utxos.map(u => BigInt(u.getAmount.asScala.head.getQuantity)).sum
        }

        /** Wait for transaction to be confirmed
          *
          * @param txHash transaction hash to wait for
          * @param maxAttempts maximum number of attempts (default: 30)
          * @param delayMs delay between attempts in milliseconds (default: 1000)
          * @return true if transaction was confirmed, false if timeout
          */
        def waitForTransaction(
            txHash: String,
            maxAttempts: Int = 30,
            delayMs: Long = 1000
        ): Boolean = {
            def checkTx(attempts: Int): Boolean = {
                if (attempts >= maxAttempts) {
                    false
                } else {
                    try {
                        val tx = getTransactionService.getTransaction(txHash)
                        tx.isSuccessful && tx.getResponse != null
                    } catch {
                        case _: Exception =>
                            Thread.sleep(delayMs)
                            checkTx(attempts + 1)
                    }
                }
            }
            checkTx(0)
        }

        /** Submit transaction and wait for confirmation
          *
          * @param txBytes transaction bytes to submit
          * @return transaction hash if successful
          */
        def submitAndWait(txBytes: Array[Byte]): Either[String, String] = {
            try {
                val result = getTransactionService.submitTransaction(txBytes)
                if (result.isSuccessful) {
                    val txHash = result.getValue
                    if (waitForTransaction(txHash)) {
                        Right(txHash)
                    } else {
                        Left(s"Transaction $txHash did not confirm in time")
                    }
                } else {
                    Left(s"Transaction submission failed: ${result.getResponse}")
                }
            } catch {
                case e: Exception =>
                    Left(s"Transaction submission error: ${e.getMessage}")
            }
        }

        /** Stop the container */
        def stop(): Unit = container.stop()
    }

    /** Create and start a Yaci DevKit container
      *
      * @param config configuration for the container
      * @return YaciDevKit instance with running container
      */
    def createYaciDevKit(config: YaciDevKitConfig = YaciDevKitConfig()): YaciDevKit = {
        // Use Yaci DevKit's default mnemonic for pre-funded accounts
        // This matches the mnemonic used by Yaci CLI for generating default accounts
        val mnemonic = "test test test test test test test test test test test test test test test test test test test test test test test sauce"
        val account = new Account(Networks.testnet(), mnemonic)

        // Create container and give it a fixed name for reuse
        val container = new YaciCardanoContainer()
        container.withCreateContainerCmdModifier(cmd => cmd.withName("binocular-yaci-devkit"))
        container.withReuse(true)

        // Add log consumer if enabled
        if (config.enableLogs) {
            container.withLogConsumer(frame => println(s"[Yaci] ${frame.getUtf8String}"))
        }

        // Start the container
        container.start()

        new YaciDevKit(container, account, config)
    }

    /** Execute test with Yaci DevKit container
      *
      * Automatically starts and stops the container around the test.
      *
      * @param config configuration for the container
      * @param testFn test function to execute with the devkit
      */
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

    /** Fixture for Yaci DevKit that can be reused across multiple tests */
    val yaciDevKitFixture = new Fixture[YaciDevKit]("yaci-devkit") {
        private var devKit: YaciDevKit = _

        def apply(): YaciDevKit = devKit

        override def beforeAll(): Unit = {
            devKit = createYaciDevKit(YaciDevKitConfig(enableLogs = false))
        }

        override def afterAll(): Unit = {
            if (devKit != null) {
                devKit.stop()
            }
        }
    }
}
