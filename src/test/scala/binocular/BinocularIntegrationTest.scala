package binocular

import com.bloxbean.cardano.client.address.Address
import com.bloxbean.cardano.client.plutus.spec.PlutusV3Script
import com.bloxbean.cardano.client.util.HexUtil
import scalus.builtin.ByteString

import scala.jdk.CollectionConverters.*

/** Integration tests for Binocular Oracle on Yaci DevKit
  *
  * These tests require Docker to be running and will start a local Cardano devnet.
  * Tests are marked with .ignore by default - remove .ignore to run them.
  *
  * To run these tests:
  * 1. Ensure Docker is running
  * 2. Run: sbt "testOnly *BinocularIntegrationTest"
  */
class BinocularIntegrationTest extends YaciDevKitSpec {

    // Get script address for testing (without full PlutusV3Script for now)
    // We'll use a placeholder testnet script address
    lazy val testScriptAddress: Address = {
        // This is a valid testnet script address format (starts with addr_test1w)
        // In production, this would be derived from the actual compiled PlutusV3 script
        new Address("addr_test1wqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq8wwwj")
    }

    test("Yaci DevKit container starts and has funded account".ignore) {
        withYaciDevKit(YaciDevKitConfig(enableLogs = true)) { devKit =>
            // Verify account was funded
            val balance = devKit.getLovelaceBalance(devKit.account.baseAddress())
            println(s"Account balance: $balance lovelace")

            assert(balance > 0, "Account should have been funded with initial lovelace")
            assert(balance >= 100_000_000_000L, "Account should have at least 100,000 ADA")
        }
    }

    test("Can create initial script UTXO with genesis state".ignore) {
        withYaciDevKit(YaciDevKitConfig(enableLogs = false)) { devKit =>
            // Create genesis ChainState
            val genesisState = createGenesisState()

            // Use test script address
            println(s"Script address: ${testScriptAddress.getAddress}")

            // Create initial UTXO at script address
            val result = TransactionBuilders.createInitialScriptUtxo(
              devKit.account,
              devKit.getBackendService,
              testScriptAddress,
              genesisState,
              lovelaceAmount = 5_000_000L
            )

            result match {
                case Right(txHash) =>
                    println(s"Created initial UTXO, tx: $txHash")

                    // Wait for confirmation
                    val confirmed = devKit.waitForTransaction(txHash, maxAttempts = 30, delayMs = 2000)
                    assert(confirmed, s"Transaction $txHash should confirm")

                    // Verify UTXO exists at script address
                    Thread.sleep(2000) // Give indexer time to catch up
                    val scriptUtxos = devKit.getUtxos(testScriptAddress.getAddress)
                    assert(scriptUtxos.nonEmpty, "Script should have at least one UTXO")

                    val scriptUtxo = scriptUtxos.head
                    assert(scriptUtxo.getAmount.asScala.head.getQuantity.longValue() == 5_000_000L, "Script UTXO should have correct amount")

                case Left(error) =>
                    fail(s"Failed to create initial UTXO: $error")
            }
        }
    }

    test("Can query protocol parameters".ignore) {
        withYaciDevKit() { devKit =>
            val paramsResult = TransactionBuilders.getProtocolParamsHelper(devKit.getBackendService)

            paramsResult match {
                case Right(params) =>
                    println(s"Protocol parameters:")
                    println(s"  Min fee A: ${params.minFeeA}")
                    println(s"  Min fee B: ${params.minFeeB}")
                    println(s"  Max TX size: ${params.maxTxSize}")
                    println(s"  Max block header size: ${params.maxBlockHeaderSize}")

                    assert(params.minFeeA > 0, "Min fee A should be positive")
                    assert(params.minFeeB > 0, "Min fee B should be positive")
                    assert(params.maxTxSize > 0, "Max TX size should be positive")

                case Left(error) =>
                    fail(s"Failed to get protocol params: $error")
            }
        }
    }

    /** Helper: Create a genesis ChainState for testing
      *
      * Uses Bitcoin block 865493 as the initial confirmed state.
      */
    private def createGenesisState(): BitcoinValidator.ChainState = {
        val genesisHeight = BigInt(865493)
        val genesisHash = ByteString.fromHex("0000000000000000000143a112c5ab741ec6e95b6c80f9834199efe2154c972b").reverse
        val genesisTarget = ByteString.fromHex("17030ecd").reverse
        val genesisTimestamp = BigInt(1736701001)
        val genesisRecentTimestamps = scalus.prelude.List.single(genesisTimestamp)

        BitcoinValidator.ChainState(
          blockHeight = genesisHeight,
          blockHash = genesisHash,
          currentTarget = genesisTarget,
          blockTimestamp = genesisTimestamp,
          recentTimestamps = genesisRecentTimestamps,
          previousDifficultyAdjustmentTimestamp = genesisTimestamp - 600 * BitcoinValidator.DifficultyAdjustmentInterval,
          confirmedBlocksRoot = genesisHash,
          forksTree = scalus.prelude.AssocMap.empty
        )
    }
}
