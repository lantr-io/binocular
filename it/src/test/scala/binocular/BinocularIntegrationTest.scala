package binocular

import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.Script
import scalus.uplc.builtin.ByteString
import scalus.cardano.onchain.plutus.prelude

/** Integration tests for Binocular Oracle on Yaci DevKit
  *
  * These tests require Docker to be running and will start a local Cardano devnet. Tests are marked
  * with .ignore by default - remove .ignore to run them.
  *
  * To run these tests:
  *   1. Ensure Docker is running
  *   2. Run: sbt "testOnly *BinocularIntegrationTest"
  */
class BinocularIntegrationTest extends YaciDevKitSpec {
    // Override default test timeout for integration tests (default is 30 seconds)
    override val munitTimeout = scala.concurrent.duration.Duration(3, "min")

    // Get the actual script address from compiled BitcoinValidator
    lazy val bitcoinScript: Script.PlutusV3 = TransactionBuilders.compiledBitcoinScript()

    lazy val scriptAddress: Address = TransactionBuilders.getScriptAddress(Network.Testnet)

    test("Bitcoin script compiles and has valid address") {
        // Verify script compiles
        assert(bitcoinScript != null, "Script should compile")

        // Verify address is valid testnet script address
        val addr = scriptAddress.encode.getOrElse("?")
        assert(addr.startsWith("addr_test1"), s"Should be testnet address, got: $addr")
        println(s"Compiled Bitcoin validator script address: $addr")
        println(s"Script size: ${BitcoinContract.bitcoinProgram.flatEncoded.length} bytes")
    }

    test("Yaci DevKit container starts and has funded account".ignore) {
        withYaciDevKit(YaciDevKitConfig(enableLogs = true)) { devKit =>
            // Verify account was funded
            val balance = devKit.getLovelaceBalance(devKit.sponsorAddress)
            println(s"Account balance: $balance lovelace")

            assert(balance > 0, "Account should have been funded with initial lovelace")
            assert(balance >= 100_000_000_000L, "Account should have at least 100,000 ADA")
        }
    }

    test("Can create initial script UTXO with genesis state") {
        withYaciDevKit(YaciDevKitConfig(enableLogs = true)) { devKit =>
            // Create genesis ChainState
            val genesisState = createGenesisState()

            // Use actual compiled script address
            println(s"[Test] Script address: ${scriptAddress.encode.getOrElse("?")}")
            println(s"[Test] Account address: ${devKit.baseAddress}")

            // Check account balance before
            val balanceBefore = devKit.getLovelaceBalance(devKit.sponsorAddress)
            println(s"[Test] Account balance: $balanceBefore lovelace")

            // Create initial UTXO at script address
            println(s"[Test] Creating initial script UTXO...")
            val result = TransactionBuilders.createInitialScriptUtxo(
              devKit.signer,
              devKit.provider,
              scriptAddress,
              devKit.sponsorAddress,
              genesisState,
              lovelaceAmount = 5_000_000L
            )

            result match {
                case Right(txHash) =>
                    println(s"Created initial UTXO, tx: $txHash")

                    // Wait for confirmation
                    val confirmed =
                        devKit.waitForTransaction(txHash, maxAttempts = 30, delayMs = 2000)
                    assert(confirmed, s"Transaction $txHash should confirm")

                    // Verify UTXO exists at script address
                    Thread.sleep(2000) // Give indexer time to catch up
                    val scriptUtxos = devKit.getUtxos(scriptAddress)
                    assert(scriptUtxos.nonEmpty, "Script should have at least one UTXO")

                    val scriptUtxo = scriptUtxos.head
                    assert(
                      scriptUtxo.output.value.coin.value == 5_000_000L,
                      "Script UTXO should have correct amount"
                    )

                case Left(error) =>
                    fail(s"Failed to create initial UTXO: $error")
            }
        }
    }

    test("Can submit sequential Bitcoin headers through Oracle".ignore) {
        withYaciDevKit(YaciDevKitConfig(enableLogs = true)) { devKit =>
            println("\n=== Sequential Oracle Update Test ===\n")

            // 1. Load fixture headers
            val fixture = BitcoinHeaderFixtures.loadFixture("bitcoin-headers-small-3")
            val headers = BitcoinHeaderFixtures.loadHeaders("bitcoin-headers-small-3")
            println(s"Loaded ${headers.length} headers from fixture")

            // 2. Create initial script UTXO with genesis state from first header
            val genesisState = BitcoinHeaderFixtures.createGenesisState(fixture)
            println(s"Created genesis state at height ${genesisState.blockHeight}")

            val createResult = TransactionBuilders.createInitialScriptUtxo(
              devKit.signer,
              devKit.provider,
              scriptAddress,
              devKit.sponsorAddress,
              genesisState,
              lovelaceAmount = 5_000_000L
            )

            val initialTxHash = createResult match {
                case Right(txHash) =>
                    println(s"Created initial UTXO, tx: $txHash")
                    val confirmed =
                        devKit.waitForTransaction(txHash, maxAttempts = 30, delayMs = 2000)
                    assert(confirmed, s"Initial transaction $txHash should confirm")
                    txHash
                case Left(error) =>
                    fail(s"Failed to create initial UTXO: $error")
            }

            // 3. Submit remaining headers sequentially (skip first, it's already in genesis)
            val remainingHeaders = headers.tail
            println(s"\nSubmitting ${remainingHeaders.length} headers sequentially...")

            var currentState = genesisState
            val currentTime = BigInt(System.currentTimeMillis() / 1000)

            remainingHeaders.zipWithIndex.foreach { case (header, idx) =>
                println(s"\n--- Submitting header ${idx + 1}/${remainingHeaders.length} ---")

                // Calculate expected new state
                val headerList = prelude.List.single(header)
                val newState =
                    TransactionBuilders.applyHeaders(currentState, headerList, currentTime)
                println(s"  Expected new height: ${newState.blockHeight}")

                // Submit UpdateOracle transaction
                val updateResult = TransactionBuilders.buildUpdateOracleTransaction(
                  devKit.signer,
                  devKit.provider,
                  scriptAddress,
                  devKit.sponsorAddress,
                  currentState,
                  newState,
                  headerList
                )

                updateResult match {
                    case Right(txHash) =>
                        println(s"  UpdateOracle tx: $txHash")
                        val confirmed =
                            devKit.waitForTransaction(txHash, maxAttempts = 30, delayMs = 2000)
                        assert(confirmed, s"UpdateOracle transaction $txHash should confirm")

                        // Update current state for next iteration
                        currentState = newState

                    case Left(error) =>
                        fail(s"Failed to submit header ${idx + 1}: $error")
                }
            }

            println(s"\nSuccessfully submitted all ${remainingHeaders.length} headers!")
            println(s"Final oracle height: ${currentState.blockHeight}")
        }
    }

    /** Helper: Create a genesis ChainState for testing
      *
      * Uses Bitcoin block 865493 as the initial confirmed state.
      */
    private def createGenesisState(): ChainState = {
        val genesisHeight = BigInt(865493)
        val genesisHash = ByteString
            .fromHex("0000000000000000000143a112c5ab741ec6e95b6c80f9834199efe2154c972b")
            .reverse
        val genesisTarget = ByteString.fromHex("17030ecd").reverse
        val genesisTimestamp = BigInt(1736701001)
        val genesisRecentTimestamps = prelude.List.single(genesisTimestamp)

        ChainState(
          blockHeight = genesisHeight,
          blockHash = genesisHash,
          currentTarget = genesisTarget,
          blockTimestamp = genesisTimestamp,
          recentTimestamps = genesisRecentTimestamps,
          previousDifficultyAdjustmentTimestamp =
              genesisTimestamp - 600 * BitcoinValidator.DifficultyAdjustmentInterval,
          confirmedBlocksTree = prelude.List(genesisHash),
          forksTree = prelude.List.Nil
        )
    }
}
