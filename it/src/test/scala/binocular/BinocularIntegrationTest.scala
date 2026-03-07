package binocular

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.{Script, Utxo}
import scalus.cardano.onchain.plutus.prelude
import scalus.testing.yaci.{YaciConfig, YaciDevKit}
import scalus.uplc.builtin.ByteString
import scalus.utils.await

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*

/** Integration tests for Binocular Oracle on Yaci DevKit
  *
  * These tests require Docker to be running and will start a local Cardano devnet. Tests are marked
  * with .ignore by default - remove .ignore to run them.
  *
  * To run these tests:
  *   1. Ensure Docker is running
  *   2. Run: sbt "it/testOnly *BinocularIntegrationTest"
  */
class BinocularIntegrationTest extends AnyFunSuite with YaciDevKit {

    override protected def yaciConfig: YaciConfig = YaciConfig(
      enableLogs = true,
      containerName = "binocular-yaci-devkit",
      reuseContainer = true
    )

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

    ignore("Yaci DevKit container starts and has funded account") {
        val ctx = createYaciContext()
        given ExecutionContext = ctx.provider.executionContext

        // Verify account was funded
        val utxos = ctx.provider.findUtxos(ctx.alice.address).await(30.seconds)
        val balance: Long = utxos match {
            case Right(u) => u.map(_._2.value.coin.value).sum
            case Left(_)  => 0L
        }
        println(s"Account balance: $balance lovelace")

        assert(balance > 0, "Account should have been funded with initial lovelace")
        assert(balance >= 100_000_000_000L, "Account should have at least 100,000 ADA")
    }

    test("Can create initial script UTXO with genesis state") {
        val ctx = createYaciContext()
        given ExecutionContext = ctx.provider.executionContext

        // Create genesis ChainState
        val genesisState = createGenesisState()

        // Use actual compiled script address
        println(s"[Test] Script address: ${scriptAddress.encode.getOrElse("?")}")
        println(s"[Test] Account address: ${ctx.alice.address.toBech32.getOrElse("?")}")

        // Check account balance before
        val utxosResult = ctx.provider.findUtxos(ctx.alice.address).await(30.seconds)
        val balanceBefore: Long = utxosResult match {
            case Right(u) => u.map(_._2.value.coin.value).sum
            case Left(_)  => 0L
        }
        println(s"[Test] Account balance: $balanceBefore lovelace")

        // Create initial UTXO at script address
        println(s"[Test] Creating initial script UTXO...")
        val result = TransactionBuilders.createInitialScriptUtxo(
          ctx.alice.signer,
          ctx.provider,
          scriptAddress,
          ctx.alice.address,
          genesisState,
          lovelaceAmount = 5_000_000L
        )

        result match {
            case Right(txHash) =>
                println(s"Created initial UTXO, tx: $txHash")

                // Wait for confirmation
                ctx.waitForBlock()

                // Verify UTXO exists at script address
                Thread.sleep(2000) // Give indexer time to catch up
                val scriptUtxosResult =
                    ctx.provider.findUtxos(scriptAddress).await(30.seconds)
                val scriptUtxos = scriptUtxosResult match {
                    case Right(u) =>
                        u.map { case (input, output) => Utxo(input, output) }.toList
                    case Left(_) => List.empty
                }
                val scriptUtxo = scriptUtxos
                    .find(_.input.transactionId.toHex == txHash)
                    .getOrElse(fail("Script UTXO not found for our tx"))
                assert(
                  scriptUtxo.output.value.coin.value == 5_000_000L,
                  "Script UTXO should have correct amount"
                )

            case Left(error) =>
                fail(s"Failed to create initial UTXO: $error")
        }
    }

    ignore("Can submit sequential Bitcoin headers through Oracle") {
        val ctx = createYaciContext()
        given ExecutionContext = ctx.provider.executionContext

        println("\n=== Sequential Oracle Update Test ===\n")

        // 1. Load fixture headers
        val fixture = BitcoinHeaderFixtures.loadFixture("bitcoin-headers-small-3")
        val headers = BitcoinHeaderFixtures.loadHeaders("bitcoin-headers-small-3")
        println(s"Loaded ${headers.length} headers from fixture")

        // 2. Create initial script UTXO with genesis state from first header
        val genesisState = BitcoinHeaderFixtures.createGenesisState(fixture)
        println(s"Created genesis state at height ${genesisState.blockHeight}")

        val createResult = TransactionBuilders.createInitialScriptUtxo(
          ctx.alice.signer,
          ctx.provider,
          scriptAddress,
          ctx.alice.address,
          genesisState,
          lovelaceAmount = 5_000_000L
        )

        val initialTxHash = createResult match {
            case Right(txHash) =>
                println(s"Created initial UTXO, tx: $txHash")
                ctx.waitForBlock()
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
              ctx.alice.signer,
              ctx.provider,
              scriptAddress,
              ctx.alice.address,
              currentState,
              newState,
              headerList
            )

            updateResult match {
                case Right(txHash) =>
                    println(s"  UpdateOracle tx: $txHash")
                    ctx.waitForBlock()

                    // Update current state for next iteration
                    currentState = newState

                case Left(error) =>
                    fail(s"Failed to submit header ${idx + 1}: $error")
            }
        }

        println(s"\nSuccessfully submitted all ${remainingHeaders.length} headers!")
        println(s"Final oracle height: ${currentState.blockHeight}")
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
              genesisTimestamp - 600 * BitcoinHelpers.DifficultyAdjustmentInterval,
          confirmedBlocksTree = prelude.List(genesisHash),
          forksTree = prelude.List.Nil
        )
    }
}
