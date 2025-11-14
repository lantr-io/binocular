package binocular.cli

import binocular.cli.commands.InitOracleCommand
import binocular.{BitcoinChainState, BitcoinValidator, OracleTransactions}
import com.bloxbean.cardano.client.address.Address
import scalus.builtin.Data
import scalus.utils.Hex.hexToBytes

import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext}

/** Integration test for InitOracleCommand
  *
  * Tests the complete flow of initializing an oracle:
  *   1. Fetches Bitcoin block header from mock RPC (using fixtures) 2. Creates initial ChainState 3. Submits
  *      transaction to Yaci DevKit 4. Verifies oracle UTxO exists with correct datum
  */
class InitOracleCommandIntegrationTest extends CliIntegrationTestBase {

    // Override default test timeout (30 seconds) to allow for Docker container startup
    override val munitTimeout = scala.concurrent.duration.Duration(120, "s")

    test("init-oracle: successfully creates oracle with Bitcoin fixture data") {
        withYaciDevKit() { devKit =>
            given ec: ExecutionContext = ExecutionContext.global

            // Use a block from our fixtures (block 866970)
            val startHeight = 866970L
            val mockRpc = new MockBitcoinRpc()

            println(s"[Test] Initializing oracle at height $startHeight")

            // Fetch initial chain state using mock RPC
            val initialStateFuture = BitcoinChainState.getInitialChainState(
              new binocular.SimpleBitcoinRpc(
                binocular.BitcoinNodeConfig(
                  url = "mock://rpc",
                  username = "test",
                  password = "test",
                  network = binocular.BitcoinNetwork.Testnet
                )
              ) {
                  // Override methods to use mock RPC
                  override def getBlockHash(height: Int) = mockRpc.getBlockHash(height)
                  override def getBlockHeader(hash: String) = mockRpc.getBlockHeader(hash)
              },
              startHeight.toInt
            )

            val initialState = Await.result(initialStateFuture, 30.seconds)

            println(s"[Test] Initial state fetched:")
            println(s"  Height: ${initialState.blockHeight}")
            println(s"  Hash: ${initialState.blockHash.toHex}")

            // Build and submit init transaction
            val scriptAddress = new Address(
              binocular.OracleConfig(network = binocular.CardanoNetwork.Testnet).scriptAddress
            )

            val txResult = OracleTransactions.buildAndSubmitInitTransaction(
              devKit.account,
              devKit.getBackendService,
              scriptAddress,
              initialState,
              5_000_000L
            )

            txResult match {
                case Right(txHash) =>
                    println(s"[Test] ✓ Oracle initialized: $txHash")

                    // Wait for transaction confirmation
                    val confirmed = devKit.waitForTransaction(txHash, maxAttempts = 30, delayMs = 1000)
                    assert(confirmed, s"Transaction $txHash did not confirm")
                    println(s"[Test] ✓ Transaction confirmed")

                    // Verify oracle UTxO exists
                    Thread.sleep(2000) // Give some time for UTxO to be indexed
                    val utxos = devKit.getUtxos(scriptAddress.getAddress)
                    assert(utxos.nonEmpty, "No UTxOs found at oracle address")
                    println(s"[Test] ✓ Oracle UTxO created: ${utxos.size} UTxO(s) at script address")

                    // Verify datum
                    val oracleUtxo = utxos.head
                    val inlineDatum = oracleUtxo.getInlineDatum
                    assert(inlineDatum != null && inlineDatum.nonEmpty, "Oracle UTxO has no datum")
                    val data = Data.fromCbor(inlineDatum.hexToBytes)
                    val chainState = data.to[BitcoinValidator.ChainState]

                    assert(
                      chainState.blockHeight == startHeight,
                      s"Block height mismatch: ${chainState.blockHeight} != $startHeight"
                    )
                    println(s"[Test] ✓ ChainState verified:")
                    println(s"    Height: ${chainState.blockHeight}")
                    println(s"    Hash: ${chainState.blockHash.toHex}")

                case Left(err) =>
                    fail(s"Failed to initialize oracle: $err")
            }
        }
    }

    test("init-oracle: fails gracefully with invalid start height") {
        withYaciDevKit() { devKit =>
            given ec: ExecutionContext = ExecutionContext.global

            // Try to use a block height that doesn't exist in fixtures
            val invalidHeight = 999999999
            val mockRpc = new MockBitcoinRpc()

            println(s"[Test] Attempting to initialize with invalid height $invalidHeight")

            val result =
                try {
                    val initialStateFuture = BitcoinChainState.getInitialChainState(
                      new binocular.SimpleBitcoinRpc(
                        binocular.BitcoinNodeConfig(
                          url = "mock://rpc",
                          username = "test",
                          password = "test",
                          network = binocular.BitcoinNetwork.Testnet
                        )
                      ) {
                          override def getBlockHash(height: Int) = mockRpc.getBlockHash(height)
                          override def getBlockHeader(hash: String) = mockRpc.getBlockHeader(hash)
                      },
                      invalidHeight
                    )
                    Await.result(initialStateFuture, 10.seconds)
                    fail("Should have thrown exception for invalid height")
                } catch {
                    case e: Exception =>
                        println(s"[Test] ✓ Correctly failed with: ${e.getMessage}")
                        assert(
                          e.getMessage.contains("No such file") || e.getMessage.contains("not found") || e.getMessage
                              .contains("FileNotFoundException")
                        )
                }
        }
    }
}
