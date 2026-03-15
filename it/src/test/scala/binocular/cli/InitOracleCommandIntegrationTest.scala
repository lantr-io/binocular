package binocular.cli

import binocular.{BitcoinChainState, ChainState, IntegrationTestContract, OracleTransactions}
import scalus.uplc.builtin.Data
import scalus.utils.await

import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext

/** Integration test for InitOracleCommand
  *
  * Tests the complete flow of initializing an oracle:
  *   1. Fetches Bitcoin block header from mock RPC (using fixtures) 2. Creates initial ChainState
  *      3. Submits transaction to Yaci DevKit 4. Verifies oracle UTxO exists with correct datum
  */
class InitOracleCommandIntegrationTest extends CliIntegrationTestBase {

    test("init-oracle: successfully creates oracle with Bitcoin fixture data") {
        val ctx = createYaciContext()
        given ec: ExecutionContext = ctx.provider.executionContext

        // Use a block from our fixtures (block 866970)
        val startHeight = 866970L
        val mockRpc = new MockBitcoinRpc()

        println(s"[Test] Initializing oracle at height $startHeight")

        // Fetch initial chain state using mock RPC
        val initialState = BitcoinChainState
            .getInitialChainState(
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
            .await(30.seconds)

        println(s"[Test] Initial state fetched:")
        println(s"  Height: ${initialState.blockHeight}")
        println(s"  Hash: ${initialState.blockHash.toHex}")

        // Build and submit init transaction
        val scriptAddress = IntegrationTestContract.testnetScriptAddress

        val txResult = OracleTransactions.buildAndSubmitInitTransaction(
          ctx.alice.signer,
          ctx.provider,
          scriptAddress,
          ctx.alice.address,
          initialState,
          5_000_000L
        )

        txResult match {
            case Right(txHash) =>
                println(s"[Test] Oracle initialized: $txHash")

                // Wait for transaction confirmation
                val confirmed = waitForTransaction(ctx.provider, txHash)
                assert(confirmed, s"Transaction $txHash did not confirm")
                println(s"[Test] Transaction confirmed")

                // Verify oracle UTxO exists
                Thread.sleep(2000) // Give some time for UTxO to be indexed
                val oracleUtxo = findOracleUtxo(ctx.provider, scriptAddress, txHash)
                println(s"[Test] Oracle UTxO created at script address")

                // Verify datum
                val chainState = oracleUtxo.output.inlineDatum.get.to[ChainState]

                assert(
                  chainState.blockHeight == startHeight,
                  s"Block height mismatch: ${chainState.blockHeight} != $startHeight"
                )
                println(s"[Test] ChainState verified:")
                println(s"    Height: ${chainState.blockHeight}")
                println(s"    Hash: ${chainState.blockHash.toHex}")

            case Left(err) =>
                fail(s"Failed to initialize oracle: $err")
        }
    }

    test("init-oracle: fails gracefully with invalid start height") {
        val ctx = createYaciContext()
        given ec: ExecutionContext = ctx.provider.executionContext

        // Try to use a block height that doesn't exist in fixtures
        val invalidHeight = 999999999
        val mockRpc = new MockBitcoinRpc()

        println(s"[Test] Attempting to initialize with invalid height $invalidHeight")

        try {
            val initialState = BitcoinChainState
                .getInitialChainState(
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
                .await(10.seconds)
            fail("Should have thrown exception for invalid height")
        } catch {
            case e: Exception =>
                println(s"[Test] Correctly failed with: ${e.getMessage}")
                assert(
                  e.getMessage.contains("No such file") || e.getMessage.contains(
                    "not found"
                  ) || e.getMessage.contains("FileNotFoundException")
                    || e.getMessage.contains("block_999999999")
                )
        }
    }
}
