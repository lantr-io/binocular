package binocular

import binocular.cli.CliIntegrationTestBase
import scalus.cardano.onchain.plutus.prelude.List as ScalusList
import scalus.utils.await

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*


/** Integration tests for Binocular Oracle on Yaci DevKit
  *
  * Tests sequential single-header oracle updates.
  */
class BinocularIntegrationTest extends CliIntegrationTestBase {

    test("Can submit sequential Bitcoin headers through Oracle") {
        val ctx = createYaciContext()
        given ec: ExecutionContext = ctx.provider.executionContext

        println("\n=== Sequential Oracle Update Test ===\n")

        val startHeight = 866970
        val mockRpc = new MockBitcoinRpc()

        // 1. Create initial oracle state and mint NFT
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
              startHeight
            )
            .await(30.seconds)

        val (oracleUtxo, itScript, scriptAddress, itParams) =
            initOracleWithNft(ctx, initialState)

        println(s"[Test] Oracle initialized at height $startHeight")

        // 2. Submit headers 866971 and 866972 one at a time
        val headersToSubmit = Seq(866971, 866972)
        var currentState = initialState
        var currentOracleUtxo = oracleUtxo

        headersToSubmit.zipWithIndex.foreach { case (height, idx) =>
            println(s"\n--- Submitting header ${idx + 1}/${headersToSubmit.length} (height $height) ---")

            // Fetch single header
            val header = (for {
                hashHex <- mockRpc.getBlockHash(height)
                headerInfo <- mockRpc.getBlockHeader(hashHex)
            } yield BitcoinChainState.convertHeader(headerInfo)).await(30.seconds)

            val headersList = ScalusList.single(header)

            // Compute new state
            val (_, validityTime) =
                OracleTransactions.computeValidityIntervalTime(ctx.provider.cardanoInfo)
            val parentPath = currentState.forkTree.findTipPath
            val newState = OracleTransactions.applyHeaders(
              currentState,
              headersList,
              parentPath,
              validityTime,
              itParams
            )

            // Submit update
            val updateResult = OracleTransactions.buildAndSubmitUpdateTransaction(
              ctx.alice.signer,
              ctx.provider,
              scriptAddress,
              ctx.alice.address,
              currentOracleUtxo,
              currentState,
              newState,
              headersList,
              parentPath,
              validityTime,
              itParams.oneShotTxOutRef,
              scriptOverride = Some(itScript)
            )

            updateResult match {
                case Right(txHash) =>
                    println(s"  UpdateOracle tx: $txHash")
                    val confirmed =
                        waitForTransaction(ctx.provider, txHash, maxAttempts = 30)
                    assert(confirmed, s"Update transaction $txHash did not confirm")
                    Thread.sleep(2000) // Wait for indexing

                    // Fetch updated oracle UTxO for next iteration
                    currentOracleUtxo =
                        findOracleUtxo(ctx.provider, scriptAddress, txHash)
                    currentState = newState

                case Left(error) =>
                    fail(s"Failed to submit header at height $height: $error")
            }
        }

        // 3. Verify final state
        val finalState =
            currentOracleUtxo.output.inlineDatum.get.to[ChainState]
        println(s"\nFinal forks tree block count: ${finalState.forkTree.blockCount}")
        assert(
          finalState.forkTree.blockCount == headersToSubmit.length,
          s"Expected ${headersToSubmit.length} blocks in forks tree"
        )

        println(s"Successfully submitted all ${headersToSubmit.length} headers sequentially!")
    }
}
