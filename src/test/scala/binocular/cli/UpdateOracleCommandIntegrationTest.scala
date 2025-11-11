package binocular.cli

import binocular.{BitcoinChainState, OracleTransactions, BitcoinValidator}
import com.bloxbean.cardano.client.address.Address
import scalus.builtin.Data.fromData
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.*

/** Integration test for UpdateOracleCommand
  *
  * Tests the complete flow of updating an oracle:
  * 1. Creates initial oracle at block N
  * 2. Fetches headers for blocks N+1 to N+M from mock RPC
  * 3. Updates oracle with new headers
  * 4. Verifies oracle state is updated correctly
  */
class UpdateOracleCommandIntegrationTest extends CliIntegrationTestBase {

    test("update-oracle: successfully updates oracle with new blocks") {
        withYaciDevKit() { devKit =>
            given ec: ExecutionContext = ExecutionContext.global

            // Use consecutive blocks from our fixtures
            // Start at 866970, update to 866971-866973 (3 blocks)
            val startHeight = 866970
            val updateToHeight = 866973
            val mockRpc = new MockBitcoinRpc()

            println(s"[Test] Step 1: Creating initial oracle at height $startHeight")

            // Create initial oracle
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
                startHeight
            )

            val initialState = Await.result(initialStateFuture, 30.seconds)

            val scriptAddress = new Address(
                binocular.OracleConfig(network = binocular.CardanoNetwork.Testnet).scriptAddress
            )

            // Submit init transaction
            val initTxResult = OracleTransactions.buildAndSubmitInitTransaction(
                devKit.account,
                devKit.getBackendService,
                scriptAddress,
                initialState
            )

            val (oracleTxHash, oracleOutputIndex) = initTxResult match {
                case Right(txHash) =>
                    println(s"[Test] ✓ Oracle initialized: $txHash")
                    devKit.waitForTransaction(txHash, maxAttempts = 30)
                    Thread.sleep(2000) // Wait for indexing
                    (txHash, 0)
                case Left(err) =>
                    fail(s"Failed to initialize oracle: $err")
            }

            println(s"[Test] Step 2: Fetching headers for blocks ${startHeight + 1} to $updateToHeight")

            // Fetch headers for update
            val headersFuture = Future.sequence(
                (startHeight + 1 to updateToHeight).map { height =>
                    for {
                        hashHex <- mockRpc.getBlockHash(height)
                        headerInfo <- mockRpc.getBlockHeader(hashHex)
                    } yield BitcoinChainState.convertHeader(headerInfo)
                }
            )

            val headers = Await.result(headersFuture, 30.seconds)
            val headersList = scalus.prelude.List.from(headers.toList)

            println(s"[Test] ✓ Fetched ${headers.length} headers")

            println(s"[Test] Step 3: Calculating new ChainState")

            // Calculate new state
            val currentTime = BigInt(System.currentTimeMillis() / 1000)
            val newState = OracleTransactions.applyHeaders(initialState, headersList, currentTime)

            println(s"[Test] ✓ New state calculated:")
            println(s"    Old height: ${initialState.blockHeight}")
            println(s"    New height: ${newState.blockHeight}")

            assert(newState.blockHeight == updateToHeight, s"Height mismatch: ${newState.blockHeight} != $updateToHeight")

            println(s"[Test] Step 4: Submitting update transaction")

            // Submit update transaction
            val updateTxResult = OracleTransactions.buildAndSubmitUpdateTransaction(
                devKit.account,
                devKit.getBackendService,
                scriptAddress,
                oracleTxHash,
                oracleOutputIndex,
                initialState,
                newState,
                headersList
            )

            updateTxResult match {
                case Right(txHash) =>
                    println(s"[Test] ✓ Oracle updated: $txHash")

                    // Wait for confirmation
                    val confirmed = devKit.waitForTransaction(txHash, maxAttempts = 30)
                    assert(confirmed, s"Update transaction did not confirm")

                    Thread.sleep(2000) // Wait for indexing

                    println(s"[Test] Step 5: Verifying updated oracle state")

                    // Verify new oracle state
                    val utxos = devKit.getUtxos(scriptAddress.getAddress)
                    assert(utxos.nonEmpty, "No UTxOs found at oracle address after update")

                    // Find the latest UTxO (should be from update tx)
                    val latestUtxo = utxos.head
                    val inlineDatum = latestUtxo.getInlineDatum

                    val plutusData = com.bloxbean.cardano.client.plutus.spec.PlutusData.deserialize(
                        com.bloxbean.cardano.client.util.HexUtil.decodeHexString(inlineDatum)
                    )
                    val data = OracleTransactions.plutusDataToScalusData(plutusData)
                    val chainState = data.to[BitcoinValidator.ChainState]

                    println(s"[Test] ✓ Updated ChainState verified:")
                    println(s"    Height: ${chainState.blockHeight}")
                    println(s"    Hash: ${chainState.blockHash.toHex}")

                    assert(chainState.blockHeight == updateToHeight, s"Updated height mismatch: ${chainState.blockHeight} != $updateToHeight")

                case Left(err) =>
                    fail(s"Failed to update oracle: $err")
            }
        }
    }

    test("update-oracle: handles empty header list") {
        withYaciDevKit() { devKit =>
            given ec: ExecutionContext = ExecutionContext.global

            val startHeight = 866970
            val mockRpc = new MockBitcoinRpc()

            println(s"[Test] Creating oracle at height $startHeight")

            // Create initial oracle
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
                startHeight
            )

            val initialState = Await.result(initialStateFuture, 30.seconds)

            val scriptAddress = new Address(
                binocular.OracleConfig(network = binocular.CardanoNetwork.Testnet).scriptAddress
            )

            val initTxResult = OracleTransactions.buildAndSubmitInitTransaction(
                devKit.account,
                devKit.getBackendService,
                scriptAddress,
                initialState
            )

            val (oracleTxHash, oracleOutputIndex) = initTxResult match {
                case Right(txHash) =>
                    devKit.waitForTransaction(txHash, maxAttempts = 30)
                    Thread.sleep(2000)
                    (txHash, 0)
                case Left(err) =>
                    fail(s"Failed to initialize oracle: $err")
            }

            println(s"[Test] Attempting update with empty header list")

            // Try to update with empty list
            val emptyHeaders = scalus.prelude.List.empty[BitcoinValidator.BlockHeader]
            val currentTime = BigInt(System.currentTimeMillis() / 1000)
            val newState = OracleTransactions.applyHeaders(initialState, emptyHeaders, currentTime)

            // State should be unchanged
            assert(newState.blockHeight == initialState.blockHeight, "State changed with empty headers")
            println(s"[Test] ✓ State correctly unchanged with empty headers")
        }
    }
}
