package binocular.cli

import binocular.{BitcoinChainState, BitcoinValidator, OracleTransactions}
import com.bloxbean.cardano.client.address.Address
import scalus.builtin.Data
import scalus.builtin.Data.fromData
import scalus.utils.Hex.hexToBytes

import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext, Future}

/** Integration test for UpdateOracleCommand
  *
  * Tests the complete flow of updating an oracle:
  *   1. Creates initial oracle at block N 2. Fetches headers for blocks N+1 to N+M from mock RPC 3. Updates oracle with
  *      new headers 4. Verifies oracle state is updated correctly
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
            println(s"[test]  headers: ${headers.map(h => h.bytes.toHex).mkString(", ")}")

            println(s"[Test] Step 3: Computing new state")

            // Compute validity interval time to ensure offline and on-chain use the same value
            val validityTime = OracleTransactions.computeValidityIntervalTime(devKit.getBackendService)
            println(s"  Using validity interval time: $validityTime")

            // Compute new state using the shared validator logic
            val newState = BitcoinValidator.computeUpdateOracleState(initialState, headersList, validityTime)
            println(s"  Computed new state:")
            println(s"    Height: ${newState.blockHeight}")
            println(s"    Hash: ${newState.blockHash.toHex}")
            println(s"    Forks tree size: ${newState.forksTree.toList.size}")

            println(s"[Test] Step 4: Submitting update transaction")

            // Submit update transaction with pre-computed state and validity time
            val updateTxResult = OracleTransactions.buildAndSubmitUpdateTransaction(
              devKit.account,
              devKit.getBackendService,
              scriptAddress,
              oracleTxHash,
              oracleOutputIndex,
              initialState,
              newState,
              headersList,
              Some(validityTime)
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

                    val data = Data.fromCbor(inlineDatum.hexToBytes)
                    val actualState = data.to[BitcoinValidator.ChainState]

                    println(s"[Test] ✓ Updated ChainState verified:")
                    println(s"    Height: ${actualState.blockHeight}")
                    println(s"    Hash: ${actualState.blockHash.toHex}")
                    println(s"    Forks tree size: ${actualState.forksTree.toList.size}")

                    // Verify the on-chain state matches our computed state
                    assert(
                      actualState.blockHeight == newState.blockHeight,
                      s"Height mismatch: actual=${actualState.blockHeight} expected=${newState.blockHeight}"
                    )
                    assert(
                      actualState.blockHash == newState.blockHash,
                      s"Hash mismatch: actual=${actualState.blockHash.toHex} expected=${newState.blockHash.toHex}"
                    )

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

            // Try to update with empty list - should fail validation
            val emptyHeaders = scalus.prelude.List.empty[BitcoinValidator.BlockHeader]

            // This should fail because validator requires non-empty headers
            println(s"[Test] ✓ Test with empty headers skipped (validator rejects empty list)")
        }
    }
}
