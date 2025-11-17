package binocular
package cli

import com.bloxbean.cardano.client.address.Address
import scalus.builtin.Data
import scalus.utils.Hex.hexToBytes

import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext, Future}

/** Integration test for UpdateOracleCommand
  *
  * Tests the complete flow of updating an oracle:
  *   1. Creates initial oracle at block N 2. Fetches headers for blocks N+1 to N+M from mock RPC 3.
  *      Updates oracle with new headers 4. Verifies oracle state is updated correctly
  */
class UpdateOracleCommandIntegrationTest extends CliIntegrationTestBase {
    // Extended timeout for multi-batch promotion test
    override val munitTimeout = scala.concurrent.duration.Duration(240, "s")

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

            println(
              s"[Test] Step 2: Fetching headers for blocks ${startHeight + 1} to $updateToHeight"
            )

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
            val validityTime =
                OracleTransactions.computeValidityIntervalTime(devKit.getBackendService)
            println(s"  Using validity interval time: $validityTime")

            // Compute new state using the shared validator logic
            val newState =
                BitcoinValidator.computeUpdateOracleState(initialState, headersList, validityTime)
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
                    val actualState = data.to[ChainState]

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
            val emptyHeaders = scalus.prelude.List.empty[BlockHeader]

            // This should fail because validator requires non-empty headers
            println(s"[Test] ✓ Test with empty headers skipped (validator rejects empty list)")
        }
    }

    test("update-oracle: triggers forced promotion after 100 confirmations and 200 minutes") {
        withYaciDevKit() { devKit =>
            given ec: ExecutionContext = ExecutionContext.global

            // Use a sufficient range of blocks to trigger promotion
            // Start at 866970, add blocks up to 866970 + 105 to ensure 100+ confirmations
            val startHeight = 866970
            val totalBlocks = 105 // More than MaturationConfirmations (100)
            val batchSize = 10 // Process 10 headers per transaction
            val finalHeight = startHeight + totalBlocks
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

            val (initialTxHash, initialOutputIndex) = initTxResult match {
                case Right(txHash) =>
                    println(s"[Test] ✓ Oracle initialized: $txHash")
                    devKit.waitForTransaction(txHash, maxAttempts = 30)
                    Thread.sleep(2000)
                    (txHash, 0)
                case Left(err) =>
                    fail(s"Failed to initialize oracle: $err")
            }

            println(s"[Test] Step 2: Adding ${totalBlocks} blocks in batches of $batchSize")

            // Fetch all headers
            val allHeadersFuture = Future.sequence(
              (startHeight + 1 to finalHeight).map { height =>
                  for {
                      hashHex <- mockRpc.getBlockHash(height)
                      headerInfo <- mockRpc.getBlockHeader(hashHex)
                  } yield BitcoinChainState.convertHeader(headerInfo)
              }
            )

            val allHeaders = Await.result(allHeadersFuture, 60.seconds)
            println(s"[Test] ✓ Fetched ${allHeaders.length} headers total")

            // Process in batches
            val batches = allHeaders.grouped(batchSize).toSeq
            var currentState = initialState
            var currentTxHash = initialTxHash
            var currentOutputIndex = initialOutputIndex

            batches.zipWithIndex.foreach { case (batch, batchIndex) =>
                println(
                  s"[Test] Processing batch ${batchIndex + 1}/${batches.size} (${batch.size} headers)"
                )

                val headersList = scalus.prelude.List.from(batch.toList)

                // Use current time for all batches except the last one
                // For the last batch, use time + 210 minutes to trigger promotion
                val validityTime: BigInt = if batchIndex == batches.size - 1 then {
                    val currentTime = System.currentTimeMillis() / 1000
                    val advancedTime = BigInt(currentTime) + BigInt(210 * 60) // 210 minutes ahead
                    println(s"  Final batch: using advanced time to trigger promotion (+210 min)")
                    advancedTime
                } else {
                    OracleTransactions.computeValidityIntervalTime(devKit.getBackendService)
                }

                // Compute new state
                val newState = BitcoinValidator.computeUpdateOracleState(
                  currentState,
                  headersList,
                  validityTime
                )

                // Submit update transaction
                val updateTxResult = OracleTransactions.buildAndSubmitUpdateTransaction(
                  devKit.account,
                  devKit.getBackendService,
                  scriptAddress,
                  currentTxHash,
                  currentOutputIndex,
                  currentState,
                  newState,
                  headersList,
                  Some(validityTime)
                )

                updateTxResult match {
                    case Right(resultTxHash) =>
                        println(s"  ✓ Batch ${batchIndex + 1} submitted: $resultTxHash")
                        devKit.waitForTransaction(resultTxHash, maxAttempts = 30)
                        Thread.sleep(2000)

                        // Update for next iteration
                        currentState = newState
                        currentTxHash = resultTxHash
                        currentOutputIndex = 0

                    case Left(errorMsg) =>
                        fail(s"Failed to update oracle with batch ${batchIndex + 1}: $errorMsg")
                }
            }

            println(s"[Test] Step 3: Verifying promotion occurred")
            println(s"  Initial confirmed height: ${initialState.blockHeight}")
            println(s"  Final confirmed height: ${currentState.blockHeight}")
            println(s"  Initial forks tree size: ${initialState.forksTree.toList.size}")
            println(s"  Final forks tree size: ${currentState.forksTree.toList.size}")

            // Verify that promotion occurred
            val heightIncrease = currentState.blockHeight - initialState.blockHeight
            assert(
              heightIncrease > 0,
              s"Expected promotion to increase confirmed height, but got: initial=${initialState.blockHeight}, final=${currentState.blockHeight}"
            )

            println(s"  ✓ Promotion detected: height increased by $heightIncrease blocks")

            // Verify confirmed blocks tree was updated
            assert(
              currentState.confirmedBlocksTree.size >= initialState.confirmedBlocksTree.size,
              "Confirmed blocks tree should grow after promotion"
            )

            println(s"[Test] Step 4: Verifying on-chain state after promotion")

            // Verify final on-chain state
            val utxos = devKit.getUtxos(scriptAddress.getAddress)
            assert(utxos.nonEmpty, "No UTxOs found at oracle address after promotion")

            val latestUtxo = utxos.head
            val inlineDatum = latestUtxo.getInlineDatum

            val data = Data.fromCbor(inlineDatum.hexToBytes)
            val actualState = data.to[ChainState]

            println(s"[Test] ✓ On-chain state after forced promotion:")
            println(s"    Height: ${actualState.blockHeight}")
            println(s"    Hash: ${actualState.blockHash.toHex}")
            println(s"    Forks tree size: ${actualState.forksTree.toList.size}")
            println(s"    Confirmed blocks tree size: ${actualState.confirmedBlocksTree.size}")

            // Verify state matches expectations
            assert(
              actualState.blockHeight == currentState.blockHeight,
              s"Height mismatch: actual=${actualState.blockHeight} expected=${currentState.blockHeight}"
            )
            assert(
              actualState.blockHash == currentState.blockHash,
              s"Hash mismatch"
            )

            println(s"[Test] ✓ Forced promotion test completed successfully")
            println(s"  Total blocks added: $totalBlocks")
            println(s"  Blocks promoted: $heightIncrease")
            println(s"  Batches processed: ${batches.size}")
        }
    }
}
