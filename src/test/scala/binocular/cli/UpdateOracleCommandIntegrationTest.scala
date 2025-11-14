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

    override def munitFixtures = List(yaciDevKitFixture)

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

                val plutusData = com.bloxbean.cardano.client.plutus.spec.PlutusData.deserialize(
                    com.bloxbean.cardano.client.util.HexUtil.decodeHexString(inlineDatum)
                )
                val data = OracleTransactions.plutusDataToScalusData(plutusData)
                val actualState = data.to[BitcoinValidator.ChainState]

                println(s"[Test] ✓ Updated ChainState verified:")
                println(s"    Height: ${actualState.blockHeight}")
                println(s"    Hash: ${actualState.blockHash.toHex}")
                println(s"    Forks tree size: ${actualState.forksTree.toList.size}")

                // Verify the on-chain state matches our computed state
                assert(actualState.blockHeight == newState.blockHeight,
                       s"Height mismatch: actual=${actualState.blockHeight} expected=${newState.blockHeight}")
                assert(actualState.blockHash == newState.blockHash,
                       s"Hash mismatch: actual=${actualState.blockHash.toHex} expected=${newState.blockHash.toHex}")

            case Left(err) =>
                fail(s"Failed to update oracle: $err")
        }
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

        println(s"[Test] Attempting update with empty header list")

        // Try to update with empty list - should fail validation
        val emptyHeaders = scalus.prelude.List.empty[BitcoinValidator.BlockHeader]

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

    test("update-oracle: forces promotion of a block after 110 confirmations and 200 minutes aging") {
        val devKit = yaciDevKitFixture()
        given ec: ExecutionContext = ExecutionContext.global

        val startHeight = 866880
        val promotionCandidateHeight = startHeight + 1
        val headersToSubmitCounts = List.fill(11)(10)
        val totalHeadersToSubmit = headersToSubmitCounts.sum
        val endHeight = startHeight + totalHeadersToSubmit
        val finalHeight = endHeight + 1
        val mockRpc = new MockBitcoinRpc()

        println(s"=== Block Promotion Test ===")
        println(s"Start: $startHeight, End: $finalHeight, Total blocks: ${finalHeight - startHeight}")

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

            val (nextTxHash, nextOutputIndex) = updateResult match {
                case Right(txHash) =>
                    println(s"✓ Submitted $count headers in tx: $txHash")
                    devKit.waitForTransaction(txHash, maxAttempts = 30)
                    Thread.sleep(2000)
                    (txHash, 0)
                case Left(error) => fail(s"Failed to submit headers batch ${i + 1}: $error")
            }
            currentTxHash = nextTxHash
            currentOutputIndex = nextOutputIndex
            currentState = newState
            currentBlockHeight = batchEndHeight
        }


        // 3. Simulate time passing and submit one more header to trigger promotion
        val simulatedTime = lastUpdateTime + BitcoinValidator.ChallengeAging + 60 // 200 minutes + buffer
        val finalHeader = Await.result(
            mockRpc.getBlockHash(finalHeight).flatMap(mockRpc.getBlockHeader).map(BitcoinChainState.convertHeader),
            30.seconds
        )
        val finalHeaderList = scalus.prelude.List.from(scala.List(finalHeader))

        println(s"✓ Submitting final header for height $finalHeight to trigger promotion...")
        println(s"  Simulated current time: $simulatedTime")

        val expectedFinalState =
            BitcoinValidator.computeUpdateOracleState(currentState, finalHeaderList, simulatedTime)
        println(s"  Expected final height (after promotion): ${expectedFinalState.blockHeight}")

        val updateResult3 = OracleTransactions.buildAndSubmitUpdateTransaction(
            devKit.account,
            devKit.getBackendService,
            scriptAddress,
            currentTxHash,
            currentOutputIndex,
            currentState,
            expectedFinalState,
            finalHeaderList,
            Some(lastUpdateTime)
        )

        updateResult3 match {
            case Right(txHash) =>
                println(s"✓ Final UpdateOracle tx: $txHash")
                devKit.waitForTransaction(txHash, maxAttempts = 30)
                Thread.sleep(2000)

                // 4. Verify promotion
                val utxos = devKit.getUtxos(scriptAddress.getAddress)
                val latestUtxo = utxos.head
                val latestDatum = latestUtxo.getInlineDatum
                val plutusData = com.bloxbean.cardano.client.plutus.spec.PlutusData
                    .deserialize(com.bloxbean.cardano.client.util.HexUtil.decodeHexString(latestDatum))
                val onChainState =
                    OracleTransactions.plutusDataToScalusData(plutusData).to[BitcoinValidator.ChainState]

                println(s"✓ Verifying on-chain state...")
                println(s"  On-chain height: ${onChainState.blockHeight}")
                println(
                    s"  On-chain confirmed root: ${BitcoinValidator.getMerkleRoot(onChainState.confirmedBlocksTree).toHex}"
                )

                assert(
                    onChainState.blockHeight == promotionCandidateHeight,
                    s"Block height should be promoted to $promotionCandidateHeight, but was ${onChainState.blockHeight}"
                )
                assert(onChainState.blockHash != initialOracleState.blockHash, "Block hash should be promoted")
                assert(
                    BitcoinValidator.getMerkleRoot(onChainState.confirmedBlocksTree) != BitcoinValidator
                        .getMerkleRoot(initialOracleState.confirmedBlocksTree),
                    "Confirmed blocks tree should have changed"
                )
                println("✓ Block promotion successful!")

            case Left(error) => fail(s"Failed to submit final header: $error")

        }

    }

}
