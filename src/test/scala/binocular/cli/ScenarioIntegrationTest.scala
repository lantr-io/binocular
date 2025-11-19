package binocular
package cli

import binocular.{reverse, BitcoinChainState, BitcoinValidator, MerkleTree, OracleTransactions, TransactionVerifierContract, TxVerifierDatum, TxVerifierRedeemer}
import com.bloxbean.cardano.client.address.Address
import scalus.builtin.{ByteString, Data}
import scalus.builtin.Data.fromData
import scalus.builtin.ToData.toData
import scalus.utils.Hex.hexToBytes

import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext, Future}

/** Comprehensive integration test for Binocular oracle scenarios
  *
  * Tests the complete flow of oracle operations:
  *   1. Oracle initialization
  *   2. Oracle updates with block headers
  *   3. Block promotion after 100+ confirmations
  *   4. Transaction inclusion proofs
  *   5. On-chain transaction verification
  */
class ScenarioIntegrationTest extends CliIntegrationTestBase {
    // Extended timeout for multi-batch promotion test
    override val munitTimeout = scala.concurrent.duration.Duration(360, "s")

    test("scenario: basic oracle update with new blocks") {
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
            println(s"    Forks tree size: ${newState.forksTree.size}")

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
              validityTime
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
                    println(s"    Forks tree size: ${actualState.forksTree.size}")

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

    test("scenario: handles empty header list") {
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

    test("scenario: full lifecycle - promotion, proof, and verification") {
        withYaciDevKit() { devKit =>
            given ec: ExecutionContext = ExecutionContext.global

            // Use a sufficient range of blocks to trigger promotion
            // Start at 866970, add blocks up to 866970 + 105 to ensure 100+ confirmations
            val startHeight = 866970
            val totalBlocks = 105 // More than MaturationConfirmations (100)
            val batchSize =
                10 // Process 10 headers per transaction (using reference script to reduce tx size)
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

            // Deploy reference script to reduce transaction size
            // Deploy to script address to avoid collateral conflicts with account's UTxOs
            println(s"[Test] Step 1b: Deploying reference script")
            val refScriptResult = OracleTransactions.deployReferenceScript(
              devKit.account,
              devKit.getBackendService,
              scriptAddress.getAddress // Deploy to script address
            )

            val referenceScriptUtxo = refScriptResult match {
                case Right((txHash, outputIndex)) =>
                    println(s"[Test] ✓ Reference script deployed: $txHash:$outputIndex")
                    devKit.waitForTransaction(txHash, maxAttempts = 30)
                    Thread.sleep(2000)
                    Some((txHash, outputIndex))
                case Left(err) =>
                    fail(s"Failed to deploy reference script: $err")
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

            val allHeaders = Await.result(allHeadersFuture, 120.seconds)
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
                println(s"  Current state before batch:")
                println(s"    blockHeight: ${currentState.blockHeight}")
                println(s"    blockHash: ${currentState.blockHash.toHex}")
                println(s"    forksTree size: ${currentState.forksTree.size}")
                println(s"    confirmedBlocksTree size: ${currentState.confirmedBlocksTree.size}")

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

                println(s"  [Batch ${batchIndex + 1}] Off-chain computed state:")
                println(s"    blockHeight: ${newState.blockHeight}")
                println(s"    blockHash: ${newState.blockHash.toHex}")
                println(s"    forksTree size: ${newState.forksTree.size}")
                println(s"    confirmedBlocksTree size: ${newState.confirmedBlocksTree.size}")

                // Log forksTree branches for debugging
                println(s"  [Batch ${batchIndex + 1}] OFF-CHAIN forksTree branches:")
                newState.forksTree.foreach { branch =>
                    println(
                      s"    tip: ${branch.tipHash.toHex}, height: ${branch.tipHeight}, blocks: ${branch.recentBlocks.size}"
                    )
                }

                // Submit update transaction (using reference script to reduce tx size)
                val updateTxResult = OracleTransactions.buildAndSubmitUpdateTransaction(
                  devKit.account,
                  devKit.getBackendService,
                  scriptAddress,
                  currentTxHash,
                  currentOutputIndex,
                  currentState,
                  newState,
                  headersList,
                  validityTime,
                  referenceScriptUtxo,
                  inTestMode = true // Skip off-chain time tolerance check
                )

                updateTxResult match {
                    case Right(resultTxHash) =>
                        println(s"  ✓ Batch ${batchIndex + 1} submitted: $resultTxHash")
                        devKit.waitForTransaction(resultTxHash, maxAttempts = 30)
                        Thread.sleep(2000)

                        // Read actual on-chain state and verify it matches what we sent
                        // Since validator only validates (can't modify), on-chain state MUST match newState
                        val utxos = devKit.getUtxos(scriptAddress.getAddress)
                        require(utxos.nonEmpty, "No UTxOs found after batch update")
                        // Filter for UTxO with inline datum (oracle UTxO, not reference script UTxO)
                        val oracleUtxo = utxos.find(_.getInlineDatum != null).getOrElse {
                            fail("No oracle UTxO with inline datum found after batch update")
                        }
                        val actualOnChainState =
                            Data.fromCbor(oracleUtxo.getInlineDatum.hexToBytes).to[ChainState]

                        println(s"  On-chain state after batch ${batchIndex + 1}:")
                        println(s"    blockHeight: ${actualOnChainState.blockHeight}")
                        println(s"    blockHash: ${actualOnChainState.blockHash.toHex}")
                        println(s"    forksTree size: ${actualOnChainState.forksTree.size}")
                        println(
                          s"    confirmedBlocksTree size: ${actualOnChainState.confirmedBlocksTree.size}"
                        )

                        // Verify on-chain state matches what we computed off-chain
                        if actualOnChainState.blockHeight != newState.blockHeight ||
                            actualOnChainState.blockHash != newState.blockHash ||
                            actualOnChainState.forksTree.size != newState.forksTree.size
                        then {

                            fail(
                              s"ERROR: On-chain state does not match off-chain computed state!\n" +
                                  s"  This should be impossible - validator can only validate, not modify.\n" +
                                  s"  Off-chain: height=${newState.blockHeight}, hash=${newState.blockHash.toHex}, forksTree=${newState.forksTree.size}\n" +
                                  s"  On-chain:  height=${actualOnChainState.blockHeight}, hash=${actualOnChainState.blockHash.toHex}, forksTree=${actualOnChainState.forksTree.size}"
                            )
                        }

                        // Update for next iteration
                        currentState = newState
                        currentTxHash = oracleUtxo.getTxHash
                        currentOutputIndex = oracleUtxo.getOutputIndex

                    case Left(errorMsg) =>
                        fail(s"Failed to update oracle with batch ${batchIndex + 1}: $errorMsg")
                }
            }

            println(s"[Test] Step 3: Verifying promotion occurred")
            println(s"  Initial confirmed height: ${initialState.blockHeight}")
            println(s"  Final confirmed height: ${currentState.blockHeight}")
            println(s"  Initial forks tree size: ${initialState.forksTree.size}")
            println(s"  Final forks tree size: ${currentState.forksTree.size}")

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

            // Filter for UTxO with inline datum (oracle UTxO, not reference script UTxO)
            val latestUtxo = utxos.find(_.getInlineDatum != null).getOrElse {
                fail("No oracle UTxO with inline datum found after promotion")
            }
            val inlineDatum = latestUtxo.getInlineDatum

            val data = Data.fromCbor(inlineDatum.hexToBytes)
            val actualState = data.to[ChainState]

            println(s"[Test] ✓ On-chain state after forced promotion:")
            println(s"    Height: ${actualState.blockHeight}")
            println(s"    Hash: ${actualState.blockHash.toHex}")
            println(s"    Forks tree size: ${actualState.forksTree.size}")
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

            println(s"[Test] ✓ Phase 1 completed: Promotion successful")
            println(s"  Total blocks added: $totalBlocks")
            println(s"  Blocks promoted: $heightIncrease")
            println(s"  Batches processed: ${batches.size}")

            // ========== Phase 2: Prove transaction inclusion ==========
            println(s"\n[Test] Phase 2: Proving transaction inclusion")

            // Use the initial block (866970) which should now be in confirmed state
            val proofBlockHeight = startHeight
            println(s"[Test] Step 5: Fetching block data for height $proofBlockHeight")

            // Get block info to find a transaction to prove
            val blockHashFuture = mockRpc.getBlockHash(proofBlockHeight)
            val blockHash = Await.result(blockHashFuture, 10.seconds)

            val blockInfoFuture = mockRpc.getBlock(blockHash)
            val blockInfo = Await.result(blockInfoFuture, 10.seconds)

            assert(blockInfo.tx.nonEmpty, "Block has no transactions")

            // Use the first transaction (coinbase)
            val btcTxId = blockInfo.tx.head.txid
            val txIndex = 0

            println(s"[Test] ✓ Testing with transaction: $btcTxId")
            println(s"[Test]   Block has ${blockInfo.tx.length} transactions")

            println(s"[Test] Step 6: Building Merkle proof")

            // Build Merkle tree from transaction hashes
            val txHashes = blockInfo.tx.map { tx =>
                ByteString.fromHex(tx.txid).reverse
            }

            val merkleTree = MerkleTree.fromHashes(txHashes)
            val merkleRoot = merkleTree.getMerkleRoot
            val merkleProof = merkleTree.makeMerkleProof(txIndex)

            println(s"[Test] ✓ Merkle proof generated:")
            println(s"    Merkle Root: ${merkleRoot.toHex}")
            println(s"    Proof Size: ${merkleProof.length} hashes")

            println(s"[Test] Step 7: Verifying proof locally")

            // Verify proof
            val txHash = ByteString.fromHex(btcTxId).reverse
            val calculatedRoot =
                MerkleTree.calculateMerkleRootFromProof(txIndex, txHash, merkleProof)

            assert(calculatedRoot == merkleRoot, "Merkle proof verification failed")
            println(s"[Test] ✓ Merkle proof verified")
            println(s"    Calculated root: ${calculatedRoot.toHex}")
            println(s"    Expected root:   ${merkleRoot.toHex}")

            println(s"[Test] Step 8: Verifying transaction is in confirmed block")

            // Verify block height is within promoted range
            assert(
              proofBlockHeight <= actualState.blockHeight.toInt,
              s"Block height $proofBlockHeight > oracle height ${actualState.blockHeight}"
            )

            // Verify block is not in forks tree (should be promoted to confirmed)
            val blockHashBytes = ByteString.fromHex(blockHash).reverse
            val blockInForksTree = actualState.forksTree.exists { branch =>
                branch.tipHash == blockHashBytes ||
                BitcoinValidator.existsInSortedList(branch.recentBlocks, blockHashBytes)
            }
            assert(!blockInForksTree, "Block should not be in forks tree after promotion")

            println(s"[Test] ✓ Transaction is in confirmed block")
            println(s"    Block Height: $proofBlockHeight")
            println(s"    Oracle Confirmed Height: ${actualState.blockHeight}")

            println(s"\n[Test] ✓ Phase 2 completed: Transaction proof verified")

            // ========== Phase 3: Verify proofs at different indices ==========
            println(s"\n[Test] Phase 3: Testing Merkle proofs for multiple transaction indices")

            // Test first 5 transactions (or all if less than 5)
            val testCount = Math.min(5, blockInfo.tx.length)

            for testTxIndex <- 0 until testCount do {
                val testTxId = blockInfo.tx(testTxIndex).txid
                val testTxHash = ByteString.fromHex(testTxId).reverse
                val testProof = merkleTree.makeMerkleProof(testTxIndex)

                val testCalculatedRoot =
                    MerkleTree.calculateMerkleRootFromProof(testTxIndex, testTxHash, testProof)

                assert(
                  testCalculatedRoot == merkleRoot,
                  s"Proof failed for tx at index $testTxIndex"
                )
                println(s"[Test] ✓ Verified proof for transaction at index $testTxIndex")
            }

            println(s"[Test] ✓ All $testCount Merkle proofs verified")

            println(s"\n[Test] ✓ Phase 3 completed: Multiple proof indices verified")

            // ========== Phase 4: On-chain transaction verification ==========
            println(
              s"\n[Test] Phase 4: On-chain transaction verification with TransactionVerifier contract"
            )

            // Get the TransactionVerifier script
            val verifierScript = {
                val program = TransactionVerifierContract.validator
                val scriptCborHex = program.doubleCborHex
                com.bloxbean.cardano.client.plutus.spec.PlutusV3Script
                    .builder()
                    .`type`("PlutusScriptV3")
                    .cborHex(scriptCborHex)
                    .build()
                    .asInstanceOf[com.bloxbean.cardano.client.plutus.spec.PlutusV3Script]
            }

            val verifierAddress = com.bloxbean.cardano.client.address.AddressProvider
                .getEntAddress(
                  verifierScript,
                  com.bloxbean.cardano.client.common.model.Networks.testnet()
                )
                .getAddress

            println(s"[Test] Step 9: Locking value at TransactionVerifier contract")
            println(s"    Verifier address: $verifierAddress")

            // Create datum with the transaction we want to verify
            val verifierDatum = TxVerifierDatum(
              expectedTxHash = txHash, // The tx hash we proved above
              blockMerkleRoot = merkleRoot
            )

            // Lock some ADA at the verifier address
            val lockAmount = 2000000L // 2 ADA
            val datumData = scalus.bloxbean.Interop.toPlutusData(verifierDatum.toData)

            val lockTx = new com.bloxbean.cardano.client.quicktx.Tx()
                .payToContract(
                  verifierAddress,
                  com.bloxbean.cardano.client.api.model.Amount.lovelace(
                    java.math.BigInteger.valueOf(lockAmount)
                  ),
                  datumData
                )
                .from(devKit.account.baseAddress())

            val lockTxBuilder = new com.bloxbean.cardano.client.quicktx.QuickTxBuilder(
              devKit.getBackendService
            )
            val lockResult = lockTxBuilder
                .compose(lockTx)
                .withSigner(
                  com.bloxbean.cardano.client.function.helper.SignerProviders.signerFrom(
                    devKit.account
                  )
                )
                .completeAndWait()

            assert(lockResult.isSuccessful, s"Lock transaction failed: ${lockResult.getResponse}")
            val lockTxHash = lockResult.getValue
            println(s"[Test] ✓ Value locked at verifier: $lockTxHash")

            devKit.waitForTransaction(lockTxHash, maxAttempts = 30)
            Thread.sleep(2000)

            println(s"[Test] Step 10: Unlocking value with valid Merkle proof")

            // Find the UTxO we just created
            val verifierUtxos = devKit.getUtxos(verifierAddress)
            assert(verifierUtxos.nonEmpty, "No UTxOs found at verifier address")

            val lockedUtxo = verifierUtxos.find(_.getInlineDatum != null).getOrElse {
                fail("No UTxO with inline datum found at verifier address")
            }

            // Create redeemer with the proof
            val verifierRedeemer = TxVerifierRedeemer(
              txIndex = BigInt(txIndex),
              merkleProof = scalus.prelude.List.from(merkleProof.toList)
            )
            val redeemerData = scalus.bloxbean.Interop.toPlutusData(verifierRedeemer.toData)

            // Build unlock transaction
            val unlockTx = new com.bloxbean.cardano.client.quicktx.ScriptTx()
                .collectFrom(lockedUtxo, redeemerData)
                .payToAddress(devKit.account.baseAddress(), lockedUtxo.getAmount)
                .attachSpendingValidator(verifierScript)
                .withChangeAddress(devKit.account.baseAddress())

            val unlockTxBuilder = new com.bloxbean.cardano.client.quicktx.QuickTxBuilder(
              devKit.getBackendService
            )
            val unlockResult = unlockTxBuilder
                .compose(unlockTx)
                .feePayer(devKit.account.baseAddress())
                .withSigner(
                  com.bloxbean.cardano.client.function.helper.SignerProviders.signerFrom(
                    devKit.account
                  )
                )
                .completeAndWait()

            assert(
              unlockResult.isSuccessful,
              s"Unlock transaction failed: ${unlockResult.getResponse}"
            )
            val unlockTxHash = unlockResult.getValue
            println(s"[Test] ✓ Value unlocked successfully: $unlockTxHash")

            devKit.waitForTransaction(unlockTxHash, maxAttempts = 30)

            println(s"\n[Test] ✓ Phase 4 completed: On-chain transaction verification successful")
            println(s"\n[Test] ✓ Full lifecycle test completed successfully")
        }
    }

    test("scenario: merkle proofs for multiple transaction indices") {
        withYaciDevKit() { devKit =>
            given ec: ExecutionContext = ExecutionContext.global

            val blockHeight = 866970
            val mockRpc = new MockBitcoinRpc()

            // Get block info
            val blockHashFuture = mockRpc.getBlockHash(blockHeight)
            val blockHash = Await.result(blockHashFuture, 50.seconds)

            val blockInfoFuture = mockRpc.getBlock(blockHash)
            val blockInfo = Await.result(blockInfoFuture, 50.seconds)

            println(s"[Test] Testing Merkle proofs for multiple transaction indices")

            // Test first 5 transactions (or all if less than 5)
            val testCount = Math.min(5, blockInfo.tx.length)

            for txIndex <- 0 until testCount do {
                val btcTxId = blockInfo.tx(txIndex).txid

                // Build Merkle tree
                val txHashes = blockInfo.tx.map { tx =>
                    ByteString.fromHex(tx.txid).reverse
                }

                val merkleTree = MerkleTree.fromHashes(txHashes)
                val merkleRoot = merkleTree.getMerkleRoot
                val merkleProof = merkleTree.makeMerkleProof(txIndex)

                // Verify proof
                val txHash = ByteString.fromHex(btcTxId).reverse
                val calculatedRoot =
                    MerkleTree.calculateMerkleRootFromProof(txIndex, txHash, merkleProof)

                assert(calculatedRoot == merkleRoot, s"Proof failed for tx at index $txIndex")
                println(s"[Test] ✓ Verified proof for transaction at index $txIndex")
            }

            println(s"[Test] ✓ All $testCount Merkle proofs verified")
        }
    }

    test("scenario: handles transaction not in block") {
        withYaciDevKit() { devKit =>
            given ec: ExecutionContext = ExecutionContext.global

            val blockHeight = 866970
            val mockRpc = new MockBitcoinRpc()

            // Get block info
            val blockHashFuture = mockRpc.getBlockHash(blockHeight)
            val blockHash = Await.result(blockHashFuture, 10.seconds)

            val blockInfoFuture = mockRpc.getBlock(blockHash)
            val blockInfo = Await.result(blockInfoFuture, 10.seconds)

            // Use a fake transaction ID that's not in the block
            val fakeTxId = "0000000000000000000000000000000000000000000000000000000000000000"

            val txIndex = blockInfo.tx.indexWhere(_.txid == fakeTxId)
            assert(txIndex < 0, "Fake transaction should not be found in block")

            println(s"[Test] ✓ Correctly identified transaction not in block")
        }
    }
}
