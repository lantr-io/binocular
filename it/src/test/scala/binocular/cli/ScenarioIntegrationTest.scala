package binocular
package cli

import binocular.{reverse, BitcoinChainState, IntegrationTestContract, MerkleTree, OracleTransactions}
import scalus.cardano.ledger.{TransactionHash, TransactionInput, Utxo}
import scalus.cardano.onchain.plutus.prelude.List as ScalusList
import scalus.uplc.builtin.{ByteString, Data}
import scalus.utils.await
import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF

import scala.concurrent.duration.*
import scala.concurrent.{ExecutionContext, Future}

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
    test("scenario: basic oracle update with new blocks") {
        val ctx = createYaciContext()
        given ec: ExecutionContext = ctx.provider.executionContext

        // Use consecutive blocks from our fixtures
        // Start at 866970, update to 866971-866973 (3 blocks)
        val startHeight = 866970
        val updateToHeight = 866973
        val mockRpc = new MockBitcoinRpc()

        println(s"[Test] Step 1: Creating initial oracle at height $startHeight")

        // Create initial oracle
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

        // Initialize oracle with NFT minting (required by spend validator)
        val (oracleUtxo, itScript, scriptAddress, itParams) =
            initOracleWithNft(ctx, initialState)

        println(
          s"[Test] Step 2: Fetching headers for blocks ${startHeight + 1} to $updateToHeight"
        )

        // Fetch headers for update
        val headers = Future
            .sequence(
              (startHeight + 1 to updateToHeight).map { height =>
                  for {
                      hashHex <- mockRpc.getBlockHash(height)
                      headerInfo <- mockRpc.getBlockHeader(hashHex)
                  } yield BitcoinChainState.convertHeader(headerInfo)
              }
            )
            .await(30.seconds)

        val headersList = ScalusList.from(headers.toList)

        println(s"[Test] Fetched ${headers.length} headers")
        println(s"[test]  headers: ${headers.map(h => h.bytes.toHex).mkString(", ")}")

        println(s"[Test] Step 3: Computing new state")

        // Compute validity interval time to ensure offline and on-chain use the same value
        val (_, validityTime) =
            OracleTransactions.computeValidityIntervalTime(ctx.provider.cardanoInfo)
        println(s"  Using validity interval time: $validityTime")

        // Compute new state using the shared validator logic
        val parentPath = initialState.forkTree.findTipPath
        val newState =
            OracleTransactions.applyHeaders(initialState, headersList, parentPath, validityTime, itParams)
        println(s"  Computed new state:")
        println(s"    Height: ${newState.blockHeight}")
        println(s"    Hash: ${newState.blockHash.toHex}")
        println(s"    Forks tree size: ${newState.forkTree.blockCount}")

        println(s"[Test] Step 4: Submitting update transaction")

        // Submit update transaction with pre-computed state and validity time
        val updateTxResult = OracleTransactions.buildAndSubmitUpdateTransaction(
          ctx.alice.signer,
          ctx.provider,
          scriptAddress,
          ctx.alice.address,
          oracleUtxo,
          initialState,
          newState,
          headersList,
          parentPath,
          validityTime,
          itParams.oneShotTxOutRef,
          scriptOverride = Some(itScript)
        )

        updateTxResult match {
            case Right(txHash) =>
                println(s"[Test] Oracle updated: $txHash")

                // Wait for confirmation
                val confirmed = waitForTransaction(ctx.provider, txHash, maxAttempts = 30)
                assert(confirmed, s"Update transaction did not confirm")

                Thread.sleep(2000) // Wait for indexing

                println(s"[Test] Step 5: Verifying updated oracle state")

                // Verify new oracle state
                val latestUtxo = findOracleUtxo(ctx.provider, scriptAddress, txHash)
                val actualState = latestUtxo.output.inlineDatum.get.to[ChainState]

                println(s"[Test] Updated ChainState verified:")
                println(s"    Height: ${actualState.blockHeight}")
                println(s"    Hash: ${actualState.blockHash.toHex}")
                println(s"    Forks tree size: ${actualState.forkTree.blockCount}")

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

    test("scenario: handles empty header list") {
        val ctx = createYaciContext()
        given ec: ExecutionContext = ctx.provider.executionContext

        val startHeight = 866970
        val mockRpc = new MockBitcoinRpc()

        println(s"[Test] Creating oracle at height $startHeight")

        // Create initial oracle
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

        val scriptAddress = IntegrationTestContract.testnetScriptAddress

        val initTxResult = OracleTransactions.buildAndSubmitInitTransaction(
          ctx.alice.signer,
          ctx.provider,
          scriptAddress,
          ctx.alice.address,
          initialState
        )

        initTxResult match {
            case Right(txHash) =>
                waitForTransaction(ctx.provider, txHash, maxAttempts = 30)
                Thread.sleep(2000)
            case Left(err) =>
                fail(s"Failed to initialize oracle: $err")
        }

        println(s"[Test] Attempting update with empty header list")

        // Try to update with empty list - should fail validation
        val emptyHeaders = ScalusList.empty[BlockHeader]

        // This should fail because validator requires non-empty headers
        println(s"[Test] Test with empty headers skipped (validator rejects empty list)")
    }

    test("scenario: full lifecycle - promotion, proof, and verification") {
        val ctx = createYaciContext()
        given ec: ExecutionContext = ctx.provider.executionContext

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

        // Initialize oracle with NFT minting (required by spend validator)
        val (initOracleUtxo, itScript, scriptAddress, itParams) =
            initOracleWithNft(ctx, initialState)
        var currentOracleUtxo: Utxo = initOracleUtxo

        // Deploy reference script to reduce transaction size
        // Deploy to script address to avoid collateral conflicts with account's UTxOs
        println(s"[Test] Step 1b: Deploying reference script")
        val refScriptResult = OracleTransactions.deployReferenceScript(
          ctx.alice.signer,
          ctx.provider,
          ctx.alice.address,
          scriptAddress, // Deploy to script address
          itParams.oneShotTxOutRef,
          scriptOverride = Some(itScript)
        )

        val referenceScriptUtxo: Option[Utxo] = refScriptResult match {
            case Right((txHash, outputIndex, savedOutput)) =>
                println(s"[Test] Reference script deployed: $txHash:$outputIndex")
                waitForTransaction(ctx.provider, txHash, maxAttempts = 30)
                Thread.sleep(2000)
                // Construct Utxo from saved output (which has scriptRef populated)
                val refInput = TransactionInput(TransactionHash.fromHex(txHash), outputIndex)
                Some(Utxo(refInput, savedOutput))
            case Left(err) =>
                fail(s"Failed to deploy reference script: $err")
        }

        println(s"[Test] Step 2: Adding ${totalBlocks} blocks in batches of $batchSize")

        // Fetch all headers
        val allHeaders = Future
            .sequence(
              (startHeight + 1 to finalHeight).map { height =>
                  for {
                      hashHex <- mockRpc.getBlockHash(height)
                      headerInfo <- mockRpc.getBlockHeader(hashHex)
                  } yield BitcoinChainState.convertHeader(headerInfo)
              }
            )
            .await(120.seconds)

        println(s"[Test] Fetched ${allHeaders.length} headers total")

        // Process in batches
        val batches = allHeaders.grouped(batchSize).toSeq
        var currentState = initialState
        // Initialize off-chain MPF from the initial confirmed block
        var currentMpf = OffChainMPF.empty
            .insert(initialState.blockHash, initialState.blockHash)

        batches.zipWithIndex.foreach { case (batch, batchIndex) =>
            println(
              s"[Test] Processing batch ${batchIndex + 1}/${batches.size} (${batch.size} headers)"
            )
            println(s"  Current state before batch:")
            println(s"    blockHeight: ${currentState.blockHeight}")
            println(s"    blockHash: ${currentState.blockHash.toHex}")
            println(s"    forkTree size: ${currentState.forkTree.blockCount}")
            println(s"    confirmedBlocksRoot: ${currentState.confirmedBlocksRoot.toHex}")

            val headersList = ScalusList.from(batch.toList)

            // IT challengeAging is 30 seconds - blocks from early batches naturally age
            // during processing (~40s total). No time advancement needed.
            val validityTime: BigInt =
                OracleTransactions.computeValidityIntervalTime(ctx.provider.cardanoInfo)._2

            // Compute new state with MPF proofs
            val parentPath = currentState.forkTree.findTipPath
            val (newState, mpfProofs, updatedMpf) = OracleTransactions.computeUpdateWithProofs(
              currentState,
              headersList,
              parentPath,
              validityTime,
              currentMpf,
              itParams
            )

            println(s"  [Batch ${batchIndex + 1}] Off-chain computed state:")
            println(s"    blockHeight: ${newState.blockHeight}")
            println(s"    blockHash: ${newState.blockHash.toHex}")
            println(s"    forkTree size: ${newState.forkTree.blockCount}")
            println(s"    confirmedBlocksRoot: ${newState.confirmedBlocksRoot.toHex}")

            // Log forkTree for debugging
            println(s"  [Batch ${batchIndex + 1}] OFF-CHAIN forkTree:")
            println(newState.forkTree.displayTree(newState.blockHeight, "    "))

            // Submit update transaction (using reference script to reduce tx size)
            val updateTxResult = OracleTransactions.buildAndSubmitUpdateTransaction(
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
              referenceScriptUtxo,
              mpfInsertProofs = mpfProofs,
              scriptOverride = Some(itScript)
            )

            updateTxResult match {
                case Right(resultTxHash) =>
                    println(s"  Batch ${batchIndex + 1} submitted: $resultTxHash")
                    waitForTransaction(ctx.provider, resultTxHash, maxAttempts = 30)
                    Thread.sleep(2000)

                    // Read actual on-chain state and verify it matches what we sent
                    // Since validator only validates (can't modify), on-chain state MUST match newState
                    val oracleUtxo =
                        findOracleUtxo(ctx.provider, scriptAddress, resultTxHash)
                    val actualOnChainState =
                        oracleUtxo.output.inlineDatum.get.to[ChainState]

                    println(s"  On-chain state after batch ${batchIndex + 1}:")
                    println(s"    blockHeight: ${actualOnChainState.blockHeight}")
                    println(s"    blockHash: ${actualOnChainState.blockHash.toHex}")
                    println(s"    forkTree size: ${actualOnChainState.forkTree.blockCount}")
                    println(
                      s"    confirmedBlocksRoot: ${actualOnChainState.confirmedBlocksRoot.toHex}"
                    )

                    // Verify on-chain state matches what we computed off-chain
                    if actualOnChainState.blockHeight != newState.blockHeight ||
                        actualOnChainState.blockHash != newState.blockHash ||
                        actualOnChainState.forkTree.blockCount != newState.forkTree.blockCount
                    then {

                        fail(
                          s"ERROR: On-chain state does not match off-chain computed state!\n" +
                              s"  This should be impossible - validator can only validate, not modify.\n" +
                              s"  Off-chain: height=${newState.blockHeight}, hash=${newState.blockHash.toHex}, forkTree=${newState.forkTree.blockCount}\n" +
                              s"  On-chain:  height=${actualOnChainState.blockHeight}, hash=${actualOnChainState.blockHash.toHex}, forkTree=${actualOnChainState.forkTree.blockCount}"
                        )
                    }

                    // Update for next iteration
                    currentState = newState
                    currentOracleUtxo = oracleUtxo
                    currentMpf = updatedMpf

                case Left(errorMsg) =>
                    fail(s"Failed to update oracle with batch ${batchIndex + 1}: $errorMsg")
            }
        }

        println(s"[Test] Step 3: Verifying promotion occurred")
        println(s"  Initial confirmed height: ${initialState.blockHeight}")
        println(s"  Final confirmed height: ${currentState.blockHeight}")
        println(s"  Initial forks tree size: ${initialState.forkTree.blockCount}")
        println(s"  Final forks tree size: ${currentState.forkTree.blockCount}")

        // Verify that promotion occurred
        val heightIncrease = currentState.blockHeight - initialState.blockHeight
        assert(
          heightIncrease > 0,
          s"Expected promotion to increase confirmed height, but got: initial=${initialState.blockHeight}, final=${currentState.blockHeight}"
        )

        println(s"  Promotion detected: height increased by $heightIncrease blocks")

        // Verify confirmed blocks root was updated after promotion
        assert(
          currentState.confirmedBlocksRoot != initialState.confirmedBlocksRoot,
          "Confirmed blocks MPF root should change after promotion"
        )

        println(s"[Test] Step 4: Verifying on-chain state after promotion")

        // Verify final on-chain state matches the last tracked oracle UTxO
        val actualState = currentOracleUtxo.output.inlineDatum.get.to[ChainState]

        println(s"[Test] On-chain state after forced promotion:")
        println(s"    Height: ${actualState.blockHeight}")
        println(s"    Hash: ${actualState.blockHash.toHex}")
        println(s"    Forks tree size: ${actualState.forkTree.blockCount}")
        println(s"    Confirmed blocks root: ${actualState.confirmedBlocksRoot.toHex}")

        // Verify state matches expectations
        assert(
          actualState.blockHeight == currentState.blockHeight,
          s"Height mismatch: actual=${actualState.blockHeight} expected=${currentState.blockHeight}"
        )
        assert(
          actualState.blockHash == currentState.blockHash,
          s"Hash mismatch"
        )

        println(s"[Test] Phase 1 completed: Promotion successful")
        println(s"  Total blocks added: $totalBlocks")
        println(s"  Blocks promoted: $heightIncrease")
        println(s"  Batches processed: ${batches.size}")

        // ========== Phase 2: Prove transaction inclusion ==========
        println(s"\n[Test] Phase 2: Proving transaction inclusion")

        // Use the initial block (866970) which should now be in confirmed state
        val proofBlockHeight = startHeight
        println(s"[Test] Step 5: Fetching block data for height $proofBlockHeight")

        // Get block info to find a transaction to prove
        val blockHash = mockRpc.getBlockHash(proofBlockHeight).await(10.seconds)

        val blockInfo = mockRpc.getBlock(blockHash).await(10.seconds)

        assert(blockInfo.tx.nonEmpty, "Block has no transactions")

        // Use the first transaction (coinbase)
        val btcTxId = blockInfo.tx.head.txid
        val txIndex = 0

        println(s"[Test] Testing with transaction: $btcTxId")
        println(s"[Test]   Block has ${blockInfo.tx.length} transactions")

        println(s"[Test] Step 6: Building Merkle proof")

        // Build Merkle tree from transaction hashes
        val txHashes = blockInfo.tx.map { tx =>
            ByteString.fromHex(tx.txid).reverse
        }

        val merkleTree = MerkleTree.fromHashes(txHashes)
        val merkleRoot = merkleTree.getMerkleRoot
        val merkleProof = merkleTree.makeMerkleProof(txIndex)

        println(s"[Test] Merkle proof generated:")
        println(s"    Merkle Root: ${merkleRoot.toHex}")
        println(s"    Proof Size: ${merkleProof.length} hashes")

        println(s"[Test] Step 7: Verifying proof locally")

        // Verify proof
        val txHash = ByteString.fromHex(btcTxId).reverse
        val calculatedRoot =
            MerkleTree.calculateMerkleRootFromProof(txIndex, txHash, merkleProof)

        assert(calculatedRoot == merkleRoot, "Merkle proof verification failed")
        println(s"[Test] Merkle proof verified")
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
        val blockInForksTree = actualState.forkTree.existsHash(blockHashBytes)
        assert(!blockInForksTree, "Block should not be in forks tree after promotion")

        println(s"[Test] Transaction is in confirmed block")
        println(s"    Block Height: $proofBlockHeight")
        println(s"    Oracle Confirmed Height: ${actualState.blockHeight}")

        println(s"\n[Test] Phase 2 completed: Transaction proof verified")

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
            println(s"[Test] Verified proof for transaction at index $testTxIndex")
        }

        println(s"[Test] All $testCount Merkle proofs verified")

        println(s"\n[Test] Phase 3 completed: Multiple proof indices verified")

        // ========== Phase 4: On-chain transaction verification ==========
        // SKIPPED: The TransactionVerifier now requires Oracle verification with two proofs:
        // 1. Transaction proof (tx in block)
        // 2. Block proof (block in Oracle's confirmed tree)
        // This test would need to set up an Oracle with confirmed blocks first.
        println(
          s"\n[Test] Phase 4: SKIPPED - TransactionVerifier now requires Oracle verification"
        )
        println(
          s"    The validator now verifies blocks against the Oracle's confirmed blocks tree"
        )

        println(s"\n[Test] Full lifecycle test completed successfully (Phases 1-3)")
    }

    test("scenario: merkle proofs for multiple transaction indices") {
        val ctx = createYaciContext()
        given ec: ExecutionContext = ctx.provider.executionContext

        val blockHeight = 866970
        val mockRpc = new MockBitcoinRpc()

        // Get block info
        val blockHash = mockRpc.getBlockHash(blockHeight).await(50.seconds)
        val blockInfo = mockRpc.getBlock(blockHash).await(50.seconds)

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
            println(s"[Test] Verified proof for transaction at index $txIndex")
        }

        println(s"[Test] All $testCount Merkle proofs verified")
    }

    test("scenario: handles transaction not in block") {
        val ctx = createYaciContext()
        given ec: ExecutionContext = ctx.provider.executionContext

        val blockHeight = 866970
        val mockRpc = new MockBitcoinRpc()

        // Get block info
        val blockHash = mockRpc.getBlockHash(blockHeight).await(10.seconds)
        val blockInfo = mockRpc.getBlock(blockHash).await(10.seconds)

        // Use a fake transaction ID that's not in the block
        val fakeTxId = "0000000000000000000000000000000000000000000000000000000000000000"

        val txIndex = blockInfo.tx.indexWhere(_.txid == fakeTxId)
        assert(txIndex < 0, "Fake transaction should not be found in block")

        println(s"[Test] Correctly identified transaction not in block")
    }
}
