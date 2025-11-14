package binocular.cli

import binocular.*
import com.bloxbean.cardano.client.address.Address
import scalus.builtin.{ByteString, Data}
import scalus.utils.Hex.hexToBytes

import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext, Future}

/** Integration test for UpdateOracle with Merkle Tree verification
  *
  * Tests that the confirmed blocks Merkle tree is correctly updated when:
  *   1. Oracle is initialized at block N 2. Multiple blocks are submitted and promoted 3. The confirmedBlocksTree field
  *      is properly updated 4. The Merkle root can be calculated from the tree
  */
class UpdateOracleWithMerkleTreeTest extends CliIntegrationTestBase {

    // Override default test timeout (30 seconds) to allow for Docker container startup
    override val munitTimeout = scala.concurrent.duration.Duration(180, "s")

    test("update-oracle: Merkle tree is updated when blocks are promoted") {
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

            // Verify initial Merkle tree state
            val initialUtxos = devKit.getUtxos(scriptAddress.getAddress)
            val initialUtxo = initialUtxos.head
            val initialData = Data.fromCbor(initialUtxo.getInlineDatum.hexToBytes)
            val initialChainState = initialData.to[BitcoinValidator.ChainState]

            println(s"[Test] ✓ Initial Merkle tree:")
            println(s"    Tree size: ${countTreeLevels(initialChainState.confirmedBlocksTree)}")
            println(s"    Initial block: ${initialState.blockHash.toHex}")

            assert(
              countTreeLevels(initialChainState.confirmedBlocksTree) == 1,
              "Initial tree should have 1 level (single block)"
            )

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

            // Calculate new state using shared validator logic
            val validityTime = OracleTransactions.computeValidityIntervalTime(devKit.getBackendService)
            val newState = BitcoinValidator.computeUpdateOracleState(initialState, headersList, validityTime)

            println(s"[Test] ✓ New state calculated:")
            println(s"    Old height: ${initialState.blockHeight}")
            println(s"    New height: ${newState.blockHeight}")

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

                    println(s"[Test] Step 5: Verifying Merkle tree was updated")

                    // Verify new oracle state
                    val utxos = devKit.getUtxos(scriptAddress.getAddress)
                    assert(utxos.nonEmpty, "No UTxOs found at oracle address after update")

                    // Find the latest UTxO (should be from update tx)
                    val latestUtxo = utxos.head
                    val inlineDatum = latestUtxo.getInlineDatum

                    val data = Data.fromCbor(inlineDatum.hexToBytes)
                    val chainState = data.to[BitcoinValidator.ChainState]

                    println(s"[Test] ✓ Updated ChainState verified:")
                    println(s"    Height: ${chainState.blockHeight}")
                    println(s"    Hash: ${chainState.blockHash.toHex}")

                    assert(
                      chainState.blockHeight == updateToHeight,
                      s"Updated height mismatch: ${chainState.blockHeight} != $updateToHeight"
                    )

                    println(s"[Test] Step 6: Verifying Merkle tree structure")

                    // Verify Merkle tree was updated
                    val treeSize = countTreeLevels(chainState.confirmedBlocksTree)
                    println(s"    Merkle tree size: $treeSize levels")

                    // For 4 blocks (866970-866973), we expect:
                    // Level 0: [empty, hash3] (after combining first 2 and rolling up)
                    // Level 1: [empty, combined01_23] (after combining pairs)
                    // Level 2: [root] (final root)
                    // But the exact structure depends on the rolling algorithm
                    // At minimum, we should have more than 1 level
                    assert(treeSize > 1, s"Tree should have grown beyond 1 level, got $treeSize")

                    println(s"[Test] ✓ Merkle tree has grown correctly")

                    println(s"[Test] Step 7: Verifying we can compute Merkle root")

                    // Try to compute the Merkle root from the tree
                    val computedRoot = BitcoinValidator.getMerkleRoot(chainState.confirmedBlocksTree)
                    println(s"    Computed Merkle root: ${computedRoot.toHex}")
                    println(s"    Root length: ${computedRoot.bytes.length} bytes")

                    assert(computedRoot.bytes.length == 32, "Merkle root should be 32 bytes")

                    println(s"[Test] ✓ Successfully computed Merkle root from tree")

                    println(s"[Test] Step 8: Verifying tree represents all blocks")

                    // Build reference Merkle tree from all block hashes
                    val allBlockHashes = scala.collection.mutable.ArrayBuffer[ByteString]()

                    // Add initial block
                    allBlockHashes += initialState.blockHash

                    // Add updated blocks (need to fetch their hashes)
                    for (height <- startHeight + 1 to updateToHeight) {
                        val hashHex = Await.result(mockRpc.getBlockHash(height), 5.seconds)
                        val hashBytes = ByteString.fromHex(hashHex).reverse
                        allBlockHashes += hashBytes
                    }

                    println(s"    Total blocks: ${allBlockHashes.size}")
                    println(s"    Block 0: ${allBlockHashes(0).toHex}")
                    println(s"    Block 1: ${allBlockHashes(1).toHex}")
                    println(s"    Block 2: ${allBlockHashes(2).toHex}")
                    println(s"    Block 3: ${allBlockHashes(3).toHex}")

                    // Build reference tree using MerkleTree (non-rolling)
                    val referenceMerkleTree = MerkleTree.fromHashes(allBlockHashes.toSeq)
                    val referenceRoot = referenceMerkleTree.getMerkleRoot

                    println(s"    Reference Merkle root: ${referenceRoot.toHex}")

                    // The rolling tree root should match the reference tree root
                    assert(
                      computedRoot == referenceRoot,
                      s"Rolling Merkle root doesn't match reference!\n  Computed: ${computedRoot.toHex}\n  Reference: ${referenceRoot.toHex}"
                    )

                    println(s"[Test] ✓✓✓ Rolling Merkle tree matches reference implementation!")

                case Left(err) =>
                    fail(s"Failed to update oracle: $err")
            }
        }
    }

    /** Helper to count non-empty levels in the Merkle tree */
    private def countTreeLevels(tree: scalus.prelude.List[ByteString]): Int = {
        def count(list: scalus.prelude.List[ByteString], acc: Int): Int = {
            list match {
                case scalus.prelude.List.Nil              => acc
                case scalus.prelude.List.Cons(head, tail) =>
                    // Count this level if it's not all zeros (empty)
                    val isEmpty = head.bytes.forall(_ == 0)
                    count(tail, if (isEmpty) acc else acc + 1)
            }
        }
        count(tree, 0)
    }
}
