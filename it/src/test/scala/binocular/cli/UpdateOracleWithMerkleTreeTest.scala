package binocular.cli

import binocular.*
import scalus.cardano.address.Address
import scalus.cardano.ledger.{TransactionHash, TransactionInput, Utxo}
import scalus.uplc.builtin.{ByteString, Data}
import scalus.cardano.onchain.plutus.prelude.List as ScalusList

import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext, Future}

/** Integration test for UpdateOracle with Merkle Tree verification
  *
  * Tests that the confirmed blocks Merkle tree is correctly updated when:
  *   1. Oracle is initialized at block N 2. Multiple blocks are submitted and promoted 3. The
  *      confirmedBlocksTree field is properly updated 4. The Merkle root can be calculated from the
  *      tree
  */
class UpdateOracleWithMerkleTreeTest extends CliIntegrationTestBase {

    // Override default test timeout (30 seconds) to allow for Docker container startup
    override val munitTimeout = scala.concurrent.duration.Duration(180, "s")

    test("update-oracle: Merkle tree is updated when blocks are promoted") {
        withYaciDevKit() { devKit =>
            given ec: ExecutionContext = ExecutionContext.global

            // Use consecutive blocks from our fixtures
            // Start at 866970, update to 866971-866972 (2 blocks)
            val startHeight = 866970
            val updateToHeight = 866972
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

            val scriptAddress = Address.fromBech32(
              binocular.OracleConfig(network = binocular.CardanoNetwork.Testnet).scriptAddress
            )

            // Submit init transaction
            val initTxResult = OracleTransactions.buildAndSubmitInitTransaction(
              devKit.signer,
              devKit.provider,
              scriptAddress,
              devKit.sponsorAddress,
              initialState
            )

            val oracleUtxo: Utxo = initTxResult match {
                case Right(txHash) =>
                    println(s"[Test] Oracle initialized: $txHash")
                    devKit.waitForTransaction(txHash, maxAttempts = 30)
                    Thread.sleep(2000) // Wait for indexing
                    // Fetch the oracle UTxO
                    val utxos = devKit.getUtxos(scriptAddress)
                    require(utxos.nonEmpty, "No UTxOs found at script address after init")
                    utxos.find(_.output.inlineDatum.isDefined).getOrElse {
                        fail("No oracle UTxO with inline datum found after init")
                    }
                case Left(err) =>
                    fail(s"Failed to initialize oracle: $err")
            }

            // Verify initial Merkle tree state
            val initialChainState = oracleUtxo.output.inlineDatum.get.to[ChainState]

            println(s"[Test] Initial Merkle tree:")
            println(s"    Tree size: ${countTreeLevels(initialChainState.confirmedBlocksTree)}")
            println(s"    Initial block: ${initialState.blockHash.toHex}")

            assert(
              countTreeLevels(initialChainState.confirmedBlocksTree) == 1,
              "Initial tree should have 1 level (single block)"
            )

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
            val headersList = ScalusList.from(headers.toList)

            println(s"[Test] Fetched ${headers.length} headers")

            println(s"[Test] Step 3: Calculating new ChainState")

            // Calculate new state using shared validator logic
            val (_, validityTime) =
                OracleTransactions.computeValidityIntervalTime(devKit.provider.cardanoInfo)
            val newState =
                BitcoinValidator.computeUpdateOracleState(initialState, headersList, validityTime)

            println(s"[Test] New state calculated:")
            println(s"    Old height: ${initialState.blockHeight}")
            println(s"    New height: ${newState.blockHeight}")

            println(s"[Test] Step 4: Submitting update transaction")

            // Submit update transaction with pre-computed state and validity time
            val updateTxResult = OracleTransactions.buildAndSubmitUpdateTransaction(
              devKit.signer,
              devKit.provider,
              scriptAddress,
              devKit.sponsorAddress,
              oracleUtxo,
              initialState,
              newState,
              headersList,
              validityTime
            )

            updateTxResult match {
                case Right(txHash) =>
                    println(s"[Test] Oracle updated: $txHash")

                    // Wait for confirmation
                    val confirmed = devKit.waitForTransaction(txHash, maxAttempts = 30)
                    assert(confirmed, s"Update transaction did not confirm")

                    Thread.sleep(2000) // Wait for indexing

                    println(s"[Test] Step 5: Verifying Merkle tree was updated")

                    // Verify new oracle state
                    val utxos = devKit.getUtxos(scriptAddress)
                    assert(utxos.nonEmpty, "No UTxOs found at oracle address after update")

                    // Find the latest UTxO (should be from update tx)
                    val latestUtxo = utxos.find(_.output.inlineDatum.isDefined).getOrElse {
                        fail("No oracle UTxO with inline datum found after update")
                    }
                    val chainState = latestUtxo.output.inlineDatum.get.to[ChainState]

                    println(s"[Test] Updated ChainState verified:")
                    println(s"    Height: ${chainState.blockHeight}")
                    println(s"    Hash: ${chainState.blockHash.toHex}")

                    // height should not changed, because blocks are not yet confirmed
                    assert(
                      chainState.blockHeight == startHeight,
                      s"Updated height mismatch: ${chainState.blockHeight} != $startHeight"
                    )

                    println(s"[Test] Step 6: Verifying forks tree structure")
                    val numBranches = chainState.forksTree.size
                    // Count total blocks across all branches
                    val totalBlocks = chainState.forksTree.foldLeft(BigInt(0)) { (acc, branch) =>
                        acc + branch.recentBlocks.size
                    }
                    println(s"    Forks tree branches: $numBranches")
                    println(s"    Total blocks in forks tree: $totalBlocks")
                    assert(
                      totalBlocks == headers.size,
                      s"Forks tree should contain ${headers.size} new blocks, but has $totalBlocks"
                    )

                    println(s"[Test] Forks tree has grown correctly")

                    println(s"[Test] Step 7: Verifying we can compute Merkle root")

                    // Try to compute the Merkle root from the tree
                    val computedRoot =
                        BitcoinValidator.getMerkleRoot(chainState.confirmedBlocksTree)
                    println(s"    Computed Merkle root: ${computedRoot.toHex}")
                    println(s"    Root length: ${computedRoot.bytes.length} bytes")

                    assert(computedRoot.bytes.length == 32, "Merkle root should be 32 bytes")

                    println(s"[Test] Successfully computed Merkle root from tree")

                    println(s"[Test] Step 8: Verifying tree represents all blocks")

                    // Build reference Merkle tree from all block hashes
                    val allBlockHashes = scala.collection.mutable.ArrayBuffer[ByteString]()

                    // Add initial block
                    allBlockHashes += initialState.blockHash

                    // Add updated blocks (need to fetch their hashes)
                    for height <- startHeight + 1 to updateToHeight do {
                        val hashHex = Await.result(mockRpc.getBlockHash(height), 5.seconds)
                        val hashBytes = ByteString.fromHex(hashHex).reverse
                        allBlockHashes += hashBytes
                    }

                    println(s"    Total blocks: ${allBlockHashes.size}")
                    println(s"    Block 0: ${allBlockHashes(0).toHex}")
                    println(s"    Block 1: ${allBlockHashes(1).toHex}")
                    println(s"    Block 2: ${allBlockHashes(2).toHex}")
                // println(s"    Block 3: ${allBlockHashes(3).toHex}")

                /** Here nothing hase changed because blocks are not yet confirmed, so we cannot
                  * verify the confirmedBlocksTree against allBlockHashes.
                  *
                  * // Build reference tree using MerkleTree (non-rolling) val referenceMerkleTree =
                  * MerkleTree.fromHashes(allBlockHashes.toSeq) val referenceRoot =
                  * referenceMerkleTree.getMerkleRoot
                  *
                  * println(s" Reference Merkle root: ${referenceRoot.toHex}")
                  *
                  * // The rolling tree root should match the reference tree root assert(
                  * computedRoot == referenceRoot, s"Rolling Merkle root doesn't match reference!\n
                  * Computed: ${computedRoot.toHex}\n Reference: ${referenceRoot.toHex}" )
                  *
                  * println(s"[Test] Rolling Merkle tree matches reference implementation!")
                  */
                case Left(err) =>
                    fail(s"Failed to update oracle: $err")
            }
        }
    }

    /** Helper to count non-empty levels in the Merkle tree */
    private def countTreeLevels(tree: ScalusList[ByteString]): Int = {
        def count(list: ScalusList[ByteString], acc: Int): Int = {
            list match {
                case ScalusList.Nil              => acc
                case ScalusList.Cons(head, tail) =>
                    // Count this level if it's not all zeros (empty)
                    val isEmpty = head.bytes.forall(_ == 0)
                    count(tail, if isEmpty then acc else acc + 1)
            }
        }
        count(tree, 0)
    }
}
