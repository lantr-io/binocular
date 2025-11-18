package binocular.cli

import binocular.{reverse, BitcoinChainState, BitcoinValidator, MerkleTree, OracleTransactions}
import com.bloxbean.cardano.client.address.Address
import scalus.builtin.ByteString
import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration.*

/** Integration test for ProveTransactionCommand
  *
  * Tests the complete flow of proving transaction inclusion:
  *   1. Creates oracle at block containing transaction
  *   2. Fetches transaction and block data from mock RPC
  *   3. Builds Merkle proof
  *   4. Verifies proof locally
  */
class ProveTransactionCommandIntegrationTest extends CliIntegrationTestBase {

    test("prove-transaction: successfully generates and verifies Merkle proof") {
        withYaciDevKit() { devKit =>
            given ec: ExecutionContext = ExecutionContext.global

            // Use block 866970 which has transactions in fixture
            val blockHeight = 866970
            val mockRpc = new MockBitcoinRpc()

            println(s"[Test] Step 1: Creating oracle at height $blockHeight")

            // Create oracle at this block
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
              blockHeight
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
                    println(s"[Test] ✓ Oracle initialized: $txHash")
                    devKit.waitForTransaction(txHash, maxAttempts = 30)
                    Thread.sleep(2000)
                    (txHash, 0)
                case Left(err) =>
                    fail(s"Failed to initialize oracle: $err")
            }

            println(s"[Test] Step 2: Fetching block data")

            // Get block info to find a transaction to prove
            val blockHashFuture = mockRpc.getBlockHash(blockHeight)
            val blockHash = Await.result(blockHashFuture, 10.seconds)

            val blockInfoFuture = mockRpc.getBlock(blockHash)
            val blockInfo = Await.result(blockInfoFuture, 10.seconds)

            assert(blockInfo.tx.nonEmpty, "Block has no transactions")

            // Use the first transaction
            val btcTxId = blockInfo.tx.head.txid
            val txIndex = 0

            println(s"[Test] ✓ Testing with transaction: $btcTxId")
            println(s"[Test]   Block has ${blockInfo.tx.length} transactions")

            println(s"[Test] Step 3: Building Merkle proof")

            // Build Merkle tree
            val txHashes = blockInfo.tx.map { tx =>
                ByteString.fromHex(tx.txid).reverse
            }

            val merkleTree = MerkleTree.fromHashes(txHashes)
            val merkleRoot = merkleTree.getMerkleRoot
            val merkleProof = merkleTree.makeMerkleProof(txIndex)

            println(s"[Test] ✓ Merkle proof generated:")
            println(s"    Merkle Root: ${merkleRoot.toHex}")
            println(s"    Proof Size: ${merkleProof.length} hashes")

            println(s"[Test] Step 4: Verifying proof locally")

            // Verify proof
            val txHash = ByteString.fromHex(btcTxId).reverse
            val calculatedRoot =
                MerkleTree.calculateMerkleRootFromProof(txIndex, txHash, merkleProof)

            assert(calculatedRoot == merkleRoot, "Merkle proof verification failed")
            println(s"[Test] ✓ Merkle proof verified")
            println(s"    Calculated root: ${calculatedRoot.toHex}")
            println(s"    Expected root:   ${merkleRoot.toHex}")

            println(s"[Test] Step 5: Verifying transaction is in confirmed block")

            // Verify block is <= oracle height
            assert(blockHeight <= initialState.blockHeight.toInt, "Block height > oracle height")

            // Verify block is not in forks tree
            val blockHashBytes = ByteString.fromHex(blockHash).reverse
            assert(!BitcoinValidator.existsInSortedList(initialState.forksTree, blockHashBytes), "Block is in forks tree")

            println(s"[Test] ✓ Transaction is in confirmed block")
            println(s"    Block Height: $blockHeight")
            println(s"    Oracle Height: ${initialState.blockHeight}")
        }
    }

    test("prove-transaction: handles transaction not in block") {
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

    test("prove-transaction: verifies proof for transaction at different indices") {
        withYaciDevKit() { devKit =>
            given ec: ExecutionContext = ExecutionContext.global

            val blockHeight = 866970
            val mockRpc = new MockBitcoinRpc()

            // Get block info
            val blockHashFuture = mockRpc.getBlockHash(blockHeight)
            val blockHash = Await.result(blockHashFuture, 10.seconds)

            val blockInfoFuture = mockRpc.getBlock(blockHash)
            val blockInfo = Await.result(blockInfoFuture, 10.seconds)

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
}
