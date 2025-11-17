package binocular

import munit.FunSuite
import scalus.builtin.ByteString
import scalus.builtin.ByteString.hex
import scalus.builtin.Builtins.sha2_256

import scala.collection.mutable.ArrayBuffer

/** Unit tests for Bitcoin Merkle tree transaction inclusion proofs.
  *
  * This test suite demonstrates:
  *   1. Building Merkle trees from transaction hashes
  *   2. Generating Merkle proofs for specific transactions
  *   3. Verifying proofs against the Merkle root
  *   4. Integration with real Bitcoin block data
  */
class MerkleProofSpec extends FunSuite {

    test("simple merkle tree with 2 transactions") {
        // Minimal case: 2 transactions
        val tx1 = hex"1111111111111111111111111111111111111111111111111111111111111111"
        val tx2 = hex"2222222222222222222222222222222222222222222222222222222222222222"

        val tree = MerkleTree.fromHashes(Seq(tx1, tx2))
        val root = tree.getMerkleRoot

        // Verify tx1 at index 0
        val proof1 = tree.makeMerkleProof(0)
        assertEquals(proof1.length, 1)
        assertEquals(proof1.head, tx2)

        val computed1 = MerkleTree.calculateMerkleRootFromProof(0, tx1, proof1)
        assertEquals(computed1, root)

        // Verify tx2 at index 1
        val proof2 = tree.makeMerkleProof(1)
        assertEquals(proof2.length, 1)
        assertEquals(proof2.head, tx1)

        val computed2 = MerkleTree.calculateMerkleRootFromProof(1, tx2, proof2)
        assertEquals(computed2, root)
    }

    test("merkle tree with 4 transactions") {
        val tx1 = hex"1111111111111111111111111111111111111111111111111111111111111111"
        val tx2 = hex"2222222222222222222222222222222222222222222222222222222222222222"
        val tx3 = hex"3333333333333333333333333333333333333333333333333333333333333333"
        val tx4 = hex"4444444444444444444444444444444444444444444444444444444444444444"

        val tree = MerkleTree.fromHashes(Seq(tx1, tx2, tx3, tx4))
        val root = tree.getMerkleRoot

        // Verify each transaction
        for i <- 0 until 4 do {
            val txHash = Seq(tx1, tx2, tx3, tx4)(i)
            val proof = tree.makeMerkleProof(i)
            assertEquals(
              proof.length,
              2,
              s"Proof length should be 2 for tx at index $i"
            ) // log2(4) = 2 levels

            val computedRoot = MerkleTree.calculateMerkleRootFromProof(i, txHash, proof)
            assertEquals(computedRoot, root, s"Computed root should match for tx at index $i")
        }
    }

    test("merkle tree with odd number of transactions (3)") {
        // Bitcoin duplicates the last hash when odd number
        val tx1 = hex"1111111111111111111111111111111111111111111111111111111111111111"
        val tx2 = hex"2222222222222222222222222222222222222222222222222222222222222222"
        val tx3 = hex"3333333333333333333333333333333333333333333333333333333333333333"

        val tree = MerkleTree.fromHashes(Seq(tx1, tx2, tx3))
        val root = tree.getMerkleRoot

        // Verify each transaction
        for i <- 0 until 3 do {
            val txHash = Seq(tx1, tx2, tx3)(i)
            val proof = tree.makeMerkleProof(i)

            val computedRoot = MerkleTree.calculateMerkleRootFromProof(i, txHash, proof)
            assertEquals(computedRoot, root, s"Computed root should match for tx at index $i")
        }
    }

    test("merkle tree with 7 transactions") {
        val txHashes = (1 to 7).map { i =>
            val bytes = Array.fill[Byte](32)(i.toByte)
            ByteString.unsafeFromArray(bytes)
        }

        val tree = MerkleTree.fromHashes(txHashes)
        val root = tree.getMerkleRoot

        // Verify each transaction
        txHashes.zipWithIndex.foreach { case (txHash, idx) =>
            val proof = tree.makeMerkleProof(idx)
            val computedRoot = MerkleTree.calculateMerkleRootFromProof(idx, txHash, proof)
            assertEquals(computedRoot, root, s"Computed root should match for tx at index $idx")
        }
    }

    test("single transaction creates valid merkle tree") {
        val tx1 = hex"1111111111111111111111111111111111111111111111111111111111111111"

        val tree = MerkleTree.fromHashes(Seq(tx1))
        val root = tree.getMerkleRoot

        // Single transaction: root IS the transaction hash
        assertEquals(root, tx1)

        // Proof should be empty for single transaction
        val proof = tree.makeMerkleProof(0)
        assertEquals(proof.length, 0)

        val computedRoot = MerkleTree.calculateMerkleRootFromProof(0, tx1, proof)
        assertEquals(computedRoot, root)
    }

    test("invalid proof fails verification") {
        val tx1 = hex"1111111111111111111111111111111111111111111111111111111111111111"
        val tx2 = hex"2222222222222222222222222222222222222222222222222222222222222222"
        val tx3 = hex"3333333333333333333333333333333333333333333333333333333333333333"

        val tree = MerkleTree.fromHashes(Seq(tx1, tx2, tx3))
        val root = tree.getMerkleRoot

        // Create incorrect proof (use proof for wrong transaction)
        val proofForTx1 = tree.makeMerkleProof(0)

        // Try to verify tx2 with tx1's proof - should fail
        val wrongRoot = MerkleTree.calculateMerkleRootFromProof(0, tx2, proofForTx1)
        assertNotEquals(wrongRoot, root, "Wrong proof should not produce correct root")
    }

    test("coinbase transaction hash calculation") {
        // This demonstrates how to extract and hash a coinbase transaction
        // The raw transaction bytes would come from Bitcoin RPC

        // Example: simplified coinbase structure
        // In real Bitcoin, coinbase is the first transaction (index 0)
        val simpleCoinbaseRaw =
            hex"01000000010000000000000000000000000000000000000000000000000000000000000000ffffffff0704ffff001d0104ffffffff0100f2052a0100000043410496b538e853519c726a2c91e61ec11600ae1390813a627c66fb8be7947be63c52da7589379515d4e0a604f8141781e62294721166bf621e73a82cbf2342c858eeac00000000"

        // Hash transaction: SHA256(SHA256(raw))
        val txHash = sha2_256(sha2_256(simpleCoinbaseRaw))

        assertEquals(txHash.bytes.length, 32)
    }

    test("MerkleTreeRootBuilder produces same root as MerkleTree") {
        val txHashes = (1 to 5).map { i =>
            val bytes = Array.fill[Byte](32)(i.toByte)
            ByteString.unsafeFromArray(bytes)
        }

        // Build using full tree
        val tree = MerkleTree.fromHashes(txHashes)
        val treeRoot = tree.getMerkleRoot

        // Build using rolling builder (used for streaming)
        val builder = new MerkleTreeRootBuilder()
        txHashes.foreach(builder.addHash)
        val builderRoot = builder.getMerkleRoot

        assertEquals(builderRoot, treeRoot)
    }
}

/** Integration tests using real Bitcoin block data.
  *
  * These tests require Bitcoin RPC connection and demonstrate:
  *   1. Fetching real block data from Bitcoin network
  *   2. Extracting transaction hashes
  *   3. Verifying Merkle proofs against actual block headers
  */
class MerkleProofRpcIntegrationSpec extends FunSuite {

    // These tests are marked as integration tests and will be skipped in regular unit test runs
    // To run: sbt "testOnly *MerkleProofRpcIntegrationSpec"

    // Test will be implemented when RPC configuration is available
    test("verify merkle proof for real Bitcoin transaction".ignore) {
        // TODO: Implement with Bitcoin RPC client
        // 1. Connect to Bitcoin node
        // 2. Fetch a block (e.g., block 100,000)
        // 3. Extract all transaction hashes
        // 4. Build Merkle tree
        // 5. Verify tree root matches block header's merkleroot field
        // 6. Generate proof for specific transaction
        // 7. Verify proof
    }

    test("verify coinbase transaction in real block".ignore) {
        // TODO: Implement coinbase-specific test
        // Coinbase is always at index 0
        // Special handling for witness commitment in coinbase
    }
}
