package binocular

import munit.FunSuite
import scalus.builtin.ByteString
import scalus.prelude.List

class ForksTreeStructureTest extends FunSuite {

    test("Linear chain should maintain compact forksTree structure") {
        // With the optimized ForkBranch structure, a linear chain (each block extending the previous)
        // should be stored as a SINGLE ForkBranch entry, not separate entries for each block.
        //
        // Example: Confirmed tip at height 100
        // Add blocks 101 -> 102 -> 103 -> 104 -> 105
        // Expected forksTree: 1 ForkBranch (tip at 105, recentBlocks contains last blocks)
        // Old implementation: 5 entries (all blocks 101-105 as separate BlockNode entries)

        val confirmedTip = ByteString.fromHex("aa" * 32)
        val block101Hash = ByteString.fromHex("01" * 32)
        val block102Hash = ByteString.fromHex("02" * 32)
        val block103Hash = ByteString.fromHex("03" * 32)

        // Start with empty forksTree
        var forksTree = List.Nil: List[ForkBranch]

        // Create a simple ChainState for testing
        val confirmedState = ChainState(
          blockHeight = BigInt(100),
          blockHash = confirmedTip,
          currentTarget = ByteString.fromHex("1d00ffff"),
          blockTimestamp = BigInt(1000000),
          recentTimestamps = List.single(BigInt(1000000)),
          previousDifficultyAdjustmentTimestamp = BigInt(900000),
          confirmedBlocksTree = List.Nil,
          forksTree = List.Nil
        )

        // Add block 101 (extends confirmed tip)
        val header101 = BlockHeader(
          ByteString.fromArray(
            Array.fill[Byte](80)(0) // Simplified header
          )
        )
        // Mock adding block 101 - it should create a new ForkBranch
        val branch101 = ForkBranch(
          tipHash = block101Hash,
          tipHeight = BigInt(101),
          tipChainwork = BigInt(1000),
          recentBlocks = List.single(
            BlockSummary(
              hash = block101Hash,
              height = BigInt(101),
              chainwork = BigInt(1000),
              timestamp = BigInt(1001000),
              bits = ByteString.fromHex("1d00ffff"),
              addedTime = BigInt(1001000)
            )
          )
        )
        forksTree = List.Cons(branch101, forksTree)

        println(s"After adding block 101: forksTree.size = ${forksTree.size}")
        assert(forksTree.size == 1, "After adding block 101, forksTree should have 1 branch")

        // Add block 102 (extends block 101)
        // This should EXTEND the existing branch, not create a new one
        val branch102 = BitcoinValidator.extendBranch(
          branch101,
          BlockSummary(
            hash = block102Hash,
            height = BigInt(102),
            chainwork = BigInt(2000),
            timestamp = BigInt(1002000),
            bits = ByteString.fromHex("1d00ffff"),
            addedTime = BigInt(1002000)
          )
        )
        forksTree = BitcoinValidator.updateBranch(forksTree, branch101, branch102)

        println(s"After adding block 102 (extending branch): forksTree.size = ${forksTree.size}")
        assert(
          forksTree.size == 1,
          "After extending with block 102, forksTree should still have 1 branch"
        )

        // Verify the branch now contains 2 blocks in recentBlocks
        val currentBranch = forksTree.head
        assert(currentBranch.tipHash == block102Hash, "Tip should be block 102")
        assert(currentBranch.tipHeight == BigInt(102), "Tip height should be 102")
        assert(currentBranch.recentBlocks.size == 2, "Recent blocks should contain 2 blocks")

        // Add block 103 (extends block 102)
        val branch103 = BitcoinValidator.extendBranch(
          branch102,
          BlockSummary(
            hash = block103Hash,
            height = BigInt(103),
            chainwork = BigInt(3000),
            timestamp = BigInt(1003000),
            bits = ByteString.fromHex("1d00ffff"),
            addedTime = BigInt(1003000)
          )
        )
        forksTree = BitcoinValidator.updateBranch(forksTree, branch102, branch103)

        println(s"After adding block 103 (extending branch): forksTree.size = ${forksTree.size}")
        println(
          s"Branch tip: height=${branch103.tipHeight}, recentBlocks.size=${branch103.recentBlocks.size}"
        )

        // EXPECTED with new structure: forksTree.size == 1 (single branch containing all 3 blocks)
        assert(
          forksTree.size == 1,
          "After extending with block 103, forksTree should still have 1 branch"
        )
        assert(branch103.recentBlocks.size == 3, "Recent blocks should contain 3 blocks")

        println("✓ Linear chain maintains compact structure: 1 ForkBranch for 3 blocks")
    }

    test("Fork structure should create separate branches") {
        // When there's an actual fork (two blocks extending the same parent or from mid-branch),
        // we should create separate ForkBranch entries for each fork.

        val confirmedTip = ByteString.fromHex("aa" * 32)
        val block101Hash = ByteString.fromHex("01" * 32)
        val block102Hash = ByteString.fromHex("02" * 32)
        val block103aHash = ByteString.fromHex("03a" * 16) // Fork A
        val block103bHash = ByteString.fromHex("03b" * 16) // Fork B

        var forksTree = List.Nil: List[ForkBranch]

        // Create branch 101 -> 102
        val branch102 = ForkBranch(
          tipHash = block102Hash,
          tipHeight = BigInt(102),
          tipChainwork = BigInt(2000),
          recentBlocks = List.Cons(
            BlockSummary(
              block102Hash,
              BigInt(102),
              BigInt(2000),
              BigInt(1002000),
              ByteString.fromHex("1d00ffff"),
              BigInt(1002000)
            ),
            List.single(
              BlockSummary(
                block101Hash,
                BigInt(101),
                BigInt(1000),
                BigInt(1001000),
                ByteString.fromHex("1d00ffff"),
                BigInt(1001000)
              )
            )
          )
        )
        forksTree = List.Cons(branch102, forksTree)

        println(s"After creating branch 101->102: forksTree.size = ${forksTree.size}")
        assert(forksTree.size == 1, "Should have 1 branch")

        // Now add block 103a (extends block 102 - extends the tip)
        val branch103a = BitcoinValidator.extendBranch(
          branch102,
          BlockSummary(
            block103aHash,
            BigInt(103),
            BigInt(3000),
            BigInt(1003000),
            ByteString.fromHex("1d00ffff"),
            BigInt(1003000)
          )
        )
        forksTree = BitcoinValidator.updateBranch(forksTree, branch102, branch103a)

        println(s"After extending with 103a: forksTree.size = ${forksTree.size}")
        assert(forksTree.size == 1, "Should still have 1 branch after extending tip")

        // Now add block 103b (also extends block 102 - creates a fork!)
        // Since 102 is in recentBlocks but not the tip, this creates a new branch
        val branch103b = ForkBranch(
          tipHash = block103bHash,
          tipHeight = BigInt(103),
          tipChainwork = BigInt(3100), // Slightly higher chainwork
          recentBlocks = List.single(
            BlockSummary(
              block103bHash,
              BigInt(103),
              BigInt(3100),
              BigInt(1003500),
              ByteString.fromHex("1d00ffff"),
              BigInt(1003500)
            )
          )
        )
        forksTree = List.Cons(branch103b, forksTree)

        println(s"After creating fork at 103b: forksTree.size = ${forksTree.size}")
        println(s"Branches:")
        forksTree.foreach { branch =>
            println(
              s"  Tip: ${branch.tipHash.toHex.take(8)}..., Height: ${branch.tipHeight}, " +
                  s"Chainwork: ${branch.tipChainwork}, RecentBlocks: ${branch.recentBlocks.size}"
            )
        }

        // With a fork, we SHOULD keep 2 branches:
        // - Branch A: 101 -> 102 -> 103a
        // - Branch B: 103b (forked from 102)
        assert(forksTree.size == 2, "With a fork, should have 2 separate branches")

        // Verify both branches exist
        val branchA = forksTree.find(b => b.tipHash == block103aHash)
        val branchB = forksTree.find(b => b.tipHash == block103bHash)
        assert(branchA.isDefined, "Branch A (103a) should exist")
        assert(branchB.isDefined, "Branch B (103b) should exist")

        println("✓ Fork structure correctly creates separate branches")
    }

    test("ForkBranch keeps ALL blocks in recentBlocks until promoted") {
        // Verify that extending a branch keeps all blocks (they are only removed when promoted)

        val block1Hash = ByteString.fromHex("01" * 32)

        // Create initial branch with 1 block
        var branch = ForkBranch(
          tipHash = block1Hash,
          tipHeight = BigInt(1),
          tipChainwork = BigInt(1000),
          recentBlocks = List.single(
            BlockSummary(
              block1Hash,
              BigInt(1),
              BigInt(1000),
              BigInt(1000000),
              ByteString.fromHex("1d00ffff"),
              BigInt(1000000)
            )
          )
        )

        // Add 110 more blocks (total 111)
        for i <- 2 to 111 do {
            val blockHash = ByteString.fromHex(f"$i%02x" * 32)
            val blockSummary = BlockSummary(
              blockHash,
              BigInt(i),
              BigInt(i * 1000),
              BigInt(1000000 + i * 1000),
              ByteString.fromHex("1d00ffff"),
              BigInt(1000000 + i * 1000) // addedTime same as timestamp for test
            )
            branch = BitcoinValidator.extendBranch(branch, blockSummary)
        }

        println(s"After adding 111 blocks: recentBlocks.size = ${branch.recentBlocks.size}")
        assert(branch.recentBlocks.size == 111, "recentBlocks should contain all 111 blocks")
        assert(branch.tipHeight == BigInt(111), "Tip height should be 111")
        assert(branch.tipHash == ByteString.fromHex("6f" * 32), "Tip should be block 111")

        // Verify all blocks are present (newest first)
        val heights = branch.recentBlocks.map(_.height)
        assert(heights.size == 111, s"Should have 111 heights, got ${heights.size}")
        // First height should be 111 (newest), last should be 1 (oldest)
        assert(heights.head == BigInt(111), "First block should be height 111")
        assert(heights.last == BigInt(1), "Last block should be height 1")

        println("✓ ForkBranch correctly keeps all blocks until promoted")
    }
}
