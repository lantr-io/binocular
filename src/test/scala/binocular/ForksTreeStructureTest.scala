package binocular

import munit.FunSuite
import scalus.builtin.ByteString
import scalus.prelude.List

class ForksTreeStructureTest extends FunSuite {

    test("Linear chain should maintain compact forksTree structure") {
        // When adding blocks in a linear chain (each extending the previous),
        // the forksTree should contain only the tip block with children references,
        // not every intermediate block as separate entries.

        // Example: Confirmed tip at height 100
        // Add blocks 101 -> 102 -> 103 -> 104 -> 105
        // Expected forksTree: 1 entry (block 105 with prevHash pointing through chain)
        // Actual (BUG): 5 entries (all blocks 101-105)

        val confirmedTip = ByteString.fromHex("aa" * 32)
        val block101Hash = ByteString.fromHex("01" * 32)
        val block102Hash = ByteString.fromHex("02" * 32)
        val block103Hash = ByteString.fromHex("03" * 32)

        // Simulate adding blocks to forksTree
        var forksTree = List.Nil: scalus.prelude.List[(ByteString, BlockNode)]

        // Add block 101 (extends confirmed tip)
        val node101 = BlockNode(
          prevBlockHash = confirmedTip,
          height = 101,
          chainwork = BigInt(1000),
          addedTimestamp = 1000,
          children = List.Nil
        )
        forksTree = BitcoinValidator.insertInSortedList(forksTree, block101Hash, node101)

        println(s"After adding block 101: forksTree.size = ${forksTree.size}")
        assert(forksTree.size == 1, "After adding block 101, forksTree should have 1 entry")

        // Add block 102 (extends block 101)
        val node102 = BlockNode(
          prevBlockHash = block101Hash,
          height = 102,
          chainwork = BigInt(2000),
          addedTimestamp = 1001,
          children = List.Nil
        )
        forksTree = BitcoinValidator.insertInSortedList(forksTree, block102Hash, node102)

        // Update parent (block 101) with child reference
        val node101Updated = node101.copy(children = List.Cons(block102Hash, node101.children))
        forksTree = BitcoinValidator.insertInSortedList(forksTree, block101Hash, node101Updated)

        println(s"After adding block 102 and updating parent: forksTree.size = ${forksTree.size}")

        // QUESTION: Should this be 1 (only tip) or 2 (tip + parent)?
        // According to user: Should be 1 (tip with children list)
        // Current implementation: 2 (both blocks as separate entries)

        println(s"forksTree keys:")
        forksTree.foreach { case (hash, node) =>
            println(s"  Hash: ${hash.toHex.take(8)}..., Height: ${node.height}, Children: ${node.children.size}")
        }

        // Add block 103 (extends block 102)
        val node103 = BlockNode(
          prevBlockHash = block102Hash,
          height = 103,
          chainwork = BigInt(3000),
          addedTimestamp = 1002,
          children = List.Nil
        )
        forksTree = BitcoinValidator.insertInSortedList(forksTree, block103Hash, node103)

        // Update parent (block 102) with child reference
        val node102Updated = node102.copy(children = List.Cons(block103Hash, node102.children))
        forksTree = BitcoinValidator.insertInSortedList(forksTree, block102Hash, node102Updated)

        println(s"After adding block 103 and updating parent: forksTree.size = ${forksTree.size}")
        println(s"forksTree keys:")
        forksTree.foreach { case (hash, node) =>
            println(s"  Hash: ${hash.toHex.take(8)}..., Height: ${node.height}, Children: ${node.children.size}")
        }

        // EXPECTED: forksTree.size == 1 (only the canonical tip with children chain)
        // ACTUAL (BUG): forksTree.size == 3 (all blocks 101, 102, 103)

        // The issue: We're storing every block as a separate entry, even though they're
        // all on the same chain. The 'children' field is tracking the relationship,
        // but we're not removing intermediate blocks.

        // QUESTION: Should intermediate blocks be removed, or is the current design intentional?
    }

    test("Fork structure should keep both branches") {
        // When there's an actual fork (two blocks extending the same parent),
        // both branches should be kept in forksTree

        val confirmedTip = ByteString.fromHex("aa" * 32)
        val block101Hash = ByteString.fromHex("01" * 32)
        val block102aHash = ByteString.fromHex("02a" * 16)  // Fork A
        val block102bHash = ByteString.fromHex("02b" * 16)  // Fork B

        var forksTree = List.Nil: scalus.prelude.List[(ByteString, BlockNode)]

        // Add block 101 (extends confirmed tip)
        val node101 = BlockNode(
          prevBlockHash = confirmedTip,
          height = 101,
          chainwork = BigInt(1000),
          addedTimestamp = 1000,
          children = List.Nil
        )
        forksTree = BitcoinValidator.insertInSortedList(forksTree, block101Hash, node101)

        // Add block 102a (extends block 101) - Fork A
        val node102a = BlockNode(
          prevBlockHash = block101Hash,
          height = 102,
          chainwork = BigInt(2000),
          addedTimestamp = 1001,
          children = List.Nil
        )
        forksTree = BitcoinValidator.insertInSortedList(forksTree, block102aHash, node102a)

        // Update parent with first child
        val node101Updated1 = node101.copy(children = List.Cons(block102aHash, node101.children))
        forksTree = BitcoinValidator.insertInSortedList(forksTree, block101Hash, node101Updated1)

        // Add block 102b (extends block 101) - Fork B
        val node102b = BlockNode(
          prevBlockHash = block101Hash,
          height = 102,
          chainwork = BigInt(2100),  // Slightly higher chainwork
          addedTimestamp = 1002,
          children = List.Nil
        )
        forksTree = BitcoinValidator.insertInSortedList(forksTree, block102bHash, node102b)

        // Update parent with second child
        val node101Updated2 = node101Updated1.copy(
          children = List.Cons(block102bHash, node101Updated1.children)
        )
        forksTree = BitcoinValidator.insertInSortedList(forksTree, block101Hash, node101Updated2)

        println(s"After creating fork at block 102: forksTree.size = ${forksTree.size}")
        println(s"forksTree keys:")
        forksTree.foreach { case (hash, node) =>
            println(s"  Hash: ${hash.toHex.take(8)}..., Height: ${node.height}, " +
                   s"Chainwork: ${node.chainwork}, Children: ${node.children.size}")
        }

        // With a fork, we SHOULD keep all 3 blocks:
        // - block 101 (parent) with children list [102a, 102b]
        // - block 102a (fork A)
        // - block 102b (fork B)
        assert(forksTree.size == 3, "With a fork, should keep parent and both fork branches")

        // Verify parent has both children
        val parent = BitcoinValidator.lookupInSortedList(forksTree, block101Hash)
        assert(parent.isDefined, "Parent block should be in forksTree")
        assert(parent.get.children.size == 2, "Parent should have 2 children")
    }
}
