package binocular

import munit.FunSuite
import scalus.builtin.ByteString
import scalus.prelude.List

class SortedListTest extends FunSuite {

    test("compareByteString should compare correctly") {
        val a = ByteString.fromHex("0292cad364cf66a04f9ade0238e96a8978e8bc01c65301000000000000000000")
        val b = ByteString.fromHex("2bb5cb6c73eec42434973626acdb6003e56e5b5f784802000000000000000000")

        val cmp = BitcoinValidator.compareByteString(a, b)
        assert(cmp == BigInt(-1), "a < b should return -1")

        val cmp2 = BitcoinValidator.compareByteString(b, a)
        assert(cmp2 == BigInt(1), "b > a should return 1")

        val cmp3 = BitcoinValidator.compareByteString(a, a)
        assert(cmp3 == BigInt(0), "a == a should return 0")
    }

    test("insertInSortedList should maintain sort order") {
        val key1 = ByteString.fromHex("0292cad364cf66a04f9ade0238e96a8978e8bc01c65301000000000000000000")
        val key2 = ByteString.fromHex("2bb5cb6c73eec42434973626acdb6003e56e5b5f784802000000000000000000")
        val key3 = ByteString.fromHex("7869d9f70ad5afaf66466dbec409fb1a92818cb2e3e901000000000000000000")

        val node1 = BlockNode(key1, BigInt(1), BigInt(100), BigInt(1000), List.Nil)
        val node2 = BlockNode(key2, BigInt(2), BigInt(200), BigInt(2000), List.Nil)
        val node3 = BlockNode(key3, BigInt(3), BigInt(300), BigInt(3000), List.Nil)

        // Insert in non-sorted order
        val list1 = BitcoinValidator.insertInSortedList(List.Nil, key2, node2)
        val list2 = BitcoinValidator.insertInSortedList(list1, key1, node1)
        val list3 = BitcoinValidator.insertInSortedList(list2, key3, node3)

        // Verify sorted order
        val keys = list3.map(_._1)
        keys match {
            case List.Cons(k1, List.Cons(k2, List.Cons(k3, List.Nil))) =>
                assertEquals(k1, key1)
                assertEquals(k2, key2)
                assertEquals(k3, key3)
            case _ => fail("Expected 3 elements in list")
        }
    }

    test("lookupInSortedList should find elements") {
        val key1 = ByteString.fromHex("0292cad364cf66a04f9ade0238e96a8978e8bc01c65301000000000000000000")
        val key2 = ByteString.fromHex("2bb5cb6c73eec42434973626acdb6003e56e5b5f784802000000000000000000")
        val key3 = ByteString.fromHex("7869d9f70ad5afaf66466dbec409fb1a92818cb2e3e901000000000000000000")

        val node1 = BlockNode(key1, BigInt(1), BigInt(100), BigInt(1000), List.Nil)
        val node2 = BlockNode(key2, BigInt(2), BigInt(200), BigInt(2000), List.Nil)
        val node3 = BlockNode(key3, BigInt(3), BigInt(300), BigInt(3000), List.Nil)

        val list = BitcoinValidator.insertInSortedList(
            BitcoinValidator.insertInSortedList(
                BitcoinValidator.insertInSortedList(List.Nil, key2, node2),
                key1,
                node1
            ),
            key3,
            node3
        )

        // Test lookup
        val found1 = BitcoinValidator.lookupInSortedList(list, key1)
        assert(found1.isDefined, "Should find key1")
        assertEquals(found1.get.height, BigInt(1))

        val found2 = BitcoinValidator.lookupInSortedList(list, key2)
        assert(found2.isDefined, "Should find key2")
        assertEquals(found2.get.height, BigInt(2))

        val found3 = BitcoinValidator.lookupInSortedList(list, key3)
        assert(found3.isDefined, "Should find key3")
        assertEquals(found3.get.height, BigInt(3))

        // Test not found
        val notFound = BitcoinValidator.lookupInSortedList(list, ByteString.fromHex("ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"))
        assert(notFound.isEmpty, "Should not find non-existent key")
    }

    test("existsInSortedList should detect presence") {
        val key1 = ByteString.fromHex("0292cad364cf66a04f9ade0238e96a8978e8bc01c65301000000000000000000")
        val key2 = ByteString.fromHex("2bb5cb6c73eec42434973626acdb6003e56e5b5f784802000000000000000000")

        val node1 = BlockNode(key1, BigInt(1), BigInt(100), BigInt(1000), List.Nil)

        val list = BitcoinValidator.insertInSortedList(List.Nil, key1, node1)

        assert(BitcoinValidator.existsInSortedList(list, key1), "Should find existing key")
        assert(!BitcoinValidator.existsInSortedList(list, key2), "Should not find missing key")
    }

    test("deleteFromSortedList should remove elements") {
        val key1 = ByteString.fromHex("0292cad364cf66a04f9ade0238e96a8978e8bc01c65301000000000000000000")
        val key2 = ByteString.fromHex("2bb5cb6c73eec42434973626acdb6003e56e5b5f784802000000000000000000")
        val key3 = ByteString.fromHex("7869d9f70ad5afaf66466dbec409fb1a92818cb2e3e901000000000000000000")

        val node1 = BlockNode(key1, BigInt(1), BigInt(100), BigInt(1000), List.Nil)
        val node2 = BlockNode(key2, BigInt(2), BigInt(200), BigInt(2000), List.Nil)
        val node3 = BlockNode(key3, BigInt(3), BigInt(300), BigInt(3000), List.Nil)

        val list = BitcoinValidator.insertInSortedList(
            BitcoinValidator.insertInSortedList(
                BitcoinValidator.insertInSortedList(List.Nil, key1, node1),
                key2,
                node2
            ),
            key3,
            node3
        )

        // Delete middle element
        val list2 = BitcoinValidator.deleteFromSortedList(list, key2)

        assert(BitcoinValidator.existsInSortedList(list2, key1), "key1 should still exist")
        assert(!BitcoinValidator.existsInSortedList(list2, key2), "key2 should be deleted")
        assert(BitcoinValidator.existsInSortedList(list2, key3), "key3 should still exist")

        assertEquals(list2.size, BigInt(2), "List should have 2 elements")
    }
}
