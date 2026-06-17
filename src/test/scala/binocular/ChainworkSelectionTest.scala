package binocular

import binocular.oracle.*
import binocular.oracle.ForkTree.*
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.uplc.builtin.ByteString

class ChainworkSelectionTest extends AnyFunSuite {

    // 32-byte hash from a short hex tag (e.g. "a1", "e3") padded with zero bytes.
    private def hash32(tag: String): ByteString =
        ByteString.fromHex(tag + "00" * (32 - tag.length / 2))

    private def block(tag: String): BlockSummary =
        BlockSummary(hash = hash32(tag), timestamp = 100, addedTimeDelta = 0)

    test("best tip follows the higher-chainwork honest branch, not the longer rogue branch") {
        // Eve: 5 light blocks, total chainwork 5. Alice: 2 heavy blocks, chainwork 100.
        val eve = Blocks(PList.from((1 to 5).map(i => block(s"e$i")).toList), chainwork = 5, next = End)
        val alice = Blocks(PList.from(List(block("a1"), block("a2"))), chainwork = 100, next = End)

        // Fork ordering invariant: existing (Eve) left, new (Alice) right.
        val tree: ForkTree = Fork(eve, alice)

        val tip = tree.bestChainTipHash
        assert(tip.contains(block("a2").hash), "Alice's heavier branch must win despite Eve having more blocks")

        val (eveWork, _, _) = BitcoinValidator.bestChainPath(eve, 0, 0)
        val (aliceWork, _, _) = BitcoinValidator.bestChainPath(alice, 0, 0)
        assert(aliceWork > eveWork)
    }
}
