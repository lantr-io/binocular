package binocular

import scalus.*
import scalus.uplc.eval
import scalus.uplc.eval.*
import scalus.builtin.ByteString.given

import scala.jdk.CollectionConverters.*
import scala.language.implicitConversions
import binocular.BitcoinValidator.BlockHeader
import scalus.builtin.ByteString
import scalus.builtin.Builtins

case class CekResult(budget: ExBudget, logs: Seq[String])

class ValidatorTests extends munit.ScalaCheckSuite {

    // test(s"Bitcoin validator size is ${bitcoinProgram.doubleCborEncoded.length}") {
    // println(compiledBitcoinValidator.showHighlighted)
    // assertEquals(bitcoinProgram.doubleCborEncoded.length, 900)
    // }

    test("Block Header serialization") {
        val blockHeader = BlockHeader(
          865494,
          hex"00000000000000000002cfdedd8358532b2284bc157e1352dbc8682b2067fb0c",
          BigInt("30000000", 16),
          hex"0000000000000000000143a112c5ab741ec6e95b6c80f9834199efe2154c972b",
          hex"e77e1d6c5cb4ab60f095c262c23d91658b8111994b82db54872bcc1d5baff4af",
          1728837961,
          hex"cd0e0317",
          hex"f9cc7dac"
        )
        val serialized = BitcoinValidator.serializeBlockHeader(blockHeader)
        assertEquals(
          serialized,
          hex"000000302b974c15e2ef994183f9806c5be9c61e74abc512a14301000000000000000000aff4af5b1dcc2b8754db824b9911818b65913dc262c295f060abb45c6c1d7ee749f90b67cd0e0317f9cc7dac"
        )
    }

    test("parseCoinbaseTx") {
        val coinbaseTx =
            hex"010000000001010000000000000000000000000000000000000000000000000000000000000000ffffffff2503233708184d696e656420627920416e74506f6f6c373946205b8160a4256c0000946e0100ffffffff02f595814a000000001976a914edf10a7fac6b32e24daa5305c723f3de58db1bc888ac0000000000000000266a24aa21a9edfaa194df59043645ba0f58aad74bfd5693fa497093174d12a4bb3b0574a878db0120000000000000000000000000000000000000000000000000000000000000000000000000"
        val scriptSig = BitcoinValidator.parseCoinbaseTxScriptSig(coinbaseTx)
        assertEquals(
          scriptSig,
          hex"03233708184d696e656420627920416e74506f6f6c373946205b8160a4256c0000946e0100"
        )
    }

    test("parseBlockHeightFromScriptSig") {
        val coinbaseTx =
            hex"010000000001010000000000000000000000000000000000000000000000000000000000000000ffffffff2503233708184d696e656420627920416e74506f6f6c373946205b8160a4256c0000946e0100ffffffff02f595814a000000001976a914edf10a7fac6b32e24daa5305c723f3de58db1bc888ac0000000000000000266a24aa21a9edfaa194df59043645ba0f58aad74bfd5693fa497093174d12a4bb3b0574a878db0120000000000000000000000000000000000000000000000000000000000000000000000000"
        val scriptSig = BitcoinValidator.parseCoinbaseTxScriptSig(coinbaseTx)
        val blockHeight = BitcoinValidator.parseBlockHeightFromScriptSig(scriptSig)
        assertEquals(blockHeight, BigInt(538403))
    }

    test("getTxHash") {
        val coinbaseTx =
            hex"010000000001010000000000000000000000000000000000000000000000000000000000000000ffffffff2503233708184d696e656420627920416e74506f6f6c373946205b8160a4256c0000946e0100ffffffff02f595814a000000001976a914edf10a7fac6b32e24daa5305c723f3de58db1bc888ac0000000000000000266a24aa21a9edfaa194df59043645ba0f58aad74bfd5693fa497093174d12a4bb3b0574a878db0120000000000000000000000000000000000000000000000000000000000000000000000000"
        val txHash = BitcoinValidator.getTxHash(coinbaseTx)
        assertEquals(
          txHash,
          hex"31e9370f45eb48f6f52ef683b0737332f09f1cead75608021185450422ec1a71".reverse
        )
    }

    test("bitsToTarget") {
        assertEquals(
          BitcoinValidator.bitsToTarget(hex"00030ecd".reverse),
          hex"0000000000000000000000000000000000000000000000000000000000030ecd"
        )
        assertEquals(
          BitcoinValidator.bitsToTarget(hex"17030ecd".reverse),
          hex"000000000000030ecd0000000000000000000000000000000000000000000000"
        )
        assertEquals(
          BitcoinValidator.bitsToTarget(hex"19030ecd".reverse),
          hex"00000000030ecd00000000000000000000000000000000000000000000000000"
        )
        assertEquals(
          BitcoinValidator.bitsToTarget(hex"1a030ecd".reverse),
          hex"00000000ffff0000000000000000000000000000000000000000000000000000"
        )
        assertEquals(
          BitcoinValidator.bitsToTarget(hex"20030ecd".reverse),
          hex"00000000ffff0000000000000000000000000000000000000000000000000000"
        )
    }

    test("Block Header hash") {
        val blockHeader = BlockHeader(
          865494,
          hex"00000000000000000002cfdedd8358532b2284bc157e1352dbc8682b2067fb0c",
          BigInt("30000000", 16),
          hex"0000000000000000000143a112c5ab741ec6e95b6c80f9834199efe2154c972b",
          hex"e77e1d6c5cb4ab60f095c262c23d91658b8111994b82db54872bcc1d5baff4af",
          1728837961,
          hex"cd0e0317",
          hex"f9cc7dac"
        )
        val hash = BitcoinValidator.blockHeaderHash(blockHeader)
        assertEquals(hash, hex"00000000000000000002cfdedd8358532b2284bc157e1352dbc8682b2067fb0c")
    }

    test("Block Header hash satisfies target") {
        val blockHeader = BlockHeader(
          865494,
          hex"00000000000000000002cfdedd8358532b2284bc157e1352dbc8682b2067fb0c",
          BigInt("30000000", 16),
          hex"0000000000000000000143a112c5ab741ec6e95b6c80f9834199efe2154c972b",
          hex"e77e1d6c5cb4ab60f095c262c23d91658b8111994b82db54872bcc1d5baff4af",
          1728837961,
          hex"cd0e0317",
          hex"f9cc7dac"
        )
        val hash = BitcoinValidator.blockHeaderHash(blockHeader)
        val target = BitcoinValidator.bitsToTarget(hex"17030ecd".reverse)
        assertEquals(hash, hex"00000000000000000002cfdedd8358532b2284bc157e1352dbc8682b2067fb0c")
        assertEquals(target, hex"000000000000030ecd0000000000000000000000000000000000000000000000")
        assert(Builtins.lessThanByteString(hash, target))
    }

}
