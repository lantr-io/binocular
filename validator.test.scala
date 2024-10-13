package binocular

import scalus.*
import scalus.uplc.eval
import scalus.uplc.eval.*
import scalus.builtin.ByteString.given

import scala.jdk.CollectionConverters.*
import scala.language.implicitConversions
import binocular.BitcoinValidator.BlockHeader
import scalus.builtin.ByteString

case class CekResult(budget: ExBudget, logs: Seq[String])

class ValidatorTests extends munit.ScalaCheckSuite {

    test(s"Bitcoin validator size is ${bitcoinProgram.doubleCborEncoded.length}") {
        println(compiledBitcoinValidator.showHighlighted)
        assertEquals(bitcoinProgram.doubleCborEncoded.length, 720)
    }

    test("Block Header serialization") {
        val blockHeader = BlockHeader(
          865494,
          hex"00000000000000000002cfdedd8358532b2284bc157e1352dbc8682b2067fb0c",
          BigInt("30000000", 16),
          hex"0000000000000000000143a112c5ab741ec6e95b6c80f9834199efe2154c972b",
          hex"e77e1d6c5cb4ab60f095c262c23d91658b8111994b82db54872bcc1d5baff4af",
          1728837961,
          BigInt("17030ecd", 16),
          2893925625L
        )
        val serialized = BitcoinValidator.serializeBlockHeader(blockHeader)
        assertEquals(
          serialized,
          hex"000000302b974c15e2ef994183f9806c5be9c61e74abc512a14301000000000000000000aff4af5b1dcc2b8754db824b9911818b65913dc262c295f060abb45c6c1d7ee749f90b67cd0e0317f9cc7dac"
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
          BigInt("17030ecd", 16),
          2893925625L
        )
        val hash = BitcoinValidator.blockHeaderHash(blockHeader)
        assertEquals(hash, hex"00000000000000000002cfdedd8358532b2284bc157e1352dbc8682b2067fb0c")
    }

}
