package binocular

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.compiler.Options
import scalus.compiler.sir.TargetLoweringBackend
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.ByteString.hex
import scalus.uplc.builtin.Data.toData
import scalus.cardano.onchain.plutus.v3.{TxOutRef, TxId}

/** Test that changing compiler options actually affects the compiled validator size. */
class CompilerOptionsTest extends AnyFunSuite {

    val testTxOutRef: TxOutRef = TxOutRef(
      TxId(hex"0000000000000000000000000000000000000000000000000000000000000000"),
      BigInt(0)
    )

    test("changing generateErrorTraces should change validator size") {
        // Compile with error traces enabled
        val contractWithTraces = {
            given Options = Options(
              optimizeUplc = true,
              generateErrorTraces = true,
              targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering
            )
            PlutusV3.compile(BitcoinValidator.validate)
        }

        // Compile with error traces disabled
        val contractWithoutTraces = {
            given Options = Options(
              optimizeUplc = true,
              generateErrorTraces = false,
              targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering
            )
            PlutusV3.compile(BitcoinValidator.validate)
        }

        val programWithTraces =
            (contractWithTraces.program.deBruijnedProgram $ testTxOutRef.toData).toProgram
        val programWithoutTraces =
            (contractWithoutTraces.program.deBruijnedProgram $ testTxOutRef.toData).toProgram

        val sizeWithTraces = programWithTraces.flatEncoded.length
        val sizeWithoutTraces = programWithoutTraces.flatEncoded.length

        println(s"Size with error traces:    $sizeWithTraces bytes")
        println(s"Size without error traces: $sizeWithoutTraces bytes")
        println(s"Difference:                ${sizeWithTraces - sizeWithoutTraces} bytes")

        assert(
          sizeWithTraces != sizeWithoutTraces,
          s"Validator size should change when generateErrorTraces changes! " +
              s"Both are $sizeWithTraces bytes."
        )
        assert(
          sizeWithTraces > sizeWithoutTraces,
          s"Validator with traces ($sizeWithTraces) should be larger than without ($sizeWithoutTraces)"
        )
    }

    test("changing optimizeUplc should change validator size") {
        // Compile with optimization enabled
        val contractOptimized = {
            given Options = Options(
              optimizeUplc = true,
              generateErrorTraces = true,
              targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering
            )
            PlutusV3.compile(BitcoinValidator.validate)
        }

        // Compile with optimization disabled
        val contractUnoptimized = {
            given Options = Options(
              optimizeUplc = false,
              generateErrorTraces = true,
              targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering
            )
            PlutusV3.compile(BitcoinValidator.validate)
        }

        val programOptimized =
            (contractOptimized.program.deBruijnedProgram $ testTxOutRef.toData).toProgram
        val programUnoptimized =
            (contractUnoptimized.program.deBruijnedProgram $ testTxOutRef.toData).toProgram

        val sizeOptimized = programOptimized.flatEncoded.length
        val sizeUnoptimized = programUnoptimized.flatEncoded.length

        println(s"Size optimized:   $sizeOptimized bytes")
        println(s"Size unoptimized: $sizeUnoptimized bytes")
        println(s"Difference:       ${sizeUnoptimized - sizeOptimized} bytes")

        assert(
          sizeOptimized != sizeUnoptimized,
          s"Validator size should change when optimizeUplc changes! " +
              s"Both are $sizeOptimized bytes."
        )
    }

    test("BitcoinContract.bitcoinProgram size matches fresh compilation with same options") {
        // Compile fresh with the same options as BitcoinContract
        val freshContract = {
            given Options = Options(
              optimizeUplc = true,
              generateErrorTraces = true,
              targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering
            )
            PlutusV3.compile(BitcoinValidator.validate)
        }

        val freshProgram =
            (freshContract.program.deBruijnedProgram $ testTxOutRef.toData).toProgram
        val cachedProgram = BitcoinContract.bitcoinProgram

        val freshSize = freshProgram.flatEncoded.length
        val cachedSize = cachedProgram.flatEncoded.length

        println(s"Fresh compilation size:          $freshSize bytes")
        println(s"BitcoinContract.bitcoinProgram:  $cachedSize bytes")

        assert(
          freshSize == cachedSize,
          s"Fresh compilation ($freshSize) should match cached ($cachedSize)"
        )
    }
}
