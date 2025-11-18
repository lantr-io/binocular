package binocular

import scalus.Compiler
import scalus.compiler.sir.TargetLoweringBackend
import scalus.*
import scalus.uplc.Program

object BitcoinContract {
    given Compiler.Options = Compiler.Options(
      optimizeUplc = true,
      generateErrorTraces = true,
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering
    )
    def compileBitcoinProgram(): Program =
        val sir = Compiler.compileWithOptions(summon[Compiler.Options], BitcoinValidator.validate2)
        //    println(sir.showHighlighted)
        //    sir.toUplcOptimized(generateErrorTraces = false).plutusV3
        sir.toUplcOptimized().plutusV3
    //    println(uplc.showHighlighted)

    lazy val bitcoinProgram: Program = compileBitcoinProgram()
}
