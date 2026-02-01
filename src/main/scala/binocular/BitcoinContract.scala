package binocular

import scalus.*
import scalus.compiler.Options
import scalus.compiler.sir.TargetLoweringBackend
import scalus.uplc.{PlutusV3, Program}

object BitcoinContract {
    given Options = Options(
      optimizeUplc = true,
      generateErrorTraces = true,
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering
    )

    lazy val contract = PlutusV3.compile(BitcoinValidator.validate)

    lazy val bitcoinProgram: Program = contract.program
}
