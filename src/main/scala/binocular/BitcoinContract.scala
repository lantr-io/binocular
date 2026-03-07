package binocular

import scalus.*
import scalus.cardano.onchain.plutus.v3.TxOutRef
import scalus.cardano.onchain.plutus.v3.TxId
import scalus.compiler.Options
import scalus.compiler.sir.TargetLoweringBackend
import scalus.uplc.{PlutusV3, Program}
import scalus.uplc.builtin.ByteString.hex
import scalus.uplc.builtin.Data.toData

object BitcoinContract {
    given Options = Options(
      optimizeUplc = true,
      generateErrorTraces = true,
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering
    )

    lazy val contract = PlutusV3.compile(BitcoinValidator.validate)

    /** Apply a TxOutRef parameter to get the final script */
    def makeScript(txOutRef: TxOutRef): Program =
        (contract.program.deBruijnedProgram $ txOutRef.toData).toProgram

    /** Dummy TxOutRef for tests */
    lazy val testTxOutRef: TxOutRef = TxOutRef(
      TxId(hex"0000000000000000000000000000000000000000000000000000000000000000"),
      BigInt(0)
    )

    /** Test program with dummy parameter applied */
    lazy val bitcoinProgram: Program = makeScript(testTxOutRef)
}
