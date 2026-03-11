package binocular

import scalus.*
import scalus.cardano.ledger.MajorProtocolVersion
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}
import scalus.compiler.Options
import scalus.uplc.builtin.ByteString.hex
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.toData
import scalus.uplc.{PlutusV3, Program}

object BitcoinContract {
    given opts: Options = Options.release.copy(
      generateErrorTraces = false,
      targetProtocolVersion = MajorProtocolVersion.plominPV
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
