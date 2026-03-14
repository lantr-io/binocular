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

    /** Apply a BitcoinValidatorParams to get the final script */
    def makeScript(params: BitcoinValidatorParams): Program =
        (contract.program.deBruijnedProgram $ params.toData).toProgram

    /** Convenience: apply just a TxOutRef with default params */
    def makeScript(txOutRef: TxOutRef): Program =
        makeScript(validatorParams(txOutRef))

    /** Build default params from a TxOutRef */
    def validatorParams(txOutRef: TxOutRef): BitcoinValidatorParams =
        BitcoinValidatorParams(
          maturationConfirmations = 100,
          challengeAging = 200 * 60,
          oneShotTxOutRef = txOutRef
        )

    /** Dummy TxOutRef for tests */
    lazy val testTxOutRef: TxOutRef = TxOutRef(
      TxId(hex"0000000000000000000000000000000000000000000000000000000000000000"),
      BigInt(0)
    )

    /** Test params with dummy TxOutRef */
    lazy val testParams: BitcoinValidatorParams = validatorParams(testTxOutRef)

    /** Test program with dummy parameter applied */
    lazy val bitcoinProgram: Program = makeScript(testParams)
}
