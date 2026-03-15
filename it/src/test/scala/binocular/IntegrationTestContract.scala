package binocular

import scalus.*
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.{Credential, MajorProtocolVersion, Script}
import scalus.cardano.onchain.plutus.v3.TxOutRef
import scalus.compiler.Options
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.toData
import scalus.uplc.{PlutusV3, Program}

/** Integration-test-specific contract compilation.
  *
  * Differs from production BitcoinContract:
  *   - Compiled with `changPV` (protocol version 9) for Yaci DevKit compatibility
  *   - Error traces enabled for easier debugging
  *   - IT-specific params with short challengeAging (30 seconds instead of 200 minutes)
  */
object IntegrationTestContract {
    given opts: Options = Options.release.copy(
      generateErrorTraces = true,
      targetProtocolVersion = MajorProtocolVersion.changPV
    )

    lazy val contract = PlutusV3.compile(BitcoinValidator.validate)

    def makeScript(params: BitcoinValidatorParams): Program =
        (contract.program.deBruijnedProgram $ params.toData).toProgram

    /** IT-specific params: short challengeAging for fast test execution */
    lazy val itParams: BitcoinValidatorParams = BitcoinValidatorParams(
      maturationConfirmations = 100,
      challengeAging = 30, // 30 seconds - blocks naturally age during batch processing
      oneShotTxOutRef = BitcoinContract.testTxOutRef
    )

    lazy val itProgram: Program = makeScript(itParams)

    lazy val itPlutusScript: Script.PlutusV3 =
        Script.PlutusV3(itProgram.cborByteString)

    lazy val testnetScriptAddress: Address = {
        val scriptHash = itPlutusScript.scriptHash
        Address(Network.Testnet, Credential.ScriptHash(scriptHash))
    }

    lazy val testnetScriptAddressBech32: String =
        testnetScriptAddress
            .asInstanceOf[scalus.cardano.address.ShelleyAddress]
            .toBech32
            .get
}
