package binocular.watchtower

import binocular.*
import binocular.blueprint.BinocularBlueprint
import scalus.cardano.blueprint.Blueprint
import scalus.cardano.blueprint.Contract
import scalus.cardano.ledger.Script
import scalus.compiler.Options
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.*

/** The TM validator parameterized with the Binocular oracle script hash and the config NFT
  * `(policy, name)`. The compiled script's hash is BOTH the TM UTxO address (spend) and the TM NFT
  * policy id (mint). All three parameters are STABLE — the address does NOT depend on any
  * participant key. `Unconfirmed` UTxOs are locked here until Confirm retires them; the TM NFT can
  * be minted by ANYONE whose posted TM chains from the bridge-state singleton's head (see
  * [[TmMintRedeemer]]).
  */
object TreasuryMovementContract extends Contract {
    given opts: Options = Options.release

    /** Curried form: `oracleScriptHash -> configNftPolicy -> configNftName -> (scriptContext ->
      * ())`. Applied via `.apply`.
      */
    lazy val parameterized: PlutusV3[ByteString => (ByteString => (ByteString => (Data => Unit)))] =
        PlutusV3.compile((oracleScriptHash: ByteString) =>
            (configNftPolicy: ByteString) =>
                (configNftName: ByteString) =>
                    (scData: Data) =>
                        TreasuryMovementValidator.validate(
                          oracleScriptHash,
                          configNftPolicy,
                          configNftName,
                          scData
                        )
        )

    def contract(
        oracleScriptHash: ByteString,
        configNftPolicy: ByteString,
        configNftName: ByteString
    ): PlutusV3[Data => Unit] =
        parameterized.apply(oracleScriptHash).apply(configNftPolicy).apply(configNftName)

    /** Treasury-movement script for the given params: the unapplied program from the generated
      * CIP-57 blueprint with the three `ByteString` params applied at the UPLC level as bare
      * bytestring constants (the validator is compiled from curried `ByteString` lambdas, not
      * `Data` — see [[parameterized]]).
      */
    def script(
        oracleScriptHash: ByteString,
        configNftPolicy: ByteString,
        configNftName: ByteString
    ): Script.PlutusV3 =
        BinocularBlueprint.script(
          "TreasuryMovementContract",
          BinocularBlueprint.bytesParam(oracleScriptHash),
          BinocularBlueprint.bytesParam(configNftPolicy),
          BinocularBlueprint.bytesParam(configNftName)
        )

    /** CIP-57 blueprint over the UNAPPLIED parameterized program: consumers (and [[script]]) apply
      * the three params UPLC-level, Aiken-style. Built manually because the `Blueprint.plutusV3`
      * helpers only model single-parameter validators.
      */
    lazy val blueprint: Blueprint = {
        // Validator would clash with plutus.v3.Validator (wildcard-imported above) — keep scoped
        import scalus.cardano.blueprint.{Preamble, Validator}
        import scalus.utils.Hex.toHex
        val title = "TreasuryMovementContract"
        val description =
            "Bifrost treasury-movement validator: holds Unconfirmed→Confirmed TM state, " +
                "parameterized by (oracleScriptHash, configNftPolicy, configNftName)."
        val bytes = BinocularBlueprint.bytesParamDescription
        Blueprint(
          preamble = Preamble(
            title,
            description,
            "1.0.0",
            plutusVersion = parameterized.language,
            license = Some("Apache-2.0")
          ),
          validators = Seq(
            Validator(
              title = title,
              description = Some(description),
              redeemer = Some(BinocularBlueprint.opaqueDataDescription),
              datum = None,
              parameters = Some(scala.List(bytes, bytes, bytes)),
              compiledCode = Some(parameterized.program.cborEncoded.toHex),
              hash = Some(parameterized.script.scriptHash.toHex)
            )
          )
        )
    }
}

/** Trace-instrumented twin of [[TreasuryMovementContract]] for Scalus diagnostic replay. Compiled
  * from the IDENTICAL validator source but with `generateErrorTraces = true` — the release compile
  * strips trace strings, so a failing on-chain confirm reports only "Error evaluated" with no clue
  * which `require` failed. This twin's script hash DIFFERS from the deployed
  * [[TreasuryMovementContract.script]] (traces change the UPLC) — which is fine and intended: it is
  * registered UNDER the deployed hash via `TxBuilder.withDebugScript` and only re-evaluated against
  * the same script context to surface the failing check. Same validator logic ⇒ same failing
  * require, now with its trace string.
  *
  * Kept in a SEPARATE object so its traces-on `Options` given does not clash with
  * [[TreasuryMovementContract.opts]] during Scalus macro expansion.
  */
object TreasuryMovementDebugContract {
    // Same options as TreasuryMovementContract (so the twin mirrors the deployed script's logic +
    // param application exactly), plus trace strings the release compile strips.
    given opts: Options = Options.release.copy(generateErrorTraces = true)

    lazy val parameterized: PlutusV3[ByteString => (ByteString => (ByteString => (Data => Unit)))] =
        PlutusV3.compile((oracleScriptHash: ByteString) =>
            (configNftPolicy: ByteString) =>
                (configNftName: ByteString) =>
                    (scData: Data) =>
                        TreasuryMovementValidator.validate(
                          oracleScriptHash,
                          configNftPolicy,
                          configNftName,
                          scData
                        )
        )

    /** Trace-compiled [[Script.PlutusV3]] twin for the given params (for `withDebugScript`). */
    def script(
        oracleScriptHash: ByteString,
        configNftPolicy: ByteString,
        configNftName: ByteString
    ): Script.PlutusV3 =
        parameterized
            .apply(oracleScriptHash)
            .apply(configNftPolicy)
            .apply(configNftName)
            .script
}
