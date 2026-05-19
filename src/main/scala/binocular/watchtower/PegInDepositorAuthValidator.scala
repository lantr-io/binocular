package binocular.watchtower

import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.v1.Credential
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.*
import scalus.compiler.{Compile, Options}
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.*
import scalus.uplc.builtin.Builtins.*

/** Rewarding-script validator that gates peg-in completion on a depositor
  * BIP340 Schnorr signature.
  *
  * Used as the target of `PegInDatum.ownerAuth =
  * CardanoWithdrawScript { hash = thisScriptHash }`. When the depositor
  * completes a peg-in, the completion transaction must include a 0-ADA
  * withdrawal from this script's reward account. Cardano then runs this
  * validator, which:
  *
  *   1. reads the peg-in input (index supplied via redeemer),
  *   2. parses its inline datum as [[PegInDatum]],
  *   3. computes `msg = sha2_256(datum.pegInUtxoId)`,
  *   4. verifies the depositor's Schnorr signature with
  *      `verifySchnorrSecp256k1Signature(datum.userSourceChainPubKey, msg, sig)`.
  *
  * The xonly used for verification is read from the datum, so this validator
  * is NOT parameterized per-depositor. One deployment serves every peg-in.
  *
  * Caveat: relies on the watchtower (or whoever minted the PegInRequest)
  * having honestly populated `userSourceChainPubKey` from the BTC OP_RETURN.
  * `peg-in.ak`'s mint handler doesn't enforce this today (TODO at line 97),
  * so the bridge as a whole still has a watchtower-honesty assumption at
  * mint time. See `internal-docs/bitfrost/watchtower/pegin-step.md`.
  */
@Compile
object PegInDepositorAuthValidator {

    inline def reward(
        redeemer: Datum,
        credential: Credential,
        tx: TxInfo
    ): Unit = {
        val r = redeemer.to[PegInDepositorAuthRedeemer]

        // Direct index lookup over scanning saves O(n) tailList traversals on
        // the inputs list. A wrong index can't help an attacker — sig is bound
        // to the input's own xonly+pegInUtxoId.
        val pegInInput = tx.inputs.at(r.pegInInputIndex).resolved

        val datum = pegInInput.datum match
            case OutputDatum.OutputDatum(d) => d.to[PegInDatum]
            case _                          => fail("Peg-in input has no inline datum")

        // Replay-safe across peg-ins: each peg-in's pegInUtxoId (BTC outpoint)
        // is globally unique, so a sig for one is useless against another.
        val msg = sha2_256(datum.pegInUtxoId)

        require(
          verifySchnorrSecp256k1Signature(
            datum.userSourceChainPubKey,
            msg,
            r.signature
          ),
          "Invalid depositor Schnorr signature"
        )
    }

    inline def validate(scData: Data): Unit = {
        val sc = scData.to[ScriptContext]
        sc.scriptInfo match
            case ScriptInfo.RewardingScript(credential) =>
                reward(sc.redeemer, credential, sc.txInfo)
            case _ => fail("Not a rewarding script")
    }
}

object PegInDepositorAuthContract {
    given opts: Options = Options.release

    lazy val contract: PlutusV3[Data => Unit] =
        PlutusV3.compile(PegInDepositorAuthValidator.validate)
}
