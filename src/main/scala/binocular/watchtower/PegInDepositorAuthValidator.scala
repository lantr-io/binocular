package binocular.watchtower

import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.v1.Credential
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.*
import scalus.compiler.{Compile, Options}
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.*
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.Data.toData

/** Rewarding-script validator that gates peg-in completion on a depositor BIP340 Schnorr signature
  * AND binds the minted fBTC to the recipient the depositor chose.
  *
  * NOTE (B1): the peg-in *completion* path no longer delegates to this withdraw — the same Schnorr
  * auth + recipient-binding is now embedded directly in `peg_in.ak::CompletePegIn`, with `btc_txid`
  * read from the referenced Confirmed TM UTxO. This validator remains the `PegInDatum.ownerAuth`
  * target (the `Cancel` path) and the home of [[mintTag]] (reused by `pegin-complete` to build the
  * signing digest). The reward logic below documents the per-mint message it shares with `peg_in.ak`.
  *
  * Used as the target of `PegInDatum.ownerAuth = CardanoWithdrawScript { hash = thisScriptHash }`.
  * `peg_in.ak`'s completion only checks the fBTC *amount*; it delegates *who is authorized and
  * where the fBTC goes* to `owner_auth`. So the recipient-binding from the protocol spec
  * (technical_documentation.md §"Complete peg-in") must live here. When the depositor completes a
  * peg-in, the completion transaction must include a 0-ADA withdrawal from this script's reward
  * account; Cardano then runs this validator, which:
  *
  *   1. selects the peg-in input as the unique input whose payment credential is `pegInScriptHash`
  *      (matching `peg-in.ak`'s `payment_credential == credential` filter +
  *      `expect [peg_in_input]`; the staking part is ignored, exactly as upstream) and parses its
  *      inline [[PegInDatum]];
  *   2. requires an output (index from redeemer) that pays exactly `peg_in_amount` fBTC
  *      (`bridgedToken{Policy,AssetName}` params) to the redeemer's `recipient` address;
  *   3. verifies the depositor's Schnorr signature over the per-mint message
  *      `"BFR-mint-v1" ‖ treasury_movement_btc_txid ‖ peg_in_utxo_id ‖ recipient`, using
  *      `datum.userSourceChainPubKey`.
  *
  * Because the signed message commits to `recipient` AND step (2) forces the fBTC to that same
  * recipient, an observed/leaked signature cannot be replayed to redirect the mint (the relayer /
  * mempool front-run gap of a recipient-free signature). `peg_in_utxo_id` makes it replay-safe
  * across peg-ins; `treasury_movement_btc_txid` binds it to the specific TM.
  *
  * The xonly used for verification is read from the datum, so this validator is NOT parameterized
  * per-depositor — one deployment serves every peg-in. It IS parameterized by the fBTC asset so it
  * can identify the minted token in step (2).
  *
  * Caveat: relies on the watchtower (or whoever minted the PegInRequest) having honestly populated
  * `userSourceChainPubKey` from the BTC OP_RETURN. `peg-in.ak`'s mint handler doesn't enforce this
  * today, so the bridge as a whole still has a watchtower-honesty assumption at mint time. See
  * `internal-docs/bitfrost/watchtower/`.
  */
@Compile
object PegInDepositorAuthValidator {

    // Domain-separation tag "BFR-mint-v1" (BIP340 practice; technical_documentation.md §Complete
    // peg-in). 42 46 52 2d 6d 69 6e 74 2d 76 31.
    val mintTag: ByteString = ByteString.fromHex("4246522d6d696e742d7631")

    def reward(
        params: PegInDepositorAuthParams,
        redeemer: Datum,
        tx: TxInfo
    ): Unit = {
        val r = redeemer.to[PegInDepositorAuthRedeemer]

        // Select the peg-in input as the UNIQUE input whose payment credential is the
        // peg_in_validator script hash — matching peg-in.ak's `payment_credential == credential`
        // filter + `expect [peg_in_input]` (the staking credential is ignored, exactly as
        // upstream). A caller-supplied index must NOT be trusted: an attacker could point it at a
        // decoy input carrying a fake PegInDatum with their own xonly and bypass the real
        // depositor's signature. peg-in.ak aborts the completion unless exactly one such input
        // exists, so the genuine peg-in is the only candidate here too.
        val pegInCredential = Credential.ScriptCredential(params.pegInScriptHash)
        val pegInInput = tx.inputs.filter { in =>
            in.resolved.address.credential.toData == pegInCredential.toData
        } match
            case List.Cons(only, List.Nil) => only.resolved
            case _ =>
                fail("Expected exactly one input with the peg-in script payment credential")

        val datum = pegInInput.datum match
            case OutputDatum.OutputDatum(d) => d.to[PegInDatum]
            case _                          => fail("Peg-in input has no inline datum")

        // 1. The minted fBTC must be paid to the recipient the depositor signed for, in the exact
        //    peg-in amount. Without this, a recipient-bound signature is still replayable to a
        //    different output (the front-run / malicious-relayer gap).
        val fbtcOutput = tx.outputs.at(r.fbtcOutputIndex)
        require(
          fbtcOutput.address.toData == r.recipient,
          "fBTC output not paid to the bound recipient"
        )
        require(
          fbtcOutput.value
              .quantityOf(params.bridgedTokenPolicyId, params.bridgedTokenAssetName)
              == datum.pegInAmount,
          "fBTC output amount != peg_in_amount"
        )

        // 2. Depositor authorizes THIS recipient and THIS treasury movement. Replay-safe across
        //    peg-ins via the globally-unique pegInUtxoId, and bound to the chosen recipient.
        val msg = sha2_256(
          appendByteString(
            mintTag,
            appendByteString(
              r.treasuryMovementBtcTxid,
              appendByteString(datum.pegInUtxoId, serialiseData(r.recipient))
            )
          )
        )
        require(
          verifySchnorrSecp256k1Signature(datum.userSourceChainPubKey, msg, r.signature),
          "Invalid depositor Schnorr signature"
        )
    }

    def validate(paramsData: Data)(scData: Data): Unit = {
        val params = paramsData.to[PegInDepositorAuthParams]
        val sc = scData.to[ScriptContext]
        sc.scriptInfo match
            case ScriptInfo.RewardingScript(_) =>
                reward(params, sc.redeemer, sc.txInfo)
            case _ => fail("Not a rewarding script")
    }
}

object PegInDepositorAuthContract {
    given opts: Options = Options.release

    lazy val contract: PlutusV3[Data => Data => Unit] =
        PlutusV3.compile(PegInDepositorAuthValidator.validate)

    /** Apply the fBTC asset parameters, yielding the concrete `Data => Unit` validator whose script
      * hash is used as `PegInDatum.ownerAuth`.
      */
    def makeContract(params: PegInDepositorAuthParams): PlutusV3[Data => Unit] =
        contract(params.toData)
}
