package binocular.watchtower

import scalus.cardano.address.{StakeAddress, StakePayload}
import scalus.cardano.ledger.{AssetName, Coin, PlutusScript, ScriptHash, Transaction, Utxo}
import scalus.cardano.node.BlockchainProvider
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry.ProofStep
import scalus.cardano.onchain.plutus.prelude.List as ScalusList
import scalus.cardano.txbuilder.{Datum, ScriptSource, ThreeArgumentPlutusScriptWitness, TwoArgumentPlutusScriptWitness as TwoArg, TxBuilder}
import scalus.cardano.wallet.hd.HdAccount
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.toData

import scala.concurrent.{ExecutionContext, Future}

/** Builds the peg-out **Complete** transaction — permissionless cleanup of a PAID PegOutRequest.
  *
  * Since spec rev 5.1 completion carries no authorization and no Bitcoin proof. A confirmed Treasury
  * Movement already paid the destination and its FROST-attested root, copied into the on-chain CPO
  * singleton at Confirm, already binds this request to that payment. All Complete does is prove
  * membership and burn the fBTC the request locked. Whoever does it keeps the request's MIN_ADA —
  * the cleanup incentive that stops PegOutRequest state from accumulating.
  *
  * ==Shape==
  *   - Spend: the PegOutRequest UTxO. `peg_out.ak`'s `spend` handler only requires a withdrawal from
  *     its own script hash, so its redeemer is ignored (`Data.unit`).
  *   - References: the Config UTxO and the CPO singleton. BOTH are referenced, never spent — which
  *     is what makes completion transactions independent of each other.
  *   - Mint: `-locked` fBTC. `bridged_token.ak` allows a burn whenever the peg-out withdraw script
  *     runs; `peg_out.ak` is what pins the amount to ALL of the locked tokens.
  *   - Withdrawal (0 ADA) from the `peg_out` reward account, carrying
  *     [[PegOutWithdrawRedeemer]]`(configRefIdx, cpoRefIdx, CompletePegOut(proof))`. The reward
  *     account must be registered (`register-bridge-creds`).
  *   - Outputs: change to the sponsor, which is where the freed MIN_ADA (and any stray tokens the
  *     request held) ends up.
  *
  * ==ONE request per transaction== `peg_out.ak`'s withdraw handler does
  * `expect [peg_out_input] = list.filter(inputs, at own credential)`, so a transaction carrying two
  * PegOutRequest inputs traps. Batching is therefore impossible by construction, not by choice.
  *
  * ==Redeemer indices== point into `reference_inputs`, not `inputs` — `utils.safe_list_at` is
  * applied to `self.reference_inputs` on both reads. They are resolved from the ASSEMBLED
  * transaction (delayed redeemer builders) because the ledger sorts both sets canonically, so the
  * position is not known until the builder has finished balancing.
  */
object PegOutCompleteTx {

    /** The two Plutus scripts that run: `peg_out` (spend + withdraw) and `bridged_token` (burn). */
    final case class Scripts(pegOut: PlutusScript, bridgedToken: PlutusScript)

    /** CIP-33 reference-script UTxOs. `None` inlines the script in the witness set — viable for
      * these two, but a reference keeps the transaction small enough to stay cheap.
      */
    final case class ScriptRefs(pegOut: Option[Utxo], bridgedToken: Option[Utxo])

    /** `pegOut` is SPENT; `config` and `completedPegOuts` are REFERENCED. */
    final case class Inputs(pegOut: Utxo, config: Utxo, completedPegOuts: Utxo)

    def build(
        provider: BlockchainProvider,
        sponsor: HdAccount,
        scripts: Scripts,
        scriptRefs: ScriptRefs,
        inputs: Inputs,
        membershipProof: ScalusList[ProofStep],
        // ALL of the fBTC the request locked. `peg_out.ak` requires the mint to be exactly its
        // negation, so this is never a partial burn.
        lockedFbtc: Long,
        bridgedTokenPolicy: ScriptHash,
        bridgedTokenAsset: AssetName,
        pegOutHash: ScriptHash
    )(using ExecutionContext): Future[Transaction] = {
        val network = provider.cardanoInfo.network
        val signer = sponsor.signerForUtxos
        val sponsorAddress = sponsor.baseAddress(network)

        def refIndex(tx: Transaction, u: Utxo): BigInt =
            BigInt(tx.body.value.referenceInputs.toIndexedSeq.indexOf(u.input))

        val pegOutWithdrawRedeemer: Transaction => Data = tx =>
            PegOutWithdrawRedeemer(
              configRefInputIndex = refIndex(tx, inputs.config),
              completedPegOutsRefInputIndex = refIndex(tx, inputs.completedPegOuts),
              actionType = PegOutActionType.CompletePegOut(membershipProof)
            ).toData

        val bridgedTokenMintRedeemer: Transaction => Data = tx =>
            BridgedTokenMintRedeemer(configRefInputIndex = refIndex(tx, inputs.config)).toData

        import ScriptSource.{PlutusScriptAttached, PlutusScriptValue}
        def source(useRef: Boolean, script: PlutusScript): ScriptSource[PlutusScript] =
            if useRef then PlutusScriptAttached else PlutusScriptValue(script)

        val pegOutSpendWitness = ThreeArgumentPlutusScriptWitness(
          scriptSource = source(scriptRefs.pegOut.isDefined, scripts.pegOut),
          redeemer = Data.unit,
          datum = Datum.DatumInlined
        )
        val pegOutWithdrawWitness: TwoArg = TwoArg(
          scriptSource = source(scriptRefs.pegOut.isDefined, scripts.pegOut),
          redeemerBuilder = pegOutWithdrawRedeemer
        )

        // `.references(...)` MUST precede any PlutusScriptAttached witness / `.mint(policyId, …)`:
        // TxBuilder verifies each attached script already has its reference input. Same ordering
        // rule as PegInCompleteTx.
        val refs = Seq(inputs.config, inputs.completedPegOuts) ++
            Seq(scriptRefs.pegOut, scriptRefs.bridgedToken).flatten
        val base = TxBuilder(provider.cardanoInfo)
            .references(refs.head, refs.tail*)
            .spend(inputs.pegOut, pegOutSpendWitness)

        val withBurn =
            if scriptRefs.bridgedToken.isDefined then
                base.mint(
                  bridgedTokenPolicy,
                  Map(bridgedTokenAsset -> -lockedFbtc),
                  bridgedTokenMintRedeemer
                )
            else
                base.mint(
                  scripts.bridgedToken,
                  Map(bridgedTokenAsset -> -lockedFbtc),
                  bridgedTokenMintRedeemer
                )

        // No `requireSignatures`: completion is permissionless since rev 5.1. `owner_auth` gates
        // Cancel only, and adding a required signer here would make the sweeper unable to complete
        // any request but its own.
        withBurn
            .withdrawRewards(
              StakeAddress(network, StakePayload.Script(pegOutHash)),
              Coin.zero,
              pegOutWithdrawWitness
            )
            .complete(provider, sponsorAddress)
            .map(_.sign(signer).transaction)
    }
}
