package binocular.watchtower

import scalus.cardano.address.{StakeAddress, StakePayload}
import scalus.cardano.ledger.{AssetName, Coin, PlutusScript, ScriptHash, Transaction, Utxo, Value}
import scalus.cardano.node.BlockchainProvider
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry.ProofStep
import scalus.cardano.onchain.plutus.prelude.List as ScalusList
import scalus.cardano.txbuilder.{ScriptSource, ThreeArgumentPlutusScriptWitness, TwoArgumentPlutusScriptWitness as TwoArg, TxBuilder}
import scalus.cardano.wallet.hd.HdAccount
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData
import scalus.uplc.DebugScript

import scala.concurrent.{ExecutionContext, Future}
import scala.util.chaining.scalaUtilChainingOps

/** Builds the P5 peg-out completion tx — burns the locked fBTC and records the peg-out in the
  * completed-peg-outs MPF, satisfying `peg_out.ak::withdraw(CompletePegOut)`.
  *
  * Unlike peg-in completion (which references a Confirmed TM UTxO), peg-out completion proves the TM
  * INLINE against the Binocular oracle: the `InputCompletePegOut` carries the block header + its MPF
  * inclusion proof in `confirmed_blocks_root`, plus the raw TM tx + its tx-merkle proof. There is no
  * depositor Schnorr — `owner_auth` is satisfied by the owner's signature (the sponsor, here).
  *
  * ==Shape==
  *   - Spend: the PegOut UTxO (peg_out script — its `spend` handler only requires a withdrawal from
  *     the same script hash; redeemer ignored) and the completed-peg-outs MPF UTxO.
  *   - References: config-NFT UTxO + Binocular oracle UTxO.
  *   - Mint: `-peg_out_amount` fBTC (burn). `bridged_token.ak`'s negative-mint branch requires a
  *     `MintRedeemer` pointing at the peg_out `CompletePegOut` withdrawal + that withdrawal present.
  *   - Withdrawals (0 ADA): `peg_out` → [[PegOutWithdrawRedeemer]] `CompletePegOut`; the
  *     produced-verifier ([[PegOutProducedVerifier]]) → the bare `List<Data>` redeemer it + peg_out.ak
  *     both read; the fBTC mint checker (config[19]) → [[FbtcMintCheckerRedeemer]] pointing at the
  *     peg_out withdrawal — `bridged_token` only requires the checker to run; the checker enforces
  *     the burn rules.
  *   - Outputs: the updated completed-peg-outs UTxO (same value+address, new MPF root); change →
  *     sponsor (carrying the burned PegOut UTxO's freed MIN_ADA).
  *
  * ==Index redeemers== mirror [[PegInCompleteTx]]: `self.redeemers` is ordered
  * `(RedeemerTag ordinal, purpose-index)` — Spend(0) inputs (sorted), then Mint(1) policies, then
  * Reward(3) withdrawals (sorted by reward account). So a reward redeemer's flat index =
  * `#scriptSpends + #mintPolicies + (its position in the sorted withdrawals)`.
  */
object PegOutCompleteTx {

    /** The five Plutus scripts that run in the completion tx. */
    final case class Scripts(
        pegOut: PlutusScript,
        completedPegOuts: PlutusScript,
        bridgedToken: PlutusScript,
        producedVerifier: PlutusScript,
        fbtcMintChecker: PlutusScript
    )

    /** The pre-existing UTxOs the tx spends (`pegOut`, `completedPegOuts`) / references (`config`,
      * `oracle`).
      */
    final case class Inputs(
        pegOut: Utxo,
        completedPegOuts: Utxo,
        config: Utxo,
        oracle: Utxo
    )

    /** CIP-33 reference-script UTxOs for the heavy scripts. `None` → inline in the witness set. The
      * produced verifier is small and always inlined.
      */
    final case class ScriptRefs(
        pegOut: Option[Utxo],
        completedPegOuts: Option[Utxo],
        bridgedToken: Option[Utxo]
    )

    def build(
        provider: BlockchainProvider,
        sponsor: HdAccount,
        scripts: Scripts,
        scriptRefs: ScriptRefs,
        inputs: Inputs,
        pegOutInfo: InputCompletePegOut,
        completedPegOutsInclusionProof: ScalusList[ProofStep],
        completedPegOutsNewRoot: ByteString,
        // The produced-verifier withdrawal redeemer — the bare list both peg_out.ak (via
        // un_list_data) and PegOutProducedVerifier read:
        // [treasury_utxo_id, destination, peg_out_utxo_id, peg_out_amount, raw_tx].
        verifierRedeemer: Data,
        pegOutAmount: Long,
        bridgedTokenPolicy: ScriptHash,
        bridgedTokenAsset: AssetName,
        completedPegOutsPolicy: ScriptHash,
        completedPegOutsAsset: AssetName,
        pegOutHash: ScriptHash,
        producedVerifierHash: ScriptHash,
        // Diagnostic only: a verbose-trace-compiled peg_out PlutusScript (same params, different
        // hash) registered under `pegOutHash` so scalus's replayWithDiagnostics emits per-condition
        // `?` traces when the deployed (release) peg_out returns false.
        debugPegOut: Option[PlutusScript] = None
    )(using ExecutionContext): Future[Transaction] = {
        val network = provider.cardanoInfo.network
        val signer = sponsor.signerForUtxos
        val sponsorAddress = sponsor.baseAddress(network)

        def stake(h: ScriptHash): StakeAddress = StakeAddress(network, StakePayload.Script(h))

        // --- index helpers over the assembled tx (see object doc) ---
        def inputsSorted(tx: Transaction) = tx.body.value.inputs.toIndexedSeq
        def refsSorted(tx: Transaction) = tx.body.value.referenceInputs.toIndexedSeq
        def outputs(tx: Transaction) = tx.body.value.outputs

        def inputIndex(tx: Transaction, u: Utxo): BigInt =
            BigInt(inputsSorted(tx).indexOf(u.input))
        def configRefIndex(tx: Transaction): BigInt =
            BigInt(refsSorted(tx).indexOf(inputs.config.input))
        def cpoOutputIndex(tx: Transaction): BigInt =
            BigInt(
              outputs(tx).indexWhere(
                _.value.value.hasAsset(completedPegOutsPolicy, completedPegOutsAsset)
              )
            )

        // Reward redeemer flat index = #scriptSpends + #mintPolicies + position in sorted withdrawals.
        def scriptSpends(tx: Transaction): Int =
            Seq(inputs.pegOut.input, inputs.completedPegOuts.input).count(inputsSorted(tx).contains)
        def mintPolicies(tx: Transaction): Int =
            tx.body.value.mint.map(_.assets.size).getOrElse(0)
        def withdrawalPos(tx: Transaction, h: ScriptHash): Int =
            tx.body.value.withdrawals
                .map(_.withdrawals.keys.toIndexedSeq.indexWhere(_.address == stake(h)))
                .getOrElse(-1)
        def rewardRedeemerIndex(tx: Transaction, h: ScriptHash): BigInt =
            BigInt(scriptSpends(tx) + mintPolicies(tx) + withdrawalPos(tx, h))

        // --- redeemers (delayed: indices depend on the assembled tx) ---
        val pegOutWithdrawRedeemer: Transaction => Data = tx => {
            val action = PegOutActionType.CompletePegOut(
              pegOutInfo = pegOutInfo,
              completedPegOutsInputIndex = inputIndex(tx, inputs.completedPegOuts),
              completedPegOutsOutputIndex = cpoOutputIndex(tx),
              addedPegOutToCompletedPegOutsInclusionProof = completedPegOutsInclusionProof,
              tmtilaspopvshWithdrawRedeemerIndex = rewardRedeemerIndex(tx, producedVerifierHash)
            )
            PegOutWithdrawRedeemer(configRefIndex(tx), action).toData
        }

        val completedPegOutsSpendRedeemer: Transaction => Data = tx =>
            CompletedPegOutsSpendRedeemer(
              configRefInputIndex = configRefIndex(tx),
              pegOutWithdrawRedeemerIndex = rewardRedeemerIndex(tx, pegOutHash)
            ).toData

        // bridged_token is a pure delegator: it only requires the fBTC mint checker (config[19])
        // withdrawal. The checker's own redeemer points at the peg_out CompletePegOut withdrawal
        // (the checker's negative-mint branch).
        val bridgedTokenMintRedeemer: Transaction => Data = tx =>
            BridgedTokenMintRedeemer(configRefInputIndex = configRefIndex(tx)).toData

        val fbtcMintCheckerRedeemer: Transaction => Data = tx =>
            FbtcMintCheckerRedeemer(
              configRefInputIndex = configRefIndex(tx),
              pegWithdrawRedeemerIndex = rewardRedeemerIndex(tx, pegOutHash)
            ).toData

        // --- output: the updated completed-peg-outs UTxO (same value + address, new MPF root) ---
        val newCpoDatum = CompletedPegOutsMerkleTreeDatum(completedPegOutsNewRoot)

        import ScriptSource.{PlutusScriptAttached, PlutusScriptValue}
        def spendSource(useRef: Boolean, script: PlutusScript): ScriptSource[PlutusScript] =
            if useRef then PlutusScriptAttached else PlutusScriptValue(script)

        val extraRefs: Seq[Utxo] =
            Seq(scriptRefs.pegOut, scriptRefs.completedPegOuts, scriptRefs.bridgedToken).flatten

        // Both spent UTxOs carry inline datums on-chain → DatumInlined.
        val pegOutSpendWitness = ThreeArgumentPlutusScriptWitness(
          scriptSource = spendSource(scriptRefs.pegOut.isDefined, scripts.pegOut),
          redeemer = Data.unit,
          datum = scalus.cardano.txbuilder.Datum.DatumInlined
        )
        val cpoSpendWitness = ThreeArgumentPlutusScriptWitness(
          scriptSource =
              spendSource(scriptRefs.completedPegOuts.isDefined, scripts.completedPegOuts),
          redeemerBuilder = completedPegOutsSpendRedeemer,
          datum = scalus.cardano.txbuilder.Datum.DatumInlined
        )
        val pegOutWithdrawWitness: TwoArg = TwoArg(
          scriptSource = spendSource(scriptRefs.pegOut.isDefined, scripts.pegOut),
          redeemerBuilder = pegOutWithdrawRedeemer
        )
        // The produced verifier is small — always inlined in the witness set (no ref UTxO needed).
        val producedVerifierWitness: TwoArg = TwoArg(
          scriptSource = PlutusScriptValue(scripts.producedVerifier),
          redeemer = verifierRedeemer
        )
        // The fBTC mint checker is likewise small — always inlined.
        val checkerWithdrawWitness: TwoArg = TwoArg(
          scriptSource = PlutusScriptValue(scripts.fbtcMintChecker),
          redeemerBuilder = fbtcMintCheckerRedeemer
        )

        // `.references(...)` MUST precede any PlutusScriptAttached / `.mint(policyId, ...)` /
        // `.withdrawRewards(... attached)` — TxBuilder verifies each AttachedScript has a ref
        // already attached. Same ordering rule as PegInCompleteTx.
        val baseBuilder = (Seq(inputs.config, inputs.oracle) ++ extraRefs) match {
            case head +: tail =>
                TxBuilder(provider.cardanoInfo)
                    .references(head, tail*)
                    .spend(inputs.pegOut, pegOutSpendWitness)
                    .spend(inputs.completedPegOuts, cpoSpendWitness)
            case Seq() =>
                throw new IllegalStateException("config + oracle refs must be present")
        }

        // Burn -pegOutAmount fBTC. policyId-based overload uses PlutusScriptAttached (needs the ref);
        // script-based overload inlines.
        val withBurn =
            if scriptRefs.bridgedToken.isDefined then
                baseBuilder.mint(
                  bridgedTokenPolicy,
                  Map(bridgedTokenAsset -> -pegOutAmount),
                  bridgedTokenMintRedeemer
                )
            else
                baseBuilder.mint(
                  scripts.bridgedToken,
                  Map(bridgedTokenAsset -> -pegOutAmount),
                  bridgedTokenMintRedeemer
                )

        withBurn
            .withdrawRewards(stake(pegOutHash), Coin.zero, pegOutWithdrawWitness)
            .withdrawRewards(stake(producedVerifierHash), Coin.zero, producedVerifierWitness)
            .withdrawRewards(
              stake(scripts.fbtcMintChecker.scriptHash),
              Coin.zero,
              checkerWithdrawWitness
            )
            // owner_auth = CardanoSignature(owner pkh); peg_out.ak checks the pkh is in the tx's
            // signatories (= required signers). The PegOut was locked with owner = sponsor pkh, so
            // require + sign with it. (If a peg-out used a different owner, only that key could
            // complete it.)
            .requireSignatures(Set(sponsor.paymentKeyHash))
            .payTo(
              inputs.completedPegOuts.output.address,
              inputs.completedPegOuts.output.value,
              newCpoDatum.toData
            )
            .pipe(b => debugPegOut.fold(b)(ds => b.withDebugScript(pegOutHash, DebugScript(ds))))
            .complete(provider, sponsorAddress)
            .map(_.sign(signer).transaction)
    }
}
