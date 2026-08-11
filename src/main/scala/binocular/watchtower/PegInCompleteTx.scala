package binocular.watchtower

import scalus.cardano.address.{Address, StakeAddress, StakePayload}
import scalus.cardano.ledger.{AssetName, Coin, PlutusScript, ScriptHash, Transaction, Utxo, Value}
import scalus.cardano.node.BlockchainProvider
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry.ProofStep
import scalus.cardano.onchain.plutus.prelude.List as ScalusList
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}
import scalus.cardano.txbuilder.TxBuilder
import scalus.cardano.wallet.hd.HdAccount
import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData

import scala.concurrent.{ExecutionContext, Future}

/** Builds the B1 peg-in completion tx: mints `peg_in_amount` fBTC to the depositor's chosen
  * recipient and records the peg-in in the completed-peg-ins MPF, satisfying
  * `peg_in.ak::withdraw(CompletePegIn)`.
  *
  * ==Rev 5.4: the singleton replaces the Confirmed record==
  * There is no `Confirmed` TM record any more ([OB-5]). The completion *references* the **bridge
  * state singleton** — the UTxO carrying the NFT `(bridge_state_policy, "BSS")`, where
  * `bridge_state_policy` is Config datum field 3 read at runtime ([CPI-10], [PAR-1]). `peg_in.ak`
  * finds it by NFT at `bridge_state_ref_input_index` and verifies the sweep with
  * `mpf.has(spi_root, peg_in_utxo_id, sweeping_tm_input_0, proof)` ([CPI-9]). The depositor BIP-322
  * auth + recipient-binding are enforced inside `peg_in.ak` itself, so the only rewarding script is
  * `peg_in` (the stake-validator delegation pattern).
  *
  * ==Shape==
  *   - Spend: the PegInRequest UTxO (peg_in script — its `spend` handler only requires a withdrawal
  *     from the same script hash; redeemer ignored) and the completed-peg-ins MPF UTxO.
  *   - References: the config-NFT UTxO (`ConfigDatum`) and the bridge state singleton ("BSS" NFT).
  *   - Mint: `+peg_in_amount` fBTC (`bridged_token` policy) — the whole amount, no leader reward
  *     ([CPI-6]; [CPI-7] is WITHDRAWN and [CPI-11]/[CPI-12] are PARKED).
  *   - Burn: `−1` PegInRequest NFT under the `peg_in` policy. `CompletePegIn` step 0 requires it
  *     ([CPI-8]): the NFT authenticates the datum, so it must not outlive the request. The `peg_in`
  *     mint handler admits a burn-only transaction unconditionally, but still decodes its redeemer
  *     as a [[PegInMintRedeemer]], so the burn carries a well-formed one.
  *   - Withdrawals (0 ADA): `peg_in` → [[PegInWithdrawRedeemer]] `CompletePegIn`. The
  *     `bridged_token` policy reads the ConfigDatum and enforces the mint against this withdrawal
  *     directly (Variant B – no separate mint checker).
  *   - Outputs: fBTC → recipient; the updated completed-peg-ins UTxO (same value+address, new MPF
  *     root); change → sponsor (the PegInRequest's MIN_ADA, without its NFT — that is burned).
  *
  * ==Index redeemers==
  * Several redeemer fields are indices into the *assembled* tx and are filled by delayed
  * `Transaction => Data` builders. The on-chain `self.redeemers` list is ordered by Scalus's
  * `(RedeemerTag ordinal, index)` – all Spend(0), then Mint(1), then Reward(3) – so a reward
  * redeemer's flat position is `#scriptSpends + #mintPolicies + (its position in the sorted
  * withdrawals)`. Inputs/reference-inputs are ordered by `(txid, index)`; outputs keep insertion
  * order.
  */
object PegInCompleteTx {

    /** The three Plutus scripts that run in the completion tx. */
    final case class Scripts(
        pegIn: PlutusScript,
        completedPegIns: PlutusScript,
        bridgedToken: PlutusScript
    )

    /** The four pre-existing UTxOs the tx spends/references. `bridgeState` is the bridge state
      * singleton (referenced, never spent) — `peg-in.ak` finds it by the NFT
      * `(bridge_state_policy, "BSS")` at `bridge_state_ref_input_index` ([CPI-10]).
      */
    final case class Inputs(
        pir: Utxo,
        completedPegIns: Utxo,
        config: Utxo,
        bridgeState: Utxo
    )

    /** The three CIP-33 reference-script UTxOs that supply the heavy Plutus scripts. Each must be
      * an existing UTxO whose `scriptRef` field carries the matching script. Without these the
      * witness set inlines ~28 KB and the tx exceeds Cardano's 16 KB max. Set to `None` to fall
      * back to inlining the script in the witness set (only viable for tiny txs).
      */
    final case class ScriptRefs(
        pegIn: Option[Utxo],
        completedPegIns: Option[Utxo],
        bridgedToken: Option[Utxo]
    )

    /** The completed-peg-ins trie as the chain holds it, plus what one completion does to it.
      *
      * @param tree
      *   the INPUT trie — every earlier completion replayed. Its root must equal the on-chain
      *   `CompletedPegInsMerkleTreeDatum.root`, or the reconstruction is incomplete.
      * @param newRoot
      *   the root the completion output must carry.
      * @param insertProof
      *   the non-membership proof of `peg_in_utxo_id` in `tree`. `peg-in.ak` uses the same proof
      *   twice: as `added_peg_in_to_completed_peg_ins_inclusion_proof` and as
      *   `peg_in_in_completed_peg_ins_exclusion_proof`.
      */
    final case class CompletedPegInsUpdate(
        tree: OffChainMPF,
        newRoot: ByteString,
        insertProof: ScalusList[ProofStep]
    )

    /** Replay the completed-peg-ins trie and insert this deposit.
      *
      * The CPI trie is keyed by `peg_in_utxo_id` and VALUED by `sweeping_tm_input_0`, the same
      * value the SPI trie holds (spec §The two deposit tries). `peg-in.ak` CompletePegIn step 5
      * checks `mpf.insert(input_tree, peg_in_utxo_id, sweeping_tm_input_0, proof) == output_tree`,
      * so an entry inserted under its own key produces a root the validator rejects — and, once one
      * such completion has landed, a reconstruction that repeats the mistake never reproduces the
      * on-chain root either.
      *
      * Every value therefore comes from `spiTrie`, the reconciled swept set
      * [[SweptPegInsProofService.confirmedTrie]] built: the two tries agree on the value by
      * construction, and a prior completion the swept set does not know is reported rather than
      * guessed at.
      *
      * @param priorPegInUtxoIds
      *   the `peg_in_utxo_id` of every earlier completion, in insertion order.
      */
    def completedPegInsUpdate(
        priorPegInUtxoIds: Seq[ByteString],
        spiTrie: OffChainMPF,
        pegInUtxoId: ByteString
    ): Either[String, CompletedPegInsUpdate] = {
        def valueOf(key: ByteString): Either[String, ByteString] =
            spiTrie
                .get(key)
                .toRight(
                  s"peg-in ${key.toHex} is not in the confirmed swept set, so its " +
                      "completed-peg-ins value (the sweeping TM's input-0 outpoint) is unknown"
                )
        val replayed =
            priorPegInUtxoIds.foldLeft[Either[String, OffChainMPF]](Right(OffChainMPF.empty)) {
                (acc, key) => acc.flatMap(t => valueOf(key).map(v => t.insert(key, v)))
            }
        for {
            tree <- replayed
            value <- valueOf(pegInUtxoId)
            _ <- Either.cond(
              tree.get(pegInUtxoId).isEmpty,
              (),
              s"peg-in ${pegInUtxoId.toHex} is already in the completed-peg-ins tree — " +
                  "already completed, or its id was passed as --prior-pegin"
            )
        } yield CompletedPegInsUpdate(
          tree = tree,
          newRoot = tree.insert(pegInUtxoId, value).rootHash,
          insertProof = tree.proveNonMembership(pegInUtxoId)
        )
    }

    /** The PegInRequest NFT the spent request carries, and the quantity the completion must mint
      * for it: `−1`.
      *
      * `peg-in.ak` CompletePegIn step 0 ([CPI-8]) expects EXACTLY one token under the `peg_in`
      * policy on the spent input, reads its asset name, and then requires
      * `quantity_of(self.mint, peg_in_policy_id, pir_nft_asset_name) == -1`. A request carrying no
      * such token is not an authentic PegInRequest, so it is refused here rather than turned into a
      * transaction that fails phase 2.
      */
    def pirNftBurn(pir: Utxo, pegInPolicy: ScriptHash): Either[String, (AssetName, Long)] =
        pir.output.value.assets.assets.get(pegInPolicy).toList.flatMap(_.toList) match {
            case (name, 1L) :: Nil => Right((name, -1L))
            case Nil =>
                Left(
                  s"the PegInRequest UTxO carries no token under the peg_in policy " +
                      s"${pegInPolicy.toHex} — it is not an authentic request ([CPI-8])"
                )
            case other =>
                Left(
                  s"the PegInRequest UTxO carries ${other.size} token(s) under the peg_in policy " +
                      s"${pegInPolicy.toHex}; [CPI-8] admits exactly one"
                )
        }

    def build(
        provider: BlockchainProvider,
        sponsor: HdAccount,
        scripts: Scripts,
        scriptRefs: ScriptRefs,
        inputs: Inputs,
        datum: PegInDatum,
        recipientAddress: Address,
        recipientData: Data,
        signature: ByteString,
        // Non-membership proof of pegInUtxoId in the INPUT completed-peg-ins tree — used both as the
        // insert proof and the exclusion (miss) proof in peg-in.ak.
        completedPegInsProof: ScalusList[ProofStep],
        completedPegInsNewRoot: ByteString,
        // [CPI-9]: the sweeping TM's input-0 outpoint (36 bytes) and the MPF membership proof of
        // (peg_in_utxo_id -> sweeping_tm_input_0) against the singleton's spi_root. Both come from
        // SweptPegInsProofService ([OB-10]).
        sweepingTmInput0: ByteString,
        pegInSweptMembershipProof: ScalusList[ProofStep],
        bridgedTokenPolicy: ScriptHash,
        bridgedTokenAsset: AssetName,
        completedPegInsPolicy: ScriptHash,
        completedPegInsAsset: AssetName,
        fbtcMinAda: Long = 2_000_000L
    )(using ExecutionContext): Future[Transaction] = {
        val network = provider.cardanoInfo.network
        val signer = sponsor.signerForUtxos
        val sponsorAddress = sponsor.baseAddress(network)

        val pegInAmount = datum.pegInAmount.toLong

        // [CPI-8]: the PegInRequest NFT must be burned in this transaction.
        val (pirNftAsset, pirNftQuantity) =
            pirNftBurn(inputs.pir, scripts.pegIn.scriptHash).fold(
              err => throw new IllegalArgumentException(err),
              identity
            )
        // The peg_in mint handler's burn-only branch returns True without reading the redeemer,
        // but Aiken still decodes it as a PegInMintRedeemer — a malformed one traps. Rebuild it
        // from the request being retired: its own outpoint and its own datum.
        val pirBurnRedeemer = PegInMintRedeemer(
          inputRef = TxOutRef(TxId(inputs.pir.input.transactionId), inputs.pir.input.index),
          newPegInRequest = PegInRequest(
            expectedDatum = datum,
            blockHeader = ByteString.empty,
            blockHeaderInSourceChainInclusionProof = ScalusList.empty,
            txInBlockHeaderInclusionProof = ScalusList.empty
          )
        )

        // --- index helpers over the assembled tx (see object doc) ---
        def inputsSorted(tx: Transaction) = tx.body.value.inputs.toIndexedSeq
        def refsSorted(tx: Transaction) = tx.body.value.referenceInputs.toIndexedSeq
        def outputs(tx: Transaction) = tx.body.value.outputs

        def inputIndex(tx: Transaction, u: Utxo): BigInt =
            BigInt(inputsSorted(tx).indexOf(u.input))
        def configRefIndex(tx: Transaction): BigInt =
            BigInt(refsSorted(tx).indexOf(inputs.config.input))
        def bridgeStateRefIndex(tx: Transaction): BigInt =
            BigInt(refsSorted(tx).indexOf(inputs.bridgeState.input))
        def outputIndexWithAsset(tx: Transaction, pol: ScriptHash, an: AssetName): BigInt =
            BigInt(outputs(tx).indexWhere(_.value.value.hasAsset(pol, an)))
        def fbtcOutputIndex(tx: Transaction): BigInt =
            BigInt(
              outputs(tx).indexWhere(_.value.value.hasAsset(bridgedTokenPolicy, bridgedTokenAsset))
            )

        // Reward redeemer flat index = #scriptSpends + #mintPolicies + position in the sorted
        // withdrawals (there is a single 0-ADA withdrawal – `peg_in`).
        // `MultiAsset.assets: SortedMap[PolicyId, SortedMap[AssetName,
        // Long]]` keys at the outer level by policy, so `.assets.size` counts distinct policies –
        // matching the on-chain Mint-tag flat-list cardinality (one redeemer per policy id,
        // irrespective of how many asset names that policy mints in the same tx).
        def stake(h: ScriptHash): StakeAddress = StakeAddress(network, StakePayload.Script(h))
        def scriptSpends(tx: Transaction): Int =
            Seq(inputs.pir.input, inputs.completedPegIns.input).count(inputsSorted(tx).contains)
        def mintPolicies(tx: Transaction): Int =
            tx.body.value.mint.map(_.assets.size).getOrElse(0)
        def withdrawalPos(tx: Transaction, h: ScriptHash): Int =
            tx.body.value.withdrawals
                .map(_.withdrawals.keys.toIndexedSeq.indexWhere(_.address == stake(h)))
                .getOrElse(-1)
        def rewardRedeemerIndex(tx: Transaction, h: ScriptHash): BigInt =
            BigInt(scriptSpends(tx) + mintPolicies(tx) + withdrawalPos(tx, h))
        def pegInWithdrawRedeemerIndex(tx: Transaction): BigInt =
            rewardRedeemerIndex(tx, scripts.pegIn.scriptHash)

        // --- redeemers ---
        val pegInWithdrawRedeemer: Transaction => Data = tx => {
            val action = PegInActionType.CompletePegIn(
              recipient = recipientData,
              fbtcOutputIndex = fbtcOutputIndex(tx),
              depositorSignature = signature,
              completedPegInUtxosInputIndex = inputIndex(tx, inputs.completedPegIns),
              completedPegInUtxosOutputIndex =
                  outputIndexWithAsset(tx, completedPegInsPolicy, completedPegInsAsset),
              addedPegInToCompletedPegInsInclusionProof = completedPegInsProof,
              pegInInCompletedPegInsExclusionProof = completedPegInsProof,
              bridgeStateRefInputIndex = bridgeStateRefIndex(tx),
              sweepingTmInput0 = sweepingTmInput0,
              pegInSweptMembershipProof = pegInSweptMembershipProof
            )
            PegInWithdrawRedeemer(configRefIndex(tx), action).toData
        }

        val completedPegInsSpendRedeemer: Transaction => Data = tx =>
            CompletedPegInsSpendRedeemer(
              configRefInputIndex = configRefIndex(tx),
              pegInWithdrawRedeemerIndex = pegInWithdrawRedeemerIndex(tx)
            ).toData

        val bridgedTokenMintRedeemer: Transaction => Data = tx =>
            BridgedTokenMintRedeemer(configRefInputIndex = configRefIndex(tx)).toData

        // --- values / outputs ---
        val fbtcValue =
            Value.lovelace(fbtcMinAda) + Value.asset(
              bridgedTokenPolicy,
              bridgedTokenAsset,
              pegInAmount
            )
        // Preserve the completed-peg-ins UTxO value (NFT + ADA) and address exactly; only the datum
        // (MPF root) changes — peg-in.ak checks without_lovelace value + address are unchanged.
        val newCpiDatum = CompletedPegInsMerkleTreeDatum(completedPegInsNewRoot)

        import scalus.cardano.txbuilder.{ScriptSource, ThreeArgumentPlutusScriptWitness, TwoArgumentPlutusScriptWitness as TwoArg}

        // Reference-script wiring (CIP-33). When the bridge's ref UTxOs are configured, attach the
        // scripts via reference inputs (PlutusScriptAttached) — drops ~28 KB of inlined script
        // bytes from the witness set and keeps the tx under Cardano's 16 KB max. When a ref is
        // missing the script falls back to inlining (PlutusScriptValue) — only works for tiny txs.
        val extraRefs: Seq[Utxo] =
            Seq(scriptRefs.pegIn, scriptRefs.completedPegIns, scriptRefs.bridgedToken).flatten

        def spendSource(useRef: Boolean, script: PlutusScript): ScriptSource[PlutusScript] =
            if useRef then ScriptSource.PlutusScriptAttached
            else ScriptSource.PlutusScriptValue(script)

        // Both spent UTxOs (PIR + CPI) carry inline datums on-chain, so `DatumInlined` is correct
        // (matches what scalus's high-level `.spend(utxo, redeemer)` derives via buildDatumWitness).
        val pegInSpendWitness = ThreeArgumentPlutusScriptWitness(
          scriptSource = spendSource(scriptRefs.pegIn.isDefined, scripts.pegIn),
          redeemer = Data.unit,
          datum = scalus.cardano.txbuilder.Datum.DatumInlined
        )
        val cpiSpendWitness = ThreeArgumentPlutusScriptWitness(
          scriptSource = spendSource(scriptRefs.completedPegIns.isDefined, scripts.completedPegIns),
          redeemerBuilder = completedPegInsSpendRedeemer,
          datum = scalus.cardano.txbuilder.Datum.DatumInlined
        )
        val withdrawWitness: TwoArg = TwoArg(
          scriptSource = spendSource(scriptRefs.pegIn.isDefined, scripts.pegIn),
          redeemerBuilder = pegInWithdrawRedeemer
        )

        // Mint: the policyId-based overload uses PlutusScriptAttached; the script-based overload
        // inlines. Branch accordingly.
        val baseBuilder = (
          Seq(inputs.config, inputs.bridgeState) ++ extraRefs
        ) match {
            case head +: tail =>
                // `.references(...)` MUST come before any `.spend(..., PlutusScriptAttached)` /
                // `.mint(policyId, ..., redeemer)` — TxBuilder verifies during build that every
                // AttachedScript witness has a corresponding ref UTxO already attached. Same
                // ordering rule applies to `withdrawRewards` below (we call it after extras).
                TxBuilder(provider.cardanoInfo)
                    .references(head, tail*)
                    .spend(inputs.pir, pegInSpendWitness)
                    .spend(inputs.completedPegIns, cpiSpendWitness)
            case Seq() =>
                throw new IllegalStateException(
                  "at least the config + bridge state singleton refs must be present"
                )
        }

        val withMint =
            if scriptRefs.bridgedToken.isDefined then
                baseBuilder.mint(
                  bridgedTokenPolicy,
                  Map(bridgedTokenAsset -> pegInAmount),
                  bridgedTokenMintRedeemer
                )
            else
                baseBuilder.mint(
                  scripts.bridgedToken,
                  Map(bridgedTokenAsset -> pegInAmount),
                  bridgedTokenMintRedeemer
                )

        // [CPI-8]: burn the PegInRequest NFT under the peg_in policy. This adds a second policy to
        // the mint map, which shifts the reward redeemer's flat index — `mintPolicies` counts it
        // off the assembled tx, so the index builders stay correct.
        val withBurn =
            if scriptRefs.pegIn.isDefined then
                withMint.mint(
                  scripts.pegIn.scriptHash,
                  Map(pirNftAsset -> pirNftQuantity),
                  pirBurnRedeemer
                )
            else
                withMint.mint(
                  scripts.pegIn,
                  Map(pirNftAsset -> pirNftQuantity),
                  pirBurnRedeemer
                )

        withBurn
            .withdrawRewards(stake(scripts.pegIn.scriptHash), Coin.zero, withdrawWitness)
            .payTo(recipientAddress, fbtcValue)
            .payTo(
              inputs.completedPegIns.output.address,
              inputs.completedPegIns.output.value,
              newCpiDatum.toData
            )
            .complete(provider, sponsorAddress)
            .map(_.sign(signer).transaction)
    }
}
