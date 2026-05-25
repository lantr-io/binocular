package binocular.watchtower

import scalus.cardano.address.{Address, StakeAddress, StakePayload}
import scalus.cardano.ledger.{AssetName, Coin, PlutusScript, ScriptHash, Transaction, Utxo, Value}
import scalus.cardano.node.BlockchainProvider
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry.ProofStep
import scalus.cardano.onchain.plutus.prelude.List as ScalusList
import scalus.cardano.txbuilder.{TwoArgumentPlutusScriptWitness, TxBuilder}
import scalus.cardano.wallet.hd.HdAccount
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData

import scala.concurrent.{ExecutionContext, Future}

/** Builds the B1 peg-in completion tx: mints `peg_in_amount` fBTC to the depositor's chosen
  * recipient and records the peg-in in the completed-peg-ins MPF, satisfying
  * `peg_in.ak::withdraw(CompletePegIn)`.
  *
  * ==B1 (full wiring)==
  * Instead of re-proving the Treasury Movement inline (a block-inclusion + tx-merkle proof and a
  * `legit_TM_verifier` withdraw), the completion *references* the **Confirmed TM UTxO** — the one
  * `confirm-tmtx` produced at the binocular [[TreasuryMovementValidator]] address, authenticated by
  * the TM NFT. `peg_in.ak` finds it by NFT among the reference inputs and reads `btc_txid` +
  * `swept_peg_in_utxo_ids` from its `Confirmed` datum, requiring this peg-in ∈ swept. The depositor
  * BIP340 Schnorr auth + recipient-binding are now enforced inside `peg_in.ak` itself, so the only
  * rewarding script is `peg_in` (the stake-validator delegation pattern).
  *
  * ==Shape==
  *   - Spend: the PegInRequest UTxO (peg_in script — its `spend` handler only requires a withdrawal
  *     from the same script hash; redeemer ignored) and the completed-peg-ins MPF UTxO.
  *   - References: the config-NFT UTxO (`ConfigDatum`) and the Confirmed TM UTxO (TM NFT).
  *   - Mint: `+peg_in_amount` fBTC (`bridged_token` policy). The PegInRequest NFT is NOT burned —
  *     `CompletePegIn` does not check it (only `Cancel` burns); it rides out in the change.
  *   - Withdrawal (0 ADA): `peg_in` → [[PegInWithdrawRedeemer]] `CompletePegIn`.
  *   - Outputs: fBTC → recipient; the updated completed-peg-ins UTxO (same value+address, new MPF
  *     root); change → sponsor (carrying the PegInRequest NFT).
  *
  * ==Index redeemers==
  * Several redeemer fields are indices into the *assembled* tx and are filled by delayed
  * `Transaction => Data` builders. The on-chain `self.redeemers` list is ordered by Scalus's
  * `(RedeemerTag ordinal, index)` — all Spend(0), then Mint(1), then Reward(3) — so the lone peg_in
  * reward redeemer's position is `#scriptSpends + #mintPolicies + 0`. Inputs/reference-inputs are
  * ordered by `(txid, index)`; outputs keep insertion order.
  */
object PegInCompleteTx {

    /** The three Plutus scripts that run in the completion tx. */
    final case class Scripts(
        pegIn: PlutusScript,
        completedPegIns: PlutusScript,
        bridgedToken: PlutusScript
    )

    /** The four pre-existing UTxOs the tx spends/references. `confirmedTm` is the Confirmed TM UTxO
      * (referenced, never spent) — `peg-in.ak` finds it by its TM NFT.
      */
    final case class Inputs(
        pir: Utxo,
        completedPegIns: Utxo,
        config: Utxo,
        confirmedTm: Utxo
    )

    def build(
        provider: BlockchainProvider,
        sponsor: HdAccount,
        scripts: Scripts,
        inputs: Inputs,
        datum: PegInDatum,
        recipientAddress: Address,
        recipientData: Data,
        signature: ByteString,
        // Non-membership proof of pegInUtxoId in the INPUT completed-peg-ins tree — used both as the
        // insert proof and the exclusion (miss) proof in peg-in.ak.
        completedPegInsProof: ScalusList[ProofStep],
        completedPegInsNewRoot: ByteString,
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

        // --- index helpers over the assembled tx (see object doc) ---
        def inputsSorted(tx: Transaction) = tx.body.value.inputs.toIndexedSeq
        def refsSorted(tx: Transaction) = tx.body.value.referenceInputs.toIndexedSeq
        def outputs(tx: Transaction) = tx.body.value.outputs

        def inputIndex(tx: Transaction, u: Utxo): BigInt =
            BigInt(inputsSorted(tx).indexOf(u.input))
        def configRefIndex(tx: Transaction): BigInt =
            BigInt(refsSorted(tx).indexOf(inputs.config.input))
        def outputIndexWithAsset(tx: Transaction, pol: ScriptHash, an: AssetName): BigInt =
            BigInt(outputs(tx).indexWhere(_.value.value.hasAsset(pol, an)))
        def fbtcOutputIndex(tx: Transaction): BigInt =
            BigInt(
              outputs(tx).indexWhere(_.value.value.hasAsset(bridgedTokenPolicy, bridgedTokenAsset))
            )

        // The lone peg_in reward redeemer's position in the on-chain `self.redeemers` list.
        def pegInWithdrawRedeemerIndex(tx: Transaction): BigInt = {
            val scriptSpends =
                Seq(inputs.pir.input, inputs.completedPegIns.input).count(inputsSorted(tx).contains)
            val mintPolicies = tx.body.value.mint.map(_.assets.size).getOrElse(0)
            BigInt(scriptSpends + mintPolicies) // + withdrawalIndex 0 (peg_in is the only withdrawal)
        }

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
              pegInInCompletedPegInsExclusionProof = completedPegInsProof
            )
            PegInWithdrawRedeemer(configRefIndex(tx), action).toData
        }

        val completedPegInsSpendRedeemer: Transaction => Data = tx =>
            CompletedPegInsSpendRedeemer(
              configRefInputIndex = configRefIndex(tx),
              pegInWithdrawRedeemerIndex = pegInWithdrawRedeemerIndex(tx)
            ).toData

        val bridgedTokenMintRedeemer: Transaction => Data = tx =>
            BridgedTokenMintRedeemer(
              configRefInputIndex = configRefIndex(tx),
              wantedPegWithdrawRedeemerIndex = pegInWithdrawRedeemerIndex(tx)
            ).toData

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

        def stake(h: ScriptHash): StakeAddress = StakeAddress(network, StakePayload.Script(h))
        import TwoArgumentPlutusScriptWitness.attached

        TxBuilder(provider.cardanoInfo)
            .spend(inputs.pir, Data.unit, scripts.pegIn)
            .spend(inputs.completedPegIns, completedPegInsSpendRedeemer, scripts.completedPegIns)
            .references(inputs.config, inputs.confirmedTm)
            .mint(
              scripts.bridgedToken,
              Map(bridgedTokenAsset -> pegInAmount),
              bridgedTokenMintRedeemer
            )
            .withdrawRewards(
              stake(scripts.pegIn.scriptHash),
              Coin.zero,
              attached(scripts.pegIn, pegInWithdrawRedeemer)
            )
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
