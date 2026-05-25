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

/** Builds the F4 peg-in completion tx: mints `peg_in_amount` fBTC to the depositor's chosen
  * recipient and records the peg-in in the completed-peg-ins MPF, satisfying
  * `peg_in.ak::withdraw(CompletePegIn)` and the three rewarding scripts it delegates to.
  *
  * ==Shape==
  *   - Spend: the PegInRequest UTxO (peg_in script — its `spend` handler only requires a withdrawal
  *     from the same script hash; redeemer ignored) and the completed-peg-ins MPF UTxO (its `spend`
  *     handler checks the peg_in withdraw redeemer is a CompletePegIn).
  *   - References: the oracle UTxO (`confirmed_blocks_root`) and the config-NFT UTxO
  *     (`ConfigDatum`).
  *   - Mint: `+peg_in_amount` fBTC (`bridged_token` policy). The PegInRequest NFT is NOT burned —
  *     `CompletePegIn` does not check it (only `Cancel` burns); it rides out in the change.
  *   - Withdrawals (all 0 ADA, the stake-validator delegation pattern):
  *     - `peg_in` → [[PegInWithdrawRedeemer]] `CompletePegIn`,
  *     - `owner_auth` ([[PegInDepositorAuthValidator]]) → [[PegInDepositorAuthRedeemer]] (the
  *       depositor's recipient-bound Schnorr signature),
  *     - `legit_TM_verifier` ([[PegInVerifierValidator]]) → the TM-verification Data list.
  *   - Outputs: fBTC → recipient; the updated completed-peg-ins UTxO (same value+address, new MPF
  *     root); change → sponsor (carrying the PegInRequest NFT).
  *
  * ==Index redeemers==
  * Several redeemer fields are indices into the *assembled* tx and are filled by delayed
  * `Transaction => Data` builders. The on-chain `self.redeemers` list is ordered by Scalus's
  * `(RedeemerTag ordinal, index)` — all Spend(0), then Mint(1), then Reward(3) — so a reward
  * redeemer's position is `#scriptSpends + #mintPolicies + withdrawalIndex`, where
  * `withdrawalIndex` follows the 28-byte credential-hash ordering the ledger uses.
  * Inputs/reference-inputs are ordered by `(txid, index)`; outputs keep insertion order. See
  * [[binocular.watchtower]] notes / f4-blockers.
  */
object PegInCompleteTx {

    /** The five Plutus scripts that run in the completion tx. */
    final case class Scripts(
        pegIn: PlutusScript,
        completedPegIns: PlutusScript,
        bridgedToken: PlutusScript,
        ownerAuth: PlutusScript,
        verifier: PlutusScript
    )

    /** The four pre-existing UTxOs the tx spends/references. */
    final case class Inputs(
        pir: Utxo,
        completedPegIns: Utxo,
        oracle: Utxo,
        config: Utxo
    )

    /** Confirmed-TM Bitcoin proof material (block header MPF membership + tx-in-block Merkle path).
      * `rawTmTxFull` is the FULL segwit serialization — peg-in.ak strips it on-chain for the txid.
      */
    final case class TmProof(
        blockHeader: ByteString,
        blockHeaderInSourceChainInclusionProof: ScalusList[ProofStep],
        rawTmTxFull: ByteString,
        txIndex: BigInt,
        txInBlockMerklePath: ScalusList[ByteString]
    )

    def build(
        provider: BlockchainProvider,
        sponsor: HdAccount,
        scripts: Scripts,
        inputs: Inputs,
        datum: PegInDatum,
        recipientAddress: Address,
        recipientData: Data,
        tmProof: TmProof,
        // Non-membership proof of pegInUtxoId in the INPUT completed-peg-ins tree — used both as the
        // insert proof and the exclusion (miss) proof in peg-in.ak.
        completedPegInsProof: ScalusList[ProofStep],
        completedPegInsNewRoot: ByteString,
        treasuryMovementBtcTxid: ByteString,
        signature: ByteString,
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

        // Reward redeemer's position in the on-chain `self.redeemers` list.
        def withdrawalIndex(tx: Transaction, target: ScriptHash): BigInt = {
            val ordered = tx.body.value.withdrawals.toList
                .flatMap(_.withdrawals.keys.toList)
                .flatMap(ra => ra.address.scriptHashOption.map(h => ByteString.fromArray(h.bytes)))
                .sortBy(_.toHex)
            BigInt(ordered.indexOf(ByteString.fromArray(target.bytes)))
        }
        def rewardBase(tx: Transaction): BigInt = {
            val scriptSpends =
                Seq(inputs.pir.input, inputs.completedPegIns.input).count(inputsSorted(tx).contains)
            val mintPolicies = tx.body.value.mint.map(_.assets.size).getOrElse(0)
            BigInt(scriptSpends + mintPolicies)
        }
        def pegInWithdrawRedeemerIndex(tx: Transaction): BigInt =
            rewardBase(tx) + withdrawalIndex(tx, scripts.pegIn.scriptHash)
        def tmWithdrawRedeemerIndex(tx: Transaction): BigInt =
            rewardBase(tx) + withdrawalIndex(tx, scripts.verifier.scriptHash)

        // --- redeemers ---
        val pegInWithdrawRedeemer: Transaction => Data = tx => {
            val info = InputCompletePegIn(
              blockHeader = tmProof.blockHeader,
              blockHeaderInSourceChainInclusionProof =
                  tmProof.blockHeaderInSourceChainInclusionProof,
              treasuryMovementRawTx = tmProof.rawTmTxFull,
              treasuryMovementTxIndex = tmProof.txIndex,
              treasuryMovementTxInclusionProof = tmProof.txInBlockMerklePath,
              pegInInCompletedPegInsExclusionProof = completedPegInsProof
            )
            val action = PegInActionType.CompletePegIn(
              completePegInInfo = info,
              completedPegInUtxosInputIndex = inputIndex(tx, inputs.completedPegIns),
              completedPegInUtxosOutputIndex =
                  outputIndexWithAsset(tx, completedPegInsPolicy, completedPegInsAsset),
              addedPegInToCompletedPegInsInclusionProof = completedPegInsProof,
              tmtilaspisvshWithdrawRedeemerIndex = tmWithdrawRedeemerIndex(tx)
            )
            PegInWithdrawRedeemer(configRefIndex(tx), action).toData
        }

        val ownerAuthRedeemer: Transaction => Data = tx =>
            PegInDepositorAuthRedeemer(
              fbtcOutputIndex = fbtcOutputIndex(tx),
              recipient = recipientData,
              treasuryMovementBtcTxid = treasuryMovementBtcTxid,
              signature = signature
            ).toData

        // legit_TM_verifier: peg-in.ak destructures [treasury_utxo_id, raw_peg_in_tx, peg_in_amount,
        // treasury_movement_raw_tx, peg_in_utxo_id, ..]; the verifier itself reads 0,1,3.
        val verifierRedeemer: Data = Data.List(
          ScalusList(
            Data.B(datum.sourceChainTreasuryUtxoId),
            Data.B(datum.sourceChainPegInRawTx),
            Data.I(datum.pegInAmount),
            Data.B(tmProof.rawTmTxFull),
            Data.B(datum.pegInUtxoId)
          )
        )

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
            .references(inputs.oracle, inputs.config)
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
            .withdrawRewards(
              stake(scripts.ownerAuth.scriptHash),
              Coin.zero,
              attached(scripts.ownerAuth, ownerAuthRedeemer)
            )
            .withdrawRewards(
              stake(scripts.verifier.scriptHash),
              Coin.zero,
              attached(scripts.verifier, verifierRedeemer)
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
