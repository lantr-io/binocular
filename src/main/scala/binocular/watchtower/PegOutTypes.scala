package binocular.watchtower

import scalus.uplc.builtin.*
import scalus.uplc.builtin.Data.{FromData, ToData}
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry.ProofStep
import scalus.cardano.onchain.plutus.prelude.List as ScalusList

// Scalus mirrors of ft-bifrost-bridge's Aiken types in onchain/lib/bifrost/types/peg-out.ak and the
// completed-peg-outs-merkle-tree.ak datum/redeemers. Variant order and field positions match Aiken —
// Plutus Constr tags are positional, so reordering would silently break wire compatibility.
// `AuthorizationMethod` is shared with the peg-in side (see PegInTypes.scala).

/** `bifrost/types/peg-out.PegOutDatum`. The UTxO at `peg_out.ak` locks fBTC + MIN_ADA; the SPO
  * group pays `sourceChainDestinationAddress` (a raw Bitcoin scriptPubKey) the locked amount in the
  * next Treasury Movement that spends `sourceChainTreasuryUtxoId`.
  */
case class PegOutDatum(
    ownerAuth: AuthorizationMethod,
    sourceChainDestinationAddress: ByteString,
    sourceChainTreasuryUtxoId: ByteString
) derives FromData,
      ToData

/** `bifrost/types/peg-out.InputCancel` — the refund branch's proof bundle (Cancel; out of scope
  * this iteration but required so the [[PegOutActionType]] enum stays decodable).
  */
case class InputCancel(
    blockHeader: ByteString,
    blockHeaderInSourceChainInclusionProof: ScalusList[ProofStep],
    treasuryMovementRawTx: ByteString,
    treasuryMovementTxIndex: BigInt,
    treasuryMovementTxInclusionProof: ScalusList[ByteString]
) derives FromData,
      ToData

/** `bifrost/types/peg-out.InputCompletePegOut` — the happy-path proof bundle: the TM raw tx + its
  * Binocular block-inclusion + tx-merkle proof, the peg-out UTxO id, and the MPF exclusion proof
  * that it isn't already in the completed-peg-outs tree.
  */
case class InputCompletePegOut(
    blockHeader: ByteString,
    blockHeaderInSourceChainInclusionProof: ScalusList[ProofStep],
    treasuryMovementRawTx: ByteString,
    treasuryMovementTxIndex: BigInt,
    treasuryMovementTxInclusionProof: ScalusList[ByteString],
    pegOutUtxoId: ByteString,
    pegOutInCompletedPegOutsExclusionProof: ScalusList[ProofStep]
) derives FromData,
      ToData

// `bifrost/types/peg-out.PegOutActionType`. Variant order: Cancel = constr 0, CompletePegOut =
// constr 1 — keep so the wire tags line up with peg-out.ak.
enum PegOutActionType derives FromData, ToData {
    case Cancel(
        cancelInfo: InputCancel,
        tmtilasponpvshWithdrawRedeemerIndex: BigInt
    )
    case CompletePegOut(
        pegOutInfo: InputCompletePegOut,
        completedPegOutsInputIndex: BigInt,
        completedPegOutsOutputIndex: BigInt,
        addedPegOutToCompletedPegOutsInclusionProof: ScalusList[ProofStep],
        tmtilaspopvshWithdrawRedeemerIndex: BigInt
    )
}

case class PegOutWithdrawRedeemer(
    configRefInputIndex: BigInt,
    actionType: PegOutActionType
) derives FromData,
      ToData

// --- completed-peg-outs-merkle-tree.ak datum + redeemers ---

/** `CompletedPegOutsMerkleTreeDatum` — the MPF root of completed peg-outs; minted with the empty
  * root (32 zero bytes), then spent+recreated with each peg-out inserted on completion.
  */
case class CompletedPegOutsMerkleTreeDatum(
    root: ByteString
) derives FromData,
      ToData

/** Spend redeemer for `completed-peg-outs-merkle-tree.ak::SpendRedeemer`. The spend handler reads
  * config[5] = peg_out_withdraw_script_hash and requires the peg_out withdraw redeemer at
  * `pegOutWithdrawRedeemerIndex` to be a `Withdraw(Script(peg_out_withdraw_hash))` carrying a
  * CompletePegOut action, plus a withdrawal from that script. Both indices are computed from the
  * assembled tx.
  */
case class CompletedPegOutsSpendRedeemer(
    configRefInputIndex: BigInt,
    pegOutWithdrawRedeemerIndex: BigInt
) derives FromData,
      ToData
