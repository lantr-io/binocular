package binocular.watchtower

import scalus.uplc.builtin.*
import scalus.uplc.builtin.Data.{FromData, ToData}
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry.ProofStep
import scalus.cardano.onchain.plutus.prelude.List as ScalusList
import scalus.cardano.onchain.plutus.v3.TxOutRef

// Scalus mirrors of ft-bifrost-bridge's Aiken types in
// onchain/lib/bifrost/types/{general,peg-in}.ak. Variant order and field
// positions match Aiken — Plutus Constr tags are positional, so reordering
// would silently break wire compatibility.

enum AuthorizationMethod derives FromData, ToData {
    case CardanoSignature(hash: ByteString)
    case CardanoSpendScript(hash: ByteString)
    case CardanoWithdrawScript(hash: ByteString)
    case CardanoMintScript(hash: ByteString)
    case CardanoTokenOwnership(policyId: ByteString, assetName: ByteString)
}

case class PegInDatum(
    ownerAuth: AuthorizationMethod,
    sourceChainPegInRawTx: ByteString,
    sourceChainPegInRawTxIndex: BigInt,
    pegInUtxoId: ByteString,
    sourceChainTreasuryUtxoId: ByteString,
    pegInAmount: BigInt,
    userSourceChainPubKey: ByteString
) derives FromData,
      ToData

// Domain-separation tag for the depositor BIP340 Schnorr message that `peg_in.ak` verifies on-chain
// at completion (the depositor auth is embedded in peg_in.ak since B1 — there is no separate
// withdraw validator). MUST equal the `mint_tag` constant in peg-in.ak:
//   completion: sha2_256(mintTag ‖ btc_txid ‖ peg_in_utxo_id ‖ recipient)
object BifrostMessages {
    // "BFR-mint-v1"
    val mintTag: ByteString = ByteString.fromHex("4246522d6d696e742d7631")
    // The depositor signs this ASCII text via BIP-322 (`signMessage(text, "bip322-simple")`).
    // peg_in.ak rebuilds it as  mint_tag ++ ":" ++ hex(binding_digest)  and verifies the BIP-322
    // signature against the depositor wallet's Taproot output key (`user_source_chain_pub_key`).
    val mintTextPrefix: String = "BFR-mint-v1:"
}

// Aiken `bifrost/types/peg-in.{PegInRequest, PegInMintRedeemer}`. Field order
// is positional in the Plutus Constr — keep it identical to the .ak file.
case class PegInRequest(
    expectedDatum: PegInDatum,
    blockHeader: ByteString,
    blockHeaderInSourceChainInclusionProof: ScalusList[ProofStep],
    txInBlockHeaderInclusionProof: ScalusList[ByteString]
) derives FromData,
      ToData

case class PegInMintRedeemer(
    inputRef: TxOutRef,
    newPegInRequest: PegInRequest
) derives FromData,
      ToData

// Aiken `bifrost/types/peg-in.{ActionType, PegInWithdrawRedeemer}` — the `withdraw(CompletePegIn)`
// path of peg_in.ak. Field order is positional in the Plutus Constr; keep identical to the .ak
// records, and keep `ActionType` variant order (Cancel = constr 0, CompletePegIn = constr 1) so the
// wire tags line up.
//
// B1 (full wiring): CompletePegIn no longer carries the TM inline proof. It references the Confirmed
// TM UTxO (authenticated by the TM NFT, supplied as a reference input — peg-in.ak finds it by NFT)
// and reads `btc_txid` + `swept_peg_in_utxo_ids` from its datum, and it embeds the depositor's
// BIP340 Schnorr auth + recipient-binding directly:
//   - recipient                     : the depositor's chosen Cardano address (as Data, the Plutus
//     Address form) — bound into both the signed message and the fBTC output.
//   - fbtcOutputIndex               : output paying `peg_in_amount` fBTC to `recipient`.
//   - depositorSignature            : BIP340 over sha2_256("BFR-mint-v1"‖btc_txid‖peg_in_utxo_id‖recipient).
//   - completedPegInUtxosInputIndex : position of the completed-peg-ins UTxO in sorted inputs.
//   - completedPegInUtxosOutputIndex: position of the updated completed-peg-ins output.
//   - addedPegInToCompletedPegInsInclusionProof / pegInInCompletedPegInsExclusionProof : MPF proofs.
//   - configRefInputIndex (on PegInWithdrawRedeemer) : position of the config-NFT UTxO in sorted
//     reference inputs.
enum PegInActionType derives FromData, ToData {
    case Cancel(burntPegInNftAssetName: ByteString)
    case CompletePegIn(
        recipient: Data,
        fbtcOutputIndex: BigInt,
        depositorSignature: ByteString,
        completedPegInUtxosInputIndex: BigInt,
        completedPegInUtxosOutputIndex: BigInt,
        addedPegInToCompletedPegInsInclusionProof: ScalusList[ProofStep],
        pegInInCompletedPegInsExclusionProof: ScalusList[ProofStep]
    )
}

case class PegInWithdrawRedeemer(
    configRefInputIndex: BigInt,
    actionType: PegInActionType
) derives FromData,
      ToData
