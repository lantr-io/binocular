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

// Parameters for PegInDepositorAuthValidator: the fBTC (bridged_token) asset the
// completion mints. They let the auth validator require that the minted fBTC is paid
// to the recipient the depositor signed for (recipient-binding).
case class PegInDepositorAuthParams(
    bridgedTokenPolicyId: ByteString,
    bridgedTokenAssetName: ByteString
) derives FromData,
      ToData

// Redeemer for PegInDepositorAuthValidator. `recipient` is the depositor's chosen
// Cardano address (as Data) — both the signed message and the fBTC output are bound to
// it. `treasuryMovementBtcTxid` is the confirmed TM txid the mint references. Together
// these implement the per-mint signing message of technical_documentation.md
// §"Complete peg-in": sig over "BFR-mint-v1" ‖ btc_txid ‖ peg_in_utxo_id ‖ recipient.
case class PegInDepositorAuthRedeemer(
    pegInInputIndex: BigInt,
    fbtcOutputIndex: BigInt,
    recipient: Data,
    treasuryMovementBtcTxid: ByteString,
    signature: ByteString
) derives FromData,
      ToData

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
