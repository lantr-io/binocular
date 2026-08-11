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

/** Aiken `bifrost/types/peg-in.PegInDatum`, rev 5.4.
  *
  * `source_chain_treasury_utxo_id` is GONE: it pinned a treasury outpoint for a scheme rev 5.1
  * superseded, and nothing reads it any more. `created` is APPENDED ([CLR-7]): the mint handler
  * pins it to the mint transaction's validity upper bound, and the never-swept Close counts its
  * grace period from it ([CLR-5]).
  *
  * Field positions are consensus-visible. Against a rev-5.4 deployment `peg_in_amount` sits at
  * position 4 as a `Data.I`, where the rev-5.1 shape had a `Data.B` — a mirror with the old shape
  * cannot decode any real PegInRequest at all.
  */
case class PegInDatum(
    ownerAuth: AuthorizationMethod,
    sourceChainPegInRawTx: ByteString,
    sourceChainPegInRawTxIndex: BigInt,
    pegInUtxoId: ByteString,
    pegInAmount: BigInt,
    userSourceChainPubKey: ByteString,
    created: BigInt
) derives FromData,
      ToData

// Domain-separation tag for the depositor message that `peg_in.ak` verifies on-chain at completion
// (the depositor auth is embedded in peg_in.ak since B1 — there is no separate withdraw validator).
// MUST equal the `mint_tag` constant in peg-in.ak:
//   completion: sha2_256(mintTag ‖ peg_in_utxo_id ‖ serialiseData(recipient))   spec [CPI-3]
//
// Rev 5.4 REVISED [CPI-3]: `btc_txid` is gone from the preimage. There is no `Confirmed` TM record
// to read it from any more, and no reader can derive the sweeping txid from the SPI trie value (the
// sweeping TM's input-0 outpoint). Nothing is lost: [CPI-9] proves the sweep against the bridge
// state singleton's `spi_root`, non-replayability comes from `peg_in_utxo_id`, and the fBTC output
// is bound to `recipient`.
object BifrostMessages {
    // "BFR-mint-v1"
    val mintTag: ByteString = ByteString.fromHex("4246522d6d696e742d7631")
    // The depositor signs this ASCII text via BIP-322 (`signMessage(text, "bip322-simple")`).
    // peg_in.ak rebuilds it as  mint_tag ++ ":" ++ hex(binding_digest)  and verifies the BIP-322
    // signature against the depositor wallet's Taproot output key (`user_source_chain_pub_key`).
    val mintTextPrefix: String = "BFR-mint-v1:"

    /** The [CPI-3] binding digest:
      * `sha2_256(mint_tag ‖ peg_in_utxo_id ‖ serialiseData(recipient))`.
      *
      * The single place this preimage is built off-chain, so `pegin-complete` and `sign-pegin-msg`
      * cannot drift apart ([OB-11]).
      *
      * @param pegInUtxoId
      *   the deposit outpoint, 36 bytes (`btc_txid` ‖ vout LE), as the `PegInDatum` carries it.
      * @param recipient
      *   the depositor's chosen fBTC destination, in the Plutus `Address` form the redeemer
      *   carries.
      */
    def completionDigest(pegInUtxoId: ByteString, recipient: Data): ByteString =
        Builtins.sha2_256(
          Builtins.appendByteString(
            mintTag,
            Builtins.appendByteString(pegInUtxoId, Builtins.serialiseData(recipient))
          )
        )

    /** The ASCII text the depositor BIP-322-signs: `"BFR-mint-v1:" ++ hex_lower(digest)`.
      *
      * Takes the digest rather than its inputs, so `sign-pegin-msg` — which is handed the digest
      * `pegin-complete --dry-run` printed, and never sees the deposit — signs the same text.
      */
    def completionSignText(digest: ByteString): String = mintTextPrefix + digest.toHex
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

// Aiken `bifrost/types/peg-in.{ClosePegInProof, ActionType, PegInWithdrawRedeemer}` — the
// `withdraw` path of peg_in.ak. Field order is positional in the Plutus Constr; keep identical to
// the .ak records, and keep `ActionType` variant order (Close = constr 0, CompletePegIn = constr 1)
// so the wire tags line up.
//
// Rev 5.4: CompletePegIn no longer references a `Confirmed` TM record — there is none ([OB-5]). It
// proves the sweep against the bridge state singleton, referenced by index and authenticated by the
// NFT (bridge_state_policy, "BSS") ([CPI-10]), and it embeds the depositor's BIP-322 auth +
// recipient-binding directly:
//   - recipient                     : the depositor's chosen Cardano address (as Data, the Plutus
//     Address form) — bound into both the signed message and the fBTC output.
//   - fbtcOutputIndex               : output paying `peg_in_amount` fBTC to `recipient`.
//   - depositorSignature            : BIP-322 over sha2_256("BFR-mint-v1"‖peg_in_utxo_id‖recipient).
//   - completedPegInUtxosInputIndex : position of the completed-peg-ins UTxO in sorted inputs.
//   - completedPegInUtxosOutputIndex: position of the updated completed-peg-ins output.
//   - addedPegInToCompletedPegInsInclusionProof / pegInInCompletedPegInsExclusionProof : MPF proofs.
//   - bridgeStateRefInputIndex      : position of the singleton UTxO in sorted reference inputs.
//   - sweepingTmInput0              : the sweeping TM's input-0 outpoint, 36 bytes — the SPI trie
//     value for `peg_in_utxo_id`, and the value the CPI trie insert records.
//   - pegInSweptMembershipProof     : MPF membership of (peg_in_utxo_id -> sweepingTmInput0) in the
//     singleton's `spi_root` ([CPI-9]).
//   - configRefInputIndex (on PegInWithdrawRedeemer) : position of the config-NFT UTxO in sorted
//     reference inputs.
// Aiken `bifrost/types/peg-in.ClosePegInProof` — the two mutually exclusive Close branches
// (spec §Close PegInRequest). NeverSwept = constr 0 ([CLR-5]/[CLR-6]), Duplicate = constr 1
// ([CLR-8]).
enum ClosePegInProof derives FromData, ToData {
    case NeverSwept(
        bridgeStateRefInputIndex: BigInt,
        spiExclusionProof: ScalusList[ProofStep]
    )
    case Duplicate(
        completedPegInsRefInputIndex: BigInt,
        sweepingTmInput0: ByteString,
        cpiMembershipProof: ScalusList[ProofStep]
    )
}

enum PegInActionType derives FromData, ToData {
    case Close(
        burntPegInNftAssetName: ByteString,
        proof: ClosePegInProof
    )
    case CompletePegIn(
        recipient: Data,
        fbtcOutputIndex: BigInt,
        depositorSignature: ByteString,
        completedPegInUtxosInputIndex: BigInt,
        completedPegInUtxosOutputIndex: BigInt,
        addedPegInToCompletedPegInsInclusionProof: ScalusList[ProofStep],
        pegInInCompletedPegInsExclusionProof: ScalusList[ProofStep],
        bridgeStateRefInputIndex: BigInt,
        sweepingTmInput0: ByteString,
        pegInSweptMembershipProof: ScalusList[ProofStep]
    )
}

case class PegInWithdrawRedeemer(
    configRefInputIndex: BigInt,
    actionType: PegInActionType
) derives FromData,
      ToData
