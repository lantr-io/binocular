package binocular.watchtower

import scalus.uplc.builtin.*
import scalus.uplc.builtin.Data.{FromData, ToData}
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry.ProofStep
import scalus.cardano.onchain.plutus.prelude.List as ScalusList

// Scalus mirrors of ft-bifrost-bridge's Aiken types in onchain/lib/bifrost/types/peg-out.ak.
// Variant order and field positions match Aiken — Plutus Constr tags are positional, so reordering
// would silently break wire compatibility. `AuthorizationMethod` is shared with the peg-in side
// (see PegInTypes.scala).

/** `bifrost/types/peg-out.PegOutDatum` (rev 5.1, 4 fields).
  *
  * The UTxO at `peg_out.ak` locks fBTC + MIN_ADA. A confirmed Treasury Movement pays
  * `sourceChainDestinationAddress` (a raw Bitcoin scriptPubKey) `locked − perPegoutFee` satoshi and
  * records the request in the completed-peg-outs trie; Complete then burns the locked fBTC against
  * a membership proof.
  *
  *   - `ownerAuth` gates Cancel ONLY. Complete is permissionless since rev 5.1 — it can only burn
  *     the exact locked amount against a value-bound attested payment, so authorization adds
  *     nothing.
  *   - `perPegoutFee` is pinned at lock time from Config field 13. Complete binds against THIS
  *     field, never a current on-chain value, so a later fee Update cannot strand a request.
  *   - `created` is POSIX ms, set by the requester; it gates Cancel at `created + 30 d`.
  *
  * The old `sourceChainTreasuryUtxoId` field is GONE: which TM pays a request is decided by the SPO
  * batcher at build time, not pinned at request time.
  */
case class PegOutDatum(
    ownerAuth: AuthorizationMethod,
    sourceChainDestinationAddress: ByteString,
    perPegoutFee: BigInt,
    created: BigInt
) derives FromData,
      ToData

/** `bifrost/types/peg-out.PegOutActionType`.
  *
  * Variant order: `CompletePegOut` = constr 0, `Cancel` = constr 1 — the declaration order in
  * `peg-out.ak`. Both carry a bare MPF proof and nothing else; the Binocular SPV bundles of the
  * pre-rev-5 design are gone, because the TM is proven once at Confirm rather than re-proven per
  * completion.
  */
enum PegOutActionType derives FromData, ToData {

    /** Prove the trie maps this POR id to `dest_spk ++ amount_le8` and burn all the locked fBTC. */
    case CompletePegOut(membershipProof: ScalusList[ProofStep])

    /** After the timeout, prove this POR id is NOT in the trie and reclaim the fBTC. */
    case Cancel(exclusionProof: ScalusList[ProofStep])
}

/** `bifrost/types/peg-out.PegOutWithdrawRedeemer`.
  *
  * Both indices point into `reference_inputs` (NOT `inputs`): the Config UTxO and the
  * completed-peg-outs singleton are both referenced by the withdraw handler, never spent. The trie
  * being a reference input is what makes completion transactions independent — any number of them
  * can sit in the same block without contending for one UTxO.
  */
case class PegOutWithdrawRedeemer(
    configRefInputIndex: BigInt,
    completedPegOutsRefInputIndex: BigInt,
    actionType: PegOutActionType
) derives FromData,
      ToData
