package binocular.watchtower

import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.{AssetName, TransactionInput, Utxo, Value}
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData

/** Shared pieces of the bridge bootstrap transactions.
  *
  * `deploy-bridge` creates the whole bridge in one tx; `bootstrap-bridge-state` re-mints only the
  * bridge-state singleton against a LIVE bridge (a §Recovery replacement, or a first bootstrap the
  * deploy did not perform). Both must produce the same singleton output SHAPE, so it lives here
  * once rather than in each command.
  */
object BridgeBootstrap {

    /** The genesis MPF root: 32 zero bytes. A first deployment's singleton starts with both roots
      * empty (spec §Why the bootstrap datum is not pinned); the CPI merkle-tree validator
      * additionally pins this literal in its mint handler.
      */
    val EmptyRoot: ByteString = ByteString.fromArray(Array.fill[Byte](32)(0))

    /** Lovelace put on each bootstrapped protocol UTxO. Comfortably above the min-UTxO for a
      * one-asset output with a small inline datum.
      */
    val BootstrapLovelace: Long = 2_000_000L

    /** Minimum lovelace a wallet UTxO must hold to be usable as a one-shot. */
    val MinOneShotLovelace: Long = 5_000_000L

    /** The bridge-state singleton output a bootstrap mint must create.
      *
      * `bridge-state.ak::mint` ([BSS-5]) requires an output at the policy's own script address
      * holding the single `"BSS"` token. The [[BridgeState]] datum is deliberately NOT pinned
      * on-chain — it is operator-supplied and observer-verified, because the same mint path serves
      * the first deployment (zero roots + the deployment anchor) and the §Recovery replacement
      * (current roots + the live tip).
      */
    def bridgeStateOutput(
        contract: BridgeStateContract,
        network: Network,
        state: BridgeState
    ): (Address, Value, Data) = {
        val asset = AssetName(BridgeStateContract.assetName)
        val value = Value.lovelace(BootstrapLovelace) +
            Value.asset(contract.policyId, asset, 1L)
        (contract.address(network), value, state.toData)
    }

    /** The Treasury state output a genesis mint must create ([TSY-5] – [TSY-8]).
      *
      * `treasury.ak::mint` requires exactly one output at the policy's own script address, holding
      * the single `"BFRTRY"` token and NOTHING else non-ADA, with a `TreasuryDatum` whose two
      * fields are both 32 bytes. The address must carry NO stake credential ([TSY-5]): a reader
      * that pins the bare script address could not find a UTxO that drifted to a delegated one.
      *
      * `currentSposFrostKey` is $Y_{federation}$ at genesis — the federation is the key-path signer
      * until the first DKG publishes $Y_{51}$, so Phase-1 address derivation, signing and
      * governance work with no special cases (spec §Bridge instance creation flow, step 7).
      * `bifrostIdentityRoot` is the empty root for a fresh deployment; [PRE-2] lets a replacement
      * deployment seed a non-empty one to carry a registered roster forward.
      */
    def treasuryStateOutput(
        contract: TreasuryInfoContract,
        network: Network,
        state: TreasuryInfoDatum
    ): (Address, Value, Data) = {
        // [TSY-8] is enforced on chain, so a short field aborts the mint and burns the deploy's
        // fee. Checking here keeps a malformed genesis off the chain entirely.
        require(
          state.bifrostIdentityRoot.bytes.length == 32,
          s"bifrost_identity_root must be 32 bytes, got ${state.bifrostIdentityRoot.bytes.length}"
        )
        require(
          state.currentSposFrostKey.bytes.length == 32,
          s"current_spos_frost_key must be 32 bytes, got ${state.currentSposFrostKey.bytes.length}"
        )
        val asset = AssetName(TreasuryInfoContract.StateAssetName)
        val value = Value.lovelace(BootstrapLovelace) +
            Value.asset(contract.policyId, asset, 1L)
        (contract.address(network), value, state.toData)
    }

    /** Pick the one-shot UTxO a bootstrap mint consumes.
      *
      * Pure-ADA only, at least [[MinOneShotLovelace]], largest first, and never one of `excluded`.
      *
      * `excluded` MUST carry every CIP-33 reference-script UTxO of the sponsor wallet. Such a UTxO
      * is pure lovelace with no native assets, so it is indistinguishable from plain change here —
      * but spending one destroys a deployed reference script, and because the provider drops
      * `scriptRef` on `findUtxos` the builder also under-estimates the Conway reference-script fee
      * surcharge and the tx is rejected with `FeeTooSmallUTxO`.
      */
    def pickOneShot(utxos: Seq[Utxo], excluded: Set[TransactionInput]): Option[Utxo] =
        utxos
            .filter(u =>
                u.output.value.assets.isEmpty &&
                    u.output.value.coin.value >= MinOneShotLovelace &&
                    !excluded.contains(u.input)
            )
            .sortBy(-_.output.value.coin.value)
            .headOption
}
