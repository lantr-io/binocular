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
