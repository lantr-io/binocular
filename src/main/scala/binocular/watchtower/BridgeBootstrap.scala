package binocular.watchtower

import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.{AssetName, TransactionInput, Utxo, Value}
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData

/** Shared pieces of the bridge bootstrap transactions.
  *
  * `deploy-bridge` creates the whole bridge in one tx; `bootstrap-completed-peg-outs` re-mints only
  * the completed-peg-outs trie against a LIVE bridge (the field-3 migration). Both must produce a
  * byte-identical trie output, so the shape lives here once rather than in each command.
  */
object BridgeBootstrap {

    /** The genesis MPF root every trie is minted with: 32 zero bytes. Both Aiken merkle-tree
      * validators pin this literal in their mint handler, so a different value cannot be minted.
      */
    val EmptyRoot: ByteString = ByteString.fromArray(Array.fill[Byte](32)(0))

    /** Lovelace put on each bootstrapped protocol UTxO. Comfortably above the min-UTxO for a
      * one-asset output with a 32-byte inline datum.
      */
    val BootstrapLovelace: Long = 2_000_000L

    /** Minimum lovelace a wallet UTxO must hold to be usable as a one-shot. */
    val MinOneShotLovelace: Long = 5_000_000L

    /** The completed-peg-outs trie output a bootstrap mint must create.
      *
      * `completed-peg-outs-merkle-tree.ak::mint` requires EXACTLY one output at the policy's own
      * script address, holding the single `"CPO"` token, with an inline datum of the empty root and
      * a `stake_credential: None` address. All four are fixed here.
      */
    def completedPegOutsOutput(
        contract: CompletedPegOutsContract,
        network: Network
    ): (Address, Value, Data) = {
        val asset = AssetName(CompletedPegOutsContract.assetName)
        val value = Value.lovelace(BootstrapLovelace) +
            Value.asset(contract.policyId, asset, 1L)
        (contract.address(network), value, CompletedPegOutsMerkleTreeDatum(EmptyRoot).toData)
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
