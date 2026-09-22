package binocular.watchtower

import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.TxBuilder
import scalus.cardano.wallet.hd.HdAccount

/** Build and sign only; the caller submits the returned transaction. Scalus owns coin selection
  * (including fragmented fBTC), minimum ADA, balancing and Cardano signing. The caller supplies a
  * datum with the live Config fee and chain-derived creation time.
  */
object PegOutRequestTx {
    def build(
        cardanoInfo: CardanoInfo,
        sponsor: HdAccount,
        walletUtxos: Utxos,
        excludeInputs: Set[TransactionInput],
        pegOutAddress: Address,
        bridgedTokenPolicy: ScriptHash,
        bridgedTokenAsset: AssetName,
        amountSat: Long,
        datum: PegOutDatum,
        minAda: Long = 0
    ): Transaction = {
        require(
          amountSat > 0 && datum.perPegoutFee >= 0 && amountSat > datum.perPegoutFee,
          "Peg-out amount must exceed its nonnegative fee"
        )
        require(minAda >= 0, "Minimum ADA cannot be negative")
        val sponsorAddress = sponsor.baseAddress(cardanoInfo.network)
        val available = CardanoFunding.eligible(walletUtxos, sponsorAddress, excludeInputs)
        // Start with zero ADA by default: the library raises it to the output's exact minimum.
        // Never copy an input's Value: unrelated assets belong in change, not in the request.
        TxBuilder(cardanoInfo)
            .payTo(
              pegOutAddress,
              Value
                  .lovelace(minAda) + Value.asset(bridgedTokenPolicy, bridgedTokenAsset, amountSat),
              datum
            )
            .complete(available, sponsorAddress)
            .sign(sponsor.signerForUtxos)
            .transaction
    }
}
