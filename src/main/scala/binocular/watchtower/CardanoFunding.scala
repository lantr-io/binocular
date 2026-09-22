package binocular.watchtower

import scalus.cardano.address.Address
import scalus.cardano.ledger.{TransactionInput, Utxos}
import scalus.cardano.node.BlockchainProvider
import scalus.cardano.txbuilder.TxBuilder

import scala.concurrent.{ExecutionContext, Future}

/** Keep reference scripts out of both normal inputs and collateral selection. Scalus 1.1.1's
  * Blockfrost provider enriches scriptRef and fails the read if enrichment fails. The caller's
  * discovered/configured exclusions provide an additional guard for other providers.
  */
object CardanoFunding {
    def eligible(
        utxos: Utxos,
        sponsorAddress: Address,
        excludeInputs: Set[TransactionInput]
    ): Utxos = utxos.filter { case (input, output) =>
        !excludeInputs(input) && output.scriptRef.isEmpty && output.address == sponsorAddress
    }

    def complete(
        builder: TxBuilder,
        provider: BlockchainProvider,
        sponsorAddress: Address,
        excludeInputs: Set[TransactionInput]
    )(using ExecutionContext): Future[TxBuilder] =
        provider.findUtxos(sponsorAddress).map { result =>
            val utxos = result.fold(
              err => throw new IllegalStateException(s"Fetching sponsor UTxOs: $err"),
              identity
            )
            builder.complete(eligible(utxos, sponsorAddress, excludeInputs), sponsorAddress)
        }
}
