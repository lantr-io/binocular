package binocular.watchtower

import binocular.cli.Console
import binocular.oracle.OracleTransactions
import scalus.cardano.address.Address
import scalus.cardano.ledger.{TransactionHash, Utxo}
import scalus.cardano.node.{BlockchainProvider, TransactionStatus}
import scalus.cardano.txbuilder.TxBuilder
import scalus.cardano.wallet.hd.HdAccount
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.Data
import scalus.utils.await

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration

/** Off-chain builder for the validated TM Confirm transaction (the on-chain counterpart is
  * [[TreasuryMovementValidator]]).
  *
  * Spends an `Unconfirmed` TM UTxO with the real treasury-movement validator, references the
  * Binocular oracle UTxO (so the validator can check the block is confirmed), and recreates the
  * UTxO at the same address with the `Confirmed` datum — preserving the value so the TM marker
  * token rides along (the validator enforces this).
  */
object TreasuryMovementTx {

    /** Build, sign, submit, and await the Confirm tx. Returns the Cardano tx hash on success. */
    def buildAndSubmitConfirm(
        provider: BlockchainProvider,
        hdAccount: HdAccount,
        tmScript: scalus.cardano.ledger.Script.PlutusV3,
        tmAddress: Address,
        unconfirmed: Utxo,
        oracle: Utxo,
        redeemer: Data,
        confirmedDatum: Data,
        timeout: Duration
    )(using ExecutionContext): Either[String, String] = {
        val signer = hdAccount.signerForUtxos
        val sponsorAddress = hdAccount.baseAddress(provider.cardanoInfo.network)
        try {
            Console.log("  Building TM Confirm transaction...")
            val tx = TxBuilder(provider.cardanoInfo)
                .spend(unconfirmed, redeemer, tmScript)
                .references(oracle)
                .payTo(tmAddress, unconfirmed.output.value, confirmedDatum)
                .complete(provider, sponsorAddress)
                .await(timeout)
                .sign(signer)
                .transaction

            Console.log("  Submitting...")
            val txHash = OracleTransactions.submitTx(provider, tx, timeout) match {
                case Right(hash) => hash
                case Left(err)   => return Left(err)
            }

            Console.log(s"  Submitted: $txHash — waiting for Cardano confirmation...")
            provider
                .pollForConfirmation(
                  TransactionHash.fromHex(txHash),
                  maxAttempts = 60,
                  delayMs = 2000
                )
                .await(timeout) match {
                case TransactionStatus.Confirmed => Right(txHash)
                case other                       => Left(s"Transaction status: $other")
            }
        } catch {
            case e: Exception => Left(e.getMessage)
        }
    }
}
