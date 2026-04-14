package binocular

import binocular.cli.Console
import scalus.cardano.address.Address
import scalus.cardano.ledger.{TransactionHash, Utxo}
import scalus.cardano.node.{BlockchainProvider, TransactionStatus}
import scalus.cardano.txbuilder.TxBuilder
import scalus.cardano.wallet.hd.HdAccount
import scalus.compiler.Options
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.{ByteString, Data}
import scalus.utils.await

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration

/** Unique alwaysOk V3 minting/spending script for TMTx UTxOs.
  *
  * The script always succeeds. A random 32-byte salt is embedded in the compiled UPLC as an outer
  * lambda application. `optimizeUplc = false` prevents the optimizer from beta-reducing away the
  * salt constant, so the script hash differs from bare `PlutusV3.alwaysOk`. On-chain the CEK
  * machine performs the single beta-reduction, which is negligible for an alwaysOk script.
  */
object TmtxScript {

    /** Random 32-byte salt — included in the compiled UPLC to produce a distinct script hash. */
    private val salt: ByteString =
        ByteString.fromHex("a7f3e82b1c49d056f7a3b9c124d8e05f6a2b7c9d3e4f0a1b2c3d4e5f6a7b8c90")

    /** PlutusV3 script: always succeeds.
      *
      * Compiled with `Options.release` (error tagging on) and `optimizeUplc = false` so the salt
      * ByteString survives as a UPLC constant and the script hash is distinct.
      */
    lazy val mintingScript: PlutusV3[Data => Unit] =
        PlutusV3
            .compile((s: ByteString) => (_: Data) => ())(using
              Options.release.copy(optimizeUplc = false)
            )
            .apply(salt)

    /** Spend the existing TMTx UTxO and recreate it with datum Constr(1, [txBytes]).
      *
      * Used by both the relay command (after BTC confirmation) and the confirm-tmtx command.
      */
    def updateDatum(
        provider: BlockchainProvider,
        hdAccount: HdAccount,
        scriptAddress: Address,
        utxo: Utxo,
        txBytes: ByteString,
        timeout: Duration
    )(using ExecutionContext): Either[String, String] = {
        import scalus.cardano.onchain.plutus.prelude.List as ScalusList

        val network = provider.cardanoInfo.network
        val signer = hdAccount.signerForUtxos
        val sponsorAddress = hdAccount.baseAddress(network)
        val newDatum = Data.Constr(1, ScalusList(Data.B(txBytes)))

        try {
            Console.log("  Building Cardano datum update transaction...")

            val tx = TxBuilder(provider.cardanoInfo)
                .spend(utxo, Data.unit, mintingScript)
                .payTo(scriptAddress, utxo.output.value, newDatum)
                .complete(provider, sponsorAddress)
                .await(timeout)
                .sign(signer)
                .transaction

            Console.log("  Submitting...")

            val txHash = OracleTransactions.submitTx(provider, tx, timeout) match {
                case Right(hash) => hash
                case Left(err)   => return Left(err)
            }

            Console.logSuccess(s"  Cardano tx submitted: $txHash")
            Console.log("  Waiting for Cardano confirmation...")

            val status = provider
                .pollForConfirmation(
                  TransactionHash.fromHex(txHash),
                  maxAttempts = 60,
                  delayMs = 2000
                )
                .await(timeout)

            status match {
                case TransactionStatus.Confirmed =>
                    Console.logSuccess(
                      s"  Cardano datum updated to Constr(1): Cardano txid=$txHash"
                    )
                    Right(txHash)
                case other =>
                    Left(s"Transaction status: $other")
            }
        } catch {
            case e: Exception =>
                Left(e.getMessage)
        }
    }
}
