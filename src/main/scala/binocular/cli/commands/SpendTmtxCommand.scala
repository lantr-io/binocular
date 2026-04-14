package binocular.cli.commands

import binocular.*
import binocular.cli.{Command, Console}
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.node.{BlockchainProvider, TransactionStatus, UtxoFilter, UtxoQuery, UtxoSource}
import scalus.cardano.txbuilder.TxBuilder
import scalus.cardano.wallet.hd.HdAccount
import scalus.uplc.builtin.Data

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.util.boundary
import boundary.break
import scalus.utils.await

/** Spend (destroy) all TMTx UTxOs at the script address.
  *
  * Finds UTxOs containing the TMTx token, spends them with the alwaysOk script, and burns the
  * tokens. The locked ADA is returned to the wallet.
  */
case class SpendTmtxCommand() extends Command {

    override def execute(config: BinocularConfig): Int = {
        Console.header("Spend TMTx UTxOs")
        println()
        spendTmtx(config)
    }

    private def spendTmtx(config: BinocularConfig): Int = boundary {
        given ec: ExecutionContext = ExecutionContext.global
        val timeout = 120.seconds

        val provider: BlockchainProvider = config.cardano.createBlockchainProvider() match {
            case Right(p) => p
            case Left(err) =>
                Console.error(s"Cardano provider: $err")
                break(1)
        }

        val hdAccount: HdAccount = config.wallet.createHdAccount() match {
            case Right(a) => a
            case Left(err) =>
                Console.error(s"Wallet: $err")
                break(1)
        }

        val network = config.cardano.scalusNetwork
        val signer = hdAccount.signerForUtxos
        val sponsorAddress = hdAccount.baseAddress(network)

        val mintingScript = TmtxScript.mintingScript
        val policyId = ScriptHash.fromHex(config.relay.tmtxPolicyId)
        val assetName = AssetName.fromString(config.relay.tmtxAssetName)
        val scriptAddress = Address(network, Credential.ScriptHash(policyId))

        Console.info("Cardano", config.cardano.network)
        Console.info("Wallet", sponsorAddress.encode.getOrElse("?"))
        Console.info("TMTx policy", policyId.toHex)
        Console.info("Script address", scriptAddress.encode.getOrElse("?"))
        Console.separator()
        println()

        // Find UTxOs at the script address that contain the TMTx token
        val query = UtxoQuery(UtxoSource.FromAddress(scriptAddress)) &&
            UtxoFilter.HasAsset(policyId, assetName)
        val utxos = provider.findUtxos(query).await(30.seconds) match {
            case Right(u) => u
            case Left(err) =>
                Console.error(s"UTxO query failed: $err")
                break(1)
        }

        if utxos.isEmpty then
            Console.warn("No UTxOs found at script address")
            break(0)

        Console.log(s"Found ${utxos.size} UTxO(s) to spend")

        // Count total tokens to burn
        var totalBurn = 0L
        for (input, output) <- utxos do {
            val count =
                output.value.assets.assets.get(policyId).flatMap(_.get(assetName)).getOrElse(0L)
            totalBurn += count
            Console.info(
              s"  ${input.transactionId.toHex}#${input.index}",
              s"${output.value.coin} lovelace, $count TMTx"
            )
        }

        Console.log(s"Burning $totalBurn TMTx token(s)...")

        try {
            // Build: spend all script UTxOs + burn tokens
            var builder = TxBuilder(provider.cardanoInfo)

            for (input, output) <- utxos do {
                val utxo = Utxo(input, output)
                builder = builder.spend(utxo, Data.unit, mintingScript)
            }

            if totalBurn > 0 then
                builder = builder.mint(mintingScript, Map(assetName -> -totalBurn), Data.unit)

            // Use a wallet UTxO as collateral
            val tx = builder
                .complete(provider, sponsorAddress)
                .await(timeout)
                .sign(signer)
                .transaction

            Console.log("Submitting transaction...")

            val txHash = OracleTransactions.submitTx(provider, tx, timeout) match {
                case Right(hash) => hash
                case Left(err) =>
                    Console.error(s"Submit failed: $err")
                    break(1)
            }

            Console.logSuccess(s"Transaction submitted: $txHash")
            Console.log("Waiting for confirmation...")

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
                      s"Done! Spent ${utxos.size} UTxO(s), burned $totalBurn TMTx token(s)"
                    )
                    0
                case other =>
                    Console.logError(s"Transaction status: $other")
                    1
            }
        } catch {
            case e: Exception =>
                Console.error(s"Failed: ${e.getMessage}")
                e.printStackTrace()
                1
        }
    }
}
