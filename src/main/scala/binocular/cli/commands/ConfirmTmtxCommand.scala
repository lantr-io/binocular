package binocular.cli.commands

import binocular.*
import binocular.cli.{Command, Console}
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.node.{BlockchainProvider, TransactionStatus, UtxoFilter, UtxoQuery, UtxoSource}
import scalus.cardano.txbuilder.TxBuilder
import scalus.cardano.wallet.hd.HdAccount
import scalus.uplc.builtin.{ByteString, Data}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.util.boundary
import boundary.break
import scalus.utils.await

/** Confirm relayed Bitcoin transactions by updating the TMTx UTxO datum.
  *
  * Polls for Bitcoin TM transactions, and whenever they've been confirmed, modifies the Treasury
  * UTxO on cardano to signify that the BTC part is complete (and thus can be used reliably).
  */

/** Result of attempting to confirm a TMTx on Bitcoin. */
enum ConfirmResult:
    case Confirmed(btcTxId: String, cardanoTxHash: String)
    case AlreadyConfirmed
    case Rejected(error: String)

case class ConfirmTmtxCommand(dryRun: Boolean = false) extends Command {

    override def execute(config: BinocularConfig): Int = {
        Console.header("Binocular TMTx Confirm")
        if dryRun then Console.warn("Dry-run mode — will check once and exit without submitting")
        println()
        runConfirm(config)
    }

    private def runConfirm(config: BinocularConfig): Int = boundary {
        given ec: ExecutionContext = ExecutionContext.global
        val relayConfig = config.relay
        val pollInterval = relayConfig.pollInterval
        val retryInterval = relayConfig.retryInterval
        val timeout = 120.seconds

        val policyId = ScriptHash.fromHex(relayConfig.tmtxPolicyId)
        val assetName = AssetName.fromString(relayConfig.tmtxAssetName)
        val scriptAddress =
            Address(config.cardano.scalusNetwork, Credential.ScriptHash(policyId))

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

        val sponsorAddress = hdAccount.baseAddress(config.cardano.scalusNetwork)

        val rpc = new SimpleBitcoinRpc(config.bitcoinNode)

        try {
            val info = rpc.getBlockchainInfo().await(30.seconds)
            Console.info("Bitcoin", s"${config.bitcoinNode.url} (${info.chain})")
        } catch {
            case e: Exception =>
                Console.error(s"Bitcoin RPC: ${e.getMessage}")
                break(1)
        }

        Console.info("Cardano", config.cardano.network)
        Console.info("Wallet", sponsorAddress.encode.getOrElse("?"))
        Console.info("TMTx policy", relayConfig.tmtxPolicyId)
        Console.info("TMTx asset", relayConfig.tmtxAssetName)
        Console.info("Script address", scriptAddress.encode.getOrElse("?"))
        Console.separator()
        println()

        // Track processed TMTx UTxOs: utxoRef -> confirm result
        val processed = scala.collection.mutable.Map[String, ConfirmResult]()

        while true do {
            try {
                val query = UtxoQuery(UtxoSource.FromAddress(scriptAddress)) &&
                    UtxoFilter.HasAsset(policyId, assetName)
                val utxosResult = provider.findUtxos(query).await(30.seconds)

                utxosResult match {
                    case Left(err) =>
                        Console.logWarn(s"UTxO query: $err")

                    case Right(utxos) if utxos.isEmpty =>
                        Console.logInPlace("Polling... no TMTx UTxOs found")

                    case Right(utxos) =>
                        val newUtxos = utxos.filterNot { case (input, _) =>
                            val key = s"${input.transactionId.toHex}#${input.index}"
                            processed.contains(key)
                        }

                        if newUtxos.isEmpty then
                            val confirmed = processed.values.count {
                                case ConfirmResult.Confirmed(_, _) => true; case _ => false
                            }
                            val alreadyConfirmed = processed.values.count {
                                case ConfirmResult.AlreadyConfirmed => true; case _ => false
                            }
                            Console.logInPlace(
                              s"Polling... ${utxos.size} TMTx UTxO(s), all processed ($confirmed confirmed, $alreadyConfirmed already confirmed)"
                            )
                        else
                            Console.log(s"Found ${newUtxos.size} TMTx UTxO(s) to check")

                            for (input, output) <- newUtxos do {
                                val utxoRef = s"${input.transactionId.toHex}#${input.index}"
                                Console.log(s"Processing UTxO: $utxoRef")

                                output.inlineDatum match {
                                    case None =>
                                        Console.logWarn(s"  No inline datum, skipping")
                                        processed(utxoRef) =
                                            ConfirmResult.Rejected("no inline datum")

                                    case Some(Data.Constr(1, _)) =>
                                        Console.log(s"  Already confirmed (Constr 1), skipping")
                                        processed(utxoRef) = ConfirmResult.AlreadyConfirmed

                                    case Some(Data.Constr(0, args)) if args.nonEmpty =>
                                        args.head match {
                                            case Data.B(txBytes) =>
                                                val btcTxId =
                                                    BitcoinHelpers.getTxHash(txBytes).reverse.toHex
                                                Console.log(
                                                  s"  BTC txid: $btcTxId"
                                                )

                                                try {
                                                    val txInfo = rpc
                                                        .getRawTransaction(btcTxId)
                                                        .await(30.seconds)

                                                    if txInfo.confirmations > 0 then
                                                        Console.logSuccess(
                                                          s"  Confirmed on Bitcoin (${txInfo.confirmations} confirmations)"
                                                        )

                                                        if dryRun then
                                                            Console.logSuccess(
                                                              s"  [dry-run] Would update datum to Constr(1)"
                                                            )
                                                            processed(utxoRef) =
                                                                ConfirmResult.Confirmed(
                                                                  btcTxId,
                                                                  "dry-run"
                                                                )
                                                        else
                                                            updateDatum(
                                                              provider,
                                                              hdAccount,
                                                              scriptAddress,
                                                              Utxo(input, output),
                                                              txBytes,
                                                              timeout
                                                            ) match {
                                                                case Right(cardanoTxHash) =>
                                                                    processed(utxoRef) =
                                                                        ConfirmResult.Confirmed(
                                                                          btcTxId,
                                                                          cardanoTxHash
                                                                        )
                                                                case Left(err) =>
                                                                    Console.logError(
                                                                      s"  Datum update failed: $err — will retry"
                                                                    )
                                                                // don't add to processed — retry next cycle
                                                            }
                                                    else
                                                        Console.log(
                                                          s"  In mempool but unconfirmed (${txInfo.confirmations} confirmations)"
                                                        )
                                                    // don't add to processed — retry next cycle
                                                } catch {
                                                    case e: Exception =>
                                                        Console.log(
                                                          s"  Not yet on Bitcoin: ${e.getMessage}"
                                                        )
                                                    // don't add to processed — retry next cycle
                                                }

                                            case other =>
                                                Console.logWarn(
                                                  s"  Datum arg is not ByteString: $other"
                                                )
                                                processed(utxoRef) = ConfirmResult.Rejected(
                                                  "datum arg is not ByteString"
                                                )
                                        }

                                    case Some(other) =>
                                        Console.logWarn(s"  Unexpected datum shape: $other")
                                        processed(utxoRef) =
                                            ConfirmResult.Rejected("unexpected datum shape")
                                }
                            }
                }

                if dryRun then break(0)
                Thread.sleep(pollInterval * 1000L)

            } catch {
                case e: Exception =>
                    Console.logError(s"Error: ${e.getMessage} — retrying in ${retryInterval}s")
                    Thread.sleep(retryInterval * 1000L)
            }
        }
        0
    }

    /** Spend the existing TMTx UTxO and recreate it with datum Constr(1, [txBytes]). */
    private def updateDatum(
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
        val mintingScript = TmtxScript.mintingScript

        // TODO: Currently using PlutusV3.alwaysOk for spending. A proper validator
        // should verify that the datum transition contains a valid inclusion proof, which a validator should check
        // for now, constr (0, <tx) -> conts(1, <tx>) for simplicity
        //

        val newDatum = Data.Constr(1, ScalusList(Data.B(txBytes)))

        try {
            Console.log("  Building datum update transaction...")

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

            Console.logSuccess(s"  Submitted: $txHash")
            Console.log("  Waiting for confirmation...")

            val status = provider
                .pollForConfirmation(
                  TransactionHash.fromHex(txHash),
                  maxAttempts = 60,
                  delayMs = 2000
                )
                .await(timeout)

            status match {
                case TransactionStatus.Confirmed =>
                    Console.logSuccess(s"  Datum updated to Constr(1) — confirmed: $txHash")
                    Right(txHash)
                case other =>
                    Left(s"Transaction status: $other")
            }
        } catch {
            case e: Exception =>
                Left(s"${e.getMessage}")
        }
    }
}
