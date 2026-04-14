package binocular.cli.commands

import binocular.*
import binocular.cli.{Command, Console}
import scalus.cardano.address.Address
import scalus.cardano.ledger.{AssetName, Credential, ScriptHash, Utxo}
import scalus.cardano.node.{BlockchainProvider, UtxoFilter, UtxoQuery, UtxoSource}
import scalus.cardano.wallet.hd.HdAccount
import scalus.uplc.builtin.{ByteString, Data}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.util.boundary
import boundary.break
import scalus.utils.await

/** Relay signed Bitcoin transactions from Cardano TMTx UTxOs to Bitcoin, then update the Cardano
  * datum to Constr(1) once the BTC transaction is confirmed.
  *
  * Polls Cardano for UTxOs containing the configured TMTx token, extracts the signed Bitcoin
  * transaction from the inline datum (Constr 0 [ByteString]), broadcasts it via Bitcoin RPC, and
  * after BTC confirmation updates the datum to Constr(1).
  */

/** Result of attempting to relay a TMTx to Bitcoin. */
enum RelayResult:
    case Relayed(btcTxId: String)
    case Rejected(error: String)

/** UTxO broadcast to Bitcoin, awaiting confirmation before datum update.
  * @param confirmedOnBitcoin
  *   true when we already know the tx is confirmed (e.g. -27 error), skip BTC check
  * @param broadcastHeight
  *   block height at broadcast time; if current height exceeds this the tx is confirmed
  *   (used when -txindex is not enabled and the tx is not a wallet transaction)
  */
private case class PendingConfirmation(
    utxo: Utxo,
    txBytes: ByteString,
    btcTxId: String,
    confirmedOnBitcoin: Boolean = false,
    broadcastHeight: Int = 0
)

/** Transient errors that warrant retrying — everything else is a permanent rejection. */
private def isTransientError(msg: String): Boolean =
    msg.contains("Connection failed")
        || msg.contains("RPC timeout")
        || msg.contains("HttpTimeoutException")

/** Already confirmed on-chain — treat as success. */
private def isAlreadyConfirmed(msg: String): Boolean =
    msg.contains("Transaction outputs already in utxo set") // code -27

case class RelayCommand(dryRun: Boolean = false) extends Command {

    override def execute(config: BinocularConfig): Int = {
        Console.header("Binocular TMTx Relay")
        if dryRun then Console.warn("Dry-run mode — will check once and exit without broadcasting")
        println()
        runRelay(config)
    }

    private def runRelay(config: BinocularConfig): Int = boundary {
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
        Console.info("Wallet", hdAccount.baseAddress(config.cardano.scalusNetwork).encode.getOrElse("?"))
        Console.info("TMTx policy", relayConfig.tmtxPolicyId)
        Console.info("TMTx asset", relayConfig.tmtxAssetName)
        Console.info("Script address", scriptAddress.encode.getOrElse("?"))
        Console.separator()
        println()

        // Fully processed UTxOs (datum updated to Constr(1) or permanently rejected)
        val transactions = scala.collection.mutable.Map[String, RelayResult]()
        // Broadcast to Bitcoin, waiting for confirmation before datum update
        val pendingConfirmation = scala.collection.mutable.Map[String, PendingConfirmation]()

        while true do {
            try {
                val currentHeight = try rpc.getBlockchainInfo().await(30.seconds).blocks
                                   catch case _ => 0

                // Check pending items: if BTC tx now confirmed, update Cardano datum
                for (utxoRef, pending) <- pendingConfirmation.toList do {
                    val confirmed: Option[Int] =
                        if pending.confirmedOnBitcoin then Some(1)
                        else
                            try
                                // Try getRawTransaction first (requires -txindex), fall back to
                                // gettransaction (wallet transactions only), fall back to
                                // block height check (works when -txindex is not enabled)
                                val info =
                                    try rpc.getRawTransaction(pending.btcTxId).await(30.seconds)
                                    catch
                                        case _ =>
                                            rpc.getWalletTransaction(pending.btcTxId)
                                                .await(30.seconds)
                                Some(info.confirmations).filter(_ > 0)
                            catch
                                case _ =>
                                    if currentHeight > pending.broadcastHeight then
                                        Some(currentHeight - pending.broadcastHeight)
                                    else
                                        Console.log(
                                          s"  Waiting for BTC confirmation: BTC txid=${pending.btcTxId}"
                                        )
                                        None

                    confirmed match {
                        case Some(confs) =>
                            Console.log(
                              s"BTC txid=${pending.btcTxId} confirmed ($confs block(s)), updating Cardano datum..."
                            )
                            TmtxScript.updateDatum(
                              provider,
                              hdAccount,
                              scriptAddress,
                              pending.utxo,
                              pending.txBytes,
                              timeout
                            ) match {
                                case Right(_) =>
                                    pendingConfirmation.remove(utxoRef)
                                    transactions(utxoRef) = RelayResult.Relayed(pending.btcTxId)
                                case Left(err) =>
                                    Console.logError(
                                      s"  Cardano datum update failed: $err — will retry"
                                    )
                            }
                        case None => // still waiting, logged above
                    }
                }

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
                            transactions.contains(key) || pendingConfirmation.contains(key)
                        }

                        if newUtxos.isEmpty then
                            val relayed = transactions.values.count {
                                case RelayResult.Relayed(_) => true; case _ => false
                            }
                            val rejected = transactions.size - relayed
                            val pending = pendingConfirmation.size
                            Console.logInPlace(
                              s"Polling... ${utxos.size} TMTx UTxO(s): $relayed relayed, $pending awaiting BTC confirmation, $rejected rejected"
                            )
                        else
                            Console.log(s"Found ${newUtxos.size} new TMTx UTxO(s) to relay")

                            for (input, output) <- newUtxos do {
                                val utxoRef = s"${input.transactionId.toHex}#${input.index}"
                                Console.log(s"Processing Cardano UTxO: $utxoRef")

                                output.inlineDatum match {
                                    case None =>
                                        Console.logWarn(s"  No inline datum, skipping")
                                        transactions(utxoRef) =
                                            RelayResult.Rejected("no inline datum")

                                    case Some(Data.Constr(1, args)) if args.nonEmpty =>
                                        args.head match {
                                            case Data.B(txBytes) =>
                                                val txid = BitcoinHelpers
                                                    .getTxHash(txBytes)
                                                    .reverse
                                                    .toHex
                                                Console.logSuccess(
                                                  s"  Already confirmed by Binocular: BTC txid=$txid"
                                                )
                                                transactions(utxoRef) =
                                                    RelayResult.Relayed(txid)
                                            case other =>
                                                Console.logWarn(
                                                  s"  Datum arg is not ByteString: $other"
                                                )
                                                transactions(utxoRef) = RelayResult.Rejected(
                                                  "datum arg is not ByteString"
                                                )
                                        }

                                    case Some(Data.Constr(0, args)) if args.nonEmpty =>
                                        args.head match {
                                            case Data.B(txBytes) =>
                                                val txHex = txBytes.toHex
                                                Console.log(
                                                  s"  BTC tx: ${txHex.take(40)}... (${txBytes.size} bytes)"
                                                )

                                                if dryRun then
                                                    Console.logSuccess(
                                                      s"  [dry-run] Would broadcast ${txBytes.size} bytes"
                                                    )
                                                    transactions(utxoRef) =
                                                        RelayResult.Relayed("dry-run")
                                                else
                                                    val btcTxId = BitcoinHelpers
                                                        .getTxHash(txBytes)
                                                        .reverse
                                                        .toHex
                                                    try {
                                                        val txid = rpc
                                                            .sendRawTransaction(txHex)
                                                            .await(30.seconds)
                                                        Console.logSuccess(
                                                          s"  Broadcast OK: BTC txid=$txid"
                                                        )
                                                        pendingConfirmation(utxoRef) =
                                                            PendingConfirmation(
                                                              Utxo(input, output),
                                                              txBytes,
                                                              txid,
                                                              broadcastHeight = currentHeight
                                                            )
                                                    } catch {
                                                        case e: Exception =>
                                                            val msg = e.getMessage
                                                            if isTransientError(msg) then
                                                                Console.logError(
                                                                  s"  Broadcast failed (will retry): $msg"
                                                                )
                                                            // don't record — will retry
                                                            else if isAlreadyConfirmed(msg) then
                                                                Console.logSuccess(
                                                                  s"  Already confirmed on Bitcoin: BTC txid=$btcTxId"
                                                                )
                                                                pendingConfirmation(utxoRef) =
                                                                    PendingConfirmation(
                                                                      Utxo(input, output),
                                                                      txBytes,
                                                                      btcTxId,
                                                                      confirmedOnBitcoin = true,
                                                                      broadcastHeight = currentHeight
                                                                    )
                                                            else
                                                                Console.logWarn(
                                                                  s"  Broadcast rejected: $msg"
                                                                )
                                                                transactions(utxoRef) =
                                                                    RelayResult.Rejected(msg)
                                                    }

                                            case other =>
                                                Console.logWarn(
                                                  s"  Datum arg is not ByteString: $other"
                                                )
                                                transactions(utxoRef) = RelayResult.Rejected(
                                                  "datum arg is not ByteString"
                                                )
                                        }

                                    case Some(other) =>
                                        Console.logWarn(s"  Unexpected datum shape: $other")
                                        transactions(utxoRef) =
                                            RelayResult.Rejected("unexpected datum shape")
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
}
