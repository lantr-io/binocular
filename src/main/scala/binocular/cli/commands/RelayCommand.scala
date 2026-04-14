package binocular.cli.commands

import binocular.*
import binocular.cli.{Command, Console}
import scalus.cardano.address.Address
import scalus.cardano.ledger.{AssetName, Credential, ScriptHash}
import scalus.cardano.node.{BlockchainProvider, UtxoFilter, UtxoQuery, UtxoSource}
import scalus.uplc.builtin.Data

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.util.boundary
import boundary.break
import scalus.utils.await

/** Relay signed Bitcoin transactions from Cardano TMTx UTxOs to Bitcoin.
  *
  * Polls Cardano for UTxOs containing the configured TMTx token, extracts the signed Bitcoin
  * transaction from the inline datum (Constr 0 [ByteString]), and broadcasts it via Bitcoin RPC.
  */

/** Result of attempting to relay a TMTx to Bitcoin. */
enum RelayResult:
    case Relayed(btcTxId: String)
    case Rejected(error: String)

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
        Console.info("TMTx policy", relayConfig.tmtxPolicyId)
        Console.info("TMTx asset", relayConfig.tmtxAssetName)
        Console.info("Script address", scriptAddress.encode.getOrElse("?"))
        Console.separator()
        println()

        // Track processed TMTx UTxOs: utxoRef -> relay result
        val transactions = scala.collection.mutable.Map[String, RelayResult]()

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
                            transactions.contains(key)
                        }

                        if newUtxos.isEmpty then
                            val relayed = transactions.values.count {
                                case RelayResult.Relayed(_) => true; case _ => false
                            }
                            val rejected = transactions.size - relayed
                            Console.logInPlace(
                              s"Polling... ${utxos.size} TMTx UTxO(s), all processed ($relayed relayed, $rejected rejected)"
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
                                                    try {
                                                        val txid = rpc
                                                            .sendRawTransaction(txHex)
                                                            .await(30.seconds)
                                                        Console.logSuccess(
                                                          s"  Broadcast OK: BTC txid=$txid"
                                                        )
                                                        transactions(utxoRef) =
                                                            RelayResult.Relayed(txid)
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
                                                                  s"  Already confirmed on-chain"
                                                                )
                                                                transactions(utxoRef) =
                                                                    RelayResult.Relayed("already-confirmed")
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
