package binocular.cli.commands

import binocular.*
import binocular.bitcoin.*
import binocular.oracle.reverse
import binocular.watchtower.*
import binocular.cli.{Command, CommandHelpers, Console}
import scalus.cardano.address.Address
import scalus.cardano.ledger.{AssetName, Credential}
import scalus.cardano.node.BlockchainProvider
import scalus.uplc.builtin.{ByteString, Data}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.util.boundary
import boundary.break
import scalus.utils.await
import cats.syntax.either.*

/** Watchtower relay: carry SPO-signed Treasury Movement (TM) transactions from Cardano to Bitcoin.
  *
  * Polls the [[TreasuryMovementValidator]] address for **Unconfirmed** TM UTxOs (datum
  * `Constr(0, [signed_btc_tx])`, as posted by heimdall's `publish.rs`), extracts the signed Bitcoin
  * transaction, and broadcasts it to the Bitcoin node. This is the Cardano→Bitcoin transport step
  * of the protocol (technical_documentation.md: "Watchtowers monitor `treasury_movement.ak` … and
  * broadcast it to the source blockchain network").
  *
  * Broadcast-only: it does **not** touch the Cardano datum. The validated `Unconfirmed → Confirmed`
  * transition is `confirm-tmtx` (which proves Bitcoin inclusion against the Binocular oracle).
  * `Confirmed` (`Constr 1`) UTxOs are skipped here.
  */

/** Result of attempting to relay a TM to Bitcoin. */
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
        Console.header("Binocular TM Relay (Cardano → Bitcoin)")
        if dryRun then Console.warn("Dry-run mode — will check once and exit without broadcasting")
        println()
        runRelay(config)
    }

    private def runRelay(config: BinocularConfig): Int = boundary {
        given ec: ExecutionContext = ExecutionContext.global
        val pollInterval = config.relay.pollInterval
        val retryInterval = config.relay.retryInterval
        val timeout = config.oracle.transactionTimeout.seconds

        val setup = CommandHelpers.setupOracle(config).valueOr { err =>
            Console.error(err); break(1)
        }
        val provider: BlockchainProvider = setup.provider
        val network = setup.network
        val oracleScriptHashBS = ByteString.fromArray(setup.script.scriptHash.bytes)
        val tmScript = TreasuryMovementContract.contract(
          oracleScriptHashBS,
          ByteString.fromHex(config.bridge.tmControlNftPolicy),
          ByteString.fromHex(config.bridge.tmControlNftName)
        )
        val tmAddress = Address(network, Credential.ScriptHash(tmScript.script.scriptHash))
        // The TM NFT (policy = validator's own hash, empty asset name) marks genuine TM UTxOs.
        val tmNftPolicy = tmScript.script.scriptHash
        val tmNftAsset = AssetName.empty

        val rpc = new SimpleBitcoinRpc(config.bitcoinNode)
        try {
            val info = rpc.getBlockchainInfo().await(30.seconds)
            Console.info("Bitcoin", s"${config.bitcoinNode.url} (${info.chain})")
        } catch {
            case e: Exception => Console.error(s"Bitcoin RPC: ${e.getMessage}"); break(1)
        }
        Console.info("Cardano", config.cardano.network)
        Console.info("TM validator", tmScript.script.scriptHash.toHex)
        Console.info("TM address", tmAddress.encode.getOrElse("?"))
        Console.separator()
        println()

        // utxoRef -> outcome; avoids rebroadcasting the same UTxO within a run.
        val processed = scala.collection.mutable.Map[String, RelayResult]()

        while true do {
            try {
                provider.findUtxos(tmAddress).await(timeout) match {
                    case Left(err) => Console.logWarn(s"UTxO query: $err")
                    case Right(utxos) =>
                        val newUtxos = utxos.toList
                            .filter { case (_, out) => out.value.hasAsset(tmNftPolicy, tmNftAsset) }
                            .filterNot { case (in, _) =>
                                processed.contains(s"${in.transactionId.toHex}#${in.index}")
                            }
                        if newUtxos.isEmpty then
                            val relayed = processed.values.count {
                                case RelayResult.Relayed(_) => true; case _ => false
                            }
                            Console.logInPlace(
                              s"Polling... ${utxos.size} UTxO(s) at TM address, $relayed relayed"
                            )
                        else
                            for (in, out) <- newUtxos do
                                val utxoRef = s"${in.transactionId.toHex}#${in.index}"
                                out.inlineDatum match {
                                    case Some(Data.Constr(0, args)) if args.nonEmpty =>
                                        args.head match {
                                            case Data.B(txBytes) =>
                                                relayOne(rpc, utxoRef, txBytes, processed)
                                            case other =>
                                                Console.logWarn(
                                                  s"  $utxoRef datum arg not ByteString: $other"
                                                )
                                                processed(utxoRef) =
                                                    RelayResult.Rejected("datum arg not ByteString")
                                        }
                                    case Some(Data.Constr(1, _)) =>
                                        Console.log(
                                          s"  $utxoRef already Confirmed (Constr 1) — skipping"
                                        )
                                        processed(utxoRef) =
                                            RelayResult.Relayed("already-confirmed")
                                    case other =>
                                        Console.logWarn(s"  $utxoRef unexpected datum: $other")
                                        processed(utxoRef) =
                                            RelayResult.Rejected("unexpected datum")
                                }
                }

                if dryRun then break(0)
                Thread.sleep(pollInterval * 1000L)
            } catch {
                case e: boundary.Break[?] =>
                    // Control-flow escape (the dry-run `break(0)` above), not an operational error:
                    // `boundary.break` throws a `Break` that extends RuntimeException, so without
                    // this guard the generic handler swallows it, logs a spurious "Error: null",
                    // and exits via `break(1)`. Re-throw so `--dry-run` unwinds cleanly.
                    throw e
                case e: Exception =>
                    Console.logError(s"Error: ${e.getMessage} — retrying in ${retryInterval}s")
                    if dryRun then break(1)
                    Thread.sleep(retryInterval * 1000L)
            }
        }
        0
    }

    /** Broadcast one Unconfirmed TM's signed Bitcoin tx (broadcast-only — no Cardano write). */
    private def relayOne(
        rpc: SimpleBitcoinRpc,
        utxoRef: String,
        txBytes: ByteString,
        processed: scala.collection.mutable.Map[String, RelayResult]
    )(using ExecutionContext): Unit = {
        // getTxHash strips witness data, which recurses on a tx-declared input count — a crafted
        // UTxO at the TM address could StackOverflow. Guard so a poison UTxO is skipped, not fatal.
        val btcTxId =
            try BitcoinHelpers.getTxHash(txBytes).reverse.toHex
            catch {
                case t: Throwable =>
                    Console.logWarn(
                      s"  $utxoRef: malformed TM bytes — skipping (${t.getClass.getSimpleName})"
                    )
                    processed(utxoRef) = RelayResult.Rejected("malformed")
                    return
            }
        Console.log(s"  $utxoRef: TM btc txid=$btcTxId (${txBytes.size} bytes)")
        if dryRun then
            Console.logSuccess(s"    [dry-run] would broadcast ${txBytes.size} bytes")
            processed(utxoRef) = RelayResult.Relayed("dry-run")
        else
            try {
                val txid = rpc.sendRawTransaction(txBytes.toHex).await(30.seconds)
                Console.logSuccess(s"    Broadcast OK: BTC txid=$txid")
                processed(utxoRef) = RelayResult.Relayed(txid)
            } catch {
                case e: Exception =>
                    val msg = e.getMessage
                    if isTransientError(msg) then
                        Console.logError(s"    Broadcast failed (will retry): $msg")
                    else if isAlreadyConfirmed(msg) then
                        Console.logSuccess(s"    Already on Bitcoin: BTC txid=$btcTxId")
                        processed(utxoRef) = RelayResult.Relayed(btcTxId)
                    else
                        Console.logWarn(s"    Broadcast rejected: $msg")
                        processed(utxoRef) = RelayResult.Rejected(msg)
            }
    }
}
