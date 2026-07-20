package binocular.cli.commands

import binocular.*
import binocular.oracle.OracleTransactions
import binocular.watchtower.*
import binocular.cli.{Command, CommandHelpers, Console}

import scalus.cardano.ledger.{AssetName, TransactionHash, Utxo}
import scalus.cardano.node.TransactionStatus
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.cardano.txbuilder.TxBuilder
import scalus.uplc.builtin.{ByteString, Data}
import scalus.utils.await

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.util.boundary
import boundary.break
import cats.syntax.either.*

/** Update the deployed bridge Config UTxO in place (config.ak `Update` redeemer) — the migration
  * path that avoids redeploying the bridge:
  *
  *   - sets/appends field 11 (`initial_btc_treasury_utxo`) — the 36-byte anchor outpoint the FIRST
  *     Treasury Movement must spend (re-runnable: re-anchors an already-updated config);
  *   - optionally swaps field 4 (`peg_in_withdraw_script_hash`) to a new peg-in hash (needed when
  *     the TM validator changes, since peg_in.ak is parameterized by the TM NFT policy).
  *
  * The spend is authorized by the config's `update_auth` (field 10) — currently the binocular owner
  * key (`oracle.owner-pkh`), whose signature is required on the tx. The config NFT, address, and
  * non-ADA value are preserved (config.ak enforces this); all other datum fields are carried over
  * verbatim.
  *
  * The config script is rebuilt from the bridge blueprint parameterized by the bootstrap one-shot
  * (`bridge.completed-peg-ins-one-shot-ref` — deploy-bridge uses ONE shared one-shot for config,
  * cpi and cpo) and the config NFT asset name.
  */
case class UpdateConfigCommand(
    initialBtcTreasuryUtxo: String,
    pegInWithdrawHash: Option[String],
    dryRun: Boolean = false
) extends Command {

    override def execute(config: BinocularConfig): Int = boundary {
        Console.header("Update Bridge Config UTxO")
        if dryRun then Console.warn("Dry-run mode — will compute the new datum but not submit")
        println()

        given ec: ExecutionContext = ExecutionContext.global
        val timeout = config.oracle.transactionTimeout.seconds

        val setup = CommandHelpers.setupOracle(config).valueOr { err =>
            Console.error(err); break(1)
        }
        val provider = setup.provider
        val network = setup.network

        val anchor =
            try BridgeConfig.outpointFromDisplay(initialBtcTreasuryUtxo.trim)
            catch {
                case e: IllegalArgumentException =>
                    Console.error(s"--initial-btc-treasury-utxo: ${e.getMessage}")
                    break(1)
            }
        val newPegInHash = pegInWithdrawHash.map { h =>
            if h.length == 56 && h.forall(c => "0123456789abcdefABCDEF".contains(c)) then
                ByteString.fromHex(h)
            else {
                Console.error(s"--peg-in-withdraw-hash must be 56 hex chars, got '$h'")
                break(1)
            }
        }

        // Rebuild the config script from the bootstrap one-shot + config NFT asset name.
        val oneShotStr = config.bridge.completedPegInsOneShotRef.trim
        val (oneShotHash, oneShotIdx) = oneShotStr.split('#') match {
            case Array(h, i) if h.length == 64 && i.toIntOption.isDefined => (h, i.toInt)
            case _ =>
                Console.error(
                  "bridge.completed-peg-ins-one-shot-ref must be TX_HASH#INDEX (the deploy-bridge " +
                      "one-shot; it also parameterizes the config script)"
                )
                break(1)
        }
        val blueprint =
            try BifrostBlueprint.fromFile(config.bridge.plutusJson)
            catch {
                case e: Exception =>
                    Console.error(
                      s"Loading bridge blueprint (${config.bridge.plutusJson}): ${e.getMessage}"
                    )
                    break(1)
            }
        val configNftAssetName = ByteString.fromHex(config.bridge.configNftAssetName)
        val configContract = ConfigContract(
          blueprint,
          ByteString.fromHex(oneShotHash),
          BigInt(oneShotIdx),
          configNftAssetName
        )
        val configAddress = configContract.address(network)
        if configContract.policyId.toHex != config.bridge.configNftPolicyId then {
            Console.error(
              s"Derived config policy ${configContract.policyId.toHex} does not match " +
                  s"bridge.config-nft-policy-id ${config.bridge.configNftPolicyId} — check the " +
                  "one-shot ref and plutus.json"
            )
            break(1)
        }

        // Locate the config UTxO by its NFT.
        val configUtxo = provider.findUtxos(configAddress).await(timeout) match {
            case Left(err) => Console.error(s"Fetching config UTxOs: $err"); break(1)
            case Right(utxos) =>
                utxos.toList
                    .collectFirst {
                        case (in, out)
                            if out.value.hasAsset(
                              configContract.policyId,
                              AssetName(configNftAssetName)
                            ) =>
                            Utxo(in, out)
                    }
                    .getOrElse {
                        Console.error(s"No config UTxO with the config NFT at $configAddress")
                        break(1)
                    }
        }

        val oldFields = configUtxo.output.inlineDatum match {
            case Some(Data.Constr(0, fields)) => fields.asScala.toList
            case other =>
                Console.error(s"Config UTxO datum is not a Constr 0 inline datum: $other")
                break(1)
        }
        val newFields =
            try UpdateConfigCommand.rewriteFields(oldFields, newPegInHash, anchor)
            catch {
                case e: IllegalArgumentException =>
                    Console.error(e.getMessage); break(1)
            }
        val newDatum: Data = Data.Constr(0, PList.from(newFields))
        val updateAuthPkh = oldFields(10) match {
            case Data.Constr(0, args) =>
                args.asScala.toList.headOption match {
                    // CardanoSignature { hash } = Constr 0 [B hash] inside Some = Constr 0 [...]
                    case Some(Data.Constr(0, sigArgs)) =>
                        sigArgs.asScala.toList.headOption match {
                            case Some(Data.B(pkh)) => pkh
                            case _ =>
                                Console.error("update_auth CardanoSignature has no pkh"); break(1)
                        }
                    case other =>
                        Console.error(
                          s"update_auth is not a CardanoSignature — this command only supports " +
                              s"signature-authorized configs, got: $other"
                        )
                        break(1)
                }
            case _ =>
                Console.error("Config update_auth (field 10) is None — config is frozen")
                break(1)
        }

        Console.info(
          "config UTxO",
          s"${configUtxo.input.transactionId.toHex}#${configUtxo.input.index}"
        )
        Console.info("old fields", oldFields.size.toString)
        Console.info("new fields", newFields.size.toString)
        Console.info("anchor (field 11)", anchor.toHex)
        newPegInHash.foreach(h => Console.info("new peg-in hash (field 4)", h.toHex))
        Console.info("update_auth pkh", updateAuthPkh.toHex)
        println()

        if dryRun then {
            Console.success("Dry-run complete (new datum computed; not submitting)")
            break(0)
        }

        // Spend the config UTxO with Update (Constr 0 []) and recreate it at the same address
        // with the same value (NFT rides along) and the new datum, signed by update_auth.
        val updateRedeemer: Data = Data.Constr(0, PList())
        val tx =
            try
                TxBuilder(provider.cardanoInfo)
                    .spend(configUtxo, updateRedeemer, configContract.script)
                    .payTo(configAddress, configUtxo.output.value, newDatum)
                    .requireSignature(
                      scalus.cardano.ledger.AddrKeyHash.fromHex(updateAuthPkh.toHex)
                    )
                    .complete(provider, setup.sponsorAddress)
                    .await(timeout)
                    .sign(setup.signer)
                    .transaction
            catch {
                case e: Exception =>
                    Console.error(s"Building config Update tx: ${e.getMessage}")
                    Option(e.getCause).foreach(c => Console.error(s"Cause: ${c.getMessage}"))
                    break(1)
            }
        val txHash = OracleTransactions.submitTx(provider, tx, timeout) match {
            case Right(h)  => h
            case Left(err) => Console.error(s"Submit: $err"); break(1)
        }
        val status = provider
            .pollForConfirmation(TransactionHash.fromHex(txHash), maxAttempts = 90, delayMs = 2000)
            .await(210.seconds)
        status match {
            case TransactionStatus.Confirmed =>
                Console.success(s"Config updated: $txHash")
                0
            case other =>
                Console.error(s"Not confirmed: $other")
                1
        }
    }
}

object UpdateConfigCommand {

    /** Pure datum rewrite: swap field 4 when a new peg-in hash is given, then set/append field 11
      * (the treasury anchor). Re-runnable: an 11-field (pre-migration) datum gets the anchor
      * appended; a 12-field datum gets it replaced (re-anchoring).
      */
    def rewriteFields(
        fields: List[Data],
        newPegInHash: Option[ByteString],
        anchor: ByteString
    ): List[Data] = {
        require(fields.size >= 11, s"config datum has ${fields.size} fields, expected >= 11")
        val swapped = newPegInHash.fold(fields)(h => fields.updated(4, Data.B(h)))
        if swapped.size == 11 then swapped :+ Data.B(anchor)
        else swapped.updated(11, Data.B(anchor))
    }
}
