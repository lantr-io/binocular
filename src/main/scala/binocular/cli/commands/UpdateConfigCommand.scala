package binocular.cli.commands

import binocular.*
import binocular.oracle.OracleTransactions
import binocular.watchtower.*
import binocular.cli.{Command, CommandHelpers, Console}

import scalus.cardano.ledger.{AssetName, TransactionHash, Utxo}
import scalus.cardano.node.TransactionStatus
import scalus.cardano.onchain.plutus.prelude.{List as PList, Option as SOption}
import scalus.cardano.txbuilder.TxBuilder
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData
import scalus.utils.await

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.util.Try
import scala.util.boundary
import boundary.break
import cats.syntax.either.*

/** Update the deployed bridge Config UTxO in place (config.ak `Update` redeemer) — the migration
  * path that avoids redeploying the bridge. Rev-5.4 layout (spec §Config datum): 0 `update_auth`, 1
  * `bridged_token_policy`, 2 `completed_peg_ins_policy`, 3 `bridge_state_policy`, 4
  * `tm_script_hash`, 5 `peg_in_script_hash`, 6 `peg_out_script_hash`, 7 `params` (nested).
  *
  *   - optionally swaps field 3 (`bridge_state_policy`) — spec §Recovery: replacing the singleton
  *     turns on this field being a live swap point;
  *   - optionally swaps field 4 (`tm_script_hash`) — spec [CFG-2]: published for off-chain readers;
  *     a TM validator redeploy must republish it here (and swap field 3 with it, since the
  *     singleton is compile-parameterized by the TM script hash);
  *   - optionally swaps field 5 (`peg_in_script_hash`) / field 6 (`peg_out_script_hash`);
  *   - optionally sets the **operational parameters** inside field 7 — `--fee-rate` /
  *     `--per-pegout-fee` / `--min-peg-out` / `--schedule` (spec §Operational parameters). No
  *     on-chain validator reads a current value; they are off-chain consensus anchors, so this
  *     command IS the governance mechanism for them. In particular the fee rate is how the bridge
  *     tracks the Bitcoin fee market (spec §Stuck-TM recovery: a fee-update Config Update, then
  *     rebuild the same frozen batch at the new rate). Every SPO's TM builder reads them at its
  *     batch snapshot slot, so an update here changes the bytes every heimdall builds —
  *     deliberately, and for all of them at once. It takes effect from the next batch, never
  *     retroactively; the schedule (params[3]) from the next epoch boundary.
  *
  * ALL of the above happen in ONE transaction: a validator swap must flip every dependent field in
  * the same Update that precedes its first use, or readers chase hashes whose UTxOs do not exist.
  *
  * The spend is authorized by the config's `update_auth` (field 0) — currently the binocular owner
  * key (`oracle.owner-pkh`), whose signature is required on the tx. The config NFT, address, and
  * non-ADA value are preserved (config.ak enforces this); all other datum fields are carried over
  * verbatim.
  *
  * The rewrite decodes the typed [[ConfigDatum]] and edits fields by name ([LIB-1]). Because the
  * whole datum is re-encoded, a deployed datum whose field count grew past the layout this build
  * knows is REFUSED up front: re-encoding it would silently drop the appended fields.
  *
  * The config script is rebuilt from the bridge blueprint parameterized by the bootstrap one-shot
  * (`bridge.completed-peg-ins-one-shot-ref` — deploy-bridge uses ONE shared one-shot for config,
  * cpi and cpo) and the config NFT asset name.
  */
case class UpdateConfigCommand(
    bridgeStatePolicy: Option[String] = None,
    tmScriptHash: Option[String] = None,
    pegInWithdrawHash: Option[String] = None,
    pegOutWithdrawHash: Option[String] = None,
    params: UpdateConfigCommand.ParamEdits = UpdateConfigCommand.ParamEdits.none,
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

        // Every swappable field holds a 28-byte script hash; reject anything else up front rather
        // than writing an undecodable datum that only fails when a validator later reads it.
        def scriptHashArg(flag: String, value: Option[String]): Option[ByteString] =
            value.map { h =>
                if h.length == 56 && h.forall(c => "0123456789abcdefABCDEF".contains(c)) then
                    ByteString.fromHex(h)
                else {
                    Console.error(s"$flag must be 56 hex chars, got '$h'")
                    break(1)
                }
            }
        val newBridgeStatePolicy = scriptHashArg("--bridge-state-policy", bridgeStatePolicy)
        val newTmScriptHash = scriptHashArg("--tm-script-hash", tmScriptHash)
        val newPegInHash = scriptHashArg("--peg-in-withdraw-hash", pegInWithdrawHash)
        val newPegOutHash = scriptHashArg("--peg-out-withdraw-hash", pegOutWithdrawHash)
        // With every option now optional, a bare `update-config` would spend and recreate the config
        // UTxO with an identical datum — a fee for nothing, and a needless spend of the config NFT.
        if newBridgeStatePolicy.isEmpty && newTmScriptHash.isEmpty && newPegInHash.isEmpty
            && newPegOutHash.isEmpty && params.isEmpty
        then {
            Console.error(
              "Nothing to update. Pass at least one of --bridge-state-policy, --tm-script-hash, " +
                  "--peg-in-withdraw-hash, --peg-out-withdraw-hash, --fee-rate, --per-pegout-fee, " +
                  "--min-peg-out, --schedule."
            )
            break(1)
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
        val (blueprint, blueprintSource) =
            try BifrostBlueprint.resolve(config.bridge.plutusJson)
            catch {
                case e: Exception =>
                    Console.error(s"Loading bridge blueprint: ${e.getMessage}"); break(1)
            }
        Console.info("blueprint", blueprintSource)
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

        val oldData = configUtxo.output.inlineDatum.getOrElse {
            Console.error("Config UTxO has no inline datum")
            break(1)
        }
        val oldConfig = UpdateConfigCommand.decodeDeployed(oldData).valueOr { err =>
            Console.error(err); break(1)
        }
        val newConfig = UpdateConfigCommand.rewrite(
          oldConfig,
          newBridgeStatePolicy,
          newTmScriptHash,
          newPegInHash,
          newPegOutHash,
          params
        )
        val newDatum: Data = newConfig.toData
        val updateAuthPkh = oldConfig.updateAuth match {
            case SOption.Some(AuthorizationMethod.CardanoSignature(pkh)) => pkh
            case SOption.Some(other) =>
                Console.error(
                  s"update_auth is not a CardanoSignature — this command only supports " +
                      s"signature-authorized configs, got: $other"
                )
                break(1)
            case SOption.None =>
                Console.error("Config update_auth (field 0) is None — config is frozen")
                break(1)
        }

        Console.info(
          "config UTxO",
          s"${configUtxo.input.transactionId.toHex}#${configUtxo.input.index}"
        )
        newBridgeStatePolicy.foreach(h =>
            Console.info("new bridge-state policy (field 3)", h.toHex)
        )
        newTmScriptHash.foreach(h => Console.info("new TM script hash (field 4)", h.toHex))
        newPegInHash.foreach(h => Console.info("new peg-in hash (field 5)", h.toHex))
        newPegOutHash.foreach(h => Console.info("new peg-out hash (field 6)", h.toHex))
        Console.info("update_auth pkh", updateAuthPkh.toHex)
        // Every changed field, old -> new. The operational parameters are consensus inputs for
        // every SPO's TM builder, so an accidental edit is worth seeing before it is signed.
        UpdateConfigCommand.diff(oldData, newDatum).foreach { case (idx, name, before, after) =>
            Console.info(s"field $idx ($name)", s"$before -> $after")
        }
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

    /** Pure datum rewrite over the rev-5.4 layout: replace each supplied script hash, then patch
      * the named operational parameters inside the nested `params` record.
      *
      * Every supplied swap is applied to the SAME datum, so one call — and therefore one
      * transaction — performs a whole validator migration (the dependent fields flip together).
      *
      * Operates on the typed [[ConfigDatum]] ([LIB-1]: fields by name). The caller guards the
      * deployed Constr arity BEFORE decoding (see `execute`): a decode-copy-reencode of a datum
      * with appended fields would silently drop them, so an unknown arity is refused, never
      * rewritten.
      *
      * @param newBridgeStatePolicy
      *   `bridge_state_policy` (spec §Recovery: the singleton swap point).
      * @param newTmScriptHash
      *   `tm_script_hash` (spec [CFG-2]).
      * @param params
      *   the governed operational parameters. Each is optional and overwrites only the field it
      *   names; `schedule` patches individual `ScheduleParams` fields by name.
      */
    def rewrite(
        cfg: ConfigDatum,
        newBridgeStatePolicy: Option[ByteString],
        newTmScriptHash: Option[ByteString],
        newPegInHash: Option[ByteString],
        newPegOutHash: Option[ByteString],
        params: ParamEdits = ParamEdits.none
    ): ConfigDatum = {
        val p = cfg.params
        cfg.copy(
          bridgeStatePolicy = newBridgeStatePolicy.getOrElse(cfg.bridgeStatePolicy),
          tmScriptHash = newTmScriptHash.getOrElse(cfg.tmScriptHash),
          pegInScriptHash = newPegInHash.getOrElse(cfg.pegInScriptHash),
          pegOutScriptHash = newPegOutHash.getOrElse(cfg.pegOutScriptHash),
          spoBansPolicyId = params.spoBansPolicyId.getOrElse(cfg.spoBansPolicyId),
          baseBanDurationMs = params.baseBanDurationMs.getOrElse(cfg.baseBanDurationMs),
          maxFaultsBeforePermanent =
              params.maxFaultsBeforePermanent.getOrElse(cfg.maxFaultsBeforePermanent),
          maxValidityWindowMs = params.maxValidityWindowMs.getOrElse(cfg.maxValidityWindowMs),
          params = p.copy(
            feeRateSatPerVb = params.feeRateSatPerVb.getOrElse(p.feeRateSatPerVb),
            perPegoutFee = params.perPegoutFee.getOrElse(p.perPegoutFee),
            minPegOutFbtc = params.minPegOutFbtc.getOrElse(p.minPegOutFbtc),
            schedule = patchSchedule(p.schedule, params.schedule)
          )
        )
    }

    /** Field count of the rev-5.4 Config datum (spec §Config datum), including the federation
      * identity appended at #15-16 ([CFG-4]).
      *
      * A Config written before that append has 15 fields and is refused below — deliberately.
      * Unlike a read-only consumer, this command re-encodes the WHOLE datum, so it cannot write
      * back a shape it does not fully know: an update that quietly invented a federation key would
      * move the treasury address every SPO derives.
      */
    val ConfigFieldCount = 17

    /** Decode the deployed Config datum for an UPDATE, refusing any Constr arity other than
      * [[ConfigFieldCount]]. Appends are the legal datum evolution and read-only consumers ignore
      * unknown trailing fields, but this command re-encodes the WHOLE datum — a
      * decode-copy-reencode of a grown datum would silently drop the appended fields, so it is
      * refused instead.
      */
    def decodeDeployed(datum: Data): Either[String, ConfigDatum] = datum match {
        case Data.Constr(0, fields) =>
            val n = fields.asScala.size
            if n != ConfigFieldCount then
                Left(
                  s"config datum has $n fields; this build knows the $ConfigFieldCount-field " +
                      "rev-5.4 layout. Re-encoding would drop the extra fields — update binocular " +
                      "instead of forcing the write."
                )
            else
                Try(datum.to[ConfigDatum]).toOption
                    .toRight("config datum does not decode as the rev-5.4 ConfigDatum")
        case other => Left(s"config datum is not a Constr 0 record: $other")
    }

    /** `ScheduleParams` field names, in record order — the `--schedule name=value` keys and the
      * positions inside the doubly-nested Constr at params[3].
      */
    val ScheduleFields: List[String] = List(
      "dkg_r1_deadline",
      "dkg_r2_deadline",
      "update_y_deadline",
      "tm_batch_interval",
      "sign_r1_window",
      "sign_r2_window",
      "leader_slot_t",
      "tm_recovery_window",
      "final_tm_cutoff",
      "stability_window"
    )

    /** Config datum field names (rev 5.4), for the change report. */
    private val FieldNames: Vector[String] = Vector(
      "update_auth",
      "bridged_token_policy",
      "completed_peg_ins_policy",
      "bridge_state_policy",
      "tm_script_hash",
      "peg_in_script_hash",
      "peg_out_script_hash",
      "spo_bans_policy_id",
      "base_ban_duration_ms",
      "max_faults_before_permanent",
      "max_validity_window_ms",
      "spos_registry_policy_id",
      "treasury_info_policy_id",
      "treasury_info_asset_name",
      "params"
    )

    /** The governed parameter edits (config fields 7-10 and inside field 14). All optional: `None`
      * means "carry the deployed value over". `schedule` patches individual `ScheduleParams` fields
      * by name, leaving the rest of the nested record untouched.
      */
    case class ParamEdits(
        feeRateSatPerVb: Option[BigInt] = None,
        perPegoutFee: Option[BigInt] = None,
        minPegOutFbtc: Option[BigInt] = None,
        schedule: Map[String, BigInt] = Map.empty,
        spoBansPolicyId: Option[ByteString] = None,
        baseBanDurationMs: Option[BigInt] = None,
        maxFaultsBeforePermanent: Option[BigInt] = None,
        maxValidityWindowMs: Option[BigInt] = None
    ) {
        def isEmpty: Boolean = !touchesTunables && !touchesBans

        /** Whether any of the nested `params` tunables (#14) is edited. */
        def touchesTunables: Boolean =
            feeRateSatPerVb.nonEmpty || perPegoutFee.nonEmpty || minPegOutFbtc.nonEmpty ||
                schedule.nonEmpty

        /** Whether the ban policy (#7-#10) is being written.
          *
          * Unlike rev 5.1, where these were an APPEND a deployed datum might not carry yet, rev 5.4
          * makes them mandatory fields — so writing one is an ordinary in-place patch and needs no
          * all-or-nothing grouping.
          */
        def touchesBans: Boolean =
            spoBansPolicyId.nonEmpty || baseBanDurationMs.nonEmpty ||
                maxFaultsBeforePermanent.nonEmpty || maxValidityWindowMs.nonEmpty
    }

    object ParamEdits {
        val none: ParamEdits = ParamEdits()

        /** Parse `--spo-bans-policy`. A wrong policy id names a well-formed ban address holding
          * nothing, so a malformed one is refused at the CLI rather than published.
          */
        def parseBanPolicy(arg: String): Either[String, ByteString] = {
            val h = arg.trim
            if h.length == 56 && h.forall(c => "0123456789abcdefABCDEF".contains(c)) then
                Right(ByteString.fromHex(h))
            else Left(s"--spo-bans-policy must be a 56-hex-char policy id, got '$arg'")
        }

        /** Parse repeated `--schedule name=value` arguments against [[ScheduleFields]]. */
        def parseSchedule(args: List[String]): Either[String, Map[String, BigInt]] =
            args.foldLeft[Either[String, Map[String, BigInt]]](Right(Map.empty)) { (acc, arg) =>
                acc.flatMap { m =>
                    arg.split('=') match {
                        case Array(name, value) if ScheduleFields.contains(name.trim) =>
                            value.trim.toLongOption match {
                                case Some(v) if v >= 0 => Right(m + (name.trim -> BigInt(v)))
                                case _ => Left(s"--schedule $arg: value must be a non-negative Int")
                            }
                        case Array(name, _) =>
                            Left(
                              s"--schedule $arg: unknown field '${name.trim}'. Known: " +
                                  ScheduleFields.mkString(", ")
                            )
                        case _ => Left(s"--schedule $arg: expected name=value")
                    }
                }
            }
    }

    /** Overwrite the named fields of a [[ScheduleParams]], keeping the others. `parseSchedule`
      * already validated every name against [[ScheduleFields]]; an unknown one here is a
      * programming error, so it throws.
      */
    private def patchSchedule(s: ScheduleParams, edits: Map[String, BigInt]): ScheduleParams =
        edits.foldLeft(s) { case (acc, (name, v)) =>
            name match {
                case "dkg_r1_deadline"    => acc.copy(dkgR1Deadline = v)
                case "dkg_r2_deadline"    => acc.copy(dkgR2Deadline = v)
                case "update_y_deadline"  => acc.copy(updateYDeadline = v)
                case "tm_batch_interval"  => acc.copy(tmBatchInterval = v)
                case "sign_r1_window"     => acc.copy(signR1Window = v)
                case "sign_r2_window"     => acc.copy(signR2Window = v)
                case "leader_slot_t"      => acc.copy(leaderSlotT = v)
                case "tm_recovery_window" => acc.copy(tmRecoveryWindow = v)
                case "final_tm_cutoff"    => acc.copy(finalTmCutoff = v)
                case "stability_window"   => acc.copy(stabilityWindow = v)
                case other =>
                    throw new IllegalArgumentException(s"unknown schedule field '$other'")
            }
        }

    /** `(index, field name, old, new)` for every top-level Config field the rewrite changed. */
    def diff(oldDatum: Data, newDatum: Data): List[(Int, String, String, String)] = {
        def fieldsOf(d: Data): List[Data] = d match {
            case Data.Constr(_, fs) => fs.asScala.toList
            case _                  => Nil
        }
        val name = (i: Int) => FieldNames.lift(i).getOrElse(s"#$i")
        val oldFields = fieldsOf(oldDatum)
        fieldsOf(newDatum).zipWithIndex.flatMap { case (after, i) =>
            oldFields.lift(i) match {
                case Some(before) if before == after => None
                case Some(before) => Some((i, name(i), render(before), render(after)))
                case None         => Some((i, name(i), "(absent)", render(after)))
            }
        }
    }

    private def render(d: Data): String = d match {
        case Data.I(v)          => v.toString
        case Data.B(b)          => b.toHex
        case Data.Constr(0, xs) => xs.asScala.toList.map(render).mkString("[", ", ", "]")
        case other              => other.toString
    }
}
