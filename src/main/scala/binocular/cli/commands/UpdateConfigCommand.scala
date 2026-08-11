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
  *   - optionally sets/appends field 11 (`initial_btc_treasury_utxo`) — the 36-byte anchor outpoint
  *     the FIRST Treasury Movement must spend (re-runnable: re-anchors an already-updated config).
  *     OMIT it to leave the deployed anchor untouched: rewriting field 11 re-anchors the whole TM
  *     chain, so an unrelated hash swap must not have to restate it;
  *   - optionally swaps field 3 (`completed_peg_outs_merkle_tree_policy_id`) to the rewritten trie
  *     validator's policy;
  *   - optionally swaps field 4 (`peg_in_withdraw_script_hash`) to a new peg-in hash (needed when
  *     the TM validator changes, since peg_in.ak is parameterized by the TM NFT policy);
  *   - optionally swaps field 5 (`peg_out_withdraw_script_hash`) to the rewritten peg-out
  *     validator's hash;
  *   - optionally sets the **operational parameters** — `--min-stake` (field 9) and `--fee-rate` /
  *     `--per-pegout-fee` / `--min-peg-out` / `--leader-reward` / `--schedule` (fields 12–16, spec
  *     §Operational parameters). No on-chain validator reads a current value; they are off-chain
  *     consensus anchors, so this command IS the governance mechanism for them. In particular the
  *     fee rate is how the bridge tracks the Bitcoin fee market (spec §Stuck-TM recovery: a
  *     fee-update Config Update, then rebuild the same frozen batch at the new rate). Every SPO's
  *     TM builder reads them at its batch snapshot slot, so an update here changes the bytes every
  *     heimdall builds — deliberately, and for all of them at once. It takes effect from the next
  *     batch, never retroactively; the schedule (#16) from the next epoch boundary.
  *   - optionally publishes the **ban policy** — `--spo-bans-policy` plus the three ban-schedule
  *     numbers (fields 17-20). This is the migration for a bridge deployed before those fields
  *     existed, and it is what lets every SPO drop its `cardano.ban_bootstrap`,
  *     `fault_proof_policies` and `*_ban_*` keys: the finished `spo_bans` policy id is an input to
  *     nothing an SPO must compute, and the ban script address follows from it. Publishing it here
  *     changes no on-chain behaviour — it changes which roster the off-chain nodes agree on, since
  *     the eligible set is the registry MINUS active bans. Appended to a 17-field datum, replaced
  *     on a 21-field one; no re-mint, no redeploy.
  *
  * ALL of the above happen in ONE transaction. That is load-bearing for the peg-out trie v2
  * migration: fields 3, 4 and 5 must flip together, in the same Update that precedes the first use
  * of the new TM script. A partial swap leaves the TM Confirm reading a field-3 policy whose trie
  * UTxO no longer exists, or peg-out completion pointed at a withdraw hash with no registered
  * reward account.
  *
  * The spend is authorized by the config's `update_auth` (field 10) — currently the binocular owner
  * key (`oracle.owner-pkh`), whose signature is required on the tx. The config NFT, address, and
  * non-ADA value are preserved (config.ak enforces this); all other datum fields are carried over
  * verbatim.
  *
  * The rewrite works on the RAW field list and never decodes a typed `ConfigDatum`, so it keeps
  * working against a deployed pre-migration config whose field count differs from the current type.
  *
  * The config script is rebuilt from the bridge blueprint parameterized by the bootstrap one-shot
  * (`bridge.completed-peg-ins-one-shot-ref` — deploy-bridge uses ONE shared one-shot for config,
  * cpi and cpo) and the config NFT asset name.
  */
case class UpdateConfigCommand(
    initialBtcTreasuryUtxo: Option[String],
    pegInWithdrawHash: Option[String],
    completedPegOutsPolicy: Option[String] = None,
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

        // The anchor is OPTIONAL. Field 11 is the outpoint the FIRST Treasury Movement must spend,
        // so rewriting it re-anchors the whole TM chain. Making it mandatory meant every unrelated
        // update (a hash swap) had to restate it, and a typo would silently re-anchor. Omitted now
        // means: leave the deployed field 11 exactly as it is.
        val anchor: Option[ByteString] =
            initialBtcTreasuryUtxo.map(_.trim).filter(_.nonEmpty).map { s =>
                try BridgeConfig.outpointFromDisplay(s)
                catch {
                    case e: IllegalArgumentException =>
                        Console.error(s"--initial-btc-treasury-utxo: ${e.getMessage}")
                        break(1)
                }
            }
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
        val newPegInHash = scriptHashArg("--peg-in-withdraw-hash", pegInWithdrawHash)
        val newCpoPolicy = scriptHashArg("--completed-peg-outs-policy", completedPegOutsPolicy)
        val newPegOutHash = scriptHashArg("--peg-out-withdraw-hash", pegOutWithdrawHash)
        // With every option now optional, a bare `update-config` would spend and recreate the config
        // UTxO with an identical datum — a fee for nothing, and a needless spend of the config NFT.
        if anchor.isEmpty && newPegInHash.isEmpty && newCpoPolicy.isEmpty && newPegOutHash.isEmpty
            && params.isEmpty
        then {
            Console.error(
              "Nothing to update. Pass at least one of --initial-btc-treasury-utxo, " +
                  "--completed-peg-outs-policy, --peg-in-withdraw-hash, --peg-out-withdraw-hash, " +
                  "--min-stake, --fee-rate, --per-pegout-fee, --min-peg-out, --leader-reward, " +
                  "--schedule, --spo-bans-policy."
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

        val oldFields = configUtxo.output.inlineDatum match {
            case Some(Data.Constr(0, fields)) => fields.asScala.toList
            case other =>
                Console.error(s"Config UTxO datum is not a Constr 0 inline datum: $other")
                break(1)
        }
        val newFields =
            try
                UpdateConfigCommand.rewriteFields(
                  oldFields,
                  newCpoPolicy,
                  newPegInHash,
                  newPegOutHash,
                  anchor,
                  params
                )
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
        Console.info("anchor (field 11)", anchor.fold("unchanged")(_.toHex))
        newCpoPolicy.foreach(h => Console.info("new completed-peg-outs policy (field 3)", h.toHex))
        newPegInHash.foreach(h => Console.info("new peg-in hash (field 4)", h.toHex))
        newPegOutHash.foreach(h => Console.info("new peg-out hash (field 5)", h.toHex))
        params.spoBansPolicyId.foreach { p =>
            Console.info("ban policy (field 17)", p.toHex)
            Console.info(
              "  effect",
              "every SPO reads its ban list from this policy and needs no ban keys"
            )
        }
        Console.info("update_auth pkh", updateAuthPkh.toHex)
        // Every changed field, old -> new. The operational parameters are consensus inputs for
        // every SPO's TM builder, so an accidental edit is worth seeing before it is signed.
        UpdateConfigCommand.diff(oldFields, newFields).foreach { case (idx, name, before, after) =>
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

    /** Pure datum rewrite: replace each supplied script hash in place, then set/append field 11
      * (the treasury anchor).
      *
      * Every supplied swap is applied to the SAME field list, so one call — and therefore one
      * transaction — performs the whole peg-out trie v2 migration (fields 3, 4 and 5 together).
      *
      * Operates on raw `Data` fields and never decodes a `ConfigDatum`, so it works against a
      * deployed pre-migration datum with fewer fields than the current type. Re-runnable: an
      * 11-field (pre-migration) datum gets the anchor appended; a 12-or-more-field datum gets it
      * replaced (re-anchoring). Fields beyond 11 are carried over verbatim.
      *
      * `anchor = None` leaves field 11 exactly as it was, so a hash-only migration cannot re-anchor
      * the TM chain by accident. On an 11-field datum that means no field 11 is added.
      *
      * @param newCpoPolicy
      *   field 3, `completed_peg_outs_merkle_tree_policy_id`.
      * @param newPegInHash
      *   field 4, `peg_in_withdraw_script_hash`.
      * @param newPegOutHash
      *   field 5, `peg_out_withdraw_script_hash`.
      * @param params
      *   the governed operational parameters: field 9, fields 12-16 and the ban policy 17-20. Each
      *   is optional and overwrites only the field it names. Editing 12-16 requires a datum that
      *   already carries them (17 fields): `config.config`'s genesis path full-casts the datum, so
      *   a partial append is a datum no reader accepts, and inventing values for the fields the
      *   operator did not name is not governance. Redeploy (deploy-bridge writes all 17) instead.
      *   Fields 17-20 are APPENDED to a 17-field datum — that is the migration path for a bridge
      *   deployed before the ban policy was published — and replaced on a 21-field one.
      */
    def rewriteFields(
        fields: List[Data],
        newCpoPolicy: Option[ByteString],
        newPegInHash: Option[ByteString],
        newPegOutHash: Option[ByteString],
        anchor: Option[ByteString],
        params: ParamEdits = ParamEdits.none
    ): List[Data] = {
        require(fields.size >= 11, s"config datum has ${fields.size} fields, expected >= 11")
        require(
          !params.touchesTunables || fields.size >= FieldsWithTunables,
          s"config datum has ${fields.size} fields — the operational parameters are #12-#16, " +
              s"which only exist on a $FieldsWithTunables-field datum. Redeploy the bridge " +
              "(deploy-bridge writes all of them) rather than appending half a record here"
        )
        require(
          !params.touchesBans || params.banFields.nonEmpty,
          "the ban policy is fields #17-#20 and they landed as one append — set all of " +
              "--spo-bans-policy, --base-ban-duration-ms, --max-faults-before-permanent and " +
              "--max-validity-window-ms, or none. A reader cannot tell a half-written record " +
              "from a bridge with no bans"
        )
        require(
          !params.touchesBans || fields.size == FieldsWithTunables || fields.size >= FieldsWithBans,
          s"config datum has ${fields.size} fields — the ban policy can be appended to a " +
              s"$FieldsWithTunables-field datum or replaced on a $FieldsWithBans-field one, but " +
              "this datum is already partially through that append"
        )
        val swaps = List(3 -> newCpoPolicy, 4 -> newPegInHash, 5 -> newPegOutHash)
        val swapped = swaps.foldLeft(fields) { case (fs, (idx, v)) =>
            v.fold(fs)(h => fs.updated(idx, Data.B(h)))
        }
        val anchored = anchor.fold(swapped)(a =>
            if swapped.size == 11 then swapped :+ Data.B(a) else swapped.updated(11, Data.B(a))
        )
        val withScalars = List(
          9 -> params.minStake,
          12 -> params.feeRateSatPerVb,
          13 -> params.perPegoutFee,
          14 -> params.minPegOutFbtc,
          15 -> params.leaderReward
        ).foldLeft(anchored) { case (fs, (idx, v)) => v.fold(fs)(x => fs.updated(idx, Data.I(x))) }
        val withSchedule =
            if params.schedule.isEmpty then withScalars
            else withScalars.updated(16, patchSchedule(withScalars(16), params.schedule))
        params.banFields match {
            case Nil                                            => withSchedule
            case ban if withSchedule.size == FieldsWithTunables => withSchedule ++ ban
            case ban => withSchedule.patch(FieldsWithTunables, ban, ban.size)
        }
    }

    /** Field count of a Config datum carrying the operational-parameter append (#12-#16). */
    val FieldsWithTunables = 17

    /** Field count of a Config datum carrying the ban-policy append (#17-#20). */
    val FieldsWithBans = 21

    /** `ScheduleParams` field names, in record order — the `--schedule name=value` keys and the
      * positions inside the nested Constr at config #16.
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

    /** Config datum field names, for the change report. */
    private val FieldNames: Vector[String] = Vector(
      "bridged_token_policy_id",
      "bridged_token_asset_name",
      "completed_peg_ins_policy_id",
      "completed_peg_outs_policy_id",
      "peg_in_withdraw_script_hash",
      "peg_out_withdraw_script_hash",
      "peg_in_close_verifier_script_hash",
      "tm_and_peg_out_produced_verifier_hash",
      "tm_and_peg_out_not_produced_verifier_hash",
      "min_stake",
      "update_auth",
      "initial_btc_treasury_utxo",
      "fee_rate_sat_per_vb",
      "per_pegout_fee",
      "min_peg_out_fbtc",
      "leader_reward",
      "schedule",
      "spo_bans_policy_id",
      "base_ban_duration_ms",
      "max_faults_before_permanent",
      "max_validity_window_ms"
    )

    /** The governed parameter edits (config #9, #12-#16 and #17-#20). All optional: `None` means
      * "carry the deployed value over". `schedule` patches individual `ScheduleParams` fields by
      * name, leaving the rest of the nested record untouched.
      *
      * The four ban fields are all-or-nothing rather than individually optional. They identify ONE
      * thing — the deployed `spo_bans` policy, whose id is derived from the schedule numbers among
      * them — so a datum carrying a new policy id beside an old schedule describes no deployment
      * that exists.
      */
    case class ParamEdits(
        minStake: Option[BigInt] = None,
        feeRateSatPerVb: Option[BigInt] = None,
        perPegoutFee: Option[BigInt] = None,
        minPegOutFbtc: Option[BigInt] = None,
        leaderReward: Option[BigInt] = None,
        schedule: Map[String, BigInt] = Map.empty,
        spoBansPolicyId: Option[ByteString] = None,
        baseBanDurationMs: Option[BigInt] = None,
        maxFaultsBeforePermanent: Option[BigInt] = None,
        maxValidityWindowMs: Option[BigInt] = None
    ) {
        def isEmpty: Boolean = minStake.isEmpty && !touchesTunables && !touchesBans

        /** Whether any of #12-#16 is edited — the fields that only exist post-append. */
        def touchesTunables: Boolean =
            feeRateSatPerVb.nonEmpty || perPegoutFee.nonEmpty || minPegOutFbtc.nonEmpty ||
                leaderReward.nonEmpty || schedule.nonEmpty

        /** Whether the ban policy (#17-#20) is being written. */
        def touchesBans: Boolean =
            spoBansPolicyId.nonEmpty || baseBanDurationMs.nonEmpty ||
                maxFaultsBeforePermanent.nonEmpty || maxValidityWindowMs.nonEmpty

        /** #17-#20 as datum fields, or `Nil` unless all four are given. */
        def banFields: List[Data] =
            (
              spoBansPolicyId,
              baseBanDurationMs,
              maxFaultsBeforePermanent,
              maxValidityWindowMs
            ) match {
                case (Some(policy), Some(base), Some(maxFaults), Some(window)) =>
                    List(Data.B(policy), Data.I(base), Data.I(maxFaults), Data.I(window))
                case _ => Nil
            }
    }

    object ParamEdits {
        val none: ParamEdits = ParamEdits()

        /** Parse the `--spo-bans-policy` argument: a 28-byte policy id. Anything else derives a ban
          * address no deployment has, and the resulting empty list reads exactly like a bridge that
          * has never banned anyone — so reject it here rather than publish it.
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

    /** Overwrite the named fields of a `ScheduleParams` Constr, keeping the other nine. */
    private def patchSchedule(current: Data, edits: Map[String, BigInt]): Data = current match {
        case Data.Constr(0, args) =>
            val slots = args.asScala.toList
            require(
              slots.size == ScheduleFields.size,
              s"config #16 (schedule) has ${slots.size} fields, expected ${ScheduleFields.size}"
            )
            val patched = ScheduleFields.zip(slots).map { case (name, old) =>
                edits.get(name).fold(old)(v => Data.I(v))
            }
            Data.Constr(0, PList.from(patched))
        case other =>
            throw new IllegalArgumentException(
              s"config #16 (schedule) is not a Constr 0 record: $other"
            )
    }

    /** `(index, field name, old, new)` for every field the rewrite changed. */
    def diff(oldFields: List[Data], newFields: List[Data]): List[(Int, String, String, String)] = {
        val name = (i: Int) => FieldNames.lift(i).getOrElse(s"#$i")
        newFields.zipWithIndex.flatMap { case (after, i) =>
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
