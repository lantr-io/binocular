package binocular.cli

import binocular.cli.commands.UpdateConfigCommand
import binocular.cli.commands.UpdateConfigCommand.ParamEdits
import binocular.cli.commands.UpdateConfigCommand.RegistryEdit
import binocular.watchtower.{AuthorizationMethod, ConfigDatum, ConfigParams, DeployedConfig, ScheduleParams}

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude.{List as PList, Option as SOption}
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}

class UpdateConfigCommandTest extends AnyFunSuite {

    /** The deployed rev-5.4 eight-field shape (spec §Config datum): the typed mirror the command
      * decodes, rewrites by name, and re-encodes.
      */
    private def schedule: ScheduleParams = ScheduleParams(
      dkgR1Deadline = 3600,
      dkgR2Deadline = 7200,
      updateYDeadline = 10800,
      tmBatchInterval = 21600,
      signR1Window = 1800,
      signR2Window = 1800,
      leaderSlotT = 600,
      tmRecoveryWindow = 129600,
      finalTmCutoff = 345600,
      stabilityWindow = 129600
    )

    private def config: ConfigDatum = ConfigDatum(
      updateAuth = SOption.Some(AuthorizationMethod.CardanoSignature(ByteString.fromHex("00"))),
      bridgedTokenPolicy = ByteString.fromHex("01"),
      completedPegInsPolicy = ByteString.fromHex("02"),
      bridgeStatePolicy = ByteString.fromHex("03"),
      tmScriptHash = ByteString.fromHex("04"),
      pegInScriptHash = ByteString.fromHex("05"),
      pegOutScriptHash = ByteString.fromHex("06"),
      spoBansPolicyId = ByteString.fromHex("07"),
      sposRegistryPolicyId = ByteString.fromHex("11"),
      treasuryInfoPolicyId = ByteString.fromHex("12"),
      yFederation = ByteString.fromHex("f9" * 32),
      federationOneShot = TxOutRef(TxId(ByteString.fromHex("c3" * 32)), BigInt(0)),
      params = ConfigParams(
        baseBanDurationMs = BigInt(0),
        maxFaultsBeforePermanent = BigInt(0),
        maxValidityWindowMs = BigInt(0),
        federationCsvBlocks = BigInt(144),
        peginRefundTimeoutBlocks = BigInt(720),
        feeRateSatPerVb = 2,
        perPegoutFee = 1000,
        minPegOutFbtc = 10000,
        schedule = schedule
      )
    )

    test("rewrite swaps all four script hashes together in one call") {
        val out = UpdateConfigCommand.rewrite(
          config,
          newBridgeStatePolicy = Some(ByteString.fromHex("33")),
          newTmScriptHash = Some(ByteString.fromHex("44")),
          newPegInHash = Some(ByteString.fromHex("55")),
          newPegOutHash = Some(ByteString.fromHex("66"))
        )
        assert(out.bridgeStatePolicy == ByteString.fromHex("33"))
        assert(out.tmScriptHash == ByteString.fromHex("44"))
        assert(out.pegInScriptHash == ByteString.fromHex("55"))
        assert(out.pegOutScriptHash == ByteString.fromHex("66"))
        // Neighbours of the swapped fields are untouched.
        assert(out.updateAuth == config.updateAuth)
        assert(out.completedPegInsPolicy == config.completedPegInsPolicy)
        assert(out.params == config.params)
    }

    test("rewrite swaps bridge_state_policy alone without disturbing the other hashes") {
        val out = UpdateConfigCommand.rewrite(
          config,
          newBridgeStatePolicy = Some(ByteString.fromHex("33")),
          newTmScriptHash = None,
          newPegInHash = None,
          newPegOutHash = None
        )
        assert(out.bridgeStatePolicy == ByteString.fromHex("33"))
        assert(out.tmScriptHash == config.tmScriptHash)
        assert(out.pegInScriptHash == config.pegInScriptHash)
        assert(out.pegOutScriptHash == config.pegOutScriptHash)
    }

    test("the operational parameters are patched individually") {
        val out = UpdateConfigCommand.rewrite(
          config,
          None,
          None,
          None,
          None,
          ParamEdits(feeRateSatPerVb = Some(BigInt(9)), minPegOutFbtc = Some(BigInt(50_000)))
        )
        assert(out.copy(params = config.params) == config) // only field 7 moved
        assert(out.params.feeRateSatPerVb == BigInt(9))
        assert(out.params.perPegoutFee == BigInt(1000)) // untouched
        assert(out.params.minPegOutFbtc == BigInt(50_000))
        assert(out.params.schedule == schedule) // untouched
    }

    // Governance "replaces the schedule wholesale", but an operator naming one slot must not have
    // the other nine silently reset to whatever this build's defaults happen to be.
    test("a schedule patch keeps the slots it does not name") {
        val out = UpdateConfigCommand.rewrite(
          config,
          None,
          None,
          None,
          None,
          ParamEdits(schedule = Map("tm_batch_interval" -> BigInt(600)))
        )
        assert(out.params.schedule == schedule.copy(tmBatchInterval = 600))
    }

    // A params update and a validator hash swap are both Config Updates; nothing stops an operator
    // doing them in one signed act, and the rewrite must not drop either half.
    test("parameter edits compose with the script hash swaps in one call") {
        val out = UpdateConfigCommand.rewrite(
          config,
          newBridgeStatePolicy = Some(ByteString.fromHex("33")),
          newTmScriptHash = Some(ByteString.fromHex("44")),
          newPegInHash = Some(ByteString.fromHex("55")),
          newPegOutHash = Some(ByteString.fromHex("66")),
          ParamEdits(feeRateSatPerVb = Some(BigInt(9)))
        )
        assert(out.bridgeStatePolicy == ByteString.fromHex("33"))
        assert(out.tmScriptHash == ByteString.fromHex("44"))
        assert(out.pegInScriptHash == ByteString.fromHex("55"))
        assert(out.pegOutScriptHash == ByteString.fromHex("66"))
        assert(out.params.feeRateSatPerVb == BigInt(9))
    }

    test("decodeDeployed round-trips the rev-5.5 datum") {
        val d = UpdateConfigCommand.decodeDeployed(config.toData)
        assert(d == Right(DeployedConfig(config, Nil)))
        assert(d.toOption.get.toData == config.toData)
    }

    /** The rev-5.5 datum with `extra` appended ([CFG-5]). */
    private def grown(extra: Data*): Data = config.toData match {
        case Data.Constr(0, fields) => Data.Constr(0, PList.from(fields.asScala.toList ++ extra))
        case other                  => fail(s"config datum is not a Constr 0: $other")
    }

    // This was a refusal: the command re-encoded the typed record, so a grown datum would have
    // lost its appended fields. It now carries them — which is what lets it write rev 5.6's #13 and
    // keep updating every other field once #13 exists.
    test("an update of a grown datum carries the appended fields through verbatim") {
        val datum = grown(Data.B(ByteString.fromHex("b2" * 28)), Data.I(BigInt(99)))
        val deployed = UpdateConfigCommand.decodeDeployed(datum).toOption.get
        assert(deployed.config == config)
        val edited = deployed.copy(config =
            UpdateConfigCommand.rewrite(
              deployed.config,
              None,
              None,
              None,
              None,
              ParamEdits(schedule = Map("tm_batch_interval" -> BigInt(600)))
            )
        )
        val out = edited.toData match {
            case Data.Constr(0, fs) => fs.asScala.toList
            case other              => fail(s"not a Constr 0: $other")
        }
        assert(out.size == 15)
        assert(out(13) == Data.B(ByteString.fromHex("b2" * 28)))
        assert(out(14) == Data.I(BigInt(99)))
    }

    private val newRegistry = ByteString.fromHex("c7" * 28)

    // spec [CFG-10]: the migration's governance Update. Field 9 moves and field 13 records what it
    // held, in one edit — on a datum written before #13 existed, which is every deployed bridge.
    test("--migrate-registry-to moves field 9 and appends the old registry as field 13") {
        val deployed = UpdateConfigCommand.decodeDeployed(config.toData).toOption.get
        val out = UpdateConfigCommand
            .applyRegistryEdit(deployed, RegistryEdit.MigrateTo(newRegistry), banPolicyMoved = true)
            .toOption
            .get
        assert(out.config.sposRegistryPolicyId == newRegistry)
        assert(out.previousSposRegistryPolicyId == Some(config.sposRegistryPolicyId))
        val d = UpdateConfigCommand.diff(config.toData, out.toData).map(x => (x._1, x._2)).toSet
        assert(d == Set((9, "spos_registry_policy_id"), (13, "previous_spos_registry_policy_id")))
    }

    // Field 13 must be what field 9 held, so the edit takes it from the datum rather than from the
    // operator; and a second migration cannot begin while the first is still recorded.
    test("a migration cannot begin while one is in progress, and ending one empties field 13") {
        val inProgress =
            UpdateConfigCommand.decodeDeployed(grown(Data.B(config.sposRegistryPolicyId))).toOption.get
        val again = UpdateConfigCommand.applyRegistryEdit(
          inProgress,
          RegistryEdit.MigrateTo(newRegistry),
          banPolicyMoved = true
        )
        assert(again.isLeft && again.swap.toOption.get.contains("already in progress"))

        val ended = UpdateConfigCommand
            .applyRegistryEdit(inProgress, RegistryEdit.EndMigration, banPolicyMoved = false)
            .toOption
            .get
        assert(ended.previousSposRegistryPolicyId.isEmpty)
        assert(ended.appended == List(Data.B(ByteString.empty)), "emptied, never removed ([CFG-5])")

        // Once ended, the next migration may begin.
        assert(
          UpdateConfigCommand
              .applyRegistryEdit(ended, RegistryEdit.MigrateTo(newRegistry), banPolicyMoved = true)
              .isRight
        )
    }

    test("a registry migration is refused without the ban policy, and when there is none to end") {
        val deployed = UpdateConfigCommand.decodeDeployed(config.toData).toOption.get
        val noBans = UpdateConfigCommand.applyRegistryEdit(
          deployed,
          RegistryEdit.MigrateTo(newRegistry),
          banPolicyMoved = false
        )
        assert(noBans.isLeft && noBans.swap.toOption.get.contains("--spo-bans-policy"))
        val same = UpdateConfigCommand.applyRegistryEdit(
          deployed,
          RegistryEdit.MigrateTo(config.sposRegistryPolicyId),
          banPolicyMoved = true
        )
        assert(same.isLeft)
        assert(
          UpdateConfigCommand
              .applyRegistryEdit(deployed, RegistryEdit.EndMigration, banPolicyMoved = false)
              .isLeft
        )
    }

    test("decodeDeployed refuses short and non-record datums") {
        val five = config.toData match {
            case Data.Constr(0, fields) =>
                Data.Constr(0, PList.from(fields.asScala.toList.take(5)))
            case other => fail(s"config datum is not a Constr 0: $other")
        }
        assert(UpdateConfigCommand.decodeDeployed(five).isLeft)
        assert(UpdateConfigCommand.decodeDeployed(Data.I(BigInt(0))).isLeft)
    }

    test("parseSchedule accepts known slot names and rejects the rest") {
        assert(
          ParamEdits.parseSchedule(List("tm_batch_interval=600", "leader_slot_t=30")) ==
              Right(Map("tm_batch_interval" -> BigInt(600), "leader_slot_t" -> BigInt(30)))
        )
        assert(ParamEdits.parseSchedule(List("tm_batch=600")).isLeft) // misspelled
        assert(ParamEdits.parseSchedule(List("tm_batch_interval")).isLeft) // no value
        assert(ParamEdits.parseSchedule(List("tm_batch_interval=-1")).isLeft) // negative
    }

    test("diff reports every changed field by name") {
        val out = UpdateConfigCommand.rewrite(
          config,
          Some(ByteString.fromHex("33")),
          None,
          None,
          None,
          ParamEdits(feeRateSatPerVb = Some(BigInt(9)))
        )
        val d = UpdateConfigCommand.diff(config.toData, out.toData)
        // Rev 5.5 indexes: params moved to 1, so bridge_state_policy is 4.
        assert(d.map(x => (x._1, x._2)).toSet == Set((4, "bridge_state_policy"), (1, "params")))
    }
}
