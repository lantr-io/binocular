package binocular.cli

import binocular.cli.commands.UpdateConfigCommand
import binocular.cli.commands.UpdateConfigCommand.ParamEdits
import binocular.watchtower.{AuthorizationMethod, ConfigDatum, ConfigParams, ScheduleParams}

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude.{List as PList, Option as SOption}
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData

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
      params = ConfigParams(
        feeRateSatPerVb = 2,
        perPegoutFee = 1000,
        minPegOutFbtc = 10000,
        baseBanDurationMs = 600000,
        maxFaultsBeforePermanent = 3,
        maxValidityWindowMs = 3600000,
        federationCsvBlocks = BigInt(0),
        schedule = schedule
      ),
      yFederation = ByteString.fromHex("13")
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
        assert(UpdateConfigCommand.decodeDeployed(config.toData) == Right(config))
    }

    // Appending is the legal datum evolution (config.ak's Update accepts any datum), and READERS
    // ignore unknown trailing fields — but this command re-encodes the whole datum, so rewriting a
    // grown datum would silently truncate it. It must be refused, not carried.
    test("decodeDeployed refuses a datum that grew past the rev-5.5 layout") {
        val thirteen = config.toData match {
            case Data.Constr(0, fields) =>
                Data.Constr(0, PList.from(fields.asScala.toList :+ (Data.I(BigInt(99)): Data)))
            case other => fail(s"config datum is not a Constr 0: $other")
        }
        val out = UpdateConfigCommand.decodeDeployed(thirteen)
        assert(out.isLeft)
        assert(out.swap.toOption.get.contains("13 fields"))
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
        // Rev 5.5 indices: bridge_state_policy moved 3 -> 4 and params 14 -> 1.
        assert(d.map(x => (x._1, x._2)).toSet == Set((4, "bridge_state_policy"), (1, "params")))
    }
}
