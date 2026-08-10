package binocular.cli

import binocular.cli.commands.UpdateConfigCommand
import binocular.cli.commands.UpdateConfigCommand.ParamEdits

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.uplc.builtin.{ByteString, Data}

class UpdateConfigCommandTest extends AnyFunSuite {

    /** The deployed rev-5.4 eight-field shape (spec §Config datum): fields 0-6 as distinct byte
      * strings, field 7 the nested ConfigParams record (3 Ints + the nested ScheduleParams).
      */
    private def scheduleSlots: Data = Data.Constr(
      0,
      PList.from(
        List(3600, 7200, 10800, 21600, 1800, 1800, 600, 129600, 345600, 129600)
            .map(v => Data.I(BigInt(v)): Data)
      )
    )

    private def paramsRecord: Data = Data.Constr(
      0,
      PList.from(
        List[Data](
          Data.I(BigInt(2)), // params[0] fee_rate_sat_per_vb
          Data.I(BigInt(1000)), // params[1] per_pegout_fee floor
          Data.I(BigInt(10000)), // params[2] min_peg_out_fbtc
          scheduleSlots // params[3] schedule
        )
      )
    )

    private def fields8: List[Data] =
        (0 to 6).map(i => Data.B(ByteString.fromHex(f"$i%02x")): Data).toList :+ paramsRecord

    test("rewriteFields swaps fields 3, 4, 5 and 6 together in one call") {
        val out = UpdateConfigCommand.rewriteFields(
          fields8,
          newBridgeStatePolicy = Some(ByteString.fromHex("33")),
          newTmScriptHash = Some(ByteString.fromHex("44")),
          newPegInHash = Some(ByteString.fromHex("55")),
          newPegOutHash = Some(ByteString.fromHex("66"))
        )
        assert(out.size == 8)
        assert(out(3) == Data.B(ByteString.fromHex("33")))
        assert(out(4) == Data.B(ByteString.fromHex("44")))
        assert(out(5) == Data.B(ByteString.fromHex("55")))
        assert(out(6) == Data.B(ByteString.fromHex("66")))
        // Neighbours of the swapped fields are untouched.
        assert(out(0) == fields8(0) && out(2) == fields8(2) && out(7) == fields8(7))
    }

    test("rewriteFields swaps field 3 alone without disturbing 4, 5 or 6") {
        val out = UpdateConfigCommand.rewriteFields(
          fields8,
          newBridgeStatePolicy = Some(ByteString.fromHex("33")),
          newTmScriptHash = None,
          newPegInHash = None,
          newPegOutHash = None
        )
        assert(out(3) == Data.B(ByteString.fromHex("33")))
        assert(out(4) == fields8(4) && out(5) == fields8(5) && out(6) == fields8(6))
    }

    test("rewriteFields rejects short datums") {
        intercept[IllegalArgumentException] {
            UpdateConfigCommand.rewriteFields(
              fields8.take(5),
              Some(ByteString.fromHex("33")),
              None,
              None,
              None
            )
        }
    }

    test("the operational parameters are patched individually inside field 7") {
        val out = UpdateConfigCommand.rewriteFields(
          fields8,
          None,
          None,
          None,
          None,
          ParamEdits(feeRateSatPerVb = Some(BigInt(9)), minPegOutFbtc = Some(BigInt(50_000)))
        )
        assert(out.size == 8)
        // Fields 0-6 are byte-identical.
        assert((0 to 6).forall(i => out(i) == fields8(i)))
        out(7) match {
            case Data.Constr(0, args) =>
                val slots = args.asScala.toList
                assert(slots(0) == Data.I(BigInt(9)))
                assert(slots(1) == Data.I(BigInt(1000))) // untouched
                assert(slots(2) == Data.I(BigInt(50_000)))
                assert(slots(3) == scheduleSlots) // untouched
            case other => fail(s"params is not a Constr 0: $other")
        }
    }

    // Governance "replaces the schedule wholesale", but an operator naming one slot must not have
    // the other nine silently reset to whatever this build's defaults happen to be.
    test("a schedule patch keeps the slots it does not name") {
        val out = UpdateConfigCommand.rewriteFields(
          fields8,
          None,
          None,
          None,
          None,
          ParamEdits(schedule = Map("tm_batch_interval" -> BigInt(600)))
        )
        out(7) match {
            case Data.Constr(0, args) =>
                args.asScala.toList(3) match {
                    case Data.Constr(0, sched) =>
                        val slots = sched.asScala.toList
                        assert(slots(3) == Data.I(BigInt(600))) // tm_batch_interval
                        assert(slots(0) == Data.I(BigInt(3600))) // dkg_r1_deadline, untouched
                        assert(slots(9) == Data.I(BigInt(129600))) // stability_window, untouched
                    case other => fail(s"schedule is not a Constr 0: $other")
                }
            case other => fail(s"params is not a Constr 0: $other")
        }
    }

    test("a params edit against a malformed params field is refused") {
        val broken = fields8.updated(7, Data.I(BigInt(0)): Data)
        intercept[IllegalArgumentException] {
            UpdateConfigCommand.rewriteFields(
              broken,
              None,
              None,
              None,
              None,
              ParamEdits(feeRateSatPerVb = Some(BigInt(9)))
            )
        }
    }

    // Appending is the legal datum evolution (config.ak's Update accepts any datum), so a datum
    // that grew past 8 fields must survive a rewrite untouched beyond field 7.
    test("rewriteFields carries appended fields beyond 7 over verbatim") {
        val nine = fields8 :+ (Data.I(BigInt(99)): Data)
        val out = UpdateConfigCommand.rewriteFields(
          nine,
          Some(ByteString.fromHex("33")),
          None,
          None,
          None
        )
        assert(out.size == 9)
        assert(out(3) == Data.B(ByteString.fromHex("33")))
        assert(out(8) == Data.I(BigInt(99)))
    }

    // A params update and a validator hash swap are both Config Updates; nothing stops an operator
    // doing them in one signed act, and the rewrite must not drop either half.
    test("parameter edits compose with the field 3-6 swaps in one call") {
        val out = UpdateConfigCommand.rewriteFields(
          fields8,
          newBridgeStatePolicy = Some(ByteString.fromHex("33")),
          newTmScriptHash = Some(ByteString.fromHex("44")),
          newPegInHash = Some(ByteString.fromHex("55")),
          newPegOutHash = Some(ByteString.fromHex("66")),
          ParamEdits(feeRateSatPerVb = Some(BigInt(9)))
        )
        assert(out(3) == Data.B(ByteString.fromHex("33")))
        assert(out(4) == Data.B(ByteString.fromHex("44")))
        assert(out(5) == Data.B(ByteString.fromHex("55")))
        assert(out(6) == Data.B(ByteString.fromHex("66")))
        out(7) match {
            case Data.Constr(0, args) => assert(args.asScala.toList(0) == Data.I(BigInt(9)))
            case other                => fail(s"params is not a Constr 0: $other")
        }
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
        val out = UpdateConfigCommand.rewriteFields(
          fields8,
          Some(ByteString.fromHex("33")),
          None,
          None,
          None,
          ParamEdits(feeRateSatPerVb = Some(BigInt(9)))
        )
        val d = UpdateConfigCommand.diff(fields8, out)
        assert(d.map(x => (x._1, x._2)).toSet == Set((3, "bridge_state_policy"), (7, "params")))
    }
}
