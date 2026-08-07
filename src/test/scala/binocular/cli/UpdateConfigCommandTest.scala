package binocular.cli

import binocular.cli.commands.UpdateConfigCommand
import binocular.cli.commands.UpdateConfigCommand.ParamEdits

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.uplc.builtin.{ByteString, Data}

class UpdateConfigCommandTest extends AnyFunSuite {

    private def fields11: List[Data] =
        (0 to 10).map(i => Data.B(ByteString.fromHex(f"$i%02x")): Data).toList

    test("rewriteFields appends field 11 and swaps field 4 on an 11-field datum") {
        val out = UpdateConfigCommand.rewriteFields(
          fields11,
          newCpoPolicy = None,
          newPegInHash = Some(ByteString.fromHex("ff")),
          newPegOutHash = None,
          anchor = Some(ByteString.fromHex("ee"))
        )
        assert(out.size == 12)
        assert(out(4) == Data.B(ByteString.fromHex("ff")))
        assert(out(11) == Data.B(ByteString.fromHex("ee")))
        // Untouched fields carried over verbatim.
        assert(out(0) == fields11(0) && out(10) == fields11(10))
    }

    test("rewriteFields replaces field 11 on a 12-field datum (re-anchoring)") {
        val twelve = fields11 :+ (Data.B(ByteString.fromHex("aa")): Data)
        val out = UpdateConfigCommand.rewriteFields(
          twelve,
          None,
          None,
          None,
          Some(ByteString.fromHex("bb"))
        )
        assert(out.size == 12)
        assert(out(4) == fields11(4)) // no swap requested
        assert(out(11) == Data.B(ByteString.fromHex("bb")))
    }

    test("rewriteFields rejects short datums") {
        intercept[IllegalArgumentException] {
            UpdateConfigCommand.rewriteFields(
              fields11.take(5),
              None,
              None,
              None,
              Some(ByteString.fromHex("bb"))
            )
        }
    }

    // The peg-out trie v2 migration flips fields 3, 4 and 5 in ONE Update transaction: a partial
    // swap leaves TM Confirm reading a field-3 policy whose trie UTxO does not exist.
    test("rewriteFields swaps fields 3, 4 and 5 together in one call") {
        val out = UpdateConfigCommand.rewriteFields(
          fields11,
          newCpoPolicy = Some(ByteString.fromHex("33")),
          newPegInHash = Some(ByteString.fromHex("44")),
          newPegOutHash = Some(ByteString.fromHex("55")),
          anchor = Some(ByteString.fromHex("ee"))
        )
        assert(out.size == 12)
        assert(out(3) == Data.B(ByteString.fromHex("33")))
        assert(out(4) == Data.B(ByteString.fromHex("44")))
        assert(out(5) == Data.B(ByteString.fromHex("55")))
        assert(out(11) == Data.B(ByteString.fromHex("ee")))
        // Neighbours of the swapped fields are untouched.
        assert(out(2) == fields11(2) && out(6) == fields11(6))
    }

    test("rewriteFields swaps field 3 alone without disturbing 4 or 5") {
        val out = UpdateConfigCommand.rewriteFields(
          fields11,
          newCpoPolicy = Some(ByteString.fromHex("33")),
          newPegInHash = None,
          newPegOutHash = None,
          anchor = Some(ByteString.fromHex("ee"))
        )
        assert(out(3) == Data.B(ByteString.fromHex("33")))
        assert(out(4) == fields11(4))
        assert(out(5) == fields11(5))
    }

    // The deployed config carries the 17-field ConfigDatum; the migration Update must not truncate
    // the tunables (#12-16) while swapping the hashes.
    // Field 11 re-anchors the whole TM chain, so a hash-only migration must not touch it.
    test("rewriteFields leaves field 11 untouched when no anchor is given") {
        val twelve = fields11 :+ (Data.B(ByteString.fromHex("aa")): Data)
        val out = UpdateConfigCommand.rewriteFields(
          twelve,
          newCpoPolicy = Some(ByteString.fromHex("33")),
          newPegInHash = None,
          newPegOutHash = Some(ByteString.fromHex("55")),
          anchor = None
        )
        assert(out.size == 12)
        assert(out(3) == Data.B(ByteString.fromHex("33")))
        assert(out(5) == Data.B(ByteString.fromHex("55")))
        assert(out(11) == Data.B(ByteString.fromHex("aa"))) // the deployed anchor survives
    }

    test("rewriteFields does not append field 11 to an 11-field datum when no anchor is given") {
        val out = UpdateConfigCommand.rewriteFields(
          fields11,
          newCpoPolicy = Some(ByteString.fromHex("33")),
          newPegInHash = None,
          newPegOutHash = None,
          anchor = None
        )
        assert(out.size == 11)
        assert(out(3) == Data.B(ByteString.fromHex("33")))
    }

    test("rewriteFields carries fields beyond 11 over verbatim") {
        val seventeen =
            fields11 ++ (11 to 16).map(i => Data.B(ByteString.fromHex(f"$i%02x")): Data).toList
        val out = UpdateConfigCommand.rewriteFields(
          seventeen,
          newCpoPolicy = Some(ByteString.fromHex("33")),
          newPegInHash = None,
          newPegOutHash = Some(ByteString.fromHex("55")),
          anchor = Some(ByteString.fromHex("ee"))
        )
        assert(out.size == 17)
        assert(out(11) == Data.B(ByteString.fromHex("ee")))
        assert((12 to 16).forall(i => out(i) == seventeen(i)))
    }

    /** The deployed 17-field shape: #0-#11 plus the operational-parameter append. Field 9
      * (min_stake) and #12-#15 are Ints; #16 is the nested ScheduleParams record.
      */
    private def fields17: List[Data] = {
        val slots: Data = Data.Constr(
          0,
          PList.from(
            List(3600, 7200, 10800, 21600, 1800, 1800, 600, 129600, 345600, 129600)
                .map(v => Data.I(BigInt(v)): Data)
          )
        )
        fields11.updated(9, Data.I(BigInt(0))) ++ List[Data](
          Data.B(ByteString.fromHex("11" * 36)), // #11 anchor
          Data.I(BigInt(2)), // #12 fee_rate
          Data.I(BigInt(1000)), // #13 per_pegout_fee floor
          Data.I(BigInt(10000)), // #14 min_peg_out_fbtc
          Data.I(BigInt(2000000)), // #15 leader_reward
          slots // #16 schedule
        )
    }

    // The reason the parameter flags exist: min_stake is written as 0 by every deploy before the
    // key existed, and it is the protocol's value — raising it is a governance act, not a per-SPO
    // config edit.
    test("min_stake alone updates field 9 and re-anchors nothing") {
        val out = UpdateConfigCommand.rewriteFields(
          fields17,
          None,
          None,
          None,
          anchor = None,
          ParamEdits(minStake = Some(BigInt(60_000_000)))
        )
        assert(out.size == 17)
        assert(out(9) == Data.I(BigInt(60_000_000)))
        // Everything else — anchor included — is byte-identical.
        assert(out.zipWithIndex.forall { case (d, i) => i == 9 || d == fields17(i) })
    }

    test("the operational parameters #12-#15 are set individually") {
        val out = UpdateConfigCommand.rewriteFields(
          fields17,
          None,
          None,
          None,
          None,
          ParamEdits(feeRateSatPerVb = Some(BigInt(9)), minPegOutFbtc = Some(BigInt(50_000)))
        )
        assert(out(12) == Data.I(BigInt(9)))
        assert(out(14) == Data.I(BigInt(50_000)))
        assert(out(13) == fields17(13) && out(15) == fields17(15)) // untouched
        assert(out(16) == fields17(16))
    }

    // Governance "replaces the schedule wholesale", but an operator naming one slot must not have
    // the other nine silently reset to whatever this build's defaults happen to be.
    test("a schedule patch keeps the slots it does not name") {
        val out = UpdateConfigCommand.rewriteFields(
          fields17,
          None,
          None,
          None,
          None,
          ParamEdits(schedule = Map("tm_batch_interval" -> BigInt(600)))
        )
        out(16) match {
            case Data.Constr(0, args) =>
                val slots = args.asScala.toList
                assert(slots(3) == Data.I(BigInt(600))) // tm_batch_interval
                assert(slots(0) == Data.I(BigInt(3600))) // dkg_r1_deadline, untouched
                assert(slots(9) == Data.I(BigInt(129600))) // stability_window, untouched
            case other => fail(s"schedule is not a Constr 0: $other")
        }
    }

    // A partial append is a datum `config.config`'s full cast rejects, and inventing values for
    // the fields the operator did not name is not governance.
    test("editing #12-#16 on a pre-append datum is refused") {
        val twelve = fields11 :+ (Data.B(ByteString.fromHex("aa")): Data)
        val err = intercept[IllegalArgumentException] {
            UpdateConfigCommand.rewriteFields(
              twelve,
              None,
              None,
              None,
              None,
              ParamEdits(feeRateSatPerVb = Some(BigInt(9)))
            )
        }
        assert(err.getMessage.contains("#12-#16"))
        // …while min_stake (#9) exists on every datum and stays editable.
        val ok = UpdateConfigCommand
            .rewriteFields(twelve, None, None, None, None, ParamEdits(minStake = Some(BigInt(5))))
        assert(ok(9) == Data.I(BigInt(5)))
    }

    // A params update and the trie v2 hash swaps are both Config Updates; nothing stops an
    // operator doing them in one signed act, and the rewrite must not drop either half.
    test("parameter edits compose with the field 3/4/5 swaps in one call") {
        val out = UpdateConfigCommand.rewriteFields(
          fields17,
          newCpoPolicy = Some(ByteString.fromHex("33")),
          newPegInHash = Some(ByteString.fromHex("44")),
          newPegOutHash = Some(ByteString.fromHex("55")),
          anchor = None,
          ParamEdits(feeRateSatPerVb = Some(BigInt(9)))
        )
        assert(out(3) == Data.B(ByteString.fromHex("33")))
        assert(out(4) == Data.B(ByteString.fromHex("44")))
        assert(out(5) == Data.B(ByteString.fromHex("55")))
        assert(out(12) == Data.I(BigInt(9)))
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
          fields17,
          None,
          None,
          None,
          None,
          ParamEdits(minStake = Some(BigInt(7)), feeRateSatPerVb = Some(BigInt(9)))
        )
        val changed = UpdateConfigCommand.diff(fields17, out)
        assert(changed.map(_._1) == List(9, 12))
        assert(changed.map(_._2) == List("min_stake", "fee_rate_sat_per_vb"))
        assert(changed.head._3 == "0" && changed.head._4 == "7")
    }

    test("ParamEdits.none is empty and touches no tunables") {
        assert(ParamEdits.none.isEmpty && !ParamEdits.none.touchesTunables)
        assert(!ParamEdits(minStake = Some(BigInt(1))).isEmpty)
        assert(ParamEdits(schedule = Map("leader_slot_t" -> BigInt(1))).touchesTunables)
    }
}
