package binocular

import binocular.watchtower.*

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude.{List as PList, Option as POption}
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData

/** Pins the Scala mirror of `lib/bifrost/types/config.ak::ConfigDatum` to the on-chain positional
  * encoding (rev 5.5, spec §Config datum): TWELVE Constr-0 fields — 0 `update_auth` (Aiken
  * `Option`: Some = Constr 0 [v], None = Constr 1 []), 1 `params` (nested ConfigParams), 2
  * `bridged_token_policy`, 3 `completed_peg_ins_policy`, 4 `bridge_state_policy`, 5
  * `tm_script_hash` ([CFG-2]), 6 `peg_in_script_hash`, 7 `peg_out_script_hash`, 8
  * `spo_bans_policy_id`, 9 `spos_registry_policy_id`, 10 `treasury_info_policy_id`, 11
  * `y_federation`.
  *
  * The indices are the whole point: `config[N]` reads on the bridge validators line up with them,
  * so a mirror that drifts by one position is a bridge whose every reader looks at the wrong field.
  * Rev 5.5 renumbered ALL of them by moving `params` from last to index 1 ([CFG-5]) — the rev-5.4
  * layout put it last and told the reader to append after it, which invites exactly the edit that
  * breaks every index: insert before `params` to keep it last.
  */
class ConfigDatumEncodingTest extends AnyFunSuite {

    test("updateAuth Some/None encode as Constr 0 [v] / Constr 1 [] (Aiken Option)") {
        val some: POption[AuthorizationMethod] =
            POption.Some(AuthorizationMethod.CardanoSignature(ByteString.fromHex("aa")))
        val none: POption[AuthorizationMethod] = POption.None
        assert(some.toData match {
            case Data.Constr(0, fields) => fields.asScala.size == 1
            case _                      => false
        })
        assert(none.toData == Data.Constr(1, PList()))
    }

    test("ConfigDatum has 12 positional fields; params is 1, tm_script_hash 5, y_federation 11") {
        val sched = ScheduleParams(
          dkgR1Deadline = BigInt(3600),
          dkgR2Deadline = BigInt(7200),
          updateYDeadline = BigInt(10800),
          tmBatchInterval = BigInt(21600),
          signR1Window = BigInt(1800),
          signR2Window = BigInt(1800),
          leaderSlotT = BigInt(600),
          tmRecoveryWindow = BigInt(129600),
          finalTmCutoff = BigInt(345600),
          stabilityWindow = BigInt(129600)
        )
        val params = ConfigParams(
          schedule = sched,
          feeRateSatPerVb = BigInt(1),
          perPegoutFee = BigInt(1000),
          minPegOutFbtc = BigInt(10000),
          baseBanDurationMs = BigInt(600000),
          maxFaultsBeforePermanent = BigInt(3),
          maxValidityWindowMs = BigInt(3600000),
          federationCsvBlocks = BigInt(144)
        )
        val tmHash = ByteString.fromHex("dd" * 28)
        val yFed = ByteString.fromHex("f0" * 32)
        val d = ConfigDatum(
          updateAuth = POption.None,
          params = params,
          bridgedTokenPolicy = ByteString.fromHex("aa" * 28),
          completedPegInsPolicy = ByteString.fromHex("bb" * 28),
          bridgeStatePolicy = ByteString.fromHex("cc" * 28),
          tmScriptHash = tmHash,
          pegInScriptHash = ByteString.fromHex("ee" * 28),
          pegOutScriptHash = ByteString.fromHex("ff" * 28),
          spoBansPolicyId = ByteString.fromHex("b8" * 28),
          sposRegistryPolicyId = ByteString.fromHex("c1" * 28),
          treasuryInfoPolicyId = ByteString.fromHex("c2" * 28),
          yFederation = yFed
        )
        d.toData match {
            case Data.Constr(0, fields) =>
                val fs = fields.asScala.toIndexedSeq
                assert(fs.size == 12)
                assert(fs(0) == Data.Constr(1, PList()))
                // params at 1, and frozen there — appends land at the tail instead of moving it.
                assert(fs(1) == params.toData)
                assert(fs(2) == Data.B(ByteString.fromHex("aa" * 28)))
                assert(fs(4) == Data.B(ByteString.fromHex("cc" * 28)))
                assert(fs(5) == Data.B(tmHash))
                assert(fs(7) == Data.B(ByteString.fromHex("ff" * 28)))
                // Federation identity, 8-11 (spec [CFG-3]).
                assert(fs(8) == Data.B(ByteString.fromHex("b8" * 28)))
                assert(fs(9) == Data.B(ByteString.fromHex("c1" * 28)))
                assert(fs(10) == Data.B(ByteString.fromHex("c2" * 28)))
                assert(fs(11) == Data.B(yFed))
            case other => fail(s"expected Constr 0, got $other")
        }
    }

    test("ConfigParams nests the schedule at slot 0, and the ban schedule follows the tunables") {
        val sched = ScheduleParams(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        val params = ConfigParams(
          schedule = sched,
          feeRateSatPerVb = BigInt(1),
          perPegoutFee = BigInt(2),
          minPegOutFbtc = BigInt(3),
          baseBanDurationMs = BigInt(4),
          maxFaultsBeforePermanent = BigInt(5),
          maxValidityWindowMs = BigInt(6),
          federationCsvBlocks = BigInt(7)
        )
        params.toData match {
            case Data.Constr(0, fields) =>
                val fs = fields.asScala.toIndexedSeq
                assert(fs.size == 8)
                // schedule first, and frozen there for the same reason params is ConfigDatum[1].
                assert(fs(0) == sched.toData)
                assert(fs(1) == Data.I(BigInt(1)))
                assert(fs(2) == Data.I(BigInt(2)))
                assert(fs(3) == Data.I(BigInt(3)))
                // The ban schedule moved in from the datum body in rev 5.5 ([CFG-6]).
                assert(fs(4) == Data.I(BigInt(4)))
                assert(fs(5) == Data.I(BigInt(5)))
                assert(fs(6) == Data.I(BigInt(6)))
                assert(fs(7) == Data.I(BigInt(7)))
            case other => fail(s"expected Constr 0, got $other")
        }
    }
}
