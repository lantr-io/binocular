package binocular

import binocular.watchtower.*

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude.{List as PList, Option as POption}
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData

/** Pins the Scala mirror of `lib/bifrost/types/config.ak::ConfigDatum` to the on-chain positional
  * encoding (rev 5.4, spec §Config datum): 8 Constr-0 fields — 0 `update_auth` (Aiken `Option`:
  * Some = Constr 0 [v], None = Constr 1 []), 1 `bridged_token_policy`, 2
  * `completed_peg_ins_policy`, 3 `bridge_state_policy`, 4 `tm_script_hash` ([CFG-2]), 5
  * `peg_in_script_hash`, 6 `peg_out_script_hash`, 7 `params` (nested ConfigParams: 3 Ints then the
  * nested ScheduleParams Constr).
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

    test("ConfigDatum has 8 positional fields; update_auth is 0, tm_script_hash 4, params 7") {
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
          feeRateSatPerVb = BigInt(1),
          perPegoutFee = BigInt(1000),
          minPegOutFbtc = BigInt(10000),
          schedule = sched
        )
        val tmHash = ByteString.fromHex("dd" * 28)
        val d = ConfigDatum(
          updateAuth = POption.None,
          bridgedTokenPolicy = ByteString.fromHex("aa" * 28),
          completedPegInsPolicy = ByteString.fromHex("bb" * 28),
          bridgeStatePolicy = ByteString.fromHex("cc" * 28),
          tmScriptHash = tmHash,
          pegInScriptHash = ByteString.fromHex("ee" * 28),
          pegOutScriptHash = ByteString.fromHex("ff" * 28),
          params = params
        )
        d.toData match {
            case Data.Constr(0, fields) =>
                val fs = fields.asScala.toIndexedSeq
                assert(fs.size == 8)
                assert(fs(0) == Data.Constr(1, PList()))
                assert(fs(1) == Data.B(ByteString.fromHex("aa" * 28)))
                assert(fs(3) == Data.B(ByteString.fromHex("cc" * 28)))
                assert(fs(4) == Data.B(tmHash))
                assert(fs(6) == Data.B(ByteString.fromHex("ff" * 28)))
                assert(fs(7) == params.toData)
            case other => fail(s"expected Constr 0, got $other")
        }
    }

    test("ConfigParams nests the schedule at slot 3") {
        val sched = ScheduleParams(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        val params = ConfigParams(BigInt(1), BigInt(2), BigInt(3), sched)
        params.toData match {
            case Data.Constr(0, fields) =>
                val fs = fields.asScala.toIndexedSeq
                assert(fs.size == 4)
                assert(fs(0) == Data.I(BigInt(1)))
                assert(fs(1) == Data.I(BigInt(2)))
                assert(fs(2) == Data.I(BigInt(3)))
                assert(fs(3) == sched.toData)
            case other => fail(s"expected Constr 0, got $other")
        }
    }

    test("the bridged-token asset name is the [CFG-1] constant \"fSAT\", not a datum field") {
        assert(ConfigDatum.BridgedTokenAssetName == ByteString.fromString("fSAT"))
    }

    test("outpointFromDisplay reverses the txid and encodes vout LE") {
        val display = "00" + "11" * 31 // display txid: 00 first...
        val op = BridgeConfig.outpointFromDisplay(s"$display:1")
        // ...so internal order ENDS with 00, and starts with the last display byte (11).
        assert(op.toHex == "11" * 31 + "00" + "01000000")
    }
}
