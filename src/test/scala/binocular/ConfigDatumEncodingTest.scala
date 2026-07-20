package binocular

import binocular.watchtower.*

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude.{List as PList, Option as POption}
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData

/** Pins the Scala mirror of `lib/bifrost/types/config.ak::ConfigDatum` to the on-chain positional
  * encoding: 17 Constr-0 fields, `update_auth` (10) as Aiken `Option` (Some = Constr 0 [v], None =
  * Constr 1 []), `initial_btc_treasury_utxo` (11) as bytes, then the operational-parameter tunables
  * (12–15 Ints, 16 the nested ScheduleParams Constr).
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

    test("ConfigDatum has 17 positional fields; update_auth is 10, anchor 11, tunables 12–16") {
        val anchor = ByteString.fromHex("bb" * 32 + "01000000")
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
        val d = ConfigDatum(
          bridgedTokenPolicyId = ByteString.fromHex("aa" * 28),
          bridgedTokenAssetName = ByteString.fromString("fSAT"),
          completedPegInsMerkleTreePolicyId = ByteString.empty,
          completedPegOutsMerkleTreePolicyId = ByteString.empty,
          pegInWithdrawScriptHash = ByteString.empty,
          pegOutWithdrawScriptHash = ByteString.empty,
          pegInCloseVerifierScriptHash = ByteString.empty,
          legitTmAndPegOutProducedVerifierScriptHash = ByteString.empty,
          legitTmAndPegOutNotProducedVerifierScriptHash = ByteString.empty,
          minStake = BigInt(0),
          updateAuth = POption.None,
          initialBtcTreasuryUtxo = anchor,
          feeRateSatPerVb = BigInt(1),
          perPegoutFee = BigInt(1000),
          minPegOutFbtc = BigInt(10000),
          leaderReward = BigInt(2000000),
          schedule = sched
        )
        d.toData match {
            case Data.Constr(0, fields) =>
                val fs = fields.asScala.toIndexedSeq
                assert(fs.size == 17)
                assert(fs(10) == Data.Constr(1, PList()))
                assert(fs(11) == Data.B(anchor))
                assert(fs(15) == Data.I(BigInt(2000000)))
                assert(fs(16) == sched.toData)
            case other => fail(s"expected Constr 0, got $other")
        }
    }

    test("outpointFromDisplay reverses the txid and encodes vout LE") {
        val display = "00" + "11" * 31 // display txid: 00 first...
        val op = BridgeConfig.outpointFromDisplay(s"$display:1")
        // ...so internal order ENDS with 00, and starts with the last display byte (11).
        assert(op.toHex == "11" * 31 + "00" + "01000000")
    }
}
