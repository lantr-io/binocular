package binocular

import binocular.watchtower.*

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude.{List as PList, Option as POption}
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData

/** Pins the Scala mirror of `lib/bifrost/types/config.ak::ConfigDatum` to the on-chain positional
  * encoding (rev 5.5, spec §Config datum): 13 Constr-0 fields — 0 `update_auth` (Aiken `Option`:
  * Some = Constr 0 [v], None = Constr 1 []), 1 `params` (nested ConfigParams, frozen at index 1 by
  * [CFG-5] so an append can never move it), 2 `bridged_token_policy`, 3 `completed_peg_ins_policy`,
  * 4 `bridge_state_policy`, 5 `tm_script_hash` ([CFG-2]), 6 `peg_in_script_hash`, 7
  * `peg_out_script_hash`, then the federation identity 8-12: `spo_bans_policy_id`,
  * `spos_registry_policy_id`, `treasury_info_policy_id`, `y_federation`, `federation_one_shot`.
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

    test("ConfigDatum has 13 positional fields; update_auth is 0, params 1, one-shot 12") {
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
          federationCsvBlocks = BigInt(144),
          peginRefundTimeoutBlocks = BigInt(720)
        )
        val tmHash = ByteString.fromHex("dd" * 28)
        val d = ConfigDatum(
          updateAuth = POption.None,
          params = params,
          bridgedTokenPolicy = ByteString.fromHex("aa" * 28),
          completedPegInsPolicy = ByteString.fromHex("bb" * 28),
          bridgeStatePolicy = ByteString.fromHex("cc" * 28),
          tmScriptHash = tmHash,
          pegInScriptHash = ByteString.fromHex("ee" * 28),
          pegOutScriptHash = ByteString.fromHex("ff" * 28),
          spoBansPolicyId = ByteString.fromHex("bb" * 28),
          sposRegistryPolicyId = ByteString.fromHex("c1" * 28),
          treasuryInfoPolicyId = ByteString.fromHex("c2" * 28),
          yFederation = ByteString.fromHex("f9" * 32),
          federationOneShot = TxOutRef(TxId(ByteString.fromHex("c3" * 32)), BigInt(7))
        )
        d.toData match {
            case Data.Constr(0, fields) =>
                val fs = fields.asScala.toIndexedSeq
                assert(fs.size == 13)
                assert(fs(0) == Data.Constr(1, PList()))
                // params at index 1 ([CFG-5]), so an append can never move it.
                assert(fs(1) == params.toData)
                assert(fs(2) == Data.B(ByteString.fromHex("aa" * 28)))
                // #4 bridge_state_policy: the field TreasuryMovementValidator reads. Rev 5.5 moved
                // it from #3, and a mirror that missed the shift would silently hand the CPI trie
                // policy to the singleton lookup.
                assert(fs(4) == Data.B(ByteString.fromHex("cc" * 28)))
                assert(fs(5) == Data.B(tmHash))
                assert(fs(7) == Data.B(ByteString.fromHex("ff" * 28)))
                // Federation identity, #8-12 (spec [CFG-3]).
                assert(fs(8) == Data.B(ByteString.fromHex("bb" * 28)))
                assert(fs(9) == Data.B(ByteString.fromHex("c1" * 28)))
                assert(fs(10) == Data.B(ByteString.fromHex("c2" * 28)))
                assert(fs(11) == Data.B(ByteString.fromHex("f9" * 32)))
                // #12 the federation one-shot. Written out by hand rather than as
                // `.toData`, because this is the assertion that the Scalus TxOutRef mirror
                // agrees with Aiken's V3 OutputReference — Constr(0, [B(txid), I(idx)]),
                // the tx id as BARE bytes with no TxId wrapper constructor. The same shape
                // BifrostContracts.banBootstrapRedeemer builds literally.
                assert(
                  fs(12) == Data.Constr(
                    0,
                    PList(Data.B(ByteString.fromHex("c3" * 32)), Data.I(BigInt(7)))
                  )
                )
            case other => fail(s"expected Constr 0, got $other")
        }
    }

    test("ConfigParams nests the schedule at slot 0 and ends at the refund delay") {
        val sched = ScheduleParams(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        val params =
            ConfigParams(
              sched,
              BigInt(1),
              BigInt(2),
              BigInt(3),
              BigInt(4),
              BigInt(5),
              BigInt(6),
              BigInt(7),
              BigInt(8)
            )
        params.toData match {
            case Data.Constr(0, fields) =>
                val fs = fields.asScala.toIndexedSeq
                assert(fs.size == 9)
                assert(fs(0) == sched.toData)
                assert(fs(1) == Data.I(BigInt(1)))
                assert(fs(2) == Data.I(BigInt(2)))
                assert(fs(3) == Data.I(BigInt(3)))
                // The ban schedule moved in from the top level in rev 5.5 ([CFG-6]).
                assert(fs(4) == Data.I(BigInt(4)))
                assert(fs(6) == Data.I(BigInt(6)))
                assert(fs(7) == Data.I(BigInt(7)))
                // params[8], appended by [CFG-9] — the peg-in refund leaf's CSV delay.
                assert(fs(8) == Data.I(BigInt(8)))
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
