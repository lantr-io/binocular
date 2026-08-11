package binocular.cli

import org.scalatest.funsuite.AnyFunSuite

/** The `update-config` command line, parsed through the real decline parser.
  *
  * Worth pinning: these flags are the governance surface for the operational parameters — the
  * values every SPO's TM builder reads — so an option that silently stops parsing, or a schedule
  * slot name that stops being validated, is a bad way to find out mid-ceremony.
  */
class UpdateConfigParseTest extends AnyFunSuite {

    private def parse(args: String*): Either[String, CliApp.Cmd] =
        CliApp.command.parse(args).left.map(_.toString).map(_._2)

    private def updateConfig(args: String*): CliApp.Cmd.UpdateConfig =
        parse(args*) match {
            case Right(u: CliApp.Cmd.UpdateConfig) => u
            case other => fail(s"expected an UpdateConfig command, got: $other")
        }

    test("every parameter flag reaches ParamEdits") {
        val cmd = updateConfig(
          "update-config",
          "--min-stake",
          "60000000",
          "--fee-rate",
          "9",
          "--per-pegout-fee",
          "1500",
          "--min-peg-out",
          "50000",
          "--leader-reward",
          "3000000",
          "--schedule",
          "tm_batch_interval=600",
          "--schedule",
          "leader_slot_t=30",
          "--dry-run"
        )
        assert(cmd.params.minStake.contains(BigInt(60000000)))
        assert(cmd.params.feeRateSatPerVb.contains(BigInt(9)))
        assert(cmd.params.perPegoutFee.contains(BigInt(1500)))
        assert(cmd.params.minPegOutFbtc.contains(BigInt(50000)))
        assert(cmd.params.leaderReward.contains(BigInt(3000000)))
        assert(
          cmd.params.schedule == Map(
            "tm_batch_interval" -> BigInt(600),
            "leader_slot_t" -> BigInt(30)
          )
        )
        assert(cmd.dryRun)
        // Nothing else was requested, so nothing else changes.
        assert(cmd.initialBtcTreasuryUtxo.isEmpty && cmd.pegInWithdrawHash.isEmpty)
    }

    // The anchor used to be mandatory, which made a params-only governance update impossible
    // without also re-anchoring the TM chain.
    test("a params-only update needs no anchor") {
        val cmd = updateConfig("update-config", "--min-stake", "5")
        assert(cmd.initialBtcTreasuryUtxo.isEmpty)
        assert(cmd.params.minStake.contains(BigInt(5)))
        assert(!cmd.params.touchesTunables)
    }

    test("the anchor + peg-in hash edits still parse") {
        val cmd = updateConfig(
          "update-config",
          "--initial-btc-treasury-utxo",
          s"${"ab" * 32}:0",
          "--peg-in-withdraw-hash",
          "cd" * 28
        )
        assert(cmd.initialBtcTreasuryUtxo.contains(s"${"ab" * 32}:0"))
        assert(cmd.pegInWithdrawHash.contains("cd" * 28))
        assert(cmd.params.isEmpty)
    }

    test("a misspelled schedule slot is a usage error, not a crash") {
        val err = parse("update-config", "--schedule", "tm_batch=600").swap.getOrElse(
          fail("expected a parse failure")
        )
        assert(err.contains("unknown field"))
        assert(err.contains("tm_batch_interval")) // the help lists the real names
    }

    test("update-config with no edits at all still parses (the command reports it)") {
        val cmd = updateConfig("update-config")
        assert(cmd.params.isEmpty && cmd.initialBtcTreasuryUtxo.isEmpty)
    }

    // Publishing the ban policy is what lets every SPO delete its ban keys, so this is the
    // command line the migration runs.
    test("the ban policy flags reach ParamEdits") {
        val cmd = updateConfig(
          "update-config",
          "--spo-bans-policy",
          "bb" * 28,
          "--base-ban-duration-ms",
          "600000",
          "--max-faults-before-permanent",
          "3",
          "--max-validity-window-ms",
          "3600000"
        )
        assert(cmd.params.spoBansPolicyId.map(_.toHex).contains("bb" * 28))
        assert(cmd.params.baseBanDurationMs.contains(BigInt(600000)))
        assert(cmd.params.maxFaultsBeforePermanent.contains(BigInt(3)))
        assert(cmd.params.maxValidityWindowMs.contains(BigInt(3600000)))
        assert(cmd.params.touchesBans && !cmd.params.touchesTunables)
    }

    test("a malformed ban policy id is a usage error, not a published wrong address") {
        val err = parse("update-config", "--spo-bans-policy", "bb").swap.getOrElse(
          fail("expected a parse failure")
        )
        assert(err.contains("56-hex-char policy id"))
    }
}
