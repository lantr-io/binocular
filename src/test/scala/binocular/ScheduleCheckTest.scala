package binocular

import binocular.oracle.CardanoNetwork
import binocular.watchtower.{ScheduleCheck, ScheduleParams}
import binocular.watchtower.ScheduleCheck.Severity

import org.scalatest.funsuite.AnyFunSuite

class ScheduleCheckTest extends AnyFunSuite {

    /** The schedule actually deployed on bridge v2 (Config UTxO e1a0ef56…#0, read 2026-08-22). */
    private val deployed = ScheduleParams(
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

    private def rules(fs: List[ScheduleCheck.Finding]) = fs.map(_.rule).toSet
    private def errors(fs: List[ScheduleCheck.Finding]) =
        fs.filter(_.isError).map(_.rule).toSet

    test("an unchanged schedule proposes no new breakage") {
        // The deployed schedule DOES violate cutoff+recovery<=epoch (345600 + 133200 > 432000),
        // but re-proposing it breaks nothing, so nothing may be reported as an error.
        val fs = ScheduleCheck.check(deployed, deployed, CardanoNetwork.Preprod)
        assert(errors(fs).isEmpty)
        assert(fs.forall(_.message.contains("pre-existing")))
    }

    test("the deployed schedule already exceeds the epoch's recovery margin") {
        // Against a hypothetical clean baseline the violation shows as an error — this is the
        // finding the pre-existing rule above is discounting, and it is real.
        val clean = deployed.copy(finalTmCutoff = 280800)
        val fs = ScheduleCheck.check(clean, deployed, CardanoNetwork.Preprod)
        assert(errors(fs).contains("cutoff+recovery<=epoch"))
    }

    test("final_tm_cutoff of 2 h leaves two opportunities and 118 h of nothing") {
        val proposed =
            deployed.copy(tmBatchInterval = 3600, stabilityWindow = 3600, finalTmCutoff = 7200)
        assert(ScheduleCheck.opportunitiesPerEpoch(proposed) == 2)
        // 432000 + 3600 - 7200 = 428400 slots = 119 h with no batch opportunity at all.
        assert(ScheduleCheck.deadSlotsPerEpoch(proposed, 432000L) == 428400L)
        val fs = ScheduleCheck.check(deployed, proposed, CardanoNetwork.Preprod)
        assert(rules(fs).contains("opportunities"))
    }

    test("an interval that does not exceed both sign windows is refused") {
        // 3600 vs sign_r1 1800 + sign_r2 1800: the next opportunity opens exactly as signing ends.
        val proposed = deployed.copy(tmBatchInterval = 3600)
        assert(errors(ScheduleCheck.check(deployed, proposed, CardanoNetwork.Preprod))
            .contains("interval>rounds"))
        // Shrinking the rounds to fit clears it.
        val fixed = proposed.copy(signR1Window = 600, signR2Window = 600)
        assert(!errors(ScheduleCheck.check(deployed, fixed, CardanoNetwork.Preprod))
            .contains("interval>rounds"))
    }

    test("lowering stability_window is an error, and staying low is a warning") {
        // Spec: derived from the host chain's 3k/f and "tunable only upward" — it is what stops a
        // rollback changing a batch's membership after every SPO has signed for it. The MOVE is
        // the error; sitting below 3k/f is the standing warning that outlives it.
        val proposed = deployed.copy(stabilityWindow = 3600)
        val fs = ScheduleCheck.check(deployed, proposed, CardanoNetwork.Preprod)
        assert(errors(fs).contains("stability-only-upward"))
        assert(fs.exists(f => f.rule == "stability>=3k/f" && f.severity == Severity.Warning))

        // Raising it back is not an error, and a schedule already below the floor still warns.
        val low = deployed.copy(stabilityWindow = 3600)
        val raised = low.copy(stabilityWindow = 7200)
        val fs2 = ScheduleCheck.check(low, raised, CardanoNetwork.Preprod)
        assert(errors(fs2).isEmpty)
        assert(rules(fs2).contains("stability>=3k/f"))
    }

    test("a cutoff below one interval means the epoch has no batch at all") {
        val proposed = deployed.copy(tmBatchInterval = 21600, finalTmCutoff = 3600)
        assert(ScheduleCheck.opportunitiesPerEpoch(proposed) == 0)
        assert(errors(ScheduleCheck.check(deployed, proposed, CardanoNetwork.Preprod))
            .contains("cutoff>=interval"))
    }

    test("DKG deadlines must stay ordered") {
        val proposed = deployed.copy(dkgR2Deadline = 1800) // now below r1
        assert(errors(ScheduleCheck.check(deployed, proposed, CardanoNetwork.Preprod))
            .contains("dkg-ordering"))
    }

    test("a recovery window under the Binocular latency warns about spurious recovery") {
        val proposed = deployed.copy(tmRecoveryWindow = 3600)
        val fs = ScheduleCheck.check(deployed, proposed, CardanoNetwork.Preprod)
        assert(rules(fs).contains("recovery>latency"))
    }

    test("the fast-testnet schedule this work was aiming at is clean") {
        // 1 h grid, 1 h stability, rounds shrunk to fit, cutoff left where the epoch can still
        // recover a stuck final TM. Only the two deliberate testnet trade-offs remain, as warnings.
        val proposed = deployed.copy(
          tmBatchInterval = 3600,
          stabilityWindow = 3600,
          signR1Window = 600,
          signR2Window = 600,
          finalTmCutoff = 280800
        )
        val fs = ScheduleCheck.check(deployed, proposed, CardanoNetwork.Preprod)
        // Exactly ONE error, and it is the deliberate one: lowering stability_window needs
        // --allow-unsafe-schedule. Every arithmetic constraint is satisfied.
        assert(errors(fs) == Set("stability-only-upward"), s"findings: $fs")
        assert(rules(fs) == Set("stability-only-upward", "stability>=3k/f"))
        assert(ScheduleCheck.opportunitiesPerEpoch(proposed) == 78)
    }
}
