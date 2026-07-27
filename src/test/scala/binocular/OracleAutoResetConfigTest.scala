package binocular

import binocular.cli.commands.SetStateCommand
import binocular.oracle.OracleConfig

import org.scalatest.funsuite.AnyFunSuite

class OracleAutoResetConfigTest extends AnyFunSuite {

    private val config = OracleConfig(maturationConfirmations = 12)

    test("auto-reset is disabled by default") {
        assert(!config.autoReset)
    }

    test("auto-reset depth defaults to maturation-confirmations") {
        assert(config.autoResetDepth.isEmpty)
        assert(config.effectiveAutoResetDepth == 12)
    }

    test("explicit auto-reset depth wins over maturation-confirmations") {
        assert(config.copy(autoResetDepth = Some(30)).effectiveAutoResetDepth == 30)
    }

    test("staleness remaining is negative once the closure-timeout window has passed") {
        // gap = intervalEnd - headTs = 700 > closureTimeout 600 => stale (remaining < 0)
        assert(SetStateCommand.stalenessRemainingSeconds(1000L, 1700L, 600L) == -100L)
    }

    test("staleness remaining is non-negative while the oracle is still fresh") {
        // gap = 500 <= 600 => not yet stale, 100 more seconds needed
        assert(SetStateCommand.stalenessRemainingSeconds(1000L, 1500L, 600L) == 100L)
        // gap == closureTimeout is still fresh (validator requires strict >)
        assert(SetStateCommand.stalenessRemainingSeconds(1000L, 1600L, 600L) == 0L)
    }

    test("auto-reset target height is tip minus depth") {
        assert(SetStateCommand.autoResetTargetHeight(145917L, 12, Some(136600L)) == Right(145905L))
    }

    test("auto-reset target height rejects a target below start-height") {
        val res = SetStateCommand.autoResetTargetHeight(136605L, 12, Some(136600L))
        assert(res.isLeft)
        assert(res.swap.toOption.get.contains("start-height"))
    }

    test("auto-reset target height requires a configured start-height") {
        assert(SetStateCommand.autoResetTargetHeight(145917L, 12, None).isLeft)
    }
}
