package binocular

import binocular.cli.CliApp
import binocular.cli.commands.InitOracleCommand

import org.scalatest.funsuite.AnyFunSuite

class InitOracleCommandTest extends AnyFunSuite {

    test("validateConfirmedRange: clean when confirmedTip is deep enough") {
        val r = InitOracleCommand.validateConfirmedRange(
          startHeight = 100L,
          confirmedTip = 200L,
          tip = 1000L,
          maturationConfirmations = 12L
        )
        assert(r == Right(None))
    }

    test("validateConfirmedRange: error when start > confirmedTip") {
        val r = InitOracleCommand.validateConfirmedRange(300L, 200L, 1000L, 12L)
        assert(r.isLeft)
    }

    test("validateConfirmedRange: error when confirmedTip > tip") {
        val r = InitOracleCommand.validateConfirmedRange(100L, 1100L, 1000L, 12L)
        assert(r.isLeft)
    }

    test("validateConfirmedRange: warns (but proceeds) when shallower than maturation") {
        val r = InitOracleCommand.validateConfirmedRange(100L, 995L, 1000L, 12L)
        assert(r.exists(_.isDefined)) // Right(Some(warning))
    }

    test("init parses --start-block, --confirmed-until, --dry-run") {
        val parsed = CliApp.command.parse(
          List("init", "--start-block", "136600", "--confirmed-until", "144450", "--dry-run")
        )
        assert(parsed == Right((None, CliApp.Cmd.Init(Some(136600L), Some(144450L), true))))
    }

    test("init without --confirmed-until leaves it None") {
        val parsed = CliApp.command.parse(List("init", "--start-block", "136600"))
        assert(parsed == Right((None, CliApp.Cmd.Init(Some(136600L), None, false))))
    }
}
