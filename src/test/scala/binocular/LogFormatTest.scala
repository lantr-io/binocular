package binocular

import binocular.cli.LogFormat
import binocular.cli.LogFormat.InPlace
import org.scalatest.funsuite.AnyFunSuite

class LogFormatTest extends AnyFunSuite {

    test("in-place action overwrites on a TTY (not plain)") {
        assert(
          LogFormat.inPlaceAction(plain = false, last = None, msg = "polling") == InPlace.Overwrite(
            "polling"
          )
        )
        // On a TTY every call overwrites, even repeats.
        assert(
          LogFormat.inPlaceAction(plain = false, last = Some("polling"), msg = "polling") == InPlace
              .Overwrite("polling")
        )
    }

    test("in-place action emits the first line under journald (plain)") {
        assert(
          LogFormat.inPlaceAction(plain = true, last = None, msg = "polling") == InPlace.Emit(
            "polling"
          )
        )
    }

    test("in-place action suppresses an unchanged heartbeat under journald") {
        assert(
          LogFormat.inPlaceAction(
            plain = true,
            last = Some("polling"),
            msg = "polling"
          ) == InPlace.Suppress
        )
    }

    test("in-place action emits again when the heartbeat text changes under journald") {
        assert(
          LogFormat.inPlaceAction(
            plain = true,
            last = Some("polling: 0 utxos"),
            msg = "polling: 1 utxo"
          )
              == InPlace.Emit("polling: 1 utxo")
        )
    }

    test("label prefix is empty when unlabeled and bracketed when labeled") {
        assert(LogFormat.labelPrefix(None) == "")
        assert(LogFormat.labelPrefix(Some("relay")) == "[relay] ")
    }

    test("colorize wraps in ANSI on a TTY and strips it when plain") {
        val red = "[31m"
        val reset = "[0m"
        assert(LogFormat.colorize(plain = false, red, reset, "boom") == s"${red}boom$reset")
        assert(LogFormat.colorize(plain = true, red, reset, "boom") == "boom")
    }
}
