package binocular

import binocular.notify.ErrorDebounce
import binocular.notify.ErrorDebounce.{Send, Suppress}
import org.scalatest.funsuite.AnyFunSuite

class ErrorDebounceTest extends AnyFunSuite {

    private val window = 1000L

    test("first occurrence of an error is sent immediately with no repeats") {
        val (_, d) = ErrorDebounce.decide(ErrorDebounce.State.empty, "boom", 0L, window)
        assert(d == Send(0))
    }

    test("identical errors inside the window are suppressed and counted") {
        var s = ErrorDebounce.State.empty
        val (s1, d1) = ErrorDebounce.decide(s, "boom", 0L, window)
        assert(d1 == Send(0))
        val (s2, d2) = ErrorDebounce.decide(s1, "boom", 100L, window)
        assert(d2 == Suppress)
        val (s3, d3) = ErrorDebounce.decide(s2, "boom", 200L, window)
        assert(d3 == Suppress)
        assert(s3.suppressed == 2)
    }

    test("the same error recurring after the window is re-sent with the collapsed count") {
        var s = ErrorDebounce.State.empty
        val (s1, _) = ErrorDebounce.decide(s, "boom", 0L, window)
        val (s2, _) = ErrorDebounce.decide(s1, "boom", 300L, window)
        val (s3, _) = ErrorDebounce.decide(s2, "boom", 600L, window)
        // window elapsed since lastSent (0) -> heartbeat carrying the 2 suppressed repeats
        val (_, d) = ErrorDebounce.decide(s3, "boom", 1200L, window)
        assert(d == Send(2))
    }

    test("a different error is sent immediately even inside the window") {
        val (s1, _) = ErrorDebounce.decide(ErrorDebounce.State.empty, "boom", 0L, window)
        val (_, d) = ErrorDebounce.decide(s1, "kaboom", 100L, window)
        assert(d == Send(0))
    }
}
