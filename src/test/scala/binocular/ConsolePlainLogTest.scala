package binocular

import binocular.cli.Console
import org.scalatest.funsuite.AnyFunSuite
import java.io.{ByteArrayOutputStream, PrintStream}

/** Console runs in `plain` mode here because the test JVM has no `System.console()` (not a TTY),
  * exercising the journald code path.
  */
class ConsolePlainLogTest extends AnyFunSuite {

    private val Esc = 27.toChar.toString // ANSI escape (0x1b)

    private def capture(body: => Unit): String = {
        val baos = new ByteArrayOutputStream()
        scala.Console.withOut(new PrintStream(baos))(body)
        baos.toString("UTF-8")
    }

    test("plain output carries no ANSI escape codes") {
        Console.setLabel(None)
        val out = capture(Console.log("hello"))
        assert(!out.contains(Esc), s"expected no ESC in: $out")
        assert(out.contains("hello"))
        assert(out.endsWith("\n"))
    }

    test("a thread label tags every line under journald") {
        Console.setLabel(Some("relay"))
        val out = capture(Console.log("broadcast ok"))
        Console.setLabel(None)
        assert(out.contains("[relay] "), s"expected [relay] tag in: $out")
    }

    test("no label means no bracket prefix") {
        Console.setLabel(None)
        val out = capture(Console.log("plain event"))
        assert(!out.contains("["), s"expected no bracket in: $out")
    }

    test("in-place heartbeat emits once, then suppresses the identical repeat under journald") {
        Console.setLabel(None)
        val msg = "polling-unique-xyz"
        val out = capture {
            Console.logInPlace(msg)
            Console.logInPlace(msg)
        }
        val occurrences = msg.r.findAllIn(out).size
        assert(occurrences == 1, s"expected exactly one emit, got $occurrences in: $out")
        assert(out.endsWith("\n"), "plain in-place line must be newline-terminated")
    }
}
