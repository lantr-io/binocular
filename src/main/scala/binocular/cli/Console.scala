package binocular.cli

import java.time.LocalTime
import java.time.format.DateTimeFormatter

/** Minimal ANSI color helper for CLI output. */
object Console {
    private val Reset = "\u001b[0m"
    private val Bold = "\u001b[1m"
    private val Dim = "\u001b[2m"
    private val Red = "\u001b[31m"
    private val Green = "\u001b[32m"
    private val Yellow = "\u001b[33m"
    private val Cyan = "\u001b[36m"
    private val Magenta = "\u001b[35m"
    private val EraseLine = "\u001b[2K"

    private val timeFmt = DateTimeFormatter.ofPattern("HH:mm:ss")
    private def now(): String = LocalTime.now().format(timeFmt)

    // Track whether the cursor is on an in-place line
    @volatile private var _inPlace = false

    /** Print a line, clearing any in-place content first. */
    private def out(msg: String): Unit = {
        if _inPlace then {
            print(s"\r$EraseLine")
            _inPlace = false
        }
        println(msg)
    }

    /** Print a line to stderr, clearing any in-place content first. */
    private def err(msg: String): Unit = {
        if _inPlace then {
            print(s"\r$EraseLine")
            _inPlace = false
        }
        System.err.println(msg)
    }

    def header(msg: String): Unit =
        out(s"$Bold$Cyan━━━ $msg ━━━$Reset")

    def step(n: Int, msg: String): Unit =
        out(s"${Bold}[$Cyan$n$Reset${Bold}]$Reset $msg")

    def info(label: String, value: Any): Unit =
        out(s"  $Dim$label:$Reset $value")

    def success(msg: String): Unit =
        out(s"  $Green✓$Reset $msg")

    def warn(msg: String): Unit =
        out(s"  $Yellow⚠$Reset $msg")

    def error(msg: String): Unit =
        err(s"  $Red✗$Reset $msg")

    def metric(label: String, value: Any): Unit =
        out(s"  $Dim$label:$Reset $Cyan$value$Reset")

    def tx(label: String, hash: String): Unit =
        out(s"  $Dim$label:$Reset $Magenta$hash$Reset")

    def separator(): Unit =
        out(s"$Dim${"─" * 62}$Reset")

    def blank(): Unit = out("")

    // Timestamped log methods for daemon loop
    def log(msg: String): Unit =
        out(s"$Dim${now()}$Reset ▸ $msg")

    def logSuccess(msg: String): Unit =
        out(s"$Dim${now()}$Reset $Green✓$Reset $msg")

    def logWarn(msg: String): Unit =
        out(s"$Dim${now()}$Reset $Yellow⚠$Reset $msg")

    def logError(msg: String): Unit =
        err(s"$Dim${now()}$Reset $Red✗$Reset $msg")

    /** Overwrite the current line in-place (no newline). */
    def logInPlace(msg: String): Unit = {
        print(s"\r$EraseLine$Dim${now()}$Reset ▸ $msg")
        System.out.flush()
        _inPlace = true
    }
}
