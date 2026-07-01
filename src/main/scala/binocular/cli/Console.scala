package binocular.cli

import binocular.cli.LogFormat.InPlace
import java.time.LocalTime
import java.time.format.DateTimeFormatter

/** Minimal ANSI color helper for CLI output.
  *
  * Auto-detects a non-interactive stdout (systemd/journald, pipes) and switches to `plain` mode: no
  * ANSI color codes, and `logInPlace` emits normal newline-terminated lines (deduped) instead of
  * carriage-return overwrites that journald cannot segment. See [[LogFormat]] for the pure decision
  * logic.
  */
object Console {

    /** True when stdout is not an interactive terminal, or colors are opted out of. Decided once at
      * startup: `System.console()` is null under systemd, when piped, and when redirected.
      */
    private val plain: Boolean =
        System.console() == null ||
            sys.env.contains("NO_COLOR") ||
            sys.env.get("BINOCULAR_LOG_PLAIN").exists(_ != "0")

    private def c(code: String): String = if plain then "" else code

    private val Reset = c("\u001b[0m")
    private val Bold = c("\u001b[1m")
    private val Dim = c("\u001b[2m")
    private val Red = c("\u001b[31m")
    private val Green = c("\u001b[32m")
    private val Yellow = c("\u001b[33m")
    private val Cyan = c("\u001b[36m")
    private val Magenta = c("\u001b[35m")
    private val EraseLine = "\u001b[2K"

    private val timeFmt = DateTimeFormatter.ofPattern("HH:mm:ss")
    private def now(): String = LocalTime.now().format(timeFmt)

    // Per-thread component label (e.g. "oracle" / "relay" / "confirm") for the watchtower; each of
    // its loops sets its own, so interleaved lines in one journal stay attributable.
    private val threadLabel = new ThreadLocal[Option[String]] {
        override def initialValue(): Option[String] = None
    }

    /** Tag every subsequent line from the current thread with `[label] ` (None clears it). */
    def setLabel(label: Option[String]): Unit = threadLabel.set(label)

    /** The current thread's component label, if any. */
    def currentLabel(): Option[String] = threadLabel.get

    private def labeled(msg: String): String = LogFormat.labelPrefix(threadLabel.get) + msg

    // Track whether the cursor is on an in-place line (TTY only)
    @volatile private var _inPlace = false
    // Last in-place heartbeat text, for plain-mode dedup. Per-thread so the watchtower's three
    // loops (each on its own thread) don't suppress each other's status lines.
    private val _lastInPlace = new ThreadLocal[Option[String]] {
        override def initialValue(): Option[String] = None
    }

    /** Print a line, clearing any in-place content first. */
    private def out(msg: String): Unit = {
        if _inPlace then {
            print(s"\r$EraseLine")
            _inPlace = false
        }
        println(labeled(msg))
    }

    /** Print a line to stderr, clearing any in-place content first. */
    private def err(msg: String): Unit = {
        if _inPlace then {
            print(s"\r$EraseLine")
            _inPlace = false
        }
        System.err.println(labeled(msg))
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

    /** Repeatedly-updated status line.
      *
      * On a TTY: overwrite the current line in place (no newline). Under journald (`plain`): emit a
      * normal line, but only when the text changes, so an unchanged heartbeat doesn't spam the log.
      */
    def logInPlace(msg: String): Unit = {
        LogFormat.inPlaceAction(plain, _lastInPlace.get, msg) match {
            case InPlace.Overwrite(m) =>
                print(s"\r$EraseLine${labeled("")}$Dim${now()}$Reset ▸ $m")
                System.out.flush()
                _inPlace = true
            case InPlace.Emit(m) =>
                out(s"$Dim${now()}$Reset ▸ $m")
            case InPlace.Suppress => ()
        }
        _lastInPlace.set(Some(msg))
    }
}
