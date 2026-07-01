package binocular.cli

/** Pure, testable log-line formatting decisions shared by [[Console]].
  *
  * Kept separate from `Console` (which owns mutable state and real IO) so the journald-vs-TTY
  * behavior can be unit-tested without a terminal.
  */
object LogFormat {

    /** What a `logInPlace` call should do, given the output mode and previous heartbeat text. */
    enum InPlace:
        /** TTY: redraw the current line in place (carriage return, no newline). */
        case Overwrite(line: String)

        /** Non-TTY (journald): print a normal newline-terminated line. */
        case Emit(line: String)

        /** Non-TTY: identical to the last heartbeat — print nothing. */
        case Suppress

    def inPlaceAction(plain: Boolean, last: Option[String], msg: String): InPlace =
        if !plain then InPlace.Overwrite(msg)
        else if last.contains(msg) then InPlace.Suppress
        else InPlace.Emit(msg)

    def labelPrefix(label: Option[String]): String =
        label.fold("")(l => s"[$l] ")

    def colorize(plain: Boolean, code: String, reset: String, msg: String): String =
        if plain then msg else s"$code$msg$reset"
}
