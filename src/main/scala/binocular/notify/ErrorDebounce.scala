package binocular.notify

/** Pure debounce decision for error notifications.
  *
  * The daemon loops re-log the same error every retry interval (seconds), which would spam the
  * notification channel. This collapses identical consecutive errors within a time window: the
  * first is sent immediately, repeats inside the window are suppressed and counted, and once the
  * window elapses the still-recurring error is sent again as a heartbeat annotated with how many
  * repeats were collapsed.
  *
  * Kept pure (no clock, no IO) so the timing logic is unit-testable — [[DiscordNotifier]] owns the
  * mutable [[State]], the clock, and the actual send. Mirrors the `LogFormat`/`Console` split.
  */
object ErrorDebounce {

    /** @param lastKey
      *   the last error key seen, if any
      * @param suppressed
      *   how many repeats have been suppressed since the last send
      * @param lastSentMs
      *   epoch-ms of the last send
      */
    final case class State(lastKey: Option[String], suppressed: Int, lastSentMs: Long)
    object State { val empty: State = State(None, 0, 0L) }

    sealed trait Decision

    /** Suppress this occurrence (a repeat inside the window). */
    case object Suppress extends Decision

    /** Send it now; `repeated` is how many repeats were collapsed into this send (0 if none). */
    final case class Send(repeated: Int) extends Decision

    def decide(state: State, key: String, nowMs: Long, windowMs: Long): (State, Decision) =
        state.lastKey match {
            case Some(prev) if prev == key && nowMs - state.lastSentMs < windowMs =>
                (state.copy(suppressed = state.suppressed + 1), Suppress)
            case Some(prev) if prev == key =>
                // same error still recurring after the window — heartbeat with the collapsed count
                (State(Some(key), 0, nowMs), Send(state.suppressed))
            case _ =>
                // first occurrence, or a different error
                (State(Some(key), 0, nowMs), Send(0))
        }
}
