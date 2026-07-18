package binocular.notify

/** Pure rate-limit / coalescing decision for routine notifications (new blocks, successes).
  *
  * Block and success events can arrive far faster than an operator wants to be pinged. This gates
  * them to at most one send per `intervalMs`: the first event after an idle gap of >= `intervalMs`
  * is sent immediately, further events inside the window are HELD and counted, and once the window
  * elapses the held events are flushed as one coalesced summary. Rare events (idle >= interval) are
  * therefore never delayed; only bursts are collapsed.
  *
  * Kept pure (no clock, no IO) so the timing is unit-testable — [[DiscordNotifier]] owns the
  * mutable [[State]], the clock, the accumulated payloads, and the actual send. Mirrors
  * [[ErrorDebounce]].
  */
object IntervalGate {

    /** @param lastSentMs
      *   epoch-ms of the last send (0 = never sent)
      * @param held
      *   events held (suppressed) since the last send
      */
    final case class State(lastSentMs: Long, held: Int)
    object State { val empty: State = State(0L, 0) }

    sealed trait Decision

    /** Send now (gate open). `heldSinceLast` is how many earlier events were folded in (0 if none),
      * so the payload can annotate the coalesced count.
      */
    final case class SendNow(heldSinceLast: Int) extends Decision

    /** Hold this event (inside the window); it is folded into a later [[SendNow]] or [[flush]]. */
    case object Hold extends Decision

    /** Decide what to do with an event arriving at `nowMs`. A first-ever event (`lastSentMs == 0`)
      * or one after an idle gap `>= intervalMs` sends now and resets the window; otherwise it is
      * held and counted.
      */
    def offer(state: State, nowMs: Long, intervalMs: Long): (State, Decision) =
        if state.lastSentMs == 0L || nowMs - state.lastSentMs >= intervalMs then
            (State(nowMs, 0), SendNow(state.held))
        else (state.copy(held = state.held + 1), Hold)

    /** Called periodically: if events are held and the window has elapsed, flush them. Returns the
      * held count (`> 0`) to summarize and resets the window; otherwise `None` and no change.
      */
    def flush(state: State, nowMs: Long, intervalMs: Long): (State, Option[Int]) =
        if state.held > 0 && nowMs - state.lastSentMs >= intervalMs then
            (State(nowMs, 0), Some(state.held))
        else (state, None)
}
