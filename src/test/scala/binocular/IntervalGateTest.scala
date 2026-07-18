package binocular

import binocular.notify.IntervalGate
import org.scalatest.funsuite.AnyFunSuite

class IntervalGateTest extends AnyFunSuite {

    private val interval = 1000L // ms

    test("the first event sends immediately (no prior send)") {
        val (state, d) = IntervalGate.offer(IntervalGate.State.empty, nowMs = 5000, interval)
        assert(d == IntervalGate.SendNow(0))
        assert(state == IntervalGate.State(5000, 0))
    }

    test("events inside the window are held and counted; none sent") {
        var s = IntervalGate.State.empty
        val (s1, d1) = IntervalGate.offer(s, 5000, interval); s = s1
        assert(d1 == IntervalGate.SendNow(0))
        val (s2, d2) = IntervalGate.offer(s, 5200, interval); s = s2
        val (s3, d3) = IntervalGate.offer(s, 5900, interval); s = s3
        assert(d2 == IntervalGate.Hold && d3 == IntervalGate.Hold)
        assert(s.held == 2)
    }

    test("an event after the window sends now and reports how many were held") {
        var s = IntervalGate.State.empty
        s = IntervalGate.offer(s, 5000, interval)._1 // send
        s = IntervalGate.offer(s, 5200, interval)._1 // hold (1)
        s = IntervalGate.offer(s, 5900, interval)._1 // hold (2)
        val (s4, d4) = IntervalGate.offer(s, 6100, interval);
        s = s4 // 1100ms since last send >= 1000
        assert(d4 == IntervalGate.SendNow(2))
        assert(s == IntervalGate.State(6100, 0))
    }

    test("rare events (idle >= interval) are never held") {
        var s = IntervalGate.State.empty
        val (s1, d1) = IntervalGate.offer(s, 1000, interval); s = s1
        val (s2, d2) = IntervalGate.offer(s, 9000, interval); s = s2
        val (s3, d3) = IntervalGate.offer(s, 20000, interval); s = s3
        assert(d1 == IntervalGate.SendNow(0))
        assert(d2 == IntervalGate.SendNow(0))
        assert(d3 == IntervalGate.SendNow(0))
    }

    test("flush emits the held count once the window elapses, then resets") {
        var s = IntervalGate.State.empty
        s = IntervalGate.offer(s, 5000, interval)._1 // send
        s = IntervalGate.offer(s, 5200, interval)._1 // hold (1)
        // Not yet elapsed → no flush.
        val (s1, none) = IntervalGate.flush(s, 5500, interval); s = s1
        assert(none.isEmpty)
        assert(s.held == 1)
        // Window elapsed → flush 1 and reset.
        val (s2, some) = IntervalGate.flush(s, 6001, interval); s = s2
        assert(some.contains(1))
        assert(s == IntervalGate.State(6001, 0))
    }

    test("flush is a no-op when nothing is held") {
        val s = IntervalGate.State(5000, 0)
        val (s1, out) = IntervalGate.flush(s, 999999, interval)
        assert(out.isEmpty)
        assert(s1 == s)
    }
}
