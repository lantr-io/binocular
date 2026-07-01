package binocular

import binocular.watchtower.Supervisor
import org.scalatest.funsuite.AnyFunSuite
import java.util.concurrent.atomic.AtomicInteger

class SupervisorTest extends AnyFunSuite {

    test("restarts a worker that keeps throwing, until stopped") {
        val calls = new AtomicInteger(0)
        val sup = new Supervisor("test", retryDelayMs = 0, sleep = _ => ())
        sup.supervise { () =>
            if calls.incrementAndGet() >= 3 then sup.stop()
            throw new RuntimeException("boom")
        }
        assert(calls.get() == 3)
    }

    test("does not propagate the worker's exception to the caller") {
        val sup = new Supervisor("test", retryDelayMs = 0, sleep = _ => ())
        // supervise must return normally even though the worker throws.
        sup.supervise { () =>
            sup.stop()
            throw new IllegalStateException("boom")
        }
        succeed
    }

    test("stops looping once stop() is signalled by a clean worker return") {
        val calls = new AtomicInteger(0)
        val sup = new Supervisor("test", retryDelayMs = 0, sleep = _ => ())
        sup.supervise { () =>
            calls.incrementAndGet()
            sup.stop()
        }
        assert(calls.get() == 1)
    }

    test("waits retryDelay between restarts (via injected sleep)") {
        val sleeps = new AtomicInteger(0)
        val calls = new AtomicInteger(0)
        val sup = new Supervisor("test", retryDelayMs = 50, sleep = _ => sleeps.incrementAndGet())
        sup.supervise { () =>
            if calls.incrementAndGet() >= 3 then sup.stop()
            throw new RuntimeException("boom")
        }
        // 3 attempts => 2 waits between them.
        assert(calls.get() == 3)
        assert(sleeps.get() == 2)
    }
}
