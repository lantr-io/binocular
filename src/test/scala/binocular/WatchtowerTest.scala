package binocular

import binocular.watchtower.Watchtower
import binocular.watchtower.Watchtower.Worker
import binocular.watchtower.UnrecoverableWorkerError
import org.scalatest.funsuite.AnyFunSuite
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.ConcurrentLinkedQueue

class WatchtowerTest extends AnyFunSuite {

    private class Unrecoverable extends RuntimeException("orphaned") with UnrecoverableWorkerError

    test("runOnce runs every worker exactly once") {
        val counter = new AtomicInteger(0)
        val workers = List(
          Worker("a", () => counter.incrementAndGet()),
          Worker("b", () => counter.incrementAndGet()),
          Worker("c", () => counter.incrementAndGet())
        )
        Watchtower.runOnce(workers, timeoutMs = 5000)
        assert(counter.get() == 3)
    }

    test("runOnce returns even if one worker blocks past the timeout") {
        val done = new AtomicInteger(0)
        val workers = List(
          Worker("quick", () => done.incrementAndGet()),
          Worker("stuck", () => Thread.sleep(60000)) // would hang forever without the bounded join
        )
        val start = System.nanoTime()
        Watchtower.runOnce(workers, timeoutMs = 300)
        val elapsedMs = (System.nanoTime() - start) / 1000000
        assert(done.get() == 1)
        assert(
          elapsedMs < 5000,
          s"runOnce should not block on the stuck worker, took ${elapsedMs}ms"
        )
    }

    test("runOnce runs each worker under its component label") {
        val seen = new ConcurrentLinkedQueue[String]()
        val workers = List(
          Worker("oracle", () => seen.add(binocular.cli.Console.currentLabel().getOrElse("none"))),
          Worker("relay", () => seen.add(binocular.cli.Console.currentLabel().getOrElse("none")))
        )
        Watchtower.runOnce(workers, timeoutMs = 5000)
        assert(seen.contains("oracle"))
        assert(seen.contains("relay"))
    }

    test("a fatal error escaping a worker triggers a full-process restart via onWorkerExit") {
        val exited = new java.util.concurrent.atomic.AtomicReference[String]("")
        // OutOfMemoryError is a VirtualMachineError → NOT NonFatal, so it escapes the Supervisor's
        // `catch NonFatal` (the exact case that silently killed a loop in production). runSupervised
        // must funnel that into onWorkerExit rather than leaving the process half-dead.
        Watchtower.runSupervised(
          workers = List(Worker("relay", () => throw new OutOfMemoryError("boom"))),
          retryDelayMs = 0,
          onWorkerExit = label => exited.set(label)
        )
        assert(exited.get() == "relay")
    }

    test("an unrecoverable worker error stops the process via onUnrecoverable, not restart") {
        val restarted = new java.util.concurrent.atomic.AtomicReference[String]("")
        val stopped = new java.util.concurrent.atomic.AtomicReference[String]("")
        // A deep-reorg-style UnrecoverableWorkerError must NOT be restarted (onWorkerExit) — it must
        // route to onUnrecoverable, which in production exits with the do-not-restart code.
        Watchtower.runSupervised(
          workers = List(Worker("oracle", () => throw new Unrecoverable)),
          retryDelayMs = 0,
          onWorkerExit = label => restarted.set(label),
          onUnrecoverable = label => stopped.set(label)
        )
        assert(stopped.get() == "oracle")
        assert(restarted.get() == "", "unrecoverable error must not trigger the restart path")
    }
}
