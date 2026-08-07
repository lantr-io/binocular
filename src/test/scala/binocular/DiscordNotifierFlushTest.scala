package binocular

import binocular.notify.DiscordNotifier
import org.scalatest.funsuite.AnyFunSuite
import java.util.concurrent.atomic.AtomicInteger

class DiscordNotifierFlushTest extends AnyFunSuite {

    /** A notifier whose delivery is a slow, counting stub instead of a real Discord POST. */
    private def counting(delayMs: Long, counter: AtomicInteger): DiscordNotifier =
        new DiscordNotifier(webhookUrl = "http://unused.invalid") {
            override protected def deliver(payload: String): Unit = {
                Thread.sleep(delayMs)
                counter.incrementAndGet()
                ()
            }
        }

    test("flush blocks until an enqueued alert is actually delivered") {
        val delivered = new AtomicInteger(0)
        val n = counting(150, delivered)
        try {
            // error() is not throttled; the first occurrence enqueues one post.
            n.error("oracle", "deep reorg — manual re-init required")
            assert(delivered.get() == 0, "post is async — nothing delivered yet")
            n.flush(5000)
            assert(delivered.get() == 1, "flush must wait for the post to be delivered")
        } finally n.close()
    }

    /** The regression test for the race behind the 2026-08-07 CI failure.
      *
      * `flush` used to infer "work outstanding" from `ThreadPoolExecutor.getQueue.size` and
      * `getActiveCount`, and a task can be in NEITHER: below core size `execute` hands it straight
      * to a newly created worker (never queued), and that worker is not counted active until it has
      * locked in on the task. `flush` could observe 0/0 and return while the post was in flight —
      * with `System.exit` next in line, losing exactly the alert it was meant to deliver.
      *
      * Asserting on accepted-but-undelivered work makes that window observable without racing: the
      * post is accounted for the instant `error()` returns, whatever the worker is doing.
      */
    test("an accepted post counts as outstanding before its worker starts") {
        val delivered = new AtomicInteger(0)
        val n = counting(150, delivered)
        try {
            n.error("oracle", "deep reorg — manual re-init required")
            assert(n.pendingDeliveries == 1, "accepted work must be outstanding immediately")
            assert(delivered.get() == 0, "…and not yet delivered")
            n.flush(5000)
            assert(n.pendingDeliveries == 0, "flush must not return with work outstanding")
            assert(delivered.get() == 1)
        } finally n.close()
    }

    /** A `deliver` that throws must still release its slot, or one failed POST would wedge every
      * later `flush` for its full timeout.
      */
    test("a failed delivery still clears its outstanding slot") {
        val n = new DiscordNotifier(webhookUrl = "http://unused.invalid") {
            override protected def deliver(payload: String): Unit =
                throw new RuntimeException("webhook exploded")
        }
        try {
            n.error("oracle", "boom")
            n.flush(2000)
            assert(n.pendingDeliveries == 0, "a throwing deliver must not leak an outstanding post")
        } finally n.close()
    }

    test("flush returns after the timeout even if delivery is slower") {
        val delivered = new AtomicInteger(0)
        val n = counting(2000, delivered)
        try {
            n.error("oracle", "boom")
            val start = System.currentTimeMillis()
            n.flush(200) // shorter than the 2s delivery
            val elapsed = System.currentTimeMillis() - start
            assert(elapsed < 1500, s"flush should honor its timeout, took ${elapsed}ms")
        } finally n.close()
    }

    test("flush is a no-op when nothing is queued") {
        val n = counting(10, new AtomicInteger(0))
        try n.flush(1000) // returns promptly
        finally n.close()
    }

    /** A notifier that records every delivered payload instead of POSTing. */
    private def recording(payloads: java.util.Queue[String]): DiscordNotifier =
        new DiscordNotifier(webhookUrl = "http://unused.invalid") {
            override protected def deliver(payload: String): Unit = {
                payloads.add(payload)
                ()
            }
        }

    test("success messages are delivered immediately, never throttled") {
        val payloads = new java.util.concurrent.ConcurrentLinkedQueue[String]()
        val n = recording(payloads)
        try {
            // relay/confirm successes are rare, operator-meaningful events: back-to-back sends
            // must each go out at once, not be coalesced behind a throttle window.
            n.success("relay", "TM relayed to Bitcoin")
            n.success("confirm", "TM confirmed on Cardano")
            n.flush(5000)
            assert(payloads.size == 2, s"expected both successes delivered, got ${payloads.size}")
        } finally n.close()
    }

    test("block notifications stay throttled: second one inside the window is held") {
        val payloads = new java.util.concurrent.ConcurrentLinkedQueue[String]()
        val n = recording(payloads)
        try {
            n.newBlock(105, 100, "aa" * 32, "2026-07-29T07:00:00Z", 1, 5, 100)
            n.newBlock(106, 101, "bb" * 32, "2026-07-29T07:10:00Z", 1, 5, 101)
            n.flush(5000)
            assert(
              payloads.size == 1,
              s"expected only the first block delivered, got ${payloads.size}"
            )
        } finally n.close()
    }
}
