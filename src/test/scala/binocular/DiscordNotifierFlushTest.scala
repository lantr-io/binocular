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
}
