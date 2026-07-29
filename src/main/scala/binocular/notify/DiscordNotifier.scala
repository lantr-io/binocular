package binocular.notify

import binocular.cli.Console

import java.net.URI
import java.net.http.{HttpClient, HttpRequest}
import java.net.http.HttpRequest.BodyPublishers
import java.net.http.HttpResponse.BodyHandlers
import java.time.Duration
import java.util.concurrent.*
import java.util.concurrent.atomic.AtomicLong

/** Posts notifications to a Discord channel via an incoming webhook.
  *
  * Fire-and-forget: every post runs on a single daemon background thread behind a bounded queue, so
  * a slow or failing Discord never blocks or crashes an oracle loop. When the queue is full posts
  * are dropped (and counted). All HTTP errors are swallowed and logged, never propagated.
  *
  * Routing per event kind:
  *   - `newBlock` is deduplicated by height (only strictly-increasing heights notify), then rate-
  *     limited via [[IntervalGate]]: at most one block notification per `throttleIntervalMs`, with
  *     bursts coalesced into a summary (`N blocks — tip H`).
  *   - `success` is NOT throttled — relay/confirm successes are rare, operator-meaningful events (a
  *     TM relayed to Bitcoin, a TM confirmed on Cardano), so each one posts at once.
  *   - `error` is NOT throttled — it is debounced only against IDENTICAL repeats via
  *     [[ErrorDebounce]] (first + any new error immediate) and carries an optional `@`-mention
  *     ping.
  *
  * A single daemon [[ScheduledExecutorService]] flushes a held block summary once its window
  * elapses, so a coalesced summary still arrives on time even if events stop coming.
  *
  * Uses the JDK's built-in [[java.net.http.HttpClient]] — no extra dependencies.
  */
class DiscordNotifier(
    webhookUrl: String,
    throttleIntervalMs: Long = 60 * 60 * 1000L,
    errorMentionUserId: Option[String] = None,
    errorDebounceWindowMs: Long = 5 * 60 * 1000L,
    queueCapacity: Int = 64
) extends Notifier {

    private val client =
        HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(10)).build()

    private val dropped = new AtomicLong(0)

    private val executor: ThreadPoolExecutor = {
        val threadFactory: ThreadFactory = (r: Runnable) => {
            val t = new Thread(r, "discord-notifier")
            t.setDaemon(true)
            t
        }
        val onReject: RejectedExecutionHandler = (_: Runnable, _: ThreadPoolExecutor) => {
            dropped.incrementAndGet()
            ()
        }
        new ThreadPoolExecutor(
          1,
          1,
          0L,
          TimeUnit.MILLISECONDS,
          new LinkedBlockingQueue[Runnable](queueCapacity),
          threadFactory,
          onReject
        )
    }

    /** Latest block held by the throttle (fork-tree fields), sent when its window flushes. */
    private final case class PendingBlock(
        height: BigInt,
        tipHash: String,
        headersAdded: Int,
        treeBlocks: Int,
        confirmedBlocks: Int
    )

    // All of the following are guarded by `this` (synchronized).
    private var lastHeight: BigInt = BigInt(-1) // newBlock dedup: only strictly-increasing heights
    private var debounce: ErrorDebounce.State = ErrorDebounce.State.empty
    private var blockGate: IntervalGate.State = IntervalGate.State.empty
    private var pendingBlock: Option[PendingBlock] = scala.None

    // Flush a held block summary when its window elapses even if events stop. Tick coarsely
    // (bounded to [1s, 60s]) — a coalesced summary arriving up to a tick late is fine.
    private val scheduler: ScheduledExecutorService = {
        val tf: ThreadFactory = (r: Runnable) => {
            val t = new Thread(r, "discord-notifier-flush")
            t.setDaemon(true)
            t
        }
        Executors.newSingleThreadScheduledExecutor(tf)
    }
    private val flushTickMs: Long = math.max(1000L, math.min(throttleIntervalMs, 60_000L))
    scheduler.scheduleAtFixedRate(
      () =>
          try flushPending()
          catch { case _: Throwable => () }, flushTickMs,
      flushTickMs,
      TimeUnit.MILLISECONDS
    )

    def newBlock(
        height: BigInt,
        tipHash: String,
        headersAdded: Int,
        treeBlocks: Int,
        confirmedBlocks: Int
    ): Unit = {
        val toSend = synchronized {
            if height <= lastHeight then scala.None
            else {
                lastHeight = height
                val (next, decision) =
                    IntervalGate.offer(blockGate, System.currentTimeMillis(), throttleIntervalMs)
                blockGate = next
                decision match {
                    case IntervalGate.SendNow(held) =>
                        pendingBlock = scala.None
                        Some(
                          DiscordPayload.newBlock(
                            height,
                            tipHash,
                            headersAdded,
                            treeBlocks,
                            confirmedBlocks,
                            sinceCount = held + 1
                          )
                        )
                    case IntervalGate.Hold =>
                        pendingBlock = Some(
                          PendingBlock(height, tipHash, headersAdded, treeBlocks, confirmedBlocks)
                        )
                        scala.None
                }
            }
        }
        toSend.foreach(enqueue)
    }

    def error(source: String, message: String): Unit = {
        val key = s"$source $message"
        val decision = synchronized {
            val (next, d) =
                ErrorDebounce.decide(
                  debounce,
                  key,
                  System.currentTimeMillis(),
                  errorDebounceWindowMs
                )
            debounce = next
            d
        }
        decision match {
            case ErrorDebounce.Suppress => ()
            case ErrorDebounce.Send(repeated) =>
                val text =
                    if repeated > 0 then s"$message (repeated $repeated× while suppressed)"
                    else message
                enqueue(DiscordPayload.error(source, text, errorMentionUserId))
        }
    }

    def success(source: String, message: String): Unit =
        enqueue(DiscordPayload.success(source, message))

    /** Emit a held block summary once its throttle window has elapsed. Runs on the scheduler. */
    private def flushPending(): Unit = {
        val posts = synchronized {
            val now = System.currentTimeMillis()
            val buf = scala.collection.mutable.ListBuffer.empty[String]

            val (nb, blockHeld) = IntervalGate.flush(blockGate, now, throttleIntervalMs)
            blockGate = nb
            blockHeld.foreach { held =>
                pendingBlock.foreach { b =>
                    buf += DiscordPayload.newBlock(
                      b.height,
                      b.tipHash,
                      b.headersAdded,
                      b.treeBlocks,
                      b.confirmedBlocks,
                      sinceCount = held
                    )
                    pendingBlock = scala.None
                }
            }
            buf.toList
        }
        posts.foreach(enqueue)
    }

    /** Block until the post queue drains (no queued and no in-flight post) or `timeoutMs` elapses.
      * Called before the process exits on an unrecoverable deep reorg so the enqueued alert is
      * actually delivered rather than killed mid-flight by `System.exit`.
      */
    override def flush(timeoutMs: Long): Unit = {
        val deadline = System.currentTimeMillis() + math.max(0L, timeoutMs)
        while (executor.getQueue.size() > 0 || executor.getActiveCount > 0) &&
            System.currentTimeMillis() < deadline
        do Thread.sleep(25)
    }

    override def close(): Unit = {
        scheduler.shutdownNow()
        executor.shutdown()
        ()
    }

    /** Dropped-post count (queue overflow), for diagnostics/tests. */
    def droppedCount: Long = dropped.get()

    private def enqueue(payload: String): Unit =
        try executor.execute(() => deliver(payload))
        catch { case _: RejectedExecutionException => dropped.incrementAndGet() }

    /** Deliver one payload. Overridable so tests can substitute a synchronous/counting sink for the
      * real HTTP POST without a live webhook.
      */
    protected def deliver(payload: String): Unit =
        try {
            val request = HttpRequest
                .newBuilder(URI.create(webhookUrl))
                .header("Content-Type", "application/json")
                .timeout(Duration.ofSeconds(10))
                .POST(BodyPublishers.ofString(payload))
                .build()
            val response = client.send(request, BodyHandlers.ofString())
            if response.statusCode() >= 300 then
                Console.logWarn(
                  s"Discord webhook returned ${response.statusCode()}: ${response.body().take(200)}"
                )
        } catch {
            case e: Exception =>
                Console.logWarn(s"Discord webhook post failed: ${e.getMessage}")
        }
}
