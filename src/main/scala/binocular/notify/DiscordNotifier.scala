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
  * `newBlock` is deduplicated by height (only strictly-increasing heights notify), and `error` is
  * debounced via [[ErrorDebounce]] so the retry loops' repeated errors don't spam the channel.
  *
  * Uses the JDK's built-in [[java.net.http.HttpClient]] — no extra dependencies.
  */
class DiscordNotifier(
    webhookUrl: String,
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

    // Guards `lastHeight`; only strictly-increasing heights notify (dedup batched/duplicate events).
    private var lastHeight: BigInt = BigInt(-1)
    // Guards `debounce`.
    private var debounce: ErrorDebounce.State = ErrorDebounce.State.empty

    def newBlock(
        height: BigInt,
        tipHash: String,
        headersAdded: Int,
        treeBlocks: Int,
        confirmedBlocks: Int
    ): Unit = {
        val shouldSend = synchronized {
            if height > lastHeight then { lastHeight = height; true }
            else false
        }
        if shouldSend then
            enqueue(
              DiscordPayload.newBlock(height, tipHash, headersAdded, treeBlocks, confirmedBlocks)
            )
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
                enqueue(DiscordPayload.error(source, text))
        }
    }

    // Successes (TM relayed / confirmed) are infrequent and each is a distinct event worth seeing,
    // so they are sent directly — no debounce, no dedup.
    def success(source: String, message: String): Unit =
        enqueue(DiscordPayload.success(source, message))

    override def close(): Unit = {
        executor.shutdown()
        ()
    }

    /** Dropped-post count (queue overflow), for diagnostics/tests. */
    def droppedCount: Long = dropped.get()

    private def enqueue(payload: String): Unit =
        try executor.execute(() => post(payload))
        catch { case _: RejectedExecutionException => dropped.incrementAndGet() }

    private def post(payload: String): Unit =
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
