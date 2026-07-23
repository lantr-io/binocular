package binocular.notify

/** Sink for operator notifications about the running watchtower.
  *
  * Two event kinds are emitted explicitly by the daemon loops (there is no `Console` hook):
  *   - [[newBlock]] when the oracle's confirmed tip advances, and
  *   - [[error]] when a loop hits an error (including the unrecoverable deep-reorg path).
  *
  * Implementations MUST be non-blocking and MUST NOT throw: a notification failure can never be
  * allowed to disturb the oracle loop.
  */
trait Notifier {

    /** The oracle advanced: new headers reached the fork tree (and possibly the confirmed tip).
      *
      * @param tipHeight
      *   fork-tree best-tip height — the newest block the oracle has seen (used for dedup/header)
      * @param confirmedHeight
      *   confirmed-tip height (lags the fork tip until blocks mature and are promoted)
      * @param confirmedHash
      *   big-endian (block-explorer) hex of the confirmed tip hash
      * @param confirmedTimeIso
      *   ISO-8601 UTC timestamp of the confirmed tip block
      * @param headersAdded
      *   how many headers this update applied (0 when unknown, e.g. an adopted UTxO)
      */
    def newBlock(
        tipHeight: BigInt,
        confirmedHeight: BigInt,
        confirmedHash: String,
        confirmedTimeIso: String,
        headersAdded: Int,
        treeBlocks: Int,
        confirmedBlocks: Int
    ): Unit

    /** A loop hit an error. `source` is the loop label (`oracle` / `relay` / `confirm`). */
    def error(source: String, message: String): Unit

    /** A loop completed a noteworthy action successfully (e.g. a TM relayed to Bitcoin or a TM
      * confirmed on Cardano). `source` is the loop label (`relay` / `confirm`).
      */
    def success(source: String, message: String): Unit

    /** Block until every already-enqueued notification has been delivered, or `timeoutMs` elapses.
      *
      * Posts are fire-and-forget on a background daemon thread, so a caller that is about to end
      * the JVM (e.g. the watchtower exiting on an unrecoverable deep reorg) MUST call this first —
      * otherwise `System.exit` halts the process before the HTTP POST completes and the alert is
      * lost. No-op for notifiers that deliver synchronously.
      */
    def flush(timeoutMs: Long): Unit = ()

    /** Release background resources (executor threads). Safe to call more than once. */
    def close(): Unit = ()
}

object Notifier {

    /** Build a notifier from config: [[DiscordNotifier]] when enabled with a non-empty webhook URL,
      * otherwise [[NoopNotifier]].
      */
    def fromConfig(config: NotificationConfig): Notifier =
        if config.enabled && config.discordWebhookUrl.trim.nonEmpty then
            new DiscordNotifier(
              webhookUrl = config.discordWebhookUrl.trim,
              throttleIntervalMs = config.throttleIntervalSeconds.toLong * 1000L,
              errorMentionUserId = Option(config.errorMentionUserId.trim).filter(_.nonEmpty)
            )
        else NoopNotifier
}

/** Discards every notification. Used when notifications are disabled or unconfigured. */
object NoopNotifier extends Notifier {
    def newBlock(
        tipHeight: BigInt,
        confirmedHeight: BigInt,
        confirmedHash: String,
        confirmedTimeIso: String,
        headersAdded: Int,
        treeBlocks: Int,
        confirmedBlocks: Int
    ): Unit = ()
    def error(source: String, message: String): Unit = ()
    def success(source: String, message: String): Unit = ()
}
