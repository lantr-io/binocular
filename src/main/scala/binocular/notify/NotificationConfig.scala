package binocular.notify

import pureconfig.*

/** Configuration for outgoing notifications (currently Discord).
  *
  * The webhook URL is a secret, so it is resolved from the `BIFROST_DISCORD_WEBHOOK` env var (see
  * reference.conf) and kept out of the checked-in config. An empty URL (or `enabled = false`)
  * yields a [[NoopNotifier]], so local runs and tests post nothing.
  *
  * @param throttleIntervalSeconds
  *   max one block-summary and one success notification per this many seconds (default 1h). Bursty
  *   block/success events inside the window are coalesced into a single message; errors are never
  *   throttled.
  * @param errorMentionUserId
  *   numeric Discord user ID to @-mention (a real ping) on error notifications. Empty = no ping.
  */
case class NotificationConfig(
    enabled: Boolean = true,
    discordWebhookUrl: String = "",
    throttleIntervalSeconds: Int = 3600,
    errorMentionUserId: String = ""
) derives ConfigReader
