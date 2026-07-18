package binocular.notify

import pureconfig.*

/** Configuration for outgoing notifications (currently Discord).
  *
  * The webhook URL is a secret, so it is resolved from the `BIFROST_DISCORD_WEBHOOK` env var (see
  * reference.conf) and kept out of the checked-in config. An empty URL (or `enabled = false`)
  * yields a [[NoopNotifier]], so local runs and tests post nothing.
  */
case class NotificationConfig(
    enabled: Boolean = true,
    discordWebhookUrl: String = ""
) derives ConfigReader
