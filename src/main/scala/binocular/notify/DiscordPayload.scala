package binocular.notify

/** Pure builder for Discord webhook JSON bodies.
  *
  * No HTTP, no state — just string→JSON — so payload shape and escaping are unit-testable. Discord
  * limits an embed title to 256 chars and a description to 4096; both are truncated here.
  */
object DiscordPayload {

    val ColorNewBlock: Int = 0x5865f2 // blurple
    val ColorError: Int = 0xe01e5a // red
    val ColorSuccess: Int = 0x57f287 // green

    private val MaxTitle = 256
    private val MaxDescription = 4096

    def newBlock(
        height: BigInt,
        tipHash: String,
        headersAdded: Int,
        treeBlocks: Int,
        confirmedBlocks: Int
    ): String = {
        val added = if headersAdded > 0 then s" (+$headersAdded)" else ""
        val title = s"🔷 New block — height $height$added"
        val description =
            s"""**Height:** $height$added
               |**Tip:** `$tipHash`
               |**Fork tree:** $treeBlocks blocks
               |**Confirmed:** $confirmedBlocks blocks""".stripMargin
        embed(title, description, ColorNewBlock)
    }

    def error(source: String, message: String): String =
        embed(s"🛑 Error [$source]", message, ColorError)

    def success(source: String, message: String): String =
        embed(s"✅ [$source]", message, ColorSuccess)

    def embed(title: String, description: String, color: Int): String =
        s"""{"embeds":[{"title":"${escape(truncate(title, MaxTitle))}",""" +
            s""""description":"${escape(truncate(description, MaxDescription))}",""" +
            s""""color":$color}]}"""

    def truncate(s: String, max: Int): String =
        if s.length <= max then s else s.take(max - 1) + "…"

    /** Minimal JSON string escaping (RFC 8259). */
    def escape(s: String): String = {
        val sb = new StringBuilder(s.length + 16)
        s.foreach {
            case '"'           => sb.append("\\\"")
            case '\\'          => sb.append("\\\\")
            case '\n'          => sb.append("\\n")
            case '\r'          => sb.append("\\r")
            case '\t'          => sb.append("\\t")
            case c if c < 0x20 => sb.append("\\u").append(f"${c.toInt}%04x")
            case c             => sb.append(c)
        }
        sb.toString
    }
}
