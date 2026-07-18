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

    /** New-block notification. `sinceCount` is how many confirmed blocks this message stands in for
      * (1 = just this one; > 1 when the throttle coalesced a burst since the last notification).
      */
    def newBlock(
        height: BigInt,
        tipHash: String,
        headersAdded: Int,
        treeBlocks: Int,
        confirmedBlocks: Int,
        sinceCount: Int = 1
    ): String = {
        val added = if headersAdded > 0 then s" (+$headersAdded)" else ""
        val title =
            if sinceCount > 1 then s"🔷 $sinceCount blocks — tip $height"
            else s"🔷 New block — height $height$added"
        val sinceLine =
            if sinceCount > 1 then s"\n**Since last alert:** $sinceCount blocks" else ""
        val description =
            s"""**Height:** $height$added
               |**Tip:** `$tipHash`
               |**Fork tree:** $treeBlocks blocks
               |**Confirmed:** $confirmedBlocks blocks$sinceLine""".stripMargin
        embed(title, description, ColorNewBlock)
    }

    /** Error notification. When `mentionUserId` is a non-empty numeric Discord user ID, the message
      * carries a top-level `content` `<@id>` plus a restrictive `allowed_mentions` (only that user
      * — never `@everyone`/roles), so it produces a real ping. Mentions inside an embed do NOT
      * ping, which is why the id goes in `content`.
      */
    def error(source: String, message: String, mentionUserId: Option[String] = None): String = {
        val embedObj = embedObject(s"🛑 Error [$source]", message, ColorError)
        mentionUserId.map(_.trim).filter(_.nonEmpty) match {
            case Some(id) =>
                s"""{"content":"<@${escape(id)}>",""" +
                    s""""allowed_mentions":{"parse":[],"users":["${escape(id)}"]},""" +
                    s""""embeds":[$embedObj]}"""
            case None =>
                s"""{"embeds":[$embedObj]}"""
        }
    }

    def success(source: String, message: String): String =
        embed(s"✅ [$source]", message, ColorSuccess)

    /** Batch of successes coalesced by the throttle. A single item renders as [[success]]; several
      * render as one embed listing each `(source, message)` line.
      */
    def successBatch(items: List[(String, String)]): String = items match {
        case Nil                  => embed("✅ Actions", "(none)", ColorSuccess)
        case (source, msg) :: Nil => success(source, msg)
        case many =>
            val body = many.map { case (s, m) => s"• [$s] $m" }.mkString("\n")
            embed(s"✅ ${many.size} actions", body, ColorSuccess)
    }

    /** A single embed object `{...}` (no `{"embeds":[...]}` wrapper), so callers can compose it
      * with a `content` field (see [[error]]).
      */
    def embedObject(title: String, description: String, color: Int): String =
        s"""{"title":"${escape(truncate(title, MaxTitle))}",""" +
            s""""description":"${escape(truncate(description, MaxDescription))}",""" +
            s""""color":$color}"""

    def embed(title: String, description: String, color: Int): String =
        s"""{"embeds":[${embedObject(title, description, color)}]}"""

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
