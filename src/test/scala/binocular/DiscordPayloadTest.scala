package binocular

import binocular.notify.DiscordPayload
import org.scalatest.funsuite.AnyFunSuite

class DiscordPayloadTest extends AnyFunSuite {

    test("newBlock shows fork tip, confirmed height + ISO timestamp, and counts") {
        val json = DiscordPayload.newBlock(
          tipHeight = BigInt(144460),
          confirmedHeight = BigInt(144448),
          confirmedHash = "00000000abcdef",
          confirmedTimeIso = "2026-07-22T00:11:49Z",
          headersAdded = 3,
          treeBlocks = 12,
          confirmedBlocks = 7850
        )
        assert(json.contains("\"color\":" + DiscordPayload.ColorNewBlock))
        assert(json.contains("New block — tip 144460"))
        assert(json.contains("(+3)"))
        assert(json.contains("144448 @ 2026-07-22T00:11:49Z"))
        assert(json.contains("00000000abcdef"))
        assert(json.contains("12 blocks"))
        assert(json.contains("7850 blocks"))
    }

    test("newBlock omits the +N suffix when no headers were added") {
        val json =
            DiscordPayload.newBlock(BigInt(1), BigInt(1), "aa", "2026-07-22T00:00:00Z", 0, 1, 1)
        assert(!json.contains("(+"))
    }

    test("newBlock shows a coalesced count and 'tip' title when sinceCount > 1") {
        val json = DiscordPayload.newBlock(
          BigInt(144600),
          BigInt(144588),
          "beef",
          "2026-07-22T04:12:00Z",
          0,
          5,
          8001,
          sinceCount = 12
        )
        assert(json.contains("12 blocks — tip 144600"))
        assert(json.contains("Since last alert:** 12 blocks"))
    }

    test("error embeds the source label and the red color") {
        val json = DiscordPayload.error("oracle", "Tx failed: boom")
        assert(json.contains("\"color\":" + DiscordPayload.ColorError))
        assert(json.contains("[oracle]"))
        assert(json.contains("Tx failed: boom"))
    }

    test("error with a mention adds a content ping and a restrictive allowed_mentions") {
        val json = DiscordPayload.error("oracle", "deep reorg", mentionUserId = Some("123456789"))
        assert(json.contains("\"content\":\"<@123456789>\""))
        // Only that user may be pinged — never @everyone/roles.
        assert(json.contains("\"allowed_mentions\":{\"parse\":[],\"users\":[\"123456789\"]}"))
        assert(json.contains("[oracle]"))
        assert(json.contains("deep reorg"))
    }

    test("error without a mention (None or blank) omits content/allowed_mentions") {
        val none = DiscordPayload.error("oracle", "boom")
        assert(!none.contains("content"))
        assert(!none.contains("allowed_mentions"))
        val blank = DiscordPayload.error("oracle", "boom", mentionUserId = Some("  "))
        assert(!blank.contains("content"))
    }

    test("successBatch renders a single item as a plain success, several as one list") {
        val single = DiscordPayload.successBatch(List("relay" -> "TM relayed"))
        assert(single.contains("[relay]"))
        assert(!single.contains("actions"))

        val many = DiscordPayload.successBatch(
          List("relay" -> "TM relayed", "confirm" -> "TM confirmed")
        )
        assert(many.contains("2 actions"))
        assert(many.contains("• [relay] TM relayed"))
        assert(many.contains("• [confirm] TM confirmed"))
        assert(many.contains("\"color\":" + DiscordPayload.ColorSuccess))
    }

    test("success embeds the source label and the green color") {
        val json = DiscordPayload.success("relay", "TM relayed to Bitcoin — btc txid `abc`")
        assert(json.contains("\"color\":" + DiscordPayload.ColorSuccess))
        assert(json.contains("[relay]"))
        assert(json.contains("TM relayed to Bitcoin"))
    }

    test("escape handles quotes, backslashes, newlines and control chars") {
        assert(DiscordPayload.escape("a\"b") == "a\\\"b")
        assert(DiscordPayload.escape("a\\b") == "a\\\\b")
        assert(DiscordPayload.escape("a\nb") == "a\\nb")
        assert(DiscordPayload.escape("a\tb") == "a\\tb")
        assert(DiscordPayload.escape("ab") == "a\\u0001b")
    }

    test("truncate caps length and appends an ellipsis") {
        assert(DiscordPayload.truncate("abcdef", 10) == "abcdef")
        assert(DiscordPayload.truncate("abcdef", 4) == "abc…")
    }

    test("a message with embedded quotes still yields parseable-looking JSON") {
        val json = DiscordPayload.error("relay", "he said \"hi\"\nand left")
        // The raw quotes/newlines from the message must be escaped, not left literal.
        assert(json.contains("he said \\\"hi\\\"\\nand left"))
    }
}
