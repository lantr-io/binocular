package binocular

import binocular.notify.DiscordPayload
import org.scalatest.funsuite.AnyFunSuite

class DiscordPayloadTest extends AnyFunSuite {

    test("newBlock embeds height, tip, and counts with the blurple color") {
        val json = DiscordPayload.newBlock(
          height = BigInt(144460),
          tipHash = "00000000abcdef",
          headersAdded = 3,
          treeBlocks = 12,
          confirmedBlocks = 7850
        )
        assert(json.contains("\"color\":" + DiscordPayload.ColorNewBlock))
        assert(json.contains("height 144460"))
        assert(json.contains("(+3)"))
        assert(json.contains("00000000abcdef"))
        assert(json.contains("12 blocks"))
        assert(json.contains("7850 blocks"))
    }

    test("newBlock omits the +N suffix when no headers were added") {
        val json = DiscordPayload.newBlock(BigInt(1), "aa", 0, 1, 1)
        assert(!json.contains("(+"))
    }

    test("error embeds the source label and the red color") {
        val json = DiscordPayload.error("oracle", "Tx failed: boom")
        assert(json.contains("\"color\":" + DiscordPayload.ColorError))
        assert(json.contains("[oracle]"))
        assert(json.contains("Tx failed: boom"))
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
