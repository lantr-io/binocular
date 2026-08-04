package binocular.cli

import binocular.cli.commands.UpdateConfigCommand

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.{ByteString, Data}

class UpdateConfigCommandTest extends AnyFunSuite {

    private def fields11: List[Data] =
        (0 to 10).map(i => Data.B(ByteString.fromHex(f"$i%02x")): Data).toList

    test("rewriteFields appends field 11 and swaps field 4 on an 11-field datum") {
        val out = UpdateConfigCommand.rewriteFields(
          fields11,
          newCpoPolicy = None,
          newPegInHash = Some(ByteString.fromHex("ff")),
          newPegOutHash = None,
          anchor = Some(ByteString.fromHex("ee"))
        )
        assert(out.size == 12)
        assert(out(4) == Data.B(ByteString.fromHex("ff")))
        assert(out(11) == Data.B(ByteString.fromHex("ee")))
        // Untouched fields carried over verbatim.
        assert(out(0) == fields11(0) && out(10) == fields11(10))
    }

    test("rewriteFields replaces field 11 on a 12-field datum (re-anchoring)") {
        val twelve = fields11 :+ (Data.B(ByteString.fromHex("aa")): Data)
        val out = UpdateConfigCommand.rewriteFields(
          twelve,
          None,
          None,
          None,
          Some(ByteString.fromHex("bb"))
        )
        assert(out.size == 12)
        assert(out(4) == fields11(4)) // no swap requested
        assert(out(11) == Data.B(ByteString.fromHex("bb")))
    }

    test("rewriteFields rejects short datums") {
        intercept[IllegalArgumentException] {
            UpdateConfigCommand.rewriteFields(
              fields11.take(5),
              None,
              None,
              None,
              Some(ByteString.fromHex("bb"))
            )
        }
    }

    // The peg-out trie v2 migration flips fields 3, 4 and 5 in ONE Update transaction: a partial
    // swap leaves TM Confirm reading a field-3 policy whose trie UTxO does not exist.
    test("rewriteFields swaps fields 3, 4 and 5 together in one call") {
        val out = UpdateConfigCommand.rewriteFields(
          fields11,
          newCpoPolicy = Some(ByteString.fromHex("33")),
          newPegInHash = Some(ByteString.fromHex("44")),
          newPegOutHash = Some(ByteString.fromHex("55")),
          anchor = Some(ByteString.fromHex("ee"))
        )
        assert(out.size == 12)
        assert(out(3) == Data.B(ByteString.fromHex("33")))
        assert(out(4) == Data.B(ByteString.fromHex("44")))
        assert(out(5) == Data.B(ByteString.fromHex("55")))
        assert(out(11) == Data.B(ByteString.fromHex("ee")))
        // Neighbours of the swapped fields are untouched.
        assert(out(2) == fields11(2) && out(6) == fields11(6))
    }

    test("rewriteFields swaps field 3 alone without disturbing 4 or 5") {
        val out = UpdateConfigCommand.rewriteFields(
          fields11,
          newCpoPolicy = Some(ByteString.fromHex("33")),
          newPegInHash = None,
          newPegOutHash = None,
          anchor = Some(ByteString.fromHex("ee"))
        )
        assert(out(3) == Data.B(ByteString.fromHex("33")))
        assert(out(4) == fields11(4))
        assert(out(5) == fields11(5))
    }

    // The deployed config carries the 17-field ConfigDatum; the migration Update must not truncate
    // the tunables (#12-16) while swapping the hashes.
    // Field 11 re-anchors the whole TM chain, so a hash-only migration must not touch it.
    test("rewriteFields leaves field 11 untouched when no anchor is given") {
        val twelve = fields11 :+ (Data.B(ByteString.fromHex("aa")): Data)
        val out = UpdateConfigCommand.rewriteFields(
          twelve,
          newCpoPolicy = Some(ByteString.fromHex("33")),
          newPegInHash = None,
          newPegOutHash = Some(ByteString.fromHex("55")),
          anchor = None
        )
        assert(out.size == 12)
        assert(out(3) == Data.B(ByteString.fromHex("33")))
        assert(out(5) == Data.B(ByteString.fromHex("55")))
        assert(out(11) == Data.B(ByteString.fromHex("aa"))) // the deployed anchor survives
    }

    test("rewriteFields does not append field 11 to an 11-field datum when no anchor is given") {
        val out = UpdateConfigCommand.rewriteFields(
          fields11,
          newCpoPolicy = Some(ByteString.fromHex("33")),
          newPegInHash = None,
          newPegOutHash = None,
          anchor = None
        )
        assert(out.size == 11)
        assert(out(3) == Data.B(ByteString.fromHex("33")))
    }

    test("rewriteFields carries fields beyond 11 over verbatim") {
        val seventeen =
            fields11 ++ (11 to 16).map(i => Data.B(ByteString.fromHex(f"$i%02x")): Data).toList
        val out = UpdateConfigCommand.rewriteFields(
          seventeen,
          newCpoPolicy = Some(ByteString.fromHex("33")),
          newPegInHash = None,
          newPegOutHash = Some(ByteString.fromHex("55")),
          anchor = Some(ByteString.fromHex("ee"))
        )
        assert(out.size == 17)
        assert(out(11) == Data.B(ByteString.fromHex("ee")))
        assert((12 to 16).forall(i => out(i) == seventeen(i)))
    }
}
