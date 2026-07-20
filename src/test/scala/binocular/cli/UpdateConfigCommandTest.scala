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
          newPegInHash = Some(ByteString.fromHex("ff")),
          anchor = ByteString.fromHex("ee")
        )
        assert(out.size == 12)
        assert(out(4) == Data.B(ByteString.fromHex("ff")))
        assert(out(11) == Data.B(ByteString.fromHex("ee")))
        // Untouched fields carried over verbatim.
        assert(out(0) == fields11(0) && out(10) == fields11(10))
    }

    test("rewriteFields replaces field 11 on a 12-field datum (re-anchoring)") {
        val twelve = fields11 :+ (Data.B(ByteString.fromHex("aa")): Data)
        val out = UpdateConfigCommand.rewriteFields(twelve, None, ByteString.fromHex("bb"))
        assert(out.size == 12)
        assert(out(4) == fields11(4)) // no swap requested
        assert(out(11) == Data.B(ByteString.fromHex("bb")))
    }

    test("rewriteFields rejects short datums") {
        intercept[IllegalArgumentException] {
            UpdateConfigCommand.rewriteFields(fields11.take(5), None, ByteString.fromHex("bb"))
        }
    }
}
