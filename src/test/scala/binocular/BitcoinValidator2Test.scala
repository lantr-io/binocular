package binocular

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.compiler.Options
import scalus.testing.kit.ScalusTest
import scalus.uplc.PlutusV3

class BitcoinValidator2Test extends AnyFunSuite with ScalusTest with ScalaCheckPropertyChecks {
    test("BitcoinValidator2 size") {
        given Options = Options.release
        val contract = PlutusV3.compile(BitcoinValidator2.validate)
        assert(contract.script.script.size == 7464)
    }

}
