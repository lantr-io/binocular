package binocular

import binocular.bitcoin.ReorgDiagnostics
import org.scalatest.funsuite.AnyFunSuite

/** Unit tests for the pure coinbase-tag / pool-identification logic, using real testnet4 coinbase
  * scriptSigs captured around the height-137607 reorg (block heights noted inline).
  */
class ReorgDiagnosticsTest extends AnyFunSuite {

    // height 137608 — Stratum V2 tag "/mara_pool_sv2//"
    private val mara =
        "03881902102f6d6172615f706f6f6c5f7376322f2f140200001d0000000000000000000000000020da3d"
    // height 137606 — common ancestor, "ckpool" suffix
    private val ckpool = "0386190200045bc41f6a04c52312190c3162f56900000100000000000a636b706f6f6c"
    // height 137607 — the reorg winner, no readable pool tag (solo / min-difficulty block)
    private val untagged607 = "03871902080e86206a00000000"
    // height 137605 — also untagged
    private val untagged605 = "03851902000455c41f6a0c744b186a0100000000000000"

    test("identifies MARA Pool from /mara_pool_sv2// tag") {
        val p = ReorgDiagnostics.identifyPool(mara)
        assert(p.name.contains("MARA Pool"))
        assert(p.display == "MARA Pool")
    }

    test("identifies CKPool from ckpool tag") {
        val p = ReorgDiagnostics.identifyPool(ckpool)
        assert(p.name.contains("CKPool"))
    }

    test("untagged coinbase reports as solo (no name, no raw tag)") {
        val p = ReorgDiagnostics.identifyPool(untagged607)
        assert(p.name.isEmpty)
        assert(p.rawTag.isEmpty)
        assert(p.display == "untagged (likely solo)")
    }

    test("untagged coinbase with only extranonce bytes has no readable run") {
        val p = ReorgDiagnostics.identifyPool(untagged605)
        assert(p.name.isEmpty)
    }

    test("readableRuns strips the BIP34 height push and recovers the tag") {
        assert(ReorgDiagnostics.readableRuns(ckpool).exists(_.contains("ckpool")))
        assert(ReorgDiagnostics.readableRuns(mara).exists(_.contains("mara_pool")))
    }

    test("readableRuns is empty for empty input") {
        assert(ReorgDiagnostics.readableRuns("").isEmpty)
    }

    test("identifyPool is robust to malformed hex") {
        val p = ReorgDiagnostics.identifyPool("zzzz")
        assert(p.name.isEmpty && p.rawTag.isEmpty)
    }

    test("humanDuration renders compactly") {
        assert(TimeFmt.humanDuration(45) == "45s")
        assert(TimeFmt.humanDuration(180) == "3m")
        assert(TimeFmt.humanDuration(3600) == "1h")
        assert(TimeFmt.humanDuration(7500) == "2h 5m")
        assert(TimeFmt.humanDuration(-5) == "0s")
    }
}
