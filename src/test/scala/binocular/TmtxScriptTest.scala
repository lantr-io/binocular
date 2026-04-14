package binocular

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.PlutusV3

class TmtxScriptTest extends AnyFunSuite {

    test("TmtxScript has a distinct policy ID from PlutusV3.alwaysOk") {
        val alwaysOkHash = PlutusV3.alwaysOk.script.scriptHash.toHex
        val tmtxHash = TmtxScript.mintingScript.script.scriptHash.toHex
        println(s"\n=== TmtxScript policy ID: $tmtxHash ===\n")
        assert(
          alwaysOkHash != tmtxHash,
          s"TmtxScript must differ from alwaysOk ($alwaysOkHash)"
        )
        // Pin the hash to catch accidental changes to the salt or script:
        assert(
          tmtxHash == "eafdc4d9733275d3e06cfe5fe54a13fff3ba5baa8d65636260537907",
          s"Policy ID changed — update relay.tmtx-policy-id in config to: $tmtxHash"
        )
    }
}
