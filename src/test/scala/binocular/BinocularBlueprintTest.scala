package binocular

import binocular.blueprint.BinocularBlueprint
import binocular.oracle.BitcoinContract
import binocular.watchtower.{PegOutNotProducedVerifierContract, PegOutProducedVerifierContract, TmtxScript, TransactionVerifierContract, TreasuryMovementContract}

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString

class BinocularBlueprintTest extends AnyFunSuite {

    private val paramFree = Seq(
      ("TmtxScript", TmtxScript.blueprint),
      ("PegOutProducedVerifierContract", PegOutProducedVerifierContract.blueprint),
      ("PegOutNotProducedVerifierContract", PegOutNotProducedVerifierContract.blueprint),
      ("TransactionVerifierContract", TransactionVerifierContract.blueprint)
    )

    test("param-free scripts loaded from generated blueprints match their declared hashes") {
        for (name, bp) <- paramFree do {
            val declared = bp.validators.head.hash.get
            val loaded = BinocularBlueprint.script(name).scriptHash.toHex
            assert(
              loaded == declared,
              s"$name: blueprint-loaded hash $loaded != declared hash $declared"
            )
        }
    }

    test("unapplied oracle and TM programs round-trip through their blueprints") {
        for name <- Seq("BitcoinContract", "TreasuryMovementContract") do {
            val prog = BinocularBlueprint.program(name)
            assert(prog.cborByteString.size > 0, s"$name: empty program")
        }
    }

    test("TM script derivation is deterministic and parameter-sensitive") {
        val a = ByteString.fromArray(Array.fill(28)(1: Byte))
        val b = ByteString.fromArray(Array.fill(28)(2: Byte))
        val name = ByteString.fromHex("544d4354524c")
        val s1 = TreasuryMovementContract.script(a, b, name)
        val s2 = TreasuryMovementContract.script(a, b, name)
        val s3 = TreasuryMovementContract.script(b, a, name)
        assert(s1.scriptHash == s2.scriptHash, "same params must give the same hash")
        assert(s1.scriptHash != s3.scriptHash, "different params must give different hashes")
    }
}
