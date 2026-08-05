package binocular

import binocular.blueprint.BinocularBlueprint
import binocular.oracle.BitcoinContract
import binocular.watchtower.{PegOutNotProducedVerifierContract, PegOutProducedVerifierContract, TmtxScript, TransactionVerifierContract, TreasuryMovementContract}

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.Program
import scalus.uplc.builtin.ByteString

class BinocularBlueprintTest extends AnyFunSuite {

    private val paramFree = Seq(
      ("TmtxScript", TmtxScript.blueprint),
      ("PegOutProducedVerifierContract", PegOutProducedVerifierContract.blueprint),
      ("PegOutNotProducedVerifierContract", PegOutNotProducedVerifierContract.blueprint),
      ("TransactionVerifierContract", TransactionVerifierContract.blueprint)
    )

    /** Every pinned blueprint under `src/main/resources/META-INF/scalus/blueprints`, paired with
      * the UNAPPLIED program its contract object compiles from source TODAY. `by-name` so a stale
      * pin is reported per contract instead of one compile failure hiding the rest.
      */
    private val pins: Seq[(String, () => Program)] = Seq(
      // Salt is applied at compile time, so the pinned entry is param-free.
      ("TmtxScript", () => TmtxScript.mintingScript.program),
      ("PegOutProducedVerifierContract", () => PegOutProducedVerifierContract.compiled.program),
      (
        "PegOutNotProducedVerifierContract",
        () => PegOutNotProducedVerifierContract.compiled.program
      ),
      ("TransactionVerifierContract", () => TransactionVerifierContract.compiled.program),
      // Both of these are pinned UNAPPLIED (params go on at the UPLC level at load time), so the
      // fresh side must be the unapplied program too.
      ("TreasuryMovementContract", () => TreasuryMovementContract.parameterized.program),
      ("BitcoinContract", () => BitcoinContract.contract.program)
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

    test("every pinned blueprint is in sync with its freshly compiled validator") {
        // The pins under src/main/resources are what the RUNTIME (and every deploy) loads;
        // generation is skipped by default (`blueprint / skip := true`), so a validator edit does
        // not move them. Without this check a stale pin ships silently: the behavioural suites
        // evaluate the freshly compiled contract, and the tests that DO load a pin only assert it
        // accepts a valid transaction — which an older, less strict script also does.
        //
        // The per-blueprint "declared hash == loaded script" test above cannot catch this: both
        // sides come from the SAME committed JSON, so it locks the pin against itself. Only a
        // fresh compile is an independent witness. If this fails, run `sbt blueprintPin` and commit
        // the diff — that commit IS the decision to deploy the changed script.
        val stale = pins.collect {
            case (name, fresh)
                if BinocularBlueprint.program(name).cborByteString != fresh().cborByteString =>
                name
        }
        assert(
          stale.isEmpty,
          s"pinned blueprints are stale: ${stale.mkString(", ")}. The validator sources compile " +
              "to different UPLC than src/main/resources/META-INF/scalus/blueprints/. Run " +
              "`sbt blueprintPin` and commit the diff."
        )
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
