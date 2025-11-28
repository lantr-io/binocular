package binocular

import munit.FunSuite
import scalus.Compiler
import scalus.compiler.sir.TargetLoweringBackend
import scalus.{toUplc, toUplcOptimized, show, plutusV3}

import java.io.{File, PrintWriter}
import java.security.MessageDigest

/** Test to verify deterministic compilation of the BitcoinValidator contract.
  *
  * PURPOSE: This test detects non-deterministic compilation in the Scalus compiler. If the contract
  * hash changes unexpectedly between compilations (without any code changes), it indicates a
  * non-determinism bug in the compiler.
  *
  * IMPORTANT: The expected hash should be updated whenever BitcoinValidator.scala is intentionally
  * modified. If this test fails after a code change, update EXPECTED_CONTRACT_HASH to the new
  * value.
  *
  * Background: We discovered a non-determinism bug in Scalus where the `topologicalSort` function
  * in Lowering.scala iterated over a Set (non-deterministic order). This caused variables with no
  * dependencies to be emitted in different orders across compilations, resulting in different de
  * Bruijn indices and different script hashes.
  *
  * The fix was to sort by `.id` before iterating:
  *   - Lowering.scala:694: value.usedUplevelVars.toList.sortBy(_.id).foreach(visit)
  *   - Lowering.scala:698: values.toList.sortBy(_.id).foreach(visit)
  */
class ContractDeterminismTest extends FunSuite {

    /** Expected contract hash (first 16 hex chars of SHA-256 of CBOR).
      *
      * UPDATE THIS VALUE when BitcoinValidator.scala is intentionally modified. The new hash will
      * be printed by BitcoinContract when a new variant is detected.
      */
    private val EXPECTED_CONTRACT_HASH = "c98d77fa1b13896b"

    /** Compute the contract hash the same way BitcoinContract does */
    private def computeContractHash(cborHex: String): String = {
        val digest = MessageDigest.getInstance("SHA-256")
        val hashBytes = digest.digest(cborHex.getBytes("UTF-8"))
        hashBytes.take(8).map("%02x".format(_)).mkString
    }

    /** Dump SIR and UPLC files for debugging when hash mismatch is detected */
    private def dumpDebugFiles(actualHash: String): Unit = {
        val debugDir = new File(".contract-debug")
        if !debugDir.exists() then debugDir.mkdirs()

        given Compiler.Options = Compiler.Options(
          optimizeUplc = true,
          generateErrorTraces = true,
          targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering
        )

        println(s"[ContractDeterminismTest] Dumping debug files for hash: $actualHash")

        val sir = Compiler.compileWithOptions(summon[Compiler.Options], BitcoinValidator.validate)
        val unoptimizedProgram = sir.toUplc().plutusV3
        val optimizedProgram = sir.toUplcOptimized().plutusV3

        // Write SIR
        val sirWriter = new PrintWriter(new File(debugDir, s"contract-$actualHash.sir"))
        try {
            sirWriter.println(s"# Contract hash: $actualHash")
            sirWriter.println(s"# Generated at: ${java.time.Instant.now()}")
            sirWriter.println()
            sirWriter.println(sir.show)
        } finally sirWriter.close()

        // Write unoptimized UPLC (named)
        val uplcWriter = new PrintWriter(new File(debugDir, s"contract-$actualHash.uplc"))
        try {
            uplcWriter.println(s"# Contract hash: $actualHash")
            uplcWriter.println(s"# Unoptimized UPLC (named)")
            uplcWriter.println()
            uplcWriter.println(unoptimizedProgram.show)
        } finally uplcWriter.close()

        // Write optimized UPLC (named)
        val uplcOptWriter = new PrintWriter(new File(debugDir, s"contract-$actualHash.uplc-opt"))
        try {
            uplcOptWriter.println(s"# Contract hash: $actualHash")
            uplcOptWriter.println(s"# Optimized UPLC (named)")
            uplcOptWriter.println()
            uplcOptWriter.println(optimizedProgram.show)
        } finally uplcOptWriter.close()

        // Write unoptimized UPLC (de Bruijn)
        val uplcDbWriter = new PrintWriter(new File(debugDir, s"contract-$actualHash.uplc-db"))
        try {
            uplcDbWriter.println(s"# Contract hash: $actualHash")
            uplcDbWriter.println(s"# Unoptimized UPLC (de Bruijn)")
            uplcDbWriter.println()
            uplcDbWriter.println(unoptimizedProgram.deBruijnedProgram.show)
        } finally uplcDbWriter.close()

        // Write optimized UPLC (de Bruijn)
        val uplcOptDbWriter =
            new PrintWriter(new File(debugDir, s"contract-$actualHash.uplc-opt-db"))
        try {
            uplcOptDbWriter.println(s"# Contract hash: $actualHash")
            uplcOptDbWriter.println(s"# Optimized UPLC (de Bruijn)")
            uplcOptDbWriter.println()
            uplcOptDbWriter.println(optimizedProgram.deBruijnedProgram.show)
        } finally uplcOptDbWriter.close()

        // Write CBOR hex
        val cborWriter = new PrintWriter(new File(debugDir, s"contract-$actualHash.cbor"))
        try {
            cborWriter.print(optimizedProgram.doubleCborHex)
        } finally cborWriter.close()

        println(s"[ContractDeterminismTest] Debug files written to ${debugDir.getPath}/")
    }

    test("BitcoinValidator contract hash should be deterministic") {
        val program = BitcoinContract.bitcoinProgram
        val cborHex = program.doubleCborHex
        val actualHash = computeContractHash(cborHex)

        if actualHash != EXPECTED_CONTRACT_HASH then {
            // Dump debug files before failing the test
            dumpDebugFiles(actualHash)
        }

        assertEquals(
          actualHash,
          EXPECTED_CONTRACT_HASH,
          s"""Contract hash mismatch!

If you intentionally modified BitcoinValidator.scala:
  Update EXPECTED_CONTRACT_HASH in ContractDeterminismTest.scala to: "$actualHash"

If you did NOT modify BitcoinValidator.scala:
  This indicates a NON-DETERMINISM BUG in the Scalus compiler!
  The contract is producing different output across compilations.
  Check .contract-debug/ directory for SIR/UPLC dumps to diagnose.
"""
        )
    }

    test("contract hash should be stable across multiple accesses") {
        // Access the program multiple times within the same JVM
        val hash1 = computeContractHash(BitcoinContract.bitcoinProgram.doubleCborHex)
        val hash2 = computeContractHash(BitcoinContract.bitcoinProgram.doubleCborHex)
        val hash3 = computeContractHash(BitcoinContract.bitcoinProgram.doubleCborHex)

        assertEquals(hash1, hash2, "Contract hash changed between accesses")
        assertEquals(hash2, hash3, "Contract hash changed between accesses")
    }
}
