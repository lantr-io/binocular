package binocular

import munit.FunSuite

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

    test("BitcoinValidator contract hash should be deterministic") {
        val program = BitcoinContract.bitcoinProgram
        val cborHex = program.doubleCborHex
        val actualHash = computeContractHash(cborHex)

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
