package binocular

import scalus.Compiler
import scalus.compiler.sir.TargetLoweringBackend
import scalus.*
import scalus.uplc.Program

import java.io.{File, PrintWriter}
import java.security.MessageDigest

object BitcoinContract {
    // Change this value to force recompilation
    val RECOMPILE_TRIGGER = 3

    given Compiler.Options = Compiler.Options(
      optimizeUplc = true,
      generateErrorTraces = true,
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering
    )

    private val debugDir = new File(".contract-debug")

    /** Compute a short hash (first 16 hex chars of SHA-256) of the CBOR hex */
    private def computeContractHash(cborHex: String): String = {
        val digest = MessageDigest.getInstance("SHA-256")
        val hashBytes = digest.digest(cborHex.getBytes("UTF-8"))
        hashBytes.take(8).map("%02x".format(_)).mkString
    }

    /** Dump SIR, UPLC (named and de Bruijn), optimized and unoptimized, to files if this contract
      * variant hasn't been seen before
      */
    private def dumpContractIfNew(
        sir: scalus.compiler.sir.SIR,
        unoptimizedProgram: Program,
        optimizedProgram: Program
    ): Unit = {
        val cborHex = optimizedProgram.doubleCborHex
        val hash = computeContractHash(cborHex)

        if !debugDir.exists() then {
            debugDir.mkdirs()
        }

        val sirFile = new File(debugDir, s"contract-$hash.sir")
        val uplcFile = new File(debugDir, s"contract-$hash.uplc")
        val uplcOptFile = new File(debugDir, s"contract-$hash.uplc-opt")
        val uplcDbFile = new File(debugDir, s"contract-$hash.uplc-db")
        val uplcOptDbFile = new File(debugDir, s"contract-$hash.uplc-opt-db")
        val cborFile = new File(debugDir, s"contract-$hash.cbor")

        // Only write if files don't exist (new contract variant)
        if !sirFile.exists() || !uplcFile.exists() then {
            println(s"[BitcoinContract] New contract variant detected: $hash")
            println(s"[BitcoinContract] Dumping SIR and UPLC to ${debugDir.getPath}/")

            // Write SIR
            val sirWriter = new PrintWriter(sirFile)
            try {
                sirWriter.println(s"# Contract hash: $hash")
                sirWriter.println(s"# Generated at: ${java.time.Instant.now()}")
                sirWriter.println()
                sirWriter.println(sir.show)
            } finally {
                sirWriter.close()
            }

            // Write unoptimized UPLC (named)
            val uplcWriter = new PrintWriter(uplcFile)
            try {
                uplcWriter.println(s"# Contract hash: $hash")
                uplcWriter.println(s"# Unoptimized UPLC (named)")
                uplcWriter.println()
                uplcWriter.println(unoptimizedProgram.show)
            } finally {
                uplcWriter.close()
            }

            // Write optimized UPLC (named)
            val uplcOptWriter = new PrintWriter(uplcOptFile)
            try {
                uplcOptWriter.println(s"# Contract hash: $hash")
                uplcOptWriter.println(s"# Optimized UPLC (named)")
                uplcOptWriter.println()
                uplcOptWriter.println(optimizedProgram.show)
            } finally {
                uplcOptWriter.close()
            }

            // Write unoptimized UPLC (de Bruijn)
            val uplcDbWriter = new PrintWriter(uplcDbFile)
            try {
                uplcDbWriter.println(s"# Contract hash: $hash")
                uplcDbWriter.println(s"# Unoptimized UPLC (de Bruijn)")
                uplcDbWriter.println()
                uplcDbWriter.println(unoptimizedProgram.deBruijnedProgram.show)
            } finally {
                uplcDbWriter.close()
            }

            // Write optimized UPLC (de Bruijn)
            val uplcOptDbWriter = new PrintWriter(uplcOptDbFile)
            try {
                uplcOptDbWriter.println(s"# Contract hash: $hash")
                uplcOptDbWriter.println(s"# Optimized UPLC (de Bruijn)")
                uplcOptDbWriter.println()
                uplcOptDbWriter.println(optimizedProgram.deBruijnedProgram.show)
            } finally {
                uplcOptDbWriter.close()
            }

            // Write CBOR hex
            val cborWriter = new PrintWriter(cborFile)
            try {
                cborWriter.print(cborHex)
            } finally {
                cborWriter.close()
            }

            println(
              s"[BitcoinContract] Files written: .sir, .uplc, .uplc-opt, .uplc-db, .uplc-opt-db, .cbor"
            )
        } else {
            println(s"[BitcoinContract] Contract hash: $hash (already recorded)")
        }
    }

    def compileBitcoinProgram(): Program =
        // validate for SirToUplcV3Lowering
        // validate2 for SumOfProductsLowering
        val sir = Compiler.compileWithOptions(summon[Compiler.Options], BitcoinValidator.validate)
        val unoptimizedProgram = sir.toUplc().plutusV3
        val optimizedProgram = sir.toUplcOptimized().plutusV3

        // Dump contract if this is a new variant
        dumpContractIfNew(sir, unoptimizedProgram, optimizedProgram)

        optimizedProgram

    lazy val bitcoinProgram: Program = compileBitcoinProgram()
}
