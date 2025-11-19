package binocular

import scalus.builtin.*
import scalus.builtin.Builtins.*
import scalus.builtin.Data.{toData, FromData, ToData}
import scalus.ledger.api.v3.*
import scalus.prelude.{List, *}
import scalus.{Compile, Compiler, *}
import scalus.compiler.sir.TargetLoweringBackend
import scalus.uplc.Program

/** Datum for the TransactionVerifier contract
  * @param expectedTxHash
  *   The Bitcoin transaction hash that must be proven to exist
  * @param blockMerkleRoot
  *   The Merkle root of the block containing the transaction
  */
case class TxVerifierDatum(
    expectedTxHash: TxHash,
    blockMerkleRoot: MerkleRoot
) derives FromData,
      ToData

@Compile
object TxVerifierDatum

/** Redeemer for proving transaction inclusion
  * @param txIndex
  *   Index of the transaction in the block
  * @param merkleProof
  *   Merkle proof path (list of sibling hashes)
  */
case class TxVerifierRedeemer(
    txIndex: BigInt,
    merkleProof: List[TxHash]
) derives FromData,
      ToData

@Compile
object TxVerifierRedeemer

/** A simple validator that verifies Bitcoin transaction inclusion using Merkle proofs.
  *
  * This validator locks funds that can only be spent when a valid Merkle proof is provided showing
  * that a specific Bitcoin transaction is included in a block.
  *
  * The validator:
  *   1. Takes the expected transaction hash and block Merkle root from the datum
  *   2. Uses the Merkle proof from the redeemer to compute the root
  *   3. Verifies the computed root matches the expected block Merkle root
  */
@Compile
object TransactionVerifierValidator {

    /** Main validator entry point - spending validator function */
    inline def spend(
        datumOpt: scalus.prelude.Option[Datum],
        redeemer: Datum,
        tx: TxInfo,
        outRef: TxOutRef
    ): Unit = {
        // Extract datum
        val datum = datumOpt match {
            case scalus.prelude.Option.Some(d) => d.to[TxVerifierDatum]
            case scalus.prelude.Option.None    => fail("Missing datum")
        }

        // Extract redeemer
        val proof = redeemer.to[TxVerifierRedeemer]

        // Compute Merkle root from proof using existing BitcoinValidator function
        val computedRoot = BitcoinValidator.merkleRootFromInclusionProof(
          proof.merkleProof,
          datum.expectedTxHash,
          proof.txIndex
        )

        // Verify computed root matches expected block Merkle root
        require(
          computedRoot == datum.blockMerkleRoot,
          "Merkle proof verification failed: computed root does not match expected block Merkle root"
        )
    }

    /** Validate entry point for Scalus compiler - parses ScriptContext and calls spend */
    def validate(scData: Data): Unit = {
        val sc = unConstrData(scData).snd
        val txInfoData = sc.head
        val redeemer = sc.tail.head
        val scriptInfo = unConstrData(sc.tail.tail.head)
        if scriptInfo.fst == BigInt(1) then
            val txOutRef = scriptInfo.snd.head.to[TxOutRef]
            val datum = scriptInfo.snd.tail.head.to[scalus.prelude.Option[Datum]]
            val txInfo = txInfoData.to[TxInfo]
            spend(datum, redeemer, txInfo, txOutRef)
        else fail("Invalid script context")
    }
}

object TransactionVerifierContract {
    given Compiler.Options = Compiler.Options(
      optimizeUplc = true,
      generateErrorTraces = true,
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering
    )

    def compileVerifierProgram(): Program = {
        val sir = Compiler.compileWithOptions(
          summon[Compiler.Options],
          TransactionVerifierValidator.validate
        )
        sir.toUplcOptimized().plutusV3
    }

    lazy val validator: Program = compileVerifierProgram()
}
