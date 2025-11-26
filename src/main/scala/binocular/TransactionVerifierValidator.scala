package binocular

import scalus.builtin.*
import scalus.builtin.Builtins.*
import scalus.builtin.Data.{toData, FromData, ToData}
import scalus.ledger.api.v2.OutputDatum
import scalus.ledger.api.v3.*
import scalus.prelude.{List, *}
import scalus.{Compile, Compiler, *}
import scalus.compiler.sir.TargetLoweringBackend
import scalus.uplc.Program

/** Datum for the TransactionVerifier contract
  *
  * @param expectedTxHash
  *   The Bitcoin transaction hash that must be proven to exist (in Bitcoin internal byte order)
  * @param expectedBlockHash
  *   The hash of the Bitcoin block containing the transaction (must be confirmed in Oracle)
  * @param oracleScriptHash
  *   Script hash of the Binocular Oracle validator (to identify Oracle UTxO in reference inputs)
  */
case class TxVerifierDatum(
    expectedTxHash: TxHash,
    expectedBlockHash: BlockHash,
    oracleScriptHash: ByteString
) derives FromData,
      ToData

@Compile
object TxVerifierDatum

/** Redeemer for proving transaction inclusion with Oracle verification
  *
  * @param txIndex
  *   Index of the transaction in the block (0-based)
  * @param txMerkleProof
  *   Merkle proof path from transaction to block's merkle root (list of sibling hashes)
  * @param blockIndex
  *   Index of the block in the Oracle's confirmed blocks tree (0-based)
  * @param blockMerkleProof
  *   Merkle proof path from block hash to confirmed blocks tree root
  * @param blockHeader
  *   The 80-byte Bitcoin block header (to extract merkle root and verify hash)
  */
case class TxVerifierRedeemer(
    txIndex: BigInt,
    txMerkleProof: List[TxHash],
    blockIndex: BigInt,
    blockMerkleProof: List[BlockHash],
    blockHeader: BlockHeader
) derives FromData,
      ToData

@Compile
object TxVerifierRedeemer

/** A validator that verifies Bitcoin transaction inclusion using the Binocular Oracle.
  *
  * This validator locks funds that can only be spent when a valid proof is provided showing that a
  * specific Bitcoin transaction exists in a block that has been confirmed by the Binocular Oracle.
  *
  * Security: Unlike a naive implementation that accepts any merkle root, this validator:
  *   1. Requires the Oracle UTxO as a reference input
  *   2. Verifies the block is in the Oracle's confirmed blocks tree
  *   3. Verifies the block header hashes to the expected block hash
  *   4. Verifies the transaction is in the block via merkle proof
  *
  * This ensures the transaction actually exists in the Bitcoin blockchain as attested by the Oracle.
  */
@Compile
object TransactionVerifierValidator {

    /** Find Oracle UTxO in reference inputs by script hash */
    def findOracleInput(
        refInputs: List[TxInInfo],
        oracleScriptHash: ByteString
    ): TxOut = {
        def search(remaining: List[TxInInfo]): TxOut = {
            remaining match
                case List.Nil => fail("Oracle reference input not found")
                case List.Cons(input, tail) =>
                    val txOut = input.resolved
                    // Check if this output is at the oracle script address
                    // The address credential should contain the script hash
                    val credential = txOut.address.credential
                    credential match
                        case Credential.ScriptCredential(hash) =>
                            if hash == oracleScriptHash then txOut
                            else search(tail)
                        case _ => search(tail)
        }
        search(refInputs)
    }

    /** Extract ChainState from Oracle UTxO datum */
    def getOracleState(oracleOutput: TxOut): ChainState = {
        oracleOutput.datum match
            case OutputDatum.OutputDatum(datum) => datum.to[ChainState]
            case _ => fail("Oracle must have inline datum")
    }

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

        // Step 1: Find and read Oracle state from reference inputs
        val oracleOutput = findOracleInput(tx.referenceInputs, datum.oracleScriptHash)
        val oracleState = getOracleState(oracleOutput)

        // Step 2: Compute the confirmed blocks tree root from Oracle state
        val confirmedTreeRoot = BitcoinValidator.getMerkleRoot(oracleState.confirmedBlocksTree)

        // Step 3: Verify block is in Oracle's confirmed blocks tree
        val computedTreeRoot = BitcoinValidator.merkleRootFromInclusionProof(
          proof.blockMerkleProof,
          datum.expectedBlockHash,
          proof.blockIndex
        )
        require(
          computedTreeRoot == confirmedTreeRoot,
          "Block not found in Oracle's confirmed blocks tree"
        )

        // Step 4: Verify block header hashes to expected block hash
        val computedBlockHash = BitcoinValidator.blockHeaderHash(proof.blockHeader)
        require(
          computedBlockHash == datum.expectedBlockHash,
          "Block header does not hash to expected block hash"
        )

        // Step 5: Extract merkle root from block header
        val blockMerkleRoot = proof.blockHeader.merkleRoot

        // Step 6: Verify transaction is in the block via merkle proof
        val computedTxRoot = BitcoinValidator.merkleRootFromInclusionProof(
          proof.txMerkleProof,
          datum.expectedTxHash,
          proof.txIndex
        )
        require(
          computedTxRoot == blockMerkleRoot,
          "Transaction merkle proof verification failed"
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
