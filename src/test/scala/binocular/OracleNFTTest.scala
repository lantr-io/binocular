package binocular

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.hex
import scalus.uplc.builtin.Data.toData
import scalus.cardano.onchain.plutus.prelude
import scalus.cardano.onchain.plutus.prelude.List as ScalusList
import scalus.cardano.onchain.plutus.v1.{Address as OnchainAddress, Credential, PolicyId, PubKeyHash, Value}
import scalus.cardano.onchain.plutus.v2.{OutputDatum, TxOut}
import scalus.cardano.onchain.plutus.v3.{ScriptContext, ScriptInfo, TxId, TxInInfo, TxInfo, TxOutRef}

/** Tests for one-shot Oracle NFT minting policy.
  *
  * The BitcoinValidator's mint method enforces:
  *   1. The parameterized TxOutRef must be consumed as input (one-shot guarantee)
  *   2. Exactly 1 token is minted with empty token name
  *   3. The oracle output (at redeemer index) contains the NFT
  *   4. The oracle output goes to the script address (policyId == script hash)
  *   5. Burning (negative mint qty) is always allowed
  */
class OracleNFTTest extends AnyFunSuite {

    // Use a deterministic policy ID for testing
    private val testPolicyId: PolicyId =
        hex"aabbccddee00112233445566778899aabbccddee00112233445566"

    // The one-shot TxOutRef that must be consumed
    private val requiredTxOutRef: TxOutRef = TxOutRef(
      TxId(hex"1111111111111111111111111111111111111111111111111111111111111111"),
      BigInt(0)
    )

    // Script address derived from policyId (since policyId == script hash for combined scripts)
    private val scriptAddress: OnchainAddress =
        OnchainAddress.fromScriptHash(testPolicyId)

    // A different address (not the script)
    private val otherAddress: OnchainAddress =
        OnchainAddress.fromPubKeyHash(
          PubKeyHash(hex"deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef0000")
        )

    private val dummyTxId: TxId =
        TxId(hex"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")

    /** Helper to build a minting ScriptContext */
    private def makeMintingContext(
        mintValue: Value,
        inputs: ScalusList[TxInInfo],
        outputs: ScalusList[TxOut],
        redeemer: BigInt = BigInt(0) // output index
    ): ScriptContext = {
        val txInfo = TxInfo(
          inputs = inputs,
          outputs = outputs,
          mint = mintValue,
          id = dummyTxId
        )
        ScriptContext(
          txInfo = txInfo,
          redeemer = redeemer.toData,
          scriptInfo = ScriptInfo.MintingScript(policyId = testPolicyId)
        )
    }

    /** Helper: TxInInfo consuming the required one-shot UTxO */
    private val requiredInput: TxInInfo = TxInInfo(
      outRef = requiredTxOutRef,
      resolved = TxOut(
        address = otherAddress,
        value = Value.lovelace(BigInt(5_000_000))
      )
    )

    /** Helper: oracle output at script address with NFT */
    private val oracleOutputWithNft: TxOut = TxOut(
      address = scriptAddress,
      value = Value.lovelace(BigInt(5_000_000)) + Value(testPolicyId, ByteString.empty, BigInt(1)),
      datum = OutputDatum.NoOutputDatum
    )

    test("mint succeeds when required UTxO is spent and exactly 1 token minted") {
        val mintValue = Value(testPolicyId, ByteString.empty, BigInt(1))

        val ctx = makeMintingContext(
          mintValue = mintValue,
          inputs = ScalusList(requiredInput),
          outputs = ScalusList(oracleOutputWithNft)
        )

        // Should not throw
        BitcoinValidator.validate(requiredTxOutRef.toData)(ctx.toData)
    }

    test("mint fails when required UTxO is NOT in inputs") {
        val mintValue = Value(testPolicyId, ByteString.empty, BigInt(1))

        // Use a different input (not the required one-shot UTxO)
        val wrongInput = TxInInfo(
          outRef = TxOutRef(
            TxId(hex"2222222222222222222222222222222222222222222222222222222222222222"),
            BigInt(0)
          ),
          resolved = TxOut(
            address = otherAddress,
            value = Value.lovelace(BigInt(5_000_000))
          )
        )

        val ctx = makeMintingContext(
          mintValue = mintValue,
          inputs = ScalusList(wrongInput),
          outputs = ScalusList(oracleOutputWithNft)
        )

        intercept[RuntimeException] {
            BitcoinValidator.validate(requiredTxOutRef.toData)(ctx.toData)
        }
    }

    test("mint fails when minting quantity != 1") {
        // Try minting 2 tokens
        val mintValue = Value(testPolicyId, ByteString.empty, BigInt(2))

        val oracleOutputWith2 = TxOut(
          address = scriptAddress,
          value = Value.lovelace(BigInt(5_000_000)) + Value(
            testPolicyId,
            ByteString.empty,
            BigInt(2)
          ),
          datum = OutputDatum.NoOutputDatum
        )

        val ctx = makeMintingContext(
          mintValue = mintValue,
          inputs = ScalusList(requiredInput),
          outputs = ScalusList(oracleOutputWith2)
        )

        intercept[RuntimeException] {
            BitcoinValidator.validate(requiredTxOutRef.toData)(ctx.toData)
        }
    }

    test("mint fails when oracle output does not contain NFT") {
        val mintValue = Value(testPolicyId, ByteString.empty, BigInt(1))

        // Output without NFT
        val oracleOutputNoNft = TxOut(
          address = scriptAddress,
          value = Value.lovelace(BigInt(5_000_000)),
          datum = OutputDatum.NoOutputDatum
        )

        val ctx = makeMintingContext(
          mintValue = mintValue,
          inputs = ScalusList(requiredInput),
          outputs = ScalusList(oracleOutputNoNft)
        )

        intercept[RuntimeException] {
            BitcoinValidator.validate(requiredTxOutRef.toData)(ctx.toData)
        }
    }

    test("mint fails when oracle output goes to wrong address") {
        val mintValue = Value(testPolicyId, ByteString.empty, BigInt(1))

        // Output at wrong address (not the script address)
        val wrongAddressOutput = TxOut(
          address = otherAddress,
          value = Value.lovelace(BigInt(5_000_000)) + Value(
            testPolicyId,
            ByteString.empty,
            BigInt(1)
          ),
          datum = OutputDatum.NoOutputDatum
        )

        val ctx = makeMintingContext(
          mintValue = mintValue,
          inputs = ScalusList(requiredInput),
          outputs = ScalusList(wrongAddressOutput)
        )

        intercept[RuntimeException] {
            BitcoinValidator.validate(requiredTxOutRef.toData)(ctx.toData)
        }
    }

    test("burn always succeeds (negative quantity, no UTxO check needed)") {
        // Burning: negative mint quantity
        val burnValue = Value(testPolicyId, ByteString.empty, BigInt(-1))

        // No required UTxO in inputs, no oracle output - still should succeed
        val someInput = TxInInfo(
          outRef = TxOutRef(
            TxId(hex"3333333333333333333333333333333333333333333333333333333333333333"),
            BigInt(0)
          ),
          resolved = TxOut(
            address = otherAddress,
            value = Value.lovelace(BigInt(5_000_000)) + Value(
              testPolicyId,
              ByteString.empty,
              BigInt(1)
            )
          )
        )

        val ctx = makeMintingContext(
          mintValue = burnValue,
          inputs = ScalusList(someInput),
          outputs = ScalusList.Nil
        )

        // Should not throw - burning is always allowed
        BitcoinValidator.validate(requiredTxOutRef.toData)(ctx.toData)
    }
}
