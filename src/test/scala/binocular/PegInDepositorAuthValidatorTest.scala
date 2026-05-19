package binocular

import binocular.watchtower.*

import org.bitcoins.crypto.ECPrivateKey
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude.{List as ScalusList, *}
import scalus.cardano.onchain.plutus.v1.{Address, Credential, Value}
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.v3.ScriptInfo.RewardingScript
import scalus.testing.kit.ScalusTest
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Builtins.sha2_256
import scalus.uplc.builtin.Data.toData
import scodec.bits.ByteVector

/** CEK-evaluation tests for [[PegInDepositorAuthValidator]].
  *
  * Builds a synthetic Plutus V3 ScriptContext (one peg-in input bearing an
  * inline `PegInDatum`, a rewarding-script invocation as the entry point),
  * produces a real BIP340 Schnorr signature via bitcoin-s `ECPrivateKey`,
  * and asserts the validator accepts/rejects as expected.
  */
class PegInDepositorAuthValidatorTest extends AnyFunSuite with ScalusTest {

    private val testContract = PegInDepositorAuthContract.contract.withErrorTraces
    private val testProgram = testContract.program.deBruijnedProgram

    // --- helpers ---

    private def filledBytes(value: Int, size: Int): ByteString =
        ByteString.fromArray(Array.fill[Byte](size)(value.toByte))

    /** 28-byte script hash placeholder. */
    private def scriptHash28(seed: Int): ByteString = filledBytes(seed, 28)

    /** Build a script-address output holding the peg-in datum inline. */
    private def pegInTxOut(datum: PegInDatum): TxOut = TxOut(
      address = Address(Credential.ScriptCredential(scriptHash28(0xab)), Option.None),
      value = Value.zero,
      datum = OutputDatum.OutputDatum(datum.toData),
      referenceScript = Option.None
    )

    /** Build a TxInInfo wrapping the peg-in output. */
    private def pegInInput(datum: PegInDatum): TxInInfo = TxInInfo(
      outRef = TxOutRef(TxId(filledBytes(0x01, 32)), BigInt(0)),
      resolved = pegInTxOut(datum)
    )

    /** Build a minimal ScriptContext: one input (the peg-in), rewarding-script
      * invocation, no other tx data.
      */
    private def buildScriptContext(
        datum: PegInDatum,
        redeemer: Data,
        inputIndex: BigInt = BigInt(0)
    ): ScriptContext = {
        val txInfo = TxInfo(
          inputs = ScalusList.Cons(pegInInput(datum), ScalusList.Nil),
          id = TxId(filledBytes(0x00, 32))
        )
        val credential = Credential.ScriptCredential(scriptHash28(0xcd))
        ScriptContext(
          txInfo = txInfo,
          redeemer = redeemer,
          scriptInfo = RewardingScript(credential)
        )
    }

    private case class KeyAndSig(xonly: ByteString, signature: ByteString)

    /** Generate a fresh secp256k1 keypair and BIP340-sign `msg`. */
    private def freshKeyAndSig(msg: ByteString): KeyAndSig = {
        val priv = ECPrivateKey.freshPrivateKey
        val xonly = priv.schnorrPublicKey.bytes.toArray
        val sig = priv.schnorrSign(ByteVector(msg.bytes)).bytes.toArray
        KeyAndSig(
          xonly = ByteString.fromArray(xonly),
          signature = ByteString.fromArray(sig)
        )
    }

    /** Construct a complete `PegInDatum` populated with placeholder values for
      * everything other than `pegInUtxoId` and `userSourceChainPubKey`, which
      * are the only fields the validator inspects.
      */
    private def datumFor(pegInUtxoId: ByteString, xonly: ByteString): PegInDatum =
        PegInDatum(
          ownerAuth = AuthorizationMethod.CardanoWithdrawScript(scriptHash28(0xcd)),
          sourceChainPegInRawTx = ByteString.empty,
          sourceChainPegInRawTxIndex = BigInt(0),
          pegInUtxoId = pegInUtxoId,
          sourceChainTreasuryUtxoId = ByteString.empty,
          pegInAmount = BigInt(0),
          userSourceChainPubKey = xonly
        )

    // --- tests ---

    test("valid BIP340 Schnorr signature on sha2_256(pegInUtxoId) succeeds") {
        val pegInUtxoId = filledBytes(0x42, 36)
        val ks = freshKeyAndSig(sha2_256(pegInUtxoId))

        val datum = datumFor(pegInUtxoId, ks.xonly)
        val redeemer = PegInDepositorAuthRedeemer(
          pegInInputIndex = BigInt(0),
          signature = ks.signature
        ).toData

        val sc = buildScriptContext(datum, redeemer)
        val result = testProgram.applyArg(sc.toData).evaluateDebug
        assert(result.isSuccess, s"Expected success, got: $result")
    }

    test("signature by a different key fails") {
        val pegInUtxoId = filledBytes(0x42, 36)
        val signer = freshKeyAndSig(sha2_256(pegInUtxoId))
        val otherKey = ECPrivateKey.freshPrivateKey
        val otherXonly = ByteString.fromArray(otherKey.schnorrPublicKey.bytes.toArray)

        val datum = datumFor(pegInUtxoId, otherXonly)
        val redeemer = PegInDepositorAuthRedeemer(
          pegInInputIndex = BigInt(0),
          signature = signer.signature
        ).toData

        val sc = buildScriptContext(datum, redeemer)
        val result = testProgram.applyArg(sc.toData).evaluateDebug
        assert(!result.isSuccess, "Expected failure when sig is from a different key")
    }

    test("signature over wrong message fails") {
        val pegInUtxoId = filledBytes(0x42, 36)
        val ks = freshKeyAndSig(sha2_256(filledBytes(0x99, 36)))

        val datum = datumFor(pegInUtxoId, ks.xonly)
        val redeemer = PegInDepositorAuthRedeemer(
          pegInInputIndex = BigInt(0),
          signature = ks.signature
        ).toData

        val sc = buildScriptContext(datum, redeemer)
        val result = testProgram.applyArg(sc.toData).evaluateDebug
        assert(!result.isSuccess, "Expected failure when sig is over a different message")
    }

    test("tampered signature byte fails") {
        val pegInUtxoId = filledBytes(0x42, 36)
        val ks = freshKeyAndSig(sha2_256(pegInUtxoId))

        // Flip the first byte of the signature.
        val tamperedSig = {
            val b = ks.signature.bytes.clone()
            b(0) = (b(0) ^ 0x01).toByte
            ByteString.fromArray(b)
        }

        val datum = datumFor(pegInUtxoId, ks.xonly)
        val redeemer = PegInDepositorAuthRedeemer(
          pegInInputIndex = BigInt(0),
          signature = tamperedSig
        ).toData

        val sc = buildScriptContext(datum, redeemer)
        val result = testProgram.applyArg(sc.toData).evaluateDebug
        assert(!result.isSuccess, "Expected failure when sig is tampered with")
    }
}
