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
import scalus.uplc.builtin.Builtins.{appendByteString, serialiseData, sha2_256}
import scalus.uplc.builtin.Data.toData
import scodec.bits.ByteVector

/** CEK-evaluation tests for the recipient-binding [[PegInDepositorAuthValidator]].
  *
  * Builds a synthetic Plutus V3 ScriptContext (one peg-in input bearing an inline `PegInDatum`, one
  * fBTC output, a rewarding-script invocation), produces a real BIP340 Schnorr signature over the
  * per-mint message via bitcoin-s `ECPrivateKey`, and asserts the validator accepts/rejects. The
  * central property is recipient-binding: a signature is only usable to pay fBTC to the address the
  * depositor signed for.
  */
class PegInDepositorAuthValidatorTest extends AnyFunSuite with ScalusTest {

    // --- fixed fBTC asset params (placeholder, mirrors BridgeConfig defaults) ---
    private val fbtcPolicy = filledBytes(0xbb, 28)
    private val fbtcAsset = ByteString.fromHex("6642544300000000")
    private val authParams = PegInDepositorAuthParams(fbtcPolicy, fbtcAsset)

    private val testContract = PegInDepositorAuthContract.makeContract(authParams).withErrorTraces
    private val testProgram = testContract.program.deBruijnedProgram

    private val pegInAmount = BigInt(4000)
    private val btcTxid = filledBytes(0x77, 32)

    // --- helpers ---

    private def filledBytes(value: Int, size: Int): ByteString =
        ByteString.fromArray(Array.fill[Byte](size)(value.toByte))

    private def scriptHash28(seed: Int): ByteString = filledBytes(seed, 28)

    /** A recipient address (the depositor's chosen Cardano address). */
    private def addr(seed: Int): Address =
        Address(Credential.ScriptCredential(scriptHash28(seed)), Option.None)

    /** Build a script-address output holding the peg-in datum inline. */
    private def pegInTxOut(datum: PegInDatum): TxOut = TxOut(
      address = Address(Credential.ScriptCredential(scriptHash28(0xab)), Option.None),
      value = Value.zero,
      datum = OutputDatum.OutputDatum(datum.toData),
      referenceScript = Option.None
    )

    private def pegInInput(datum: PegInDatum): TxInInfo = TxInInfo(
      outRef = TxOutRef(TxId(filledBytes(0x01, 32)), BigInt(0)),
      resolved = pegInTxOut(datum)
    )

    /** An fBTC output paying `amount` of the fBTC asset to `to`. */
    private def fbtcOut(to: Address, amount: BigInt): TxOut = TxOut(
      address = to,
      value = Value.unsafeFromList(ScalusList((fbtcPolicy, ScalusList((fbtcAsset, amount))))),
      datum = OutputDatum.NoOutputDatum,
      referenceScript = Option.None
    )

    /** ScriptContext: one peg-in input, the given outputs, rewarding-script entry. */
    private def buildScriptContext(
        datum: PegInDatum,
        redeemer: Data,
        outputs: ScalusList[TxOut]
    ): ScriptContext = {
        val txInfo = TxInfo(
          inputs = ScalusList.Cons(pegInInput(datum), ScalusList.Nil),
          outputs = outputs,
          id = TxId(filledBytes(0x00, 32))
        )
        ScriptContext(
          txInfo = txInfo,
          redeemer = redeemer,
          scriptInfo = RewardingScript(Credential.ScriptCredential(scriptHash28(0xcd)))
        )
    }

    /** The per-mint message: "BFR-mint-v1" ‖ btc_txid ‖ peg_in_utxo_id ‖ recipient, then sha2_256.
      */
    private def mintMessage(pegInUtxoId: ByteString, recipient: Data): ByteString =
        sha2_256(
          appendByteString(
            PegInDepositorAuthValidator.mintTag,
            appendByteString(btcTxid, appendByteString(pegInUtxoId, serialiseData(recipient)))
          )
        )

    private case class KeyAndSig(xonly: ByteString, signature: ByteString)

    private def freshKeyAndSig(msg: ByteString): KeyAndSig = {
        val priv = ECPrivateKey.freshPrivateKey
        KeyAndSig(
          xonly = ByteString.fromArray(priv.schnorrPublicKey.bytes.toArray),
          signature = ByteString.fromArray(priv.schnorrSign(ByteVector(msg.bytes)).bytes.toArray)
        )
    }

    private def datumFor(pegInUtxoId: ByteString, xonly: ByteString): PegInDatum =
        PegInDatum(
          ownerAuth = AuthorizationMethod.CardanoWithdrawScript(scriptHash28(0xcd)),
          sourceChainPegInRawTx = ByteString.empty,
          sourceChainPegInRawTxIndex = BigInt(0),
          pegInUtxoId = pegInUtxoId,
          sourceChainTreasuryUtxoId = ByteString.empty,
          pegInAmount = pegInAmount,
          userSourceChainPubKey = xonly
        )

    private def redeemerFor(
        recipient: Data,
        signature: ByteString,
        fbtcOutputIndex: BigInt = BigInt(0)
    ): Data =
        PegInDepositorAuthRedeemer(
          pegInInputIndex = BigInt(0),
          fbtcOutputIndex = fbtcOutputIndex,
          recipient = recipient,
          treasuryMovementBtcTxid = btcTxid,
          signature = signature
        ).toData

    // --- tests ---

    test("valid sig + fBTC paid to the signed recipient in the right amount succeeds") {
        val pegInUtxoId = filledBytes(0x42, 36)
        val recipient = addr(0x10)
        val ks = freshKeyAndSig(mintMessage(pegInUtxoId, recipient.toData))

        val datum = datumFor(pegInUtxoId, ks.xonly)
        val sc = buildScriptContext(
          datum,
          redeemerFor(recipient.toData, ks.signature),
          ScalusList.Cons(fbtcOut(recipient, pegInAmount), ScalusList.Nil)
        )
        val result = testProgram.applyArg(sc.toData).evaluateDebug
        assert(result.isSuccess, s"Expected success, got: $result")
    }

    test("front-run: signature valid for recipient, but fBTC routed elsewhere → fails") {
        // Attacker holds the depositor's signature (over recipient=victim) but tries to send the
        // minted fBTC to themselves. redeemer.recipient must equal the signed address (or the sig
        // fails), so the output address no longer matches → rejected.
        val pegInUtxoId = filledBytes(0x42, 36)
        val victim = addr(0x10)
        val attacker = addr(0x20)
        val ks = freshKeyAndSig(mintMessage(pegInUtxoId, victim.toData))

        val datum = datumFor(pegInUtxoId, ks.xonly)
        val sc = buildScriptContext(
          datum,
          redeemerFor(victim.toData, ks.signature),
          ScalusList.Cons(fbtcOut(attacker, pegInAmount), ScalusList.Nil) // routed to attacker
        )
        val result = testProgram.applyArg(sc.toData).evaluateDebug
        assert(
          !result.isSuccess,
          "Expected failure when fBTC is routed away from the signed recipient"
        )
    }

    test("wrong fBTC amount fails") {
        val pegInUtxoId = filledBytes(0x42, 36)
        val recipient = addr(0x10)
        val ks = freshKeyAndSig(mintMessage(pegInUtxoId, recipient.toData))

        val datum = datumFor(pegInUtxoId, ks.xonly)
        val sc = buildScriptContext(
          datum,
          redeemerFor(recipient.toData, ks.signature),
          ScalusList.Cons(fbtcOut(recipient, pegInAmount + 1), ScalusList.Nil)
        )
        val result = testProgram.applyArg(sc.toData).evaluateDebug
        assert(!result.isSuccess, "Expected failure when fBTC amount != peg_in_amount")
    }

    test("signature by a different key fails") {
        val pegInUtxoId = filledBytes(0x42, 36)
        val recipient = addr(0x10)
        val signer = freshKeyAndSig(mintMessage(pegInUtxoId, recipient.toData))
        val otherXonly =
            ByteString.fromArray(ECPrivateKey.freshPrivateKey.schnorrPublicKey.bytes.toArray)

        val datum = datumFor(pegInUtxoId, otherXonly)
        val sc = buildScriptContext(
          datum,
          redeemerFor(recipient.toData, signer.signature),
          ScalusList.Cons(fbtcOut(recipient, pegInAmount), ScalusList.Nil)
        )
        val result = testProgram.applyArg(sc.toData).evaluateDebug
        assert(!result.isSuccess, "Expected failure when sig is from a different key")
    }

    test("signature over a different recipient fails") {
        // The depositor signed for recipientA, but the redeemer + output use recipientB. The sig
        // verification (which serializes redeemer.recipient into the message) no longer matches.
        val pegInUtxoId = filledBytes(0x42, 36)
        val recipientA = addr(0x10)
        val recipientB = addr(0x20)
        val ks = freshKeyAndSig(mintMessage(pegInUtxoId, recipientA.toData))

        val datum = datumFor(pegInUtxoId, ks.xonly)
        val sc = buildScriptContext(
          datum,
          redeemerFor(recipientB.toData, ks.signature),
          ScalusList.Cons(fbtcOut(recipientB, pegInAmount), ScalusList.Nil)
        )
        val result = testProgram.applyArg(sc.toData).evaluateDebug
        assert(
          !result.isSuccess,
          "Expected failure when the signed recipient differs from the redeemer"
        )
    }
}
