package binocular

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.script.{
    EmptyScriptPubKey, EmptyScriptSignature, ScriptPubKey, ScriptSignature,
    ScriptWitness, TaprootKeyPath
}
import org.bitcoins.core.protocol.transaction.*
import org.bitcoins.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude.{List as ScalusList, *}
import scalus.cardano.onchain.plutus.v1.Credential
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.v3.ScriptInfo.RewardingScript
import scalus.testing.kit.ScalusTest
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData
import scalus.uplc.eval.Result
import scodec.bits.ByteVector

class PegInVerifierValidatorTest extends AnyFunSuite with ScalusTest {

    private val testContract = PegInVerifierContract.contract.withErrorTraces
    private val testProgram = testContract.program.deBruijnedProgram

    // --- bitcoin-s helpers ---

    /** Build a witness-serialized Bitcoin tx from given inputs, witnesses, and a single dummy output.
      * Each input must have a corresponding witness entry. Use `TaprootKeyPath.dummy` for key-path
      * spends and `scriptPathWitness` for fake script-path spends in failure tests.
      */
    private def buildWitnessTx(
        inputs: Seq[TransactionInput],
        witnesses: Seq[ScriptWitness]
    ): WitnessTransaction =
        WitnessTransaction(
          Int32.two,
          inputs.toVector,
          Vector(TransactionOutput(Satoshis.zero, EmptyScriptPubKey)),
          UInt32.zero,
          TransactionWitness(witnesses.toVector)
        )

    /** Serialize a transaction to bytes.
      * BaseTransaction → non-witness bytes (used for peg-in tx, rawPegInTx).
      * WitnessTransaction → full witness-serialized bytes (used for TM tx, rawTmTx).
      */
    private def txBytes(tx: Transaction): ByteString =
        ByteString.fromArray(tx.bytes.toArray)

    /** Build a fake script-path witness from raw stack items (2+ items = not a key-path spend).
      * Uses raw ScriptWitness because TaprootScriptPath validates the real Taproot stack format.
      */
    private def scriptPathWitness(items: Array[Byte]*): ScriptWitness =
        ScriptWitness(items.map(ByteVector(_)).toVector)

    /** Create a TransactionInput from a 32-byte txid (display/BE order) and vout. */
    private def txInput(txidBE: Array[Byte], vout: Int): TransactionInput = {
        val outpoint = TransactionOutPoint(
          DoubleSha256DigestBE.fromBytes(ByteVector(txidBE)),
          UInt32(vout)
        )
        TransactionInput(outpoint, EmptyScriptSignature, UInt32.max)
    }

    /** Create a TransactionInput from a 32-byte txid in internal/LE byte order (e.g. sha256d
      * output) and vout.
      */
    private def txInputLE(txidLE: Array[Byte], vout: Int): TransactionInput = {
        val outpoint = TransactionOutPoint(
          DoubleSha256Digest.fromBytes(ByteVector(txidLE)),
          UInt32(vout)
        )
        TransactionInput(outpoint, EmptyScriptSignature, UInt32.max)
    }

    /** Build a simple peg-in tx: 1 input, 2 outputs (payment + OP_RETURN marker). */
    private def buildPegInTx(
        prevTxid: Array[Byte],
        prevVout: Int,
        paymentSatoshis: Long,
        scriptPubKey: ScriptPubKey
    ): BaseTransaction = {
        val input = txInput(prevTxid, prevVout)
        val paymentOutput = TransactionOutput(Satoshis(paymentSatoshis), scriptPubKey)
        // OP_RETURN "BFR" marker
        val opReturnScript = ScriptPubKey.fromAsmBytes(
          ByteVector(0x6a, 0x03, 'B'.toByte, 'F'.toByte, 'R'.toByte)
        )
        val opReturnOutput = TransactionOutput(Satoshis.zero, opReturnScript)
        BaseTransaction(
          Int32.two,
          Vector(input),
          Vector(paymentOutput, opReturnOutput),
          UInt32.zero
        )
    }

    /** Compute the outpoint bytes (txid LE 32 + vout LE 4) for a tx input. */
    private def outpointBytes(input: TransactionInput): ByteString =
        ByteString.fromArray(input.previousOutput.bytes.toArray)

    /** Deterministic 32-byte "txid" from an index. */
    private def fakeTxid(index: Int): Array[Byte] = {
        val arr = new Array[Byte](32)
        arr(0) = index.toByte
        arr(31) = (index + 1).toByte
        arr
    }

    // --- CEK machine evaluation ---

    /** Build the redeemer Data list matching the Aiken peg_in.ak contract expectations:
      * [treasury_utxo_id, raw_peg_in_tx, peg_in_amount, raw_tm_tx]
      */
    private def buildRedeemer(
        treasuryOutpoint: ByteString,
        rawPegInTx: ByteString,
        pegInAmount: Long,
        rawTmTx: ByteString
    ): Data =
        Data.List(
          ScalusList(
            Data.B(treasuryOutpoint),
            Data.B(rawPegInTx),
            Data.I(BigInt(pegInAmount)),
            Data.B(rawTmTx)
          )
        )

    /** Build a ScriptContext for a Rewarding (stake/withdraw) validator and evaluate via CEK. */
    private def evalValidator(redeemer: Data): Result = {
        val credential = Credential.ScriptCredential(ByteString.unsafeFromArray(Array.fill(28)(0)))
        val txId = TxId(ByteString.unsafeFromArray(Array.fill(32)(0)))
        val txInfo = TxInfo(
          inputs = ScalusList.Nil,
          id = txId
        )
        val scriptContext = ScriptContext(
          txInfo = txInfo,
          redeemer = redeemer,
          scriptInfo = RewardingScript(credential)
        )
        testProgram.applyArg(scriptContext.toData).evaluateDebug
    }

    // --- Tests ---

    test("CEK: valid TM tx spending treasury and peg-in succeeds") {
        val treasuryInput = txInput(fakeTxid(0x10), 0)

        val pegInTx = buildPegInTx(
          prevTxid = fakeTxid(0x01),
          prevVout = 0,
          paymentSatoshis = 100_000,
          scriptPubKey = ScriptPubKey.fromAsmHex("5120" + "0" * 64)
        )
        val rawPegInTx = txBytes(pegInTx)
        val pegInTxid = sha2_256(sha2_256(rawPegInTx)).bytes
        val pegInInput = txInputLE(pegInTxid, 0)

        val tmTx = buildWitnessTx(
          inputs = Seq(treasuryInput, pegInInput),
          witnesses = Seq(TaprootKeyPath.dummy, TaprootKeyPath.dummy)
        )
        val rawTmTx = txBytes(tmTx)

        val redeemer = buildRedeemer(outpointBytes(treasuryInput), rawPegInTx, 100_000, rawTmTx)
        val result = evalValidator(redeemer)
        assert(result.isInstanceOf[Result.Success], s"Expected success but got: $result")
        info(s"CPU: ${result.asInstanceOf[Result.Success].budget.steps}, Mem: ${result.asInstanceOf[Result.Success].budget.memory}")
    }

    test("CEK: fails when first input is not treasury") {
        val treasuryInput = txInput(fakeTxid(0x10), 0)
        val wrongFirstInput = txInput(fakeTxid(0x99), 0)

        val pegInTx = buildPegInTx(
          prevTxid = fakeTxid(0x01),
          prevVout = 0,
          paymentSatoshis = 100_000,
          scriptPubKey = ScriptPubKey.fromAsmHex("5120" + "0" * 64)
        )
        val rawPegInTx = txBytes(pegInTx)
        val pegInTxid = sha2_256(sha2_256(rawPegInTx)).bytes
        val pegInInput = txInputLE(pegInTxid, 0)

        val tmTx = buildWitnessTx(
          inputs = Seq(wrongFirstInput, pegInInput),
          witnesses = Seq(TaprootKeyPath.dummy, TaprootKeyPath.dummy)
        )
        val rawTmTx = txBytes(tmTx)

        val redeemer = buildRedeemer(outpointBytes(treasuryInput), rawPegInTx, 100_000, rawTmTx)
        val result = evalValidator(redeemer)
        assert(result.isInstanceOf[Result.Failure], s"Expected failure but got: $result")
    }

    test("CEK: fails when peg-in not among TM inputs") {
        val treasuryInput = txInput(fakeTxid(0x10), 0)
        val unrelatedInput = txInput(fakeTxid(0x77), 0)

        val pegInTx = buildPegInTx(
          prevTxid = fakeTxid(0x01),
          prevVout = 0,
          paymentSatoshis = 100_000,
          scriptPubKey = ScriptPubKey.fromAsmHex("5120" + "0" * 64)
        )
        val rawPegInTx = txBytes(pegInTx)

        val tmTx = buildWitnessTx(
          inputs = Seq(treasuryInput, unrelatedInput),
          witnesses = Seq(TaprootKeyPath.dummy, TaprootKeyPath.dummy)
        )
        val rawTmTx = txBytes(tmTx)

        val redeemer = buildRedeemer(outpointBytes(treasuryInput), rawPegInTx, 100_000, rawTmTx)
        val result = evalValidator(redeemer)
        assert(result.isInstanceOf[Result.Failure], s"Expected failure but got: $result")
    }

    test("CEK: peg-in found as Nth input succeeds") {
        val treasuryInput = txInput(fakeTxid(0x10), 0)
        val otherInput1 = txInput(fakeTxid(0x20), 0)
        val otherInput2 = txInput(fakeTxid(0x30), 1)

        val pegInTx = buildPegInTx(
          prevTxid = fakeTxid(0x01),
          prevVout = 0,
          paymentSatoshis = 50_000,
          scriptPubKey = ScriptPubKey.fromAsmHex("5120" + "0" * 64)
        )
        val rawPegInTx = txBytes(pegInTx)
        val pegInTxid = sha2_256(sha2_256(rawPegInTx)).bytes
        val pegInInput = txInputLE(pegInTxid, 0)

        // treasury=0, other1=1, other2=2, peg-in=3
        val tmTx = buildWitnessTx(
          inputs = Seq(treasuryInput, otherInput1, otherInput2, pegInInput),
          witnesses = Seq(
            TaprootKeyPath.dummy, TaprootKeyPath.dummy,
            TaprootKeyPath.dummy, TaprootKeyPath.dummy
          )
        )
        val rawTmTx = txBytes(tmTx)

        val redeemer = buildRedeemer(outpointBytes(treasuryInput), rawPegInTx, 50_000, rawTmTx)
        val result = evalValidator(redeemer)
        assert(result.isInstanceOf[Result.Success], s"Expected success but got: $result")
    }

    test("CEK: fails when treasury vout does not match") {
        val treasuryInput = txInput(fakeTxid(0x10), 0)

        val pegInTx = buildPegInTx(
          prevTxid = fakeTxid(0x01),
          prevVout = 0,
          paymentSatoshis = 100_000,
          scriptPubKey = ScriptPubKey.fromAsmHex("5120" + "0" * 64)
        )
        val rawPegInTx = txBytes(pegInTx)
        val pegInTxid = sha2_256(sha2_256(rawPegInTx)).bytes
        val pegInInput = txInputLE(pegInTxid, 0)

        val tmTx = buildWitnessTx(
          inputs = Seq(treasuryInput, pegInInput),
          witnesses = Seq(TaprootKeyPath.dummy, TaprootKeyPath.dummy)
        )
        val rawTmTx = txBytes(tmTx)

        // Claim treasury is at vout=1 but TM actually spends vout=0
        val wrongTreasuryOutpoint = outpointBytes(txInput(fakeTxid(0x10), 1))
        val redeemer = buildRedeemer(wrongTreasuryOutpoint, rawPegInTx, 100_000, rawTmTx)
        val result = evalValidator(redeemer)
        assert(result.isInstanceOf[Result.Failure], s"Expected failure but got: $result")
    }

    test("CEK: TM with non-empty scriptSig in inputs still parses correctly") {
        val treasuryOutpoint = TransactionOutPoint(
          DoubleSha256DigestBE.fromBytes(ByteVector(fakeTxid(0x10))),
          UInt32.zero
        )
        val scriptSig = ScriptSignature.fromAsmBytes(ByteVector(0x00, 0x14) ++ ByteVector.fill(20)(0xab))
        val treasuryInput = TransactionInput(treasuryOutpoint, scriptSig, UInt32.max)

        val pegInTx = buildPegInTx(
          prevTxid = fakeTxid(0x01),
          prevVout = 0,
          paymentSatoshis = 200_000,
          scriptPubKey = ScriptPubKey.fromAsmHex("5120" + "0" * 64)
        )
        val rawPegInTx = txBytes(pegInTx)
        val pegInTxid = sha2_256(sha2_256(rawPegInTx)).bytes
        val pegInOutpoint = TransactionOutPoint(
          DoubleSha256Digest.fromBytes(ByteVector(pegInTxid)),
          UInt32.zero
        )
        val pegInScriptSig = ScriptSignature.fromAsmBytes(ByteVector(0x00, 0x14) ++ ByteVector.fill(20)(0xcd))
        val pegInInputWithScript = TransactionInput(pegInOutpoint, pegInScriptSig, UInt32.max)

        val tmTx = WitnessTransaction(
          Int32.two,
          Vector(treasuryInput, pegInInputWithScript),
          Vector(TransactionOutput(Satoshis.zero, EmptyScriptPubKey)),
          UInt32.zero,
          TransactionWitness(Vector(TaprootKeyPath.dummy, TaprootKeyPath.dummy))
        )
        val rawTmTx = txBytes(tmTx)

        val redeemer = buildRedeemer(
          ByteString.fromArray(treasuryOutpoint.bytes.toArray),
          rawPegInTx,
          200_000,
          rawTmTx
        )
        val result = evalValidator(redeemer)
        assert(result.isInstanceOf[Result.Success], s"Expected success but got: $result")
    }

    test("CEK: fails when peg-in spent via Taproot script-path witness") {
        val treasuryInput = txInput(fakeTxid(0x10), 0)

        val pegInTx = buildPegInTx(
          prevTxid = fakeTxid(0x01),
          prevVout = 0,
          paymentSatoshis = 100_000,
          scriptPubKey = ScriptPubKey.fromAsmHex("5120" + "0" * 64)
        )
        val rawPegInTx = txBytes(pegInTx)
        val pegInTxid = sha2_256(sha2_256(rawPegInTx)).bytes
        val pegInInput = txInputLE(pegInTxid, 0)

        // Treasury is key-path; peg-in uses a script-path witness (2 items: script + control block)
        val tmTx = buildWitnessTx(
          inputs = Seq(treasuryInput, pegInInput),
          witnesses = Seq(TaprootKeyPath.dummy, scriptPathWitness(Array(0x51.toByte), Array.fill(33)(0xc0.toByte)))
        )
        val rawTmTx = txBytes(tmTx)

        val redeemer = buildRedeemer(outpointBytes(treasuryInput), rawPegInTx, 100_000, rawTmTx)
        val result = evalValidator(redeemer)
        assert(result.isInstanceOf[Result.Failure], s"Expected failure but got: $result")
    }

    test("CEK: fails when treasury spent via Taproot script-path witness") {
        val treasuryInput = txInput(fakeTxid(0x10), 0)

        val pegInTx = buildPegInTx(
          prevTxid = fakeTxid(0x01),
          prevVout = 0,
          paymentSatoshis = 100_000,
          scriptPubKey = ScriptPubKey.fromAsmHex("5120" + "0" * 64)
        )
        val rawPegInTx = txBytes(pegInTx)
        val pegInTxid = sha2_256(sha2_256(rawPegInTx)).bytes
        val pegInInput = txInputLE(pegInTxid, 0)

        // Treasury uses script-path witness (2 items); peg-in is key-path — must still fail
        val tmTx = buildWitnessTx(
          inputs = Seq(treasuryInput, pegInInput),
          witnesses = Seq(scriptPathWitness(Array(0x51.toByte), Array.fill(33)(0xc0.toByte)), TaprootKeyPath.dummy)
        )
        val rawTmTx = txBytes(tmTx)

        val redeemer = buildRedeemer(outpointBytes(treasuryInput), rawPegInTx, 100_000, rawTmTx)
        val result = evalValidator(redeemer)
        assert(result.isInstanceOf[Result.Failure], s"Expected failure but got: $result")
    }

    test("CEK: fails when TM is not witness-serialized") {
        val treasuryInput = txInput(fakeTxid(0x10), 0)

        val pegInTx = buildPegInTx(
          prevTxid = fakeTxid(0x01),
          prevVout = 0,
          paymentSatoshis = 100_000,
          scriptPubKey = ScriptPubKey.fromAsmHex("5120" + "0" * 64)
        )
        val rawPegInTx = txBytes(pegInTx)
        val pegInTxid = sha2_256(sha2_256(rawPegInTx)).bytes
        val pegInInput = txInputLE(pegInTxid, 0)

        // Non-witness tx (BaseTransaction.bytes has no marker/flag/witnesses)
        val tmTx = BaseTransaction(
          Int32.two,
          Vector(treasuryInput, pegInInput),
          Vector(TransactionOutput(Satoshis.zero, EmptyScriptPubKey)),
          UInt32.zero
        )
        val rawTmTx = txBytes(tmTx)

        val redeemer = buildRedeemer(outpointBytes(treasuryInput), rawPegInTx, 100_000, rawTmTx)
        val result = evalValidator(redeemer)
        assert(result.isInstanceOf[Result.Failure], s"Expected failure but got: $result")
    }

    test("PegInVerifierContract compiles and produces non-empty CBOR") {
        val contract = PegInVerifierContract.contract
        assert(contract.program.cborEncoded.nonEmpty)
        info(s"Script size: ${contract.program.cborEncoded.length} bytes")
        info(s"Script hash: ${contract.script.scriptHash.toHex}")
    }
}
