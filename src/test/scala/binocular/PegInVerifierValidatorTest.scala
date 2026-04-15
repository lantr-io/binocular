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

    /** Fake 3-item protocol script-path witness: [sig (64 B), leaf_script (34 B), ctrl_block (65 B)].
      * Matches the structure of all Bifrost 1-of-1 script-path spends (Y_67 OP_CHECKSIG or
      * timeout OP_CSV OP_DROP Y_fed OP_CHECKSIG). Byte content is arbitrary — Bitcoin validates
      * signatures; the Cardano validator only checks item count to classify the spending path.
      */
    private def scriptPathWitness: ScriptWitness = ScriptWitness(Vector(
        ByteVector(Array.fill(64)(0x00.toByte)),         // fake Schnorr signature
        ByteVector(Array(0x20.toByte) ++ Array.fill(32)(0xab.toByte) ++ Array(0xac.toByte)), // fake leaf script
        ByteVector(Array(0xc0.toByte) ++ Array.fill(64)(0x01.toByte))  // fake control block
    ))

    /** Fake 4-item depositor CSV-refund witness: [sig, pubkey (32 B), leaf_script, ctrl_block].
      * Depositor refund scripts do `OP_HASH160 <hash> OP_EQUALVERIFY OP_CHECKSIG`, which requires
      * the x-only pubkey as an explicit witness item — producing 4 items total.
      */
    private def depositorRefundWitness: ScriptWitness = ScriptWitness(Vector(
        ByteVector(Array.fill(64)(0x00.toByte)),         // fake sig
        ByteVector(Array.fill(32)(0xdd.toByte)),          // fake x-only pubkey (32 bytes)
        ByteVector(Array(0x20.toByte) ++ Array.fill(32)(0xab.toByte) ++ Array(0xac.toByte)), // fake leaf script
        ByteVector(Array(0xc0.toByte) ++ Array.fill(64)(0x01.toByte))  // fake control block
    ))

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

    test("CEK: fails when peg-in is script-path but treasury is key-path (depositor refund)") {
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
          witnesses = Seq(TaprootKeyPath.dummy, scriptPathWitness)
        )
        val rawTmTx = txBytes(tmTx)

        val redeemer = buildRedeemer(outpointBytes(treasuryInput), rawPegInTx, 100_000, rawTmTx)
        val result = evalValidator(redeemer)
        assert(result.isInstanceOf[Result.Failure], s"Expected failure but got: $result")
    }

    test("CEK: 67% mode succeeds (treasury script-path Y_67, peg-in key-path Y_51)") {
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

        // 67% mode: treasury via Y_67 script leaf (script-path, 2 items), peg-in via Y_51 key-path
        val tmTx = buildWitnessTx(
          inputs = Seq(treasuryInput, pegInInput),
          witnesses = Seq(scriptPathWitness, TaprootKeyPath.dummy)
        )
        val rawTmTx = txBytes(tmTx)

        val redeemer = buildRedeemer(outpointBytes(treasuryInput), rawPegInTx, 100_000, rawTmTx)
        val result = evalValidator(redeemer)
        assert(result.isInstanceOf[Result.Success], s"Expected success but got: $result")
    }

    test("CEK: federation mode succeeds (treasury script-path Y_fed, peg-in script-path Y_fed)") {
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

        // Federation mode: both treasury and peg-in spent via Y_fed script leaf (script-path)
        val fedScriptPath = scriptPathWitness
        val tmTx = buildWitnessTx(
          inputs = Seq(treasuryInput, pegInInput),
          witnesses = Seq(fedScriptPath, fedScriptPath)
        )
        val rawTmTx = txBytes(tmTx)

        val redeemer = buildRedeemer(outpointBytes(treasuryInput), rawPegInTx, 100_000, rawTmTx)
        val result = evalValidator(redeemer)
        assert(result.isInstanceOf[Result.Success], s"Expected success but got: $result")
    }

    test("CEK: fails when peg-in uses depositor refund witness (4 items) even in federation mode") {
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

        // Federation-mode treasury (3 items) but peg-in via depositor CSV refund (4 items)
        val tmTx = buildWitnessTx(
          inputs = Seq(treasuryInput, pegInInput),
          witnesses = Seq(scriptPathWitness, depositorRefundWitness)
        )
        val rawTmTx = txBytes(tmTx)

        val redeemer = buildRedeemer(outpointBytes(treasuryInput), rawPegInTx, 100_000, rawTmTx)
        val result = evalValidator(redeemer)
        assert(result.isInstanceOf[Result.Failure], s"Expected failure but got: $result")
    }

    test("CEK: fails when treasury has invalid witness structure (not 1 or 3 items)") {
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

        // 2-item treasury witness (neither key-path nor a protocol script-path)
        val invalidTreasuryWitness = ScriptWitness(Vector(
            ByteVector(Array.fill(64)(0x00.toByte)),
            ByteVector(Array.fill(33)(0xc0.toByte))
        ))
        val tmTx = buildWitnessTx(
          inputs = Seq(treasuryInput, pegInInput),
          witnesses = Seq(invalidTreasuryWitness, TaprootKeyPath.dummy)
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
