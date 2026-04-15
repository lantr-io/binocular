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

    /** Build a minimal raw witness-serialized transaction from scratch, bypassing bitcoin-s
      * ScriptWitness type validation. Used for tests that need witness stacks bitcoin-s would reject
      * (e.g., script-path witnesses with fake control blocks).
      *
      * Wire format: [version 4B][0x00][0x01][nInputs][inputs][nOutputs][outputs][witnesses][locktime 4B]
      * Each witness: [nStackItems varint][item len varint + bytes]...
      *
      * @param inputOutpoints 36-byte outpoints (prevTxid 32B LE + vout 4B LE) per input
      * @param witnessStacks  per-input list of raw stack item byte arrays
      */
    private def rawWitnessTxBytes(
        inputOutpoints: Seq[Array[Byte]],
        witnessStacks: Seq[Seq[Array[Byte]]]
    ): ByteString = {
        require(inputOutpoints.length == witnessStacks.length)
        val buf = scala.collection.mutable.ArrayBuffer[Byte]()
        buf ++= Array(0x02, 0x00, 0x00, 0x00).map(_.toByte) // version 2
        buf += 0x00.toByte                                   // marker
        buf += 0x01.toByte                                   // flag
        buf += inputOutpoints.length.toByte                  // input count
        for outpoint <- inputOutpoints do
            buf ++= outpoint                                 // 36-byte outpoint
            buf += 0x00.toByte                               // empty scriptSig
            buf ++= Array(0xff, 0xff, 0xff, 0xff).map(_.toByte) // sequence
        buf += 0x01.toByte                                   // 1 dummy output
        buf ++= Array.fill(8)(0x00.toByte)                   // value = 0
        buf += 0x00.toByte                                   // empty scriptPubKey
        for stack <- witnessStacks do
            buf += stack.length.toByte                       // stack item count
            for item <- stack do
                buf += item.length.toByte                    // item length
                buf ++= item                                 // item bytes
        buf ++= Array(0x00, 0x00, 0x00, 0x00).map(_.toByte) // locktime
        ByteString.fromArray(buf.toArray)
    }

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
        println(testContract.program.showHighlighted)
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
        val pegInOutpointBytes = pegInTxid ++ Array.fill(4)(0.toByte)

        // Treasury is key-path (1 item, 64 bytes); peg-in uses a script-path witness (2 items)
        // rawWitnessTxBytes bypasses bitcoin-s TaprootScriptPath construction validation
        val keyPath = Seq(Array.fill(64)(0x00.toByte))
        val scriptPath = Seq(Array(0x51.toByte), Array.fill(33)(0xc0.toByte))
        val rawTmTx = rawWitnessTxBytes(
          inputOutpoints = Seq(outpointBytes(treasuryInput).bytes, pegInOutpointBytes),
          witnessStacks = Seq(keyPath, scriptPath)
        )

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
        val pegInOutpointBytes = pegInTxid ++ Array.fill(4)(0.toByte)

        // Treasury uses script-path witness (2 items); peg-in is key-path — must still fail
        val keyPath = Seq(Array.fill(64)(0x00.toByte))
        val scriptPath = Seq(Array(0x51.toByte), Array.fill(33)(0xc0.toByte))
        val rawTmTx = rawWitnessTxBytes(
          inputOutpoints = Seq(outpointBytes(treasuryInput).bytes, pegInOutpointBytes),
          witnessStacks = Seq(scriptPath, keyPath)
        )

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
