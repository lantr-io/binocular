package binocular
import binocular.BitcoinValidator.{Action, BlockHeader, ChainState}
import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.common.ADAConversionUtil
import com.bloxbean.cardano.client.plutus.spec.{ExUnits, PlutusV3Script, Redeemer, RedeemerTag}
import com.bloxbean.cardano.client.transaction.spec
import com.bloxbean.cardano.client.transaction.spec.*
import com.bloxbean.cardano.client.transaction.util.TransactionUtil
import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import org.bouncycastle.crypto.generators.Ed25519KeyPairGenerator
import org.bouncycastle.crypto.params.{
    Ed25519KeyGenerationParameters,
    Ed25519PrivateKeyParameters,
    Ed25519PublicKeyParameters
}
import org.bouncycastle.crypto.signers.Ed25519Signer
import org.scalacheck.Prop.forAll
import scalus.*
import scalus.bloxbean.Interop.{getScriptInfoV3, getTxInfoV3, toScalusData}
import scalus.bloxbean.{Interop, SlotConfig}
import scalus.builtin.ByteString.hex
import scalus.builtin.Data.toData
import scalus.builtin.{Builtins, ByteString, Data}
import scalus.ledger.api.v3.{PubKeyHash, ScriptContext}
import scalus.uplc.eval
import scalus.uplc.eval.*
import upickle.default.*

import java.math.BigInteger
import java.nio.file.{Files, Path}
import java.security.SecureRandom
import java.util
import scala.jdk.CollectionConverters.*
import scala.language.implicitConversions

case class BitcoinBlock(
    hash: String,
    confirmations: Int,
    height: Int,
    version: Long,
    versionHex: String,
    merkleroot: String,
    time: Long,
    mediantime: Long,
    nonce: Long,
    bits: String,
    difficulty: Double,
    chainwork: String,
    nTx: Int,
    previousblockhash: String,
    nextblockhash: String,
    strippedsize: Int,
    size: Int,
    weight: Int,
    tx: List[String]
) derives ReadWriter

case class CekResult(budget: ExBudget, logs: Seq[String])

class ValidatorTest extends munit.ScalaCheckSuite {
    private given PlutusVM = PlutusVM.makePlutusV3VM()

    // test(s"Bitcoin validator size is ${bitcoinProgram.doubleCborEncoded.length}") {
    // println(compiledBitcoinValidator.showHighlighted)
    // assertEquals(bitcoinProgram.doubleCborEncoded.length, 900)
    // }

    test("reverse") {
        forAll { (a: Array[Byte]) =>
            val bs = ByteString.unsafeFromArray(a)
            assertEquals(bs, bs.reverse.reverse)
        }
    }

    test("Tx size makes sense") {
        val blockHeader =
            hex"000000302b974c15e2ef994183f9806c5be9c61e74abc512a14301000000000000000000aff4af5b1dcc2b8754db824b9911818b65913dc262c295f060abb45c6c1d7ee749f90b67cd0e0317f9cc7dac"
        val coinbaseTx =
            hex"010000000001010000000000000000000000000000000000000000000000000000000000000000ffffffff2503233708184d696e656420627920416e74506f6f6c373946205b8160a4256c0000946e0100ffffffff02f595814a000000001976a914edf10a7fac6b32e24daa5305c723f3de58db1bc888ac0000000000000000266a24aa21a9edfaa194df59043645ba0f58aad74bfd5693fa497093174d12a4bb3b0574a878db0120000000000000000000000000000000000000000000000000000000000000000000000000"
        assertEquals(blockHeader.size, 80)
        val coinbase = Bitcoin.makeCoinbaseTxFromByteString(coinbaseTx)
        val coinbaseSize = coinbase.toData.toCbor.length
        println(s"Coinbase size: $coinbaseSize")
        //        val action = BitcoinValidator.Action.NewTip(blockHeader, coinbase)
    }

    test("parseCoinbaseTx") {
        val coinbaseTx =
            hex"010000000001010000000000000000000000000000000000000000000000000000000000000000ffffffff2503233708184d696e656420627920416e74506f6f6c373946205b8160a4256c0000946e0100ffffffff02f595814a000000001976a914edf10a7fac6b32e24daa5305c723f3de58db1bc888ac0000000000000000266a24aa21a9edfaa194df59043645ba0f58aad74bfd5693fa497093174d12a4bb3b0574a878db0120000000000000000000000000000000000000000000000000000000000000000000000000"
        val scriptSig = BitcoinValidator.parseCoinbaseTxScriptSig(coinbaseTx)
        assertEquals(
          scriptSig,
          hex"03233708184d696e656420627920416e74506f6f6c373946205b8160a4256c0000946e0100"
        )
    }

    test("construct CoinbaseTx") {
        val coinbaseTx =
            hex"010000000001010000000000000000000000000000000000000000000000000000000000000000ffffffff2503233708184d696e656420627920416e74506f6f6c373946205b8160a4256c0000946e0100ffffffff02f595814a000000001976a914edf10a7fac6b32e24daa5305c723f3de58db1bc888ac0000000000000000266a24aa21a9edfaa194df59043645ba0f58aad74bfd5693fa497093174d12a4bb3b0574a878db0120000000000000000000000000000000000000000000000000000000000000000000000000"
        val coinbase = Bitcoin.makeCoinbaseTxFromByteString(coinbaseTx)
        val txHash = BitcoinValidator.getCoinbaseTxHash(coinbase)
        assertEquals(
          txHash,
          hex"31e9370f45eb48f6f52ef683b0737332f09f1cead75608021185450422ec1a71".reverse
        )
    }

    test("parseBlockHeightFromScriptSig") {
        val coinbaseTx =
            hex"010000000001010000000000000000000000000000000000000000000000000000000000000000ffffffff2503233708184d696e656420627920416e74506f6f6c373946205b8160a4256c0000946e0100ffffffff02f595814a000000001976a914edf10a7fac6b32e24daa5305c723f3de58db1bc888ac0000000000000000266a24aa21a9edfaa194df59043645ba0f58aad74bfd5693fa497093174d12a4bb3b0574a878db0120000000000000000000000000000000000000000000000000000000000000000000000000"
        val scriptSig = BitcoinValidator.parseCoinbaseTxScriptSig(coinbaseTx)
        val blockHeight = BitcoinValidator.parseBlockHeightFromScriptSig(scriptSig)
        assertEquals(blockHeight, BigInt(538403))
    }

    test("getTxHash") {
        val coinbaseTx =
            hex"010000000001010000000000000000000000000000000000000000000000000000000000000000ffffffff2503233708184d696e656420627920416e74506f6f6c373946205b8160a4256c0000946e0100ffffffff02f595814a000000001976a914edf10a7fac6b32e24daa5305c723f3de58db1bc888ac0000000000000000266a24aa21a9edfaa194df59043645ba0f58aad74bfd5693fa497093174d12a4bb3b0574a878db0120000000000000000000000000000000000000000000000000000000000000000000000000"
        val txHash = BitcoinValidator.getTxHash(coinbaseTx)
        assertEquals(
          txHash,
          hex"31e9370f45eb48f6f52ef683b0737332f09f1cead75608021185450422ec1a71".reverse
        )
    }

    test("pow") {
        forAll { (a: Int, b: Byte) =>
            val positiveExponent = b & 0x7f
            val result = BitcoinValidator.pow(a, BigInt(positiveExponent))
            assertEquals(result, BigInt(a).pow(positiveExponent))
        }
    }

    test("compactBitsToTarget") {
        // 0 exponent
        assertEquals(
          BitcoinValidator.compactBitsToTarget(hex"0002f128".reverse),
          BigInt("0", 16)
        )
        // real bits from block 867936
        assertEquals(
          BitcoinValidator.compactBitsToTarget(hex"0202f128".reverse),
          BigInt("00000000000000000000000000000000000000000000000000000000000002f1", 16)
        )
        assertEquals(
          BitcoinValidator.compactBitsToTarget(hex"1a030ecd".reverse),
          BigInt("000000000000030ecd0000000000000000000000000000000000000000000000", 16)
        )
        assertEquals(
          BitcoinValidator.compactBitsToTarget(hex"1d00ffff".reverse),
          BigInt("00000000ffff0000000000000000000000000000000000000000000000000000", 16)
        )
        // too large exponent
        intercept[RuntimeException](BitcoinValidator.compactBitsToTarget(hex"1e00ffff".reverse))
    }

    test("targetToCompactBits") {
        // Test cases from Bitcoin Core's BOOST_AUTO_TEST_CASE(bignum_SetCompact)
        // These verify that targetToCompactBits (GetCompact) produces the expected 4-byte compact representation

        // Zero target
        assertEquals(BitcoinValidator.targetToCompactBits(BigInt(0)), BigInt(0))

        // Case: target = 0x12 -> GetCompact = 0x01120000
        assertEquals(BitcoinValidator.targetToCompactBits(BigInt(0x12)), BigInt(0x01120000L))

        // Case: target = 0x80 -> GetCompact = 0x02008000 (sign bit handling)
//        assertEquals(BitcoinValidator.targetToCompactBits(BigInt(0x80)), BigInt(0x02008000L))

        // Case: target = 0x1234 -> GetCompact = 0x02123400
        assertEquals(BitcoinValidator.targetToCompactBits(BigInt(0x1234)), BigInt(0x02123400L))

        // Case: target = 0x123456 -> GetCompact = 0x03123456
        assertEquals(BitcoinValidator.targetToCompactBits(BigInt(0x123456)), BigInt(0x03123456L))

        // Case: target = 0x12345600 -> GetCompact = 0x04123456
        assertEquals(BitcoinValidator.targetToCompactBits(BigInt(0x12345600L)), BigInt(0x04123456L))

//         Case: target = 0x92340000 -> GetCompact = 0x05009234
//        assertEquals(BitcoinValidator.targetToCompactBits(BigInt(0x92340000L)), BigInt(0x05009234L))

        // Large number case from the test
        // target with leading 0x1234560000... -> GetCompact = 0x20123456
        val largeTarget = BigInt("1234560000000000000000000000000000000000000000000000000000000000", 16)
        assertEquals(BitcoinValidator.targetToCompactBits(largeTarget), BigInt(0x20123456L))
    }

    test("Block Header hash") {
        val blockHeader = BlockHeader(
          hex"000000302b974c15e2ef994183f9806c5be9c61e74abc512a14301000000000000000000aff4af5b1dcc2b8754db824b9911818b65913dc262c295f060abb45c6c1d7ee749f90b67cd0e0317f9cc7dac"
        )
        val hash = BitcoinValidator.blockHeaderHash(blockHeader)
        assertEquals(hash.reverse, hex"00000000000000000002cfdedd8358532b2284bc157e1352dbc8682b2067fb0c")
    }

    test("Block Header hash satisfies proof of work") {
        val blockHeader = BlockHeader(
          hex"000000302b974c15e2ef994183f9806c5be9c61e74abc512a14301000000000000000000aff4af5b1dcc2b8754db824b9911818b65913dc262c295f060abb45c6c1d7ee749f90b67cd0e0317f9cc7dac"
        )
        val hash = BitcoinValidator.blockHeaderHash(blockHeader)
        val target = BitcoinValidator.compactBitsToTarget(hex"17030ecd".reverse)
        val proofOfWork = Builtins.byteStringToInteger(false, hash)
        assertEquals(hash, hex"00000000000000000002cfdedd8358532b2284bc157e1352dbc8682b2067fb0c".reverse)
        assertEquals(target, BigInt("000000000000000000030ecd0000000000000000000000000000000000000000", 16))
        assert(proofOfWork <= target, clue(s"proofOfWork: $proofOfWork, target: $target"))
    }

    test("insertReverseSorted") {
        forAll { (xs: Seq[BigInt], x: BigInt) =>
            val sorted = xs.sorted(using Ordering[BigInt].reverse)
            val sortedList = prelude.List.from(sorted)
            val inserted = BitcoinValidator.insertReverseSorted(x, sortedList)
            val expected = (sorted :+ x).sorted(using Ordering[BigInt].reverse)
            assertEquals(inserted, prelude.List.from(expected))
        }
    }

    test("Evaluate") {
        val block = read[BitcoinBlock](
          Files.readString(
            Path.of("00000000000000000002cfdedd8358532b2284bc157e1352dbc8682b2067fb0c.json")
          )
        )
        //        println(s"block.merkleroot = ${block.merkleroot}")

        val coinbaseTx =
            hex"010000000001010000000000000000000000000000000000000000000000000000000000000000ffffffff4f03d6340d082f5669614254432f2cfabe6d6d7853581d1f2abd53fe30833a1e6b8397b200ec3b658fdf6568edc5f54118f99d10000000000000001003b7b5047d947d88f58877bd2cb73c0000000000ffffffff0342db4a15000000001976a914fb37342f6275b13936799def06f2eb4c0f20151588ac00000000000000002b6a2952534b424c4f434b3aec94a488ba1f588e9cb6bb507c70283e9f10057ef683d2d460d3700900678b710000000000000000266a24aa21a9edb9df509533c6a9526f5180b0c34cc48b86a40cc94abfae661242a596393ef47d0120000000000000000000000000000000000000000000000000000000000000000000000000"
        val coinbase = Bitcoin.makeCoinbaseTxFromByteString(coinbaseTx)
        val coinbaseHash = BitcoinValidator.getCoinbaseTxHash(coinbase)
        //        println(s"Coinbase hash: ${coinbaseHash.reverse}")

        val txHashes = block.tx.map(h => ByteString.fromHex(h).reverse)
        val merkleTree = MerkleTree.fromHashes(txHashes)
        val merkleRoot = merkleTree.getMerkleRoot
        val merkleProof = merkleTree.makeMerkleProof(0)
        //        println(s"Merkle root: ${merkleRoot.reverse}")
        //        println(s"Merkle proof: ${merkleProof}")

        val coinbaseTxInclusionProof = scalus.prelude.List.from(merkleProof).toData

        val blockHeader = BlockHeader(
          bytes =
              hex"000000302b974c15e2ef994183f9806c5be9c61e74abc512a14301000000000000000000aff4af5b1dcc2b8754db824b9911818b65913dc262c295f060abb45c6c1d7ee749f90b67cd0e0317f9cc7dac"
        )
        val hash = BitcoinValidator.blockHeaderHash(blockHeader)
        val bits = hex"17030ecd".reverse
        val target = BitcoinValidator.compactBitsToTarget(bits)
        val timestamp = blockHeader.timestamp

        val keyPairGenerator = new Ed25519KeyPairGenerator()
        val RANDOM = new SecureRandom()
        keyPairGenerator.init(new Ed25519KeyGenerationParameters(RANDOM))
        val asymmetricCipherKeyPair: AsymmetricCipherKeyPair = keyPairGenerator.generateKeyPair()
        val privateKey: Ed25519PrivateKeyParameters =
            asymmetricCipherKeyPair.getPrivate.asInstanceOf[Ed25519PrivateKeyParameters];
        val publicKeyParams: Ed25519PublicKeyParameters =
            asymmetricCipherKeyPair.getPublic.asInstanceOf[Ed25519PublicKeyParameters];
        val publicKey: ByteString = ByteString.fromArray(publicKeyParams.getEncoded)

        val signature = signMessage(hash, privateKey)

        val redeemer = Action
            .NewTip(
              blockHeader,
              publicKey,
              signature
            )
            .toData

        println(s"Redeemer size: ${redeemer.toCbor.length}")

        val prevState = ChainState(
          865493,
          blockHash = hex"0000000000000000000143a112c5ab741ec6e95b6c80f9834199efe2154c972b".reverse,
          currentTarget = bits,
          blockTimestamp = timestamp - 600,
          recentTimestamps = prelude.List(timestamp - 600),
          previousDifficultyAdjustmentTimestamp = timestamp - 600 * BitcoinValidator.DifficultyAdjustmentInterval
        )
        val newState = ChainState(
          prevState.blockHeight + 1,
          blockHash = hex"00000000000000000002cfdedd8358532b2284bc157e1352dbc8682b2067fb0c".reverse,
          currentTarget = bits,
          blockTimestamp = timestamp,
          recentTimestamps = prelude.List(timestamp, timestamp - 600),
          previousDifficultyAdjustmentTimestamp = timestamp - 600 * BitcoinValidator.DifficultyAdjustmentInterval
        )
        val (scriptContext, tx) = makeScriptContextAndTransaction(
          timestamp.toLong,
          prevState.toData,
          newState.toData,
          redeemer,
          Seq.empty
        )
        println(s"Tx size: ${tx.serialize().length}")
        import scalus.uplc.TermDSL.given
        val applied = BitcoinContract.bitcoinProgram $ scriptContext.toData
        println(s"Validator size: ${BitcoinContract.bitcoinProgram.flatEncoded.length}")
        BitcoinValidator.validate(scriptContext.toData)
        applied.evaluateDebug match
            case r: Result.Success => println(r)
            case r: Result.Failure => fail(r.toString)
    }

    def posixTimeToSlot(time: Long, sc: SlotConfig): Long = {
        val msAfterBegin = time - sc.zeroTime
        msAfterBegin / sc.slotLength + sc.zeroSlot
    }

    def signMessage(claim: ByteString, privateKey: Ed25519PrivateKeyParameters): ByteString =
        val signer = new Ed25519Signer();
        signer.init(true, privateKey);
        signer.update(claim.bytes, 0, claim.size)
        ByteString.fromArray(signer.generateSignature())

    def getScriptContextV3(
        redeemer: Redeemer,
        datum: Option[Data],
        tx: Transaction,
        txhash: String,
        utxos: Map[TransactionInput, TransactionOutput],
        slotConfig: SlotConfig,
        protocolVersion: Int
    ): ScriptContext = {
        import scala.jdk.CollectionConverters.*
        val scriptInfo = getScriptInfoV3(tx, redeemer, datum)
        val datums = tx.getWitnessSet.getPlutusDataList.asScala.map { plutusData =>
            ByteString.fromArray(plutusData.getDatumHashAsBytes) -> Interop.toScalusData(plutusData)
        }
        val txInfo = getTxInfoV3(tx, txhash, datums, utxos, slotConfig, protocolVersion)
        val scriptContext = ScriptContext(txInfo, toScalusData(redeemer.getData), scriptInfo)
        scriptContext
    }
    lazy val sender = new Account()

    def makeScriptContextAndTransaction(
        intervalStartMs: Long,
        prevState: Data,
        datum: Data,
        redeemer: Data,
        signatories: Seq[PubKeyHash]
    ): (ScriptContext, Transaction) =
        val script = PlutusV3Script
            .builder()
            .`type`("PlutusScriptV3")
            .cborHex(BitcoinContract.bitcoinProgram.doubleCborHex)
            .build()
            .asInstanceOf[PlutusV3Script]
        val payeeAddress = sender.baseAddress()
        val rdmr =
            Redeemer
                .builder()
                .tag(RedeemerTag.Spend)
                .data(Interop.toPlutusData(redeemer))
                .index(0)
                .exUnits(
                  ExUnits
                      .builder()
                      .steps(BigInteger.valueOf(1000))
                      .mem(BigInteger.valueOf(1000))
                      .build()
                )
                .build()

        val input = TransactionInput
            .builder()
            .transactionId("1ab6879fc08345f51dc9571ac4f530bf8673e0d798758c470f9af6f98e2f3982")
            .index(0)
            .build()

        val scriptRefInput = TransactionInput
            .builder()
            .transactionId("1ab6879fc08345f51dc9571ac4f530bf8673e0d798758c470f9af6f98e2f3982")
            .index(1)
            .build()
        val inputs = util.List.of(input)

        val utxo = Map(
          input -> TransactionOutput
              .builder()
              .value(spec.Value.builder().coin(BigInteger.valueOf(20)).build())
              .address(payeeAddress)
              .inlineDatum(Interop.toPlutusData(prevState))
              .build(),
          scriptRefInput -> TransactionOutput
              .builder()
              .value(spec.Value.builder().coin(BigInteger.valueOf(20)).build())
              .address(payeeAddress)
              .scriptRef(script)
              .build()
        )
        val slot = posixTimeToSlot(intervalStartMs * 1000, SlotConfig.Mainnet)
        val tx = Transaction
            .builder()
            .body(
              TransactionBody
                  .builder()
                  .validityStartInterval(slot)
                  .fee(ADAConversionUtil.adaToLovelace(0.2))
                  .inputs(inputs)
                  .referenceInputs(util.List.of(scriptRefInput))
                  .requiredSigners(signatories.map(_.hash.bytes).asJava)
                  .build()
            )
            .witnessSet(
              TransactionWitnessSet
                  .builder()
                  .redeemers(util.List.of(rdmr))
                  .build()
            )
            .build()

        val scriptContext = getScriptContextV3(
          rdmr,
          Some(datum),
          tx,
          TransactionUtil.getTxHash(tx),
          utxo,
          SlotConfig.Mainnet,
          protocolVersion = 9
        )
        (scriptContext, tx)

    test("merkleRootFromInclusionProof - single transaction") {
        val txHash = hex"abcd1234567890abcdef1234567890abcdef1234567890abcdef1234567890ab"
        val emptyProof = scalus.prelude.List.empty[ByteString]
        val result = BitcoinValidator.merkleRootFromInclusionProof(emptyProof, txHash, BigInt(0))
        assertEquals(result, txHash)
    }

    test("merkleRootFromInclusionProof - two transactions, left") {
        val leftTxHash = hex"abcd1234567890abcdef1234567890abcdef1234567890abcdef1234567890ab"
        val rightTxHash = hex"1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcd"
        val proof = scalus.prelude.List.single(rightTxHash)
        val expectedRoot = Builtins.sha2_256(Builtins.sha2_256(leftTxHash ++ rightTxHash))
        val result = BitcoinValidator.merkleRootFromInclusionProof(proof, leftTxHash, BigInt(0))
        assertEquals(result, expectedRoot)
    }

    test("merkleRootFromInclusionProof - two transactions, right") {
        val leftTxHash = hex"abcd1234567890abcdef1234567890abcdef1234567890abcdef1234567890ab"
        val rightTxHash = hex"1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcd"
        val proof = scalus.prelude.List.single(leftTxHash)
        val expectedRoot = Builtins.sha2_256(Builtins.sha2_256(leftTxHash ++ rightTxHash))
        val result = BitcoinValidator.merkleRootFromInclusionProof(proof, rightTxHash, BigInt(1))
        assertEquals(result, expectedRoot)
    }

    test("merkleRootFromInclusionProof - four transactions, various positions") {
        val tx0 = hex"0000000000000000000000000000000000000000000000000000000000000000"
        val tx1 = hex"1111111111111111111111111111111111111111111111111111111111111111"
        val tx2 = hex"2222222222222222222222222222222222222222222222222222222222222222"
        val tx3 = hex"3333333333333333333333333333333333333333333333333333333333333333"

        // Build tree manually
        val hash01 = Builtins.sha2_256(Builtins.sha2_256(tx0 ++ tx1))
        val hash23 = Builtins.sha2_256(Builtins.sha2_256(tx2 ++ tx3))
        val root = Builtins.sha2_256(Builtins.sha2_256(hash01 ++ hash23))

        // Test tx0 (index 0): proof should be [tx1, hash23]
        val proof0 = scalus.prelude.List.from(Seq(tx1, hash23))
        val result0 = BitcoinValidator.merkleRootFromInclusionProof(proof0, tx0, BigInt(0))
        assertEquals(result0, root)

        // Test tx1 (index 1): proof should be [tx0, hash23]
        val proof1 = scalus.prelude.List.from(Seq(tx0, hash23))
        val result1 = BitcoinValidator.merkleRootFromInclusionProof(proof1, tx1, BigInt(1))
        assertEquals(result1, root)

        // Test tx2 (index 2): proof should be [tx3, hash01]
        val proof2 = scalus.prelude.List.from(Seq(tx3, hash01))
        val result2 = BitcoinValidator.merkleRootFromInclusionProof(proof2, tx2, BigInt(2))
        assertEquals(result2, root)

        // Test tx3 (index 3): proof should be [tx2, hash01]
        val proof3 = scalus.prelude.List.from(Seq(tx2, hash01))
        val result3 = BitcoinValidator.merkleRootFromInclusionProof(proof3, tx3, BigInt(3))
        assertEquals(result3, root)
    }

    test("merkleRootFromInclusionProof - real Bitcoin data") {
        // Use the existing test data from the "Evaluate" test
        val block = read[BitcoinBlock](
          Files.readString(
            Path.of("00000000000000000002cfdedd8358532b2284bc157e1352dbc8682b2067fb0c.json")
          )
        )

        val txHashes = block.tx.map(h => ByteString.fromHex(h).reverse)
        val merkleTree = MerkleTree.fromHashes(txHashes)
        val expectedMerkleRoot = merkleTree.getMerkleRoot

        // Test coinbase transaction (index 0)
        val coinbaseHash = txHashes.head
        val merkleProof = merkleTree.makeMerkleProof(0)
        val proofData = scalus.prelude.List.from(merkleProof)

        val computedRoot = BitcoinValidator.merkleRootFromInclusionProof(
          proofData,
          coinbaseHash,
          BigInt(0)
        )

        assertEquals(computedRoot, expectedMerkleRoot)

        // Test a few other transactions if there are more than one
        if (txHashes.length > 1) {
            val lastTxHash = txHashes.last
            val lastIndex = txHashes.length - 1
            val lastMerkleProof = merkleTree.makeMerkleProof(lastIndex)
            val lastProofData = scalus.prelude.List.from(lastMerkleProof)

            val lastComputedRoot = BitcoinValidator.merkleRootFromInclusionProof(
              lastProofData,
              lastTxHash,
              BigInt(lastIndex)
            )

            assertEquals(lastComputedRoot, expectedMerkleRoot)
        }
    }

    test("merkleRootFromInclusionProof - edge cases") {
        val txHash = hex"abcd1234567890abcdef1234567890abcdef1234567890abcdef1234567890ab"

        // Empty proof with index 0 should return the hash itself
        val emptyProof = scalus.prelude.List.empty[ByteString]
        val result = BitcoinValidator.merkleRootFromInclusionProof(emptyProof, txHash, BigInt(0))
        assertEquals(result, txHash)
    }

    test("getNextWorkRequired - difficulty adjustment timing") {
        val currentTarget = hex"1d00ffff".reverse
        val blockTime = BigInt(1234567890)
        val firstBlockTime =
            BigInt(
              1234567890 - 2016 * 600 / 2
            ) // 2016 blocks * 10 blocks in half the time (should double the difficulty)

        // Test block heights that should NOT trigger difficulty adjustment
        val noAdjustmentHeights = Seq(0, 1, 100, 1000, 2014) // Not at 2016 interval
        for height <- noAdjustmentHeights do
            val result = BitcoinValidator.getNextWorkRequired(height, currentTarget, blockTime, firstBlockTime)
            assertEquals(result, currentTarget, s"Height $height should not trigger difficulty adjustment")

        // Test block heights that SHOULD trigger difficulty adjustment
        val adjustmentHeights = Seq(2015, 4031, 6047) // Heights where (height + 1) % 2016 == 0
        for height <- adjustmentHeights do
            val result = BitcoinValidator.getNextWorkRequired(height, currentTarget, blockTime, firstBlockTime)
            // Result should be different from currentTarget (actually calculated)
            assert(result != currentTarget, s"Height $height should trigger difficulty adjustment")
    }

    test("getNextWorkRequired - operator precedence regression test") {
        // This test specifically checks for the operator precedence bug
        // If the bug existed: nHeight + 1 % 2016 == 0 would be nHeight + (1 % 2016) == 0
        // which means nHeight + 1 == 0, only true for height -1 (impossible)

        val currentTarget = hex"1d00ffff".reverse // smallest difficulty
        val blockTime = BigInt(1234567890)
        val firstBlockTime =
            BigInt(1234567890 - 2016 * 600 / 2) // Double block production, should make difficulty harder

        // Height 2015: (2015 + 1) % 2016 = 0 (should adjust)
        // With bug: 2015 + (1 % 2016) = 2015 + 1 = 2016 ≠ 0 (would not adjust)
        val result2015 = BitcoinValidator.getNextWorkRequired(2015, currentTarget, blockTime, firstBlockTime)
        assert(result2015 != currentTarget, "Height 2015 should trigger difficulty adjustment")

        // Height 4031: (4031 + 1) % 2016 = 0 (should adjust)
        val result4031 = BitcoinValidator.getNextWorkRequired(4031, currentTarget, blockTime, firstBlockTime)
        assert(result4031 != currentTarget, "Height 4031 should trigger difficulty adjustment")

        // Height 1000: (1000 + 1) % 2016 = 1001 ≠ 0 (should not adjust)
        val result1000 = BitcoinValidator.getNextWorkRequired(1000, currentTarget, blockTime, firstBlockTime)
        assertEquals(result1000, currentTarget, "Height 1000 should not trigger difficulty adjustment")
    }
}
