package binocular

import binocular.BitcoinValidator.Action
import binocular.BitcoinValidator.BlockHeader
import binocular.BitcoinValidator.State
import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.common.ADAConversionUtil
import com.bloxbean.cardano.client.plutus.spec.ExUnits
import com.bloxbean.cardano.client.plutus.spec.PlutusV3Script
import com.bloxbean.cardano.client.plutus.spec.Redeemer
import com.bloxbean.cardano.client.plutus.spec.RedeemerTag
import com.bloxbean.cardano.client.transaction.spec
import com.bloxbean.cardano.client.transaction.spec.Transaction
import com.bloxbean.cardano.client.transaction.spec.TransactionBody
import com.bloxbean.cardano.client.transaction.spec.TransactionInput
import com.bloxbean.cardano.client.transaction.spec.TransactionOutput
import com.bloxbean.cardano.client.transaction.spec.TransactionWitnessSet
import com.bloxbean.cardano.client.transaction.util.TransactionUtil
import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import org.bouncycastle.crypto.generators.Ed25519KeyPairGenerator
import org.bouncycastle.crypto.params.Ed25519KeyGenerationParameters
import org.bouncycastle.crypto.params.Ed25519PrivateKeyParameters
import org.bouncycastle.crypto.params.Ed25519PublicKeyParameters
import org.bouncycastle.crypto.signers.Ed25519Signer
import scalus.*
import scalus.bloxbean.Interop
import scalus.bloxbean.Interop.getScriptInfoV3
import scalus.bloxbean.Interop.getTxInfoV3
import scalus.bloxbean.Interop.toScalusData
import scalus.bloxbean.SlotConfig
import scalus.builtin.Builtins
import scalus.builtin.ByteString
import scalus.builtin.ByteString.hex
import scalus.builtin.Data
import scalus.builtin.Data.toData
import scalus.ledger.api.v3.PubKeyHash
import scalus.ledger.api.v3.ScriptContext
import scalus.uplc.eval
import scalus.uplc.eval.*
import upickle.default.*

import java.math.BigInteger
import java.nio.file.Files
import java.nio.file.Path
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

class ValidatorTests extends munit.ScalaCheckSuite {

    // test(s"Bitcoin validator size is ${bitcoinProgram.doubleCborEncoded.length}") {
    // println(compiledBitcoinValidator.showHighlighted)
    // assertEquals(bitcoinProgram.doubleCborEncoded.length, 900)
    // }

    test("Tx size makes sense") {
        val blockHeader =
            hex"000000302b974c15e2ef994183f9806c5be9c61e74abc512a14301000000000000000000aff4af5b1dcc2b8754db824b9911818b65913dc262c295f060abb45c6c1d7ee749f90b67cd0e0317f9cc7dac"
        val coinbaseTx =
            hex"010000000001010000000000000000000000000000000000000000000000000000000000000000ffffffff2503233708184d696e656420627920416e74506f6f6c373946205b8160a4256c0000946e0100ffffffff02f595814a000000001976a914edf10a7fac6b32e24daa5305c723f3de58db1bc888ac0000000000000000266a24aa21a9edfaa194df59043645ba0f58aad74bfd5693fa497093174d12a4bb3b0574a878db0120000000000000000000000000000000000000000000000000000000000000000000000000"
        println(blockHeader.length)
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

    test("bitsToTarget") {
        assertEquals(
          BitcoinValidator.bitsToTarget(hex"00030ecd".reverse),
          hex"0000000000000000000000000000000000000000000000000000000000030ecd"
        )
        assertEquals(
          BitcoinValidator.bitsToTarget(hex"17030ecd".reverse),
          hex"000000000000030ecd0000000000000000000000000000000000000000000000"
        )
        assertEquals(
          BitcoinValidator.bitsToTarget(hex"19030ecd".reverse),
          hex"00000000030ecd00000000000000000000000000000000000000000000000000"
        )
        assertEquals(
          BitcoinValidator.bitsToTarget(hex"1a030ecd".reverse),
          hex"00000000ffff0000000000000000000000000000000000000000000000000000"
        )
        assertEquals(
          BitcoinValidator.bitsToTarget(hex"20030ecd".reverse),
          hex"00000000ffff0000000000000000000000000000000000000000000000000000"
        )
    }

    test("Block Header hash") {
        val blockHeader = BlockHeader(
          hex"000000302b974c15e2ef994183f9806c5be9c61e74abc512a14301000000000000000000aff4af5b1dcc2b8754db824b9911818b65913dc262c295f060abb45c6c1d7ee749f90b67cd0e0317f9cc7dac"
        )
        val hash = BitcoinValidator.blockHeaderHash(blockHeader)
        assertEquals(hash, hex"00000000000000000002cfdedd8358532b2284bc157e1352dbc8682b2067fb0c")
    }

    test("Block Header hash satisfies target") {
        val blockHeader = BlockHeader(
          hex"000000302b974c15e2ef994183f9806c5be9c61e74abc512a14301000000000000000000aff4af5b1dcc2b8754db824b9911818b65913dc262c295f060abb45c6c1d7ee749f90b67cd0e0317f9cc7dac"
        )
        val hash = BitcoinValidator.blockHeaderHash(blockHeader)
        val target = BitcoinValidator.bitsToTarget(hex"17030ecd".reverse)
        assertEquals(hash, hex"00000000000000000002cfdedd8358532b2284bc157e1352dbc8682b2067fb0c")
        assertEquals(target, hex"000000000000030ecd0000000000000000000000000000000000000000000000")
        assert(Builtins.lessThanByteString(hash, target))
    }

    test("Evaluate") {
        import scalus.ledger.api.v3.ToDataInstances.given
        import scalus.builtin.ToDataInstances.given
        val block = read[BitcoinBlock](
          Files.readString(
            Path.of("00000000000000000002cfdedd8358532b2284bc157e1352dbc8682b2067fb0c.json")
          )
        )
        println(s"block.merkleroot = ${block.merkleroot}")

        val coinbaseTx =
            hex"010000000001010000000000000000000000000000000000000000000000000000000000000000ffffffff4f03d6340d082f5669614254432f2cfabe6d6d7853581d1f2abd53fe30833a1e6b8397b200ec3b658fdf6568edc5f54118f99d10000000000000001003b7b5047d947d88f58877bd2cb73c0000000000ffffffff0342db4a15000000001976a914fb37342f6275b13936799def06f2eb4c0f20151588ac00000000000000002b6a2952534b424c4f434b3aec94a488ba1f588e9cb6bb507c70283e9f10057ef683d2d460d3700900678b710000000000000000266a24aa21a9edb9df509533c6a9526f5180b0c34cc48b86a40cc94abfae661242a596393ef47d0120000000000000000000000000000000000000000000000000000000000000000000000000"
        val coinbase = Bitcoin.makeCoinbaseTxFromByteString(coinbaseTx)
        val coinbaseHash = BitcoinValidator.getCoinbaseTxHash(coinbase)
        println(s"Coinbase hash: ${coinbaseHash.reverse}")

        val txHashes = block.tx.map(h => ByteString.fromHex(h).reverse)
        val merkleTree = MerkleTree.fromHashes(txHashes)
        val merkleRoot = merkleTree.getMerkleRoot
        val merkleProof = merkleTree.makeMerkleProof(0)
        println(s"Merkle root: ${merkleRoot.reverse}")
        println(s"Merkle proof: ${merkleProof}")

        val coinbaseTxInclusionProof = scalus.prelude.List.from(merkleProof).toData

        val blockHeader = BlockHeader(
          bytes =
              hex"000000302b974c15e2ef994183f9806c5be9c61e74abc512a14301000000000000000000aff4af5b1dcc2b8754db824b9911818b65913dc262c295f060abb45c6c1d7ee749f90b67cd0e0317f9cc7dac"
        )
        val hash = BitcoinValidator.blockHeaderHash(blockHeader)
        val target = BitcoinValidator.bitsToTarget(hex"17030ecd".reverse)

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
              signature,
              coinbase,
              coinbaseTxInclusionProof
            )
            .toData

        println(s"Redeemer size: ${redeemer.toCbor.length}")

        val (scriptContext, tx) = makeScriptContextAndTransaction(
          State(865494, blockHash = hex"00000000000000000002cfdedd8358532b2284bc157e1352dbc8682b2067fb0c").toData,
          redeemer,
          Seq.empty
        )
        println(s"Tx size: ${tx.serialize().length}")
        import scalus.uplc.TermDSL.{*, given}
        val applied = bitcoinValidator $ scriptContext.toData
        BitcoinValidator.validator(scriptContext.toData)
        println(applied.evalDebug)
    }

    def signMessage(claim: ByteString, privateKey: Ed25519PrivateKeyParameters): ByteString =
        val signer = new Ed25519Signer();
        signer.init(true, privateKey);
        signer.update(claim.bytes, 0, claim.length)
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
        datum: Data,
        redeemer: Data,
        signatories: Seq[PubKeyHash]
    ): (ScriptContext, Transaction) =
        val script = PlutusV3Script
            .builder()
            .`type`("PlutusScriptV3")
            .cborHex(bitcoinProgram.doubleCborHex)
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
        val inputs = util.List.of(input)

        val utxo = Map(
          input -> TransactionOutput
              .builder()
              .value(spec.Value.builder().coin(BigInteger.valueOf(20)).build())
              .address(payeeAddress)
              .inlineDatum(Interop.toPlutusData(datum))
              .build()
        )
        val tx = Transaction
            .builder()
            .body(
              TransactionBody
                  .builder()
                  .fee(ADAConversionUtil.adaToLovelace(0.2))
                  .inputs(inputs)
                  .requiredSigners(signatories.map(_.hash.bytes).asJava)
                  .build()
            )
            .witnessSet(
              TransactionWitnessSet
                  .builder()
                  .plutusV3Scripts(util.List.of(script))
                  .redeemers(util.List.of(rdmr))
                  .build()
            )
            .build()

        val protocolVersion = 9
        val scriptContext = getScriptContextV3(
          rdmr,
          Some(datum),
          tx,
          TransactionUtil.getTxHash(tx),
          utxo,
          SlotConfig.Mainnet,
          protocolVersion
        )
        (scriptContext, tx)

}
