package binocular
import binocular.BitcoinValidator.{Action, BlockHeader, ChainState}
import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.address.Address
import com.bloxbean.cardano.client.common.ADAConversionUtil
import com.bloxbean.cardano.client.plutus.spec.{ExUnits, PlutusV3Script, Redeemer, RedeemerTag}
import com.bloxbean.cardano.client.transaction.spec
import com.bloxbean.cardano.client.transaction.spec.*
import com.bloxbean.cardano.client.transaction.util.TransactionUtil
import org.bouncycastle.crypto.params.Ed25519PrivateKeyParameters
import org.bouncycastle.crypto.signers.Ed25519Signer
import org.scalacheck.Prop.forAll
import scalus.*
import scalus.bloxbean.Interop.{getScriptInfoV3, getTxInfoV3, toScalusData}
import scalus.bloxbean.{Interop, SlotConfig}
import scalus.builtin.ByteString.hex
import scalus.builtin.Data.toData
import scalus.builtin.{Builtins, ByteString, Data}
import scalus.cardano.ledger
import scalus.cardano.ledger.{CardanoInfo, Coin}
import scalus.ledger.api.v3.{PubKeyHash, ScriptContext}
import scalus.uplc.eval
import scalus.uplc.eval.*
import upickle.default.*

import java.math.BigInteger
import java.nio.file.{Files, Path}
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

        val redeemer = Action
            .UpdateOracle(scalus.prelude.List.single(blockHeader), timestamp)
            .toData

        println(s"Redeemer size: ${redeemer.toCbor.length}")

        val prevState = ChainState(
          865493,
          blockHash = hex"0000000000000000000143a112c5ab741ec6e95b6c80f9834199efe2154c972b".reverse,
          currentTarget = bits,
          blockTimestamp = timestamp - 600,
          recentTimestamps = prelude.List(timestamp - 600),
          previousDifficultyAdjustmentTimestamp = timestamp - 600 * BitcoinValidator.DifficultyAdjustmentInterval,
          confirmedBlocksTree =
              prelude.List(hex"0000000000000000000143a112c5ab741ec6e95b6c80f9834199efe2154c972b".reverse),
          forksTree = scalus.prelude.SortedMap.empty
        )

        // Calculate chainwork for the new block (matching BitcoinValidator logic)
        // When parent is confirmed tip, parent chainwork = compactBitsToTarget(confirmedState.currentTarget)
        val parentChainwork = BitcoinValidator.compactBitsToTarget(prevState.currentTarget)
        val blockWork = BitcoinValidator.PowLimit / target
        val newBlockChainwork = parentChainwork + blockWork
        val newBlockHeight = prevState.blockHeight + 1

        // Expected state: block added to forks tree, but NOT promoted (doesn't meet 100 confirmations + 200 min criteria)
        val newBlockNode = BitcoinValidator.BlockNode(
          prevBlockHash = prevState.blockHash,
          height = newBlockHeight,
          chainwork = newBlockChainwork,
          addedTimestamp = timestamp, // Uses blockHeader.timestamp, which is the block's timestamp
          children = prelude.List.empty
        )
        val newForksTree = scalus.prelude.SortedMap.singleton(hash, newBlockNode)

        val newState = ChainState(
          prevState.blockHeight, // Height unchanged - no promotion
          blockHash = prevState.blockHash, // Hash unchanged - no promotion
          currentTarget = prevState.currentTarget, // Target unchanged - no promotion
          blockTimestamp = prevState.blockTimestamp, // Timestamp unchanged - no promotion
          recentTimestamps = prevState.recentTimestamps, // Timestamps unchanged - no promotion
          previousDifficultyAdjustmentTimestamp =
              prevState.previousDifficultyAdjustmentTimestamp, // Unchanged - no promotion
          confirmedBlocksTree = prevState.confirmedBlocksTree, // Tree unchanged - no promotion
          forksTree = newForksTree // Block added to forks tree
        )
        println(s"Block prevHash: ${blockHeader.prevBlockHash.toHex}")
        println(s"Expected forksTree size: ${newForksTree.toList.size}")
        println(s"Expected newState.forksTree size: ${newState.forksTree.toList.size}")

        val scriptAddress = new Address(
          binocular.OracleConfig(network = binocular.CardanoNetwork.Testnet).scriptAddress
        )
        val outputAmount = BigInteger.valueOf(5000000) // 5 ADA

        val (scriptContext, tx) = makeScriptContextAndTransaction(
          timestamp.toLong,
          prevState.toData,
          newState.toData,
          redeemer,
          Seq.empty,
          scriptAddress,
          outputAmount
        )
        println(s"Tx size: ${tx.serialize().length}")
        val applied = BitcoinContract.bitcoinProgram $ scriptContext.toData
        println(s"Validator size: ${BitcoinContract.bitcoinProgram.flatEncoded.length}")

        // Try to extract computed state for debugging
        try {
            BitcoinValidator.validate(scriptContext.toData)
        } catch {
            case e: Exception =>
                println(s"Validation error: ${e.getMessage}")
                throw e
        }
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
        signatories: Seq[PubKeyHash],
        scriptAddress: Address,
        outputAmount: BigInteger
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
                      .steps(BigInteger.valueOf(10000000000L))
                      .mem(BigInteger.valueOf(20000000))
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
              .value(spec.Value.builder().coin(outputAmount).build())
              .address(scriptAddress.getAddress)
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
                  .outputs(
                    util.List.of(
                      TransactionOutput
                          .builder()
                          .address(scriptAddress.getAddress)
                          .value(Value.builder().coin(outputAmount).build())
                          .inlineDatum(Interop.toPlutusData(datum))
                          .build()
                    )
                  )
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

    // ===== TEST INFRASTRUCTURE HELPERS =====

    /** Generate a sequence of valid timestamps for testing */
    def makeTimestampSequence(baseTime: BigInt, count: Int, spacing: BigInt = 600): scalus.prelude.List[BigInt] = {
        def buildSeq(remaining: Int, currentTime: BigInt, acc: List[BigInt]): List[BigInt] = {
            if remaining == 0 then acc.reverse
            else buildSeq(remaining - 1, currentTime - spacing, currentTime :: acc)
        }
        scalus.prelude.List.from(buildSeq(count, baseTime, Nil))
    }

    /** Calculate accumulated chainwork for a block */
    def computeChainwork(parentChainwork: BigInt, target: BigInt): BigInt = {
        // Chainwork formula: parent_chainwork + 2^256 / (target + 1)
        // Simplified for testing: just add work based on target
        val work = (BigInt(2).pow(256) / (target + 1))
        parentChainwork + work
    }

    /** Build a simple forks tree with linear chain of blocks */
    def buildSimpleForksTree(
        confirmedTip: BitcoinValidator.BlockHash,
        confirmedHeight: BigInt,
        confirmedChainwork: BigInt,
        blockCount: Int,
        baseTimestamp: BigInt,
        bits: BitcoinValidator.CompactBits
    ): scalus.prelude.SortedMap[BitcoinValidator.BlockHash, BitcoinValidator.BlockNode] = {
        val target = BitcoinValidator.compactBitsToTarget(bits)

        def buildChain(
            remaining: Int,
            prevHash: BitcoinValidator.BlockHash,
            height: BigInt,
            chainwork: BigInt,
            timestamp: BigInt,
            tree: scalus.prelude.SortedMap[BitcoinValidator.BlockHash, BitcoinValidator.BlockNode]
        ): scalus.prelude.SortedMap[BitcoinValidator.BlockHash, BitcoinValidator.BlockNode] = {
            if remaining == 0 then tree
            else
                // Create synthetic block hash (for testing only)
                val blockHash = ByteString.fromArray(
                  Array.fill(32)(((height % 256).toByte))
                )
                val newChainwork = computeChainwork(chainwork, target)
                val node = BitcoinValidator.BlockNode(
                  prevBlockHash = prevHash,
                  height = height,
                  chainwork = newChainwork,
                  addedTimestamp = timestamp,
                  children = scalus.prelude.List.empty
                )
                val updatedTree = tree.insert(blockHash, node)
                buildChain(remaining - 1, blockHash, height + 1, newChainwork, timestamp + 600, updatedTree)
        }

        buildChain(
          blockCount,
          confirmedTip,
          confirmedHeight + 1,
          confirmedChainwork,
          baseTimestamp,
          scalus.prelude.SortedMap.empty
        )
    }

    /** Build a test ChainState with populated forks tree */
    def buildTestChainState(
        blockHeight: BigInt,
        blockHash: BitcoinValidator.BlockHash,
        bits: BitcoinValidator.CompactBits,
        baseTimestamp: BigInt,
        forksTreeSize: Int = 0
    ): BitcoinValidator.ChainState = {
        val forksTree =
            if forksTreeSize > 0 then
                buildSimpleForksTree(
                  blockHash,
                  blockHeight,
                  blockHeight * BigInt(1000000), // Simple chainwork approximation
                  forksTreeSize,
                  baseTimestamp + 600,
                  bits
                )
            else scalus.prelude.SortedMap.empty

        BitcoinValidator.ChainState(
          blockHeight = blockHeight,
          blockHash = blockHash,
          currentTarget = bits,
          blockTimestamp = baseTimestamp,
          recentTimestamps = scalus.prelude.List(baseTimestamp),
          previousDifficultyAdjustmentTimestamp = baseTimestamp - 600 * 2016,
          confirmedBlocksTree = prelude.List(blockHash),
          forksTree = forksTree
        )
    }

    // ===== EXISTING TESTS =====

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

    // ===== FORK HANDLING TESTS =====

    test("validateForkSubmission - reject duplicate blocks") {
        // Create a block header
        val blockHeader = BlockHeader(
          hex"000000302b974c15e2ef994183f9806c5be9c61e74abc512a14301000000000000000000aff4af5b1dcc2b8754db824b9911818b65913dc262c295f060abb45c6c1d7ee749f90b67cd0e0317f9cc7dac"
        )

        val confirmedTip = hex"0000000000000000000143a112c5ab741ec6e95b6c80f9834199efe2154c972b".reverse
        val forksTree: scalus.prelude.SortedMap[BitcoinValidator.BlockHash, BitcoinValidator.BlockNode] =
            scalus.prelude.SortedMap.empty

        // Submit same block twice - should fail
        val duplicateHeaders = scalus.prelude.List.from(Seq(blockHeader, blockHeader))

        intercept[RuntimeException] {
            BitcoinValidator.validateForkSubmission(duplicateHeaders, forksTree, confirmedTip)
        }
    }

    test("validateForkSubmission - accept pure canonical extension") {
        val confirmedTip = hex"0000000000000000000143a112c5ab741ec6e95b6c80f9834199efe2154c972b".reverse

        // Block extending confirmed tip
        val blockExtendingTip = BlockHeader(
          hex"000000302b974c15e2ef994183f9806c5be9c61e74abc512a14301000000000000000000aff4af5b1dcc2b8754db824b9911818b65913dc262c295f060abb45c6c1d7ee749f90b67cd0e0317f9cc7dac"
        )

        val forksTree: scalus.prelude.SortedMap[BitcoinValidator.BlockHash, BitcoinValidator.BlockNode] =
            scalus.prelude.SortedMap.empty
        val headers = scalus.prelude.List.single(blockExtendingTip)

        // Should NOT throw - pure canonical extension is valid
        BitcoinValidator.validateForkSubmission(headers, forksTree, confirmedTip)
    }

    test("validateForkSubmission - reject pure fork submission") {
        val confirmedTip = hex"0000000000000000000143a112c5ab741ec6e95b6c80f9834199efe2154c972b".reverse

        // Block NOT extending confirmed tip (fork)
        val forkBlock = BlockHeader(
          hex"00000020ffffffffffffffffffffffffffffffffffffffffffffffffffffffff00000000aff4af5b1dcc2b8754db824b9911818b65913dc262c295f060abb45c6c1d7ee749f90b67cd0e0317f9cc7dac"
        )

        val forksTree: scalus.prelude.SortedMap[BitcoinValidator.BlockHash, BitcoinValidator.BlockNode] =
            scalus.prelude.SortedMap.empty
        val headers = scalus.prelude.List.single(forkBlock)

        // Should throw - fork without canonical extension
        intercept[RuntimeException] {
            BitcoinValidator.validateForkSubmission(headers, forksTree, confirmedTip)
        }
    }

    test("validateForkSubmission - accept canonical + fork") {
        val confirmedTip = hex"0000000000000000000143a112c5ab741ec6e95b6c80f9834199efe2154c972b".reverse

        // Block extending confirmed tip (canonical)
        val canonicalBlock = BlockHeader(
          hex"000000302b974c15e2ef994183f9806c5be9c61e74abc512a14301000000000000000000aff4af5b1dcc2b8754db824b9911818b65913dc262c295f060abb45c6c1d7ee749f90b67cd0e0317f9cc7dac"
        )

        // Block NOT extending confirmed tip (fork)
        val forkBlock = BlockHeader(
          hex"00000020ffffffffffffffffffffffffffffffffffffffffffffffffffffffff00000000aff4af5b1dcc2b8754db824b9911818b65913dc262c295f060abb45c6c1d7ee749f90b67cd0e0317f9cc7dac"
        )

        val forksTree: scalus.prelude.SortedMap[BitcoinValidator.BlockHash, BitcoinValidator.BlockNode] =
            scalus.prelude.SortedMap.empty
        val headers = scalus.prelude.List.from(Seq(canonicalBlock, forkBlock))

        // Should NOT throw - canonical + fork is valid
        BitcoinValidator.validateForkSubmission(headers, forksTree, confirmedTip)
    }

    test("addBlockToForksTree - block extending confirmed tip") {
        val confirmedTip = hex"0000000000000000000143a112c5ab741ec6e95b6c80f9834199efe2154c972b".reverse
        val bits = hex"17030ecd".reverse

        // Create a block extending the confirmed tip with valid timestamp
        val newBlockHeader = BlockHeader(
          hex"000000302b974c15e2ef994183f9806c5be9c61e74abc512a14301000000000000000000aff4af5b1dcc2b8754db824b9911818b65913dc262c295f060abb45c6c1d7ee749f90b67cd0e0317f9cc7dac"
        )
        val blockTimestamp = newBlockHeader.timestamp

        // Use current time for realistic validation
        val currentTime = BigInt(System.currentTimeMillis() / 1000)
        // val blockTimestamp = currentTime - 100  // Block is 100 seconds in the past

        val confirmedState = ChainState(
          blockHeight = 1000,
          blockHash = confirmedTip,
          currentTarget = bits,
          blockTimestamp = blockTimestamp - 600,
          recentTimestamps = scalus.prelude.List(blockTimestamp - 600),
          previousDifficultyAdjustmentTimestamp = blockTimestamp - 600 * 2016,
          confirmedBlocksTree = prelude.List(confirmedTip),
          forksTree = scalus.prelude.SortedMap.empty
        )

        // println(s"[DEBUG] Adding block to forks tree...,  confiremrdState.medianTimestamp = ${BitcoinValidator.getMedianTimePast(confirmedState.recentTimestamps, confirmedState.recentTimestamps.size)}"))
        // println(s"[DEBUG] newBlockHeader.timestamp = ${newBlockHeader.timestamp}, currentTime = $currentTime")

        // Add block to empty forksTree - should succeed
        val updatedForksTree = BitcoinValidator.addBlockToForksTree(
          confirmedState.forksTree,
          newBlockHeader,
          confirmedState,
          currentTime
        )

        val newBlockHash = BitcoinValidator.blockHeaderHash(newBlockHeader)

        // Verify block was added
        assert(BitcoinValidator.lookupBlock(updatedForksTree, newBlockHash).isDefined)

        // Verify block extends confirmed tip
        val blockNode = BitcoinValidator.lookupBlock(updatedForksTree, newBlockHash).getOrFail("Block not found")
        assertEquals(blockNode.prevBlockHash, confirmedTip)
        assertEquals(blockNode.height, BigInt(1001))
    }

    test("forksTree structure - block extending another block in tree") {
        // Test the fork tree structure logic by manually creating nodes
        val confirmedTip = hex"1000000000000000000000000000000000000000000000000000000000000000"
        val firstBlockHash = hex"1001000000000000000000000000000000000000000000000000000000000000"
        val secondBlockHash = hex"1002000000000000000000000000000000000000000000000000000000000000"

        // Create first block node extending confirmed tip
        val firstNode = BitcoinValidator.BlockNode(
          prevBlockHash = confirmedTip,
          height = 1001,
          chainwork = BigInt(1000000),
          addedTimestamp = BigInt(1234567890),
          children = scalus.prelude.List.empty
        )

        // Create second block node extending first block
        val secondNode = BitcoinValidator.BlockNode(
          prevBlockHash = firstBlockHash,
          height = 1002,
          chainwork = BigInt(2000000),
          addedTimestamp = BigInt(1234567900),
          children = scalus.prelude.List.empty
        )

        // Build fork tree with both blocks
        val forksTree = scalus.prelude.SortedMap.empty
            .insert(firstBlockHash, firstNode)
            .insert(secondBlockHash, secondNode)

        // Verify both blocks are in tree
        assert(BitcoinValidator.lookupBlock(forksTree, firstBlockHash).isDefined)
        assert(BitcoinValidator.lookupBlock(forksTree, secondBlockHash).isDefined)

        // Verify second block extends first
        val retrievedSecondNode =
            BitcoinValidator.lookupBlock(forksTree, secondBlockHash).getOrFail("Second block not found")
        assertEquals(retrievedSecondNode.prevBlockHash, firstBlockHash)
        assertEquals(retrievedSecondNode.height, BigInt(1002))

        // Verify canonical chain selection picks highest chainwork
        val canonicalTip = BitcoinValidator.selectCanonicalChain(forksTree)
        assertEquals(canonicalTip, scalus.prelude.Option.Some(secondBlockHash))
    }

    test("selectCanonicalChain - empty forksTree returns None") {
        val emptyForksTree: scalus.prelude.SortedMap[BitcoinValidator.BlockHash, BitcoinValidator.BlockNode] =
            scalus.prelude.SortedMap.empty
        val result = BitcoinValidator.selectCanonicalChain(emptyForksTree)

        assert(result.isEmpty)
    }

    test("selectCanonicalChain - single block returns that block") {
        val confirmedTip = hex"0000000000000000000143a112c5ab741ec6e95b6c80f9834199efe2154c972b".reverse
        val blockHash = hex"00000000000000000002cfdedd8358532b2284bc157e1352dbc8682b2067fb0c".reverse

        val blockNode = BitcoinValidator.BlockNode(
          prevBlockHash = confirmedTip,
          height = 1001,
          chainwork = BigInt(1000000),
          addedTimestamp = BigInt(1234567890),
          children = scalus.prelude.List.empty
        )

        val forksTree = scalus.prelude.SortedMap.empty.insert(blockHash, blockNode)
        val result = BitcoinValidator.selectCanonicalChain(forksTree)

        assertEquals(result, scalus.prelude.Option.Some(blockHash))
    }

    test("selectCanonicalChain - selects highest chainwork") {
        val confirmedTip = hex"0000000000000000000143a112c5ab741ec6e95b6c80f9834199efe2154c972b".reverse

        // Block with lower chainwork
        val lowChainworkHash = hex"1111111111111111111111111111111111111111111111111111111111111111"
        val lowChainworkNode = BitcoinValidator.BlockNode(
          prevBlockHash = confirmedTip,
          height = 1001,
          chainwork = BigInt(1000000),
          addedTimestamp = BigInt(1234567890),
          children = scalus.prelude.List.empty
        )

        // Block with higher chainwork
        val highChainworkHash = hex"2222222222222222222222222222222222222222222222222222222222222222"
        val highChainworkNode = BitcoinValidator.BlockNode(
          prevBlockHash = confirmedTip,
          height = 1001,
          chainwork = BigInt(2000000),
          addedTimestamp = BigInt(1234567890),
          children = scalus.prelude.List.empty
        )

        val forksTree = scalus.prelude.SortedMap.empty
            .insert(lowChainworkHash, lowChainworkNode)
            .insert(highChainworkHash, highChainworkNode)

        val result = BitcoinValidator.selectCanonicalChain(forksTree)

        // Should select block with highest chainwork
        assertEquals(result, scalus.prelude.Option.Some(highChainworkHash))
    }

    test("forksTree after promotion - disconnected forks are valid") {
        // Scenario: After promoting blocks 1001-1003, old fork 1001' remains
        // This fork extends old confirmed tip 1000, not new tip 1003
        // This is a VALID state - garbage collection will clean it up

        val oldConfirmedTip = hex"1000000000000000000000000000000000000000000000000000000000000000"
        val newConfirmedTip = hex"1003000000000000000000000000000000000000000000000000000000000000"

        // Old fork block extending block 1000 (no longer confirmed tip)
        val oldForkHash = hex"1001111111111111111111111111111111111111111111111111111111111111"
        val oldForkNode = BitcoinValidator.BlockNode(
          prevBlockHash = oldConfirmedTip,
          height = 1001,
          chainwork = BigInt(500000),
          addedTimestamp = BigInt(1234567890),
          children = scalus.prelude.List.empty
        )

        val forksTree = scalus.prelude.SortedMap.empty.insert(oldForkHash, oldForkNode)

        // This state is valid even though oldFork doesn't extend newConfirmedTip
        // The fork is "disconnected" but will be removed by garbage collection
        assert(forksTree.toList.size == 1)

        val node = BitcoinValidator.lookupBlock(forksTree, oldForkHash).getOrFail("Fork not found")
        // Fork still points to old tip
        assertEquals(node.prevBlockHash, oldConfirmedTip)
    }

    // ===== UPDATE ORACLE UNIT TESTS =====

    test("UpdateOracle - add single block to forks tree") {
        // Use real Bitcoin blocks 864800 -> 864801
        // Block 864800: hash=0000000000000000000202c4f7f242ab864a6162b8d1e745de6a86ae979e130b
        val block864800Hash = "0000000000000000000202c4f7f242ab864a6162b8d1e745de6a86ae979e130b"
        val block864800Height = 864800
        val block864800Timestamp = 1728414569L

        // Block 864801: extends 864800
        val block864801Hash = "00000000000000000002935605126a1c5587bf4b642fffe943595e6d669c817d"
        val block864801PrevHash = "0000000000000000000202c4f7f242ab864a6162b8d1e745de6a86ae979e130b"
        val block864801Bits = "17032f14"
        val block864801Timestamp = 1728414644L
        val block864801Nonce = 4254568467L
        val block864801Version = 666378240L
        val block864801Merkleroot = "dc2ad1c7a27d394d45961c13a9c36e1d5e6cdbea928c47cb06263bb049a9cd0f"

        val confirmedTip = ByteString.fromHex(block864800Hash).reverse
        val bits = ByteString.fromHex(block864801Bits).reverse
        val baseTime = BigInt(block864801Timestamp)

        // Construct the block header for 864801
        def longToLE4Bytes(n: Long): ByteString = {
            ByteString.fromArray(
              Array(
                (n & 0xff).toByte,
                ((n >> 8) & 0xff).toByte,
                ((n >> 16) & 0xff).toByte,
                ((n >> 24) & 0xff).toByte
              )
            )
        }

        val blockHeader = BlockHeader(
          longToLE4Bytes(block864801Version) ++
              ByteString.fromHex(block864801PrevHash).reverse ++
              ByteString.fromHex(block864801Merkleroot).reverse ++
              longToLE4Bytes(block864801Timestamp) ++
              ByteString.fromHex(block864801Bits).reverse ++
              longToLE4Bytes(block864801Nonce)
        )

        // Verify the header hash matches the expected block hash
        val computedHash = BitcoinValidator.blockHeaderHash(blockHeader)
        assertEquals(computedHash.reverse.toHex, block864801Hash, "Block header hash mismatch")

        // Create initial state with block 864800 as confirmed tip
        val prevState = buildTestChainState(
          blockHeight = block864800Height,
          blockHash = confirmedTip,
          bits = bits,
          baseTimestamp = BigInt(block864800Timestamp),
          forksTreeSize = 0
        )

        // Manually compute expected new state matching BitcoinValidator logic
        // After processing, the block should be in forks tree (not promoted - no 100 confirmations yet)
        val newBlockHash = BitcoinValidator.blockHeaderHash(blockHeader)
        val target = BitcoinValidator.compactBitsToTarget(bits)

        // Parent chainwork: when parent is confirmed tip, use compactBitsToTarget(confirmedState.currentTarget)
        val parentChainwork = BitcoinValidator.compactBitsToTarget(prevState.currentTarget)
        val blockWork = BitcoinValidator.PowLimit / target
        val newChainwork = parentChainwork + blockWork

        val expectedNode = BitcoinValidator.BlockNode(
          prevBlockHash = prevState.blockHash,
          height = prevState.blockHeight + 1,
          chainwork = newChainwork,
          addedTimestamp = baseTime,
          children = scalus.prelude.List.empty
        )

        val expectedForksTree = prevState.forksTree.insert(newBlockHash, expectedNode)

        val expectedState = BitcoinValidator.ChainState(
          blockHeight = prevState.blockHeight,
          blockHash = prevState.blockHash,
          currentTarget = prevState.currentTarget,
          blockTimestamp = prevState.blockTimestamp,
          recentTimestamps = prevState.recentTimestamps,
          previousDifficultyAdjustmentTimestamp = prevState.previousDifficultyAdjustmentTimestamp,
          confirmedBlocksTree = prevState.confirmedBlocksTree,
          forksTree = expectedForksTree
        )

        // Create redeemer
        val redeemer = Action.UpdateOracle(scalus.prelude.List.single(blockHeader), baseTime).toData

        // Create script context and transaction
        val scriptAddress = new Address(
          binocular.OracleConfig(network = binocular.CardanoNetwork.Testnet).scriptAddress
        )
        val outputAmount = BigInteger.valueOf(5000000) // 5 ADA

        val (scriptContext, tx) = makeScriptContextAndTransaction(
          baseTime.toLong,
          prevState.toData,
          expectedState.toData,
          redeemer,
          Seq.empty,
          scriptAddress,
          outputAmount
        )

        // Validate - should succeed
        BitcoinValidator.validate(scriptContext.toData)

        // Verify the computation internally matches
        val result = BitcoinContract.bitcoinProgram $ scriptContext.toData
        val prices = CardanoInfo.mainnet.protocolParams.executionUnitPrices
        result.evaluateDebug match
            case r: Result.Success =>
                println(s"✓ UpdateOracle single block validation succeeded, budget used: ${r.budget.showJson}")
                println(r)
                assertEquals(r.budget, ledger.ExUnits(557896, 172_133254), "Unexpected resource usage")
                assertEquals(r.budget.fee(prices), Coin(44585), "Unexpected fee cost")
            case r: Result.Failure =>
                fail(s"Validation failed: $r")
    }

    test("UpdateOracle - reject empty block headers list") {
        val baseTime = BigInt(System.currentTimeMillis() / 1000)
        val bits = hex"17030ecd".reverse
        val confirmedTip = hex"1000000000000000000000000000000000000000000000000000000000000000"

        val prevState = buildTestChainState(
          blockHeight = 1000,
          blockHash = confirmedTip,
          bits = bits,
          baseTimestamp = baseTime - 1000,
          forksTreeSize = 0
        )

        // Create redeemer with empty list
        val redeemer = Action.UpdateOracle(scalus.prelude.List.empty, BigInt(System.currentTimeMillis() / 1000)).toData

        val scriptAddress = new Address(
          binocular.OracleConfig(network = binocular.CardanoNetwork.Testnet).scriptAddress
        )
        val outputAmount = BigInteger.valueOf(5000000) // 5 ADA

        val (scriptContext, tx) = makeScriptContextAndTransaction(
          baseTime.toLong * 1000,
          prevState.toData,
          prevState.toData, // Expected state same as previous (no change)
          redeemer,
          Seq.empty,
          scriptAddress,
          outputAmount
        )

        // Should fail with "Empty block headers list"
        intercept[RuntimeException] {
            BitcoinValidator.validate(scriptContext.toData)
        }
    }

    // ===== BLOCK PROMOTION TESTS =====

    test("promoteQualifiedBlocks - empty forks tree returns empty list") {
        val confirmedTip = hex"1000000000000000000000000000000000000000000000000000000000000000"
        val emptyForksTree: scalus.prelude.SortedMap[BitcoinValidator.BlockHash, BitcoinValidator.BlockNode] =
            scalus.prelude.SortedMap.empty
        val currentTime = BigInt(System.currentTimeMillis() / 1000)

        val (promotedBlocks, updatedTree) = BitcoinValidator.promoteQualifiedBlocks(
          emptyForksTree,
          confirmedTip,
          confirmedHeight = 1000,
          currentTime
        )

        assert(promotedBlocks.isEmpty, "Should not promote any blocks from empty tree")
        assertEquals(updatedTree.toList.size, BigInt(0), "Tree should remain empty")
    }

    test("promoteQualifiedBlocks - block meets both criteria (100+ conf, 200+ min)") {
        val confirmedTip = hex"1000000000000000000000000000000000000000000000000000000000000000"
        val block1001Hash = hex"1001000000000000000000000000000000000000000000000000000000000000"
        val currentTime = BigInt(System.currentTimeMillis() / 1000)

        // Block at height 1001, added 201 minutes ago
        // Will be at depth 100 when canonical tip reaches 1101
        val block1001Node = BitcoinValidator.BlockNode(
          prevBlockHash = confirmedTip,
          height = 1001,
          chainwork = BigInt(1001000),
          addedTimestamp = currentTime - (201 * 60), // 201 minutes ago
          children = scalus.prelude.List.empty
        )

        // Build complete chain from 1001 to 1101 (100 confirmations)
        var forksTree = scalus.prelude.SortedMap.empty.insert(block1001Hash, block1001Node)
        var prevHash = block1001Hash
        var prevChainwork = BigInt(1001000)

        for (h <- 1002 to 1101) {
            val hash = ByteString.fromArray(Array.fill(32)(((h % 256).toByte)))
            val node = BitcoinValidator.BlockNode(
              prevBlockHash = prevHash,
              height = h,
              chainwork = prevChainwork + 1000,
              addedTimestamp = currentTime - (201 * 60) + (h - 1002) * 60,
              children = scalus.prelude.List.empty
            )
            forksTree = forksTree.insert(hash, node)
            prevHash = hash
            prevChainwork = prevChainwork + 1000
        }

        val (promotedBlocks, updatedTree) = BitcoinValidator.promoteQualifiedBlocks(
          forksTree,
          confirmedTip,
          confirmedHeight = 1000,
          currentTime
        )

        assertEquals(promotedBlocks.length, BigInt(1), "Should promote 1 block")
        assertEquals(promotedBlocks.head, block1001Hash, "Should promote the qualified block")

        // Block 1001 should be removed, but 1002-1101 should remain
        assert(BitcoinValidator.lookupBlock(updatedTree, block1001Hash).isEmpty, "Block 1001 should be removed")
    }

    test("promoteQualifiedBlocks - reject block with insufficient confirmations") {
        val confirmedTip = hex"1000000000000000000000000000000000000000000000000000000000000000"
        val blockHash = hex"1001000000000000000000000000000000000000000000000000000000000000"
        val currentTime = BigInt(System.currentTimeMillis() / 1000)

        // Block at height 1001, added 201 minutes ago
        // Current canonical tip at height 1050 (only 49 confirmations - needs 100)
        val canonicalTipHash = hex"1050000000000000000000000000000000000000000000000000000000000000"
        val blockNode = BitcoinValidator.BlockNode(
          prevBlockHash = confirmedTip,
          height = 1001,
          chainwork = BigInt(1001000),
          addedTimestamp = currentTime - (201 * 60), // 201 minutes ago (sufficient)
          children = scalus.prelude.List.empty
        )

        // Build chain to height 1050
        var forksTree = scalus.prelude.SortedMap.empty.insert(blockHash, blockNode)
        var prevHash = blockHash
        var prevChainwork = BigInt(1001000)
        for (h <- 1002 to 1050) {
            val hash = ByteString.fromArray(Array.fill(32)(((h % 256).toByte)))
            val node = BitcoinValidator.BlockNode(
              prevBlockHash = prevHash,
              height = h,
              chainwork = prevChainwork + 1000,
              addedTimestamp = currentTime - (200 * 60) + (h - 1002) * 60,
              children = scalus.prelude.List.empty
            )
            forksTree = forksTree.insert(hash, node)
            prevHash = hash
            prevChainwork = prevChainwork + 1000
        }

        val (promotedBlocks, updatedTree) = BitcoinValidator.promoteQualifiedBlocks(
          forksTree,
          confirmedTip,
          confirmedHeight = 1000,
          currentTime
        )

        assertEquals(promotedBlocks.length, BigInt(0), "Should not promote block with insufficient confirmations")
        assertEquals(updatedTree.toList.size, forksTree.toList.size, "Tree should remain unchanged")
    }

    test("promoteQualifiedBlocks - reject block with insufficient age") {
        val confirmedTip = hex"1000000000000000000000000000000000000000000000000000000000000000"
        val blockHash = hex"1001000000000000000000000000000000000000000000000000000000000000"
        val currentTime = BigInt(System.currentTimeMillis() / 1000)

        // Block at height 1001, added only 150 minutes ago (needs 200)
        // Current canonical tip at height 1101 (100 confirmations - sufficient)
        val blockNode = BitcoinValidator.BlockNode(
          prevBlockHash = confirmedTip,
          height = 1001,
          chainwork = BigInt(1001000),
          addedTimestamp = currentTime - (150 * 60), // 150 minutes ago (insufficient)
          children = scalus.prelude.List.empty
        )

        // Build chain to height 1101 (100+ confirmations)
        var forksTree = scalus.prelude.SortedMap.empty.insert(blockHash, blockNode)
        var prevHash = blockHash
        var prevChainwork = BigInt(1001000)
        for (h <- 1002 to 1101) {
            val hash = ByteString.fromArray(Array.fill(32)(((h % 256).toByte)))
            val node = BitcoinValidator.BlockNode(
              prevBlockHash = prevHash,
              height = h,
              chainwork = prevChainwork + 1000,
              addedTimestamp = currentTime - (150 * 60) + (h - 1002) * 60,
              children = scalus.prelude.List.empty
            )
            forksTree = forksTree.insert(hash, node)
            prevHash = hash
            prevChainwork = prevChainwork + 1000
        }

        val (promotedBlocks, updatedTree) = BitcoinValidator.promoteQualifiedBlocks(
          forksTree,
          confirmedTip,
          confirmedHeight = 1000,
          currentTime
        )

        assertEquals(promotedBlocks.length, BigInt(0), "Should not promote block with insufficient age")
        assertEquals(updatedTree.toList.size, forksTree.toList.size, "Tree should remain unchanged")
    }

    test("promoteQualifiedBlocks - partial promotion stops at first non-qualified block") {
        val confirmedTip = hex"1000000000000000000000000000000000000000000000000000000000000000"
        val currentTime = BigInt(System.currentTimeMillis() / 1000)

        // Block 1001: OLD (201 min ago) - should promote
        val block1001Hash = hex"1001000000000000000000000000000000000000000000000000000000000000"
        val block1001Node = BitcoinValidator.BlockNode(
          prevBlockHash = confirmedTip,
          height = 1001,
          chainwork = BigInt(1001000),
          addedTimestamp = currentTime - (201 * 60),
          children = scalus.prelude.List.empty
        )

        // Block 1002: OLD (201 min ago) - should promote
        val block1002Hash = hex"1002000000000000000000000000000000000000000000000000000000000000"
        val block1002Node = BitcoinValidator.BlockNode(
          prevBlockHash = block1001Hash,
          height = 1002,
          chainwork = BigInt(1002000),
          addedTimestamp = currentTime - (201 * 60),
          children = scalus.prelude.List.empty
        )

        // Block 1003: TOO RECENT (only 100 min ago) - should NOT promote
        val block1003Hash = hex"1003000000000000000000000000000000000000000000000000000000000000"
        val block1003Node = BitcoinValidator.BlockNode(
          prevBlockHash = block1002Hash,
          height = 1003,
          chainwork = BigInt(1003000),
          addedTimestamp = currentTime - (100 * 60), // Only 100 minutes (insufficient)
          children = scalus.prelude.List.empty
        )

        // Build rest of chain to 1103 (to satisfy 100 confirmation requirement for 1001-1003)
        // Need tip at 1103 to have 100+ confirmations for block 1003
        var forksTree = scalus.prelude.SortedMap.empty
            .insert(block1001Hash, block1001Node)
            .insert(block1002Hash, block1002Node)
            .insert(block1003Hash, block1003Node)

        var prevHash = block1003Hash
        var prevChainwork = BigInt(1003000)
        for (h <- 1004 to 1103) {
            val hash = ByteString.fromArray(Array.fill(32)(((h % 256).toByte)))
            val node = BitcoinValidator.BlockNode(
              prevBlockHash = prevHash,
              height = h,
              chainwork = prevChainwork + 1000,
              addedTimestamp = currentTime - (100 * 60) + (h - 1003) * 60,
              children = scalus.prelude.List.empty
            )
            forksTree = forksTree.insert(hash, node)
            prevHash = hash
            prevChainwork = prevChainwork + 1000
        }

        val (promotedBlocks, updatedTree) = BitcoinValidator.promoteQualifiedBlocks(
          forksTree,
          confirmedTip,
          confirmedHeight = 1000,
          currentTime
        )

        // Should promote blocks 1001 and 1002, but stop at 1003
        assertEquals(promotedBlocks.length, BigInt(2), "Should promote exactly 2 blocks")

        // Convert to Scala list for assertions
        val promotedList = scala.collection.mutable.ListBuffer[BitcoinValidator.BlockHash]()
        def collectList(list: scalus.prelude.List[BitcoinValidator.BlockHash]): Unit = list match {
            case scalus.prelude.List.Nil => ()
            case scalus.prelude.List.Cons(head, tail) =>
                promotedList += head
                collectList(tail)
        }
        collectList(promotedBlocks)

        assert(promotedList.contains(block1001Hash), "Should include block 1001")
        assert(promotedList.contains(block1002Hash), "Should include block 1002")
        assert(!promotedList.contains(block1003Hash), "Should NOT include block 1003")

        // Blocks 1001 and 1002 should be removed from tree
        assert(BitcoinValidator.lookupBlock(updatedTree, block1001Hash).isEmpty, "Block 1001 should be removed")
        assert(BitcoinValidator.lookupBlock(updatedTree, block1002Hash).isEmpty, "Block 1002 should be removed")
        assert(BitcoinValidator.lookupBlock(updatedTree, block1003Hash).isDefined, "Block 1003 should remain")
    }

    test("promoteQualifiedBlocks - edge case: exactly 100 confirmations and 200 minutes") {
        val confirmedTip = hex"1000000000000000000000000000000000000000000000000000000000000000"
        val blockHash = hex"1001000000000000000000000000000000000000000000000000000000000000"
        val currentTime = BigInt(System.currentTimeMillis() / 1000)

        // Block at height 1001, added EXACTLY 200 minutes ago
        // Current canonical tip at height 1101 (EXACTLY 100 confirmations)
        val blockNode = BitcoinValidator.BlockNode(
          prevBlockHash = confirmedTip,
          height = 1001,
          chainwork = BigInt(1001000),
          addedTimestamp = currentTime - (200 * 60), // Exactly 200 minutes
          children = scalus.prelude.List.empty
        )

        // Build chain to height 1101 (exactly 100 confirmations)
        var forksTree = scalus.prelude.SortedMap.empty.insert(blockHash, blockNode)
        var prevHash = blockHash
        var prevChainwork = BigInt(1001000)
        for (h <- 1002 to 1101) {
            val hash = ByteString.fromArray(Array.fill(32)(((h % 256).toByte)))
            val node = BitcoinValidator.BlockNode(
              prevBlockHash = prevHash,
              height = h,
              chainwork = prevChainwork + 1000,
              addedTimestamp = currentTime - (200 * 60) + (h - 1002) * 60,
              children = scalus.prelude.List.empty
            )
            forksTree = forksTree.insert(hash, node)
            prevHash = hash
            prevChainwork = prevChainwork + 1000
        }

        val (promotedBlocks, updatedTree) = BitcoinValidator.promoteQualifiedBlocks(
          forksTree,
          confirmedTip,
          confirmedHeight = 1000,
          currentTime
        )

        assertEquals(promotedBlocks.length, BigInt(1), "Should promote block at exact boundary")
        assertEquals(promotedBlocks.head, blockHash)
    }

    test("promoteQualifiedBlocks - edge case: 99 confirmations (just below threshold)") {
        val confirmedTip = hex"1000000000000000000000000000000000000000000000000000000000000000"
        val blockHash = hex"1001000000000000000000000000000000000000000000000000000000000000"
        val currentTime = BigInt(System.currentTimeMillis() / 1000)

        val blockNode = BitcoinValidator.BlockNode(
          prevBlockHash = confirmedTip,
          height = 1001,
          chainwork = BigInt(1001000),
          addedTimestamp = currentTime - (201 * 60), // Sufficient age
          children = scalus.prelude.List.empty
        )

        // Build chain to height 1100 (only 99 confirmations)
        var forksTree = scalus.prelude.SortedMap.empty.insert(blockHash, blockNode)
        var prevHash = blockHash
        var prevChainwork = BigInt(1001000)
        for (h <- 1002 to 1100) {
            val hash = ByteString.fromArray(Array.fill(32)(((h % 256).toByte)))
            val node = BitcoinValidator.BlockNode(
              prevBlockHash = prevHash,
              height = h,
              chainwork = prevChainwork + 1000,
              addedTimestamp = currentTime - (201 * 60) + (h - 1002) * 60,
              children = scalus.prelude.List.empty
            )
            forksTree = forksTree.insert(hash, node)
            prevHash = hash
            prevChainwork = prevChainwork + 1000
        }

        val (promotedBlocks, updatedTree) = BitcoinValidator.promoteQualifiedBlocks(
          forksTree,
          confirmedTip,
          confirmedHeight = 1000,
          currentTime
        )

        assertEquals(promotedBlocks.length, BigInt(0), "Should not promote with only 99 confirmations")
    }

    test("promoteQualifiedBlocks - edge case: 199 minutes (just below threshold)") {
        val confirmedTip = hex"1000000000000000000000000000000000000000000000000000000000000000"
        val blockHash = hex"1001000000000000000000000000000000000000000000000000000000000000"
        val currentTime = BigInt(System.currentTimeMillis() / 1000)

        val blockNode = BitcoinValidator.BlockNode(
          prevBlockHash = confirmedTip,
          height = 1001,
          chainwork = BigInt(1001000),
          addedTimestamp = currentTime - (199 * 60), // Just under 200 minutes
          children = scalus.prelude.List.empty
        )

        // Build chain to height 1101 (100 confirmations - sufficient)
        var forksTree = scalus.prelude.SortedMap.empty.insert(blockHash, blockNode)
        var prevHash = blockHash
        var prevChainwork = BigInt(1001000)
        for (h <- 1002 to 1101) {
            val hash = ByteString.fromArray(Array.fill(32)(((h % 256).toByte)))
            val node = BitcoinValidator.BlockNode(
              prevBlockHash = prevHash,
              height = h,
              chainwork = prevChainwork + 1000,
              addedTimestamp = currentTime - (199 * 60) + (h - 1002) * 60,
              children = scalus.prelude.List.empty
            )
            forksTree = forksTree.insert(hash, node)
            prevHash = hash
            prevChainwork = prevChainwork + 1000
        }

        val (promotedBlocks, updatedTree) = BitcoinValidator.promoteQualifiedBlocks(
          forksTree,
          confirmedTip,
          confirmedHeight = 1000,
          currentTime
        )

        assertEquals(promotedBlocks.length, BigInt(0), "Should not promote with only 199 minutes")
    }

    test("promoteQualifiedBlocks - multiple blocks all qualify for promotion") {
        val confirmedTip = hex"1000000000000000000000000000000000000000000000000000000000000000"
        val currentTime = BigInt(System.currentTimeMillis() / 1000)

        // Create 5 blocks, all old enough (201 min ago)
        val blockHashes = (1001 to 1005).map(h => ByteString.fromArray(Array.fill(32)(((h % 256).toByte))))

        var forksTree: scalus.prelude.SortedMap[BitcoinValidator.BlockHash, BitcoinValidator.BlockNode] =
            scalus.prelude.SortedMap.empty
        var prevHash = confirmedTip
        var prevChainwork = BigInt(1000000)

        // Add blocks 1001-1005
        for ((hash, height) <- blockHashes.zip(1001 to 1005)) {
            val node = BitcoinValidator.BlockNode(
              prevBlockHash = prevHash,
              height = height,
              chainwork = prevChainwork + 1000,
              addedTimestamp = currentTime - (201 * 60),
              children = scalus.prelude.List.empty
            )
            forksTree = forksTree.insert(hash, node)
            prevHash = hash
            prevChainwork = prevChainwork + 1000
        }

        // Add more blocks to reach 100 confirmations (to height 1105)
        for (h <- 1006 to 1105) {
            val hash = ByteString.fromArray(Array.fill(32)(((h % 256).toByte)))
            val node = BitcoinValidator.BlockNode(
              prevBlockHash = prevHash,
              height = h,
              chainwork = prevChainwork + 1000,
              addedTimestamp = currentTime - (201 * 60) + (h - 1006) * 60,
              children = scalus.prelude.List.empty
            )
            forksTree = forksTree.insert(hash, node)
            prevHash = hash
            prevChainwork = prevChainwork + 1000
        }

        val (promotedBlocks, updatedTree) = BitcoinValidator.promoteQualifiedBlocks(
          forksTree,
          confirmedTip,
          confirmedHeight = 1000,
          currentTime
        )

        assertEquals(promotedBlocks.length, BigInt(5), "Should promote all 5 qualified blocks")

        // Verify all promoted blocks are removed from tree
        for (hash <- blockHashes) {
            assert(BitcoinValidator.lookupBlock(updatedTree, hash).isEmpty, s"Block should be removed from tree")
        }
    }

}
