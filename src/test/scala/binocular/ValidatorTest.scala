package binocular
import org.bouncycastle.crypto.params.Ed25519PrivateKeyParameters
import org.bouncycastle.crypto.signers.Ed25519Signer
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.*
import scalus.cardano.ledger
import scalus.cardano.ledger.{AssetName, CardanoInfo, Coin, DatumOption, Output, ScriptHash, ScriptRef, TransactionHash, TransactionInput, Utxo, Utxos, Value}
import scalus.cardano.txbuilder.RedeemerPurpose.ForSpend
import scalus.cardano.txbuilder.txBuilder
import scalus.testing.kit.TestUtil.getScriptContextV3
import scalus.testing.kit.ScalusTest
import scalus.uplc.builtin.ByteString.hex
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.{ByteString, Data}
import scalus.cardano.onchain.plutus.prelude
import scalus.cardano.onchain.plutus.v3.ScriptContext
import scalus.uplc.eval
import scalus.uplc.eval.*
import upickle.default.*
import binocular.BitcoinHelpers.*

import java.nio.file.{Files, Path}
import java.time.Instant
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

case class CekResult(budget: ledger.ExUnits, logs: Seq[String])

class ValidatorTest extends AnyFunSuite with ScalusTest with ScalaCheckPropertyChecks {
    private given env: CardanoInfo = CardanoInfo.mainnet

    private val testContract =
        BitcoinContract.contract.withErrorTraces(BitcoinContract.testTxOutRef.toData)
    private val testScript = testContract.script
    private val testScriptHash = testScript.scriptHash
    private val testScriptAddr = testContract.address(env.network)

    test("reverse") {
        forAll { (a: Array[Byte]) =>
            val bs = ByteString.unsafeFromArray(a)
            assert(bs == bs.reverse.reverse)
        }
    }

    test("BitcoinValidator size") {
        assert(BitcoinContract.contract.script.script.size == 8686)
    }

    test("Tx size makes sense") {
        val blockHeader =
            hex"000000302b974c15e2ef994183f9806c5be9c61e74abc512a14301000000000000000000aff4af5b1dcc2b8754db824b9911818b65913dc262c295f060abb45c6c1d7ee749f90b67cd0e0317f9cc7dac"
        val coinbaseTx =
            hex"010000000001010000000000000000000000000000000000000000000000000000000000000000ffffffff2503233708184d696e656420627920416e74506f6f6c373946205b8160a4256c0000946e0100ffffffff02f595814a000000001976a914edf10a7fac6b32e24daa5305c723f3de58db1bc888ac0000000000000000266a24aa21a9edfaa194df59043645ba0f58aad74bfd5693fa497093174d12a4bb3b0574a878db0120000000000000000000000000000000000000000000000000000000000000000000000000"
        assert(blockHeader.size == 80)
        val coinbase = Bitcoin.makeCoinbaseTxFromByteString(coinbaseTx)
        val coinbaseSize = coinbase.toData.toCbor.length
        println(s"Coinbase size: $coinbaseSize")
    }

    test("ChallengeAging matches the 200-minute whitepaper parameter") {
        assert(BitcoinValidator.ChallengeAging == 200 * 60)
    }

    test("computeValidityIntervalTime can target a requested future time") {
        val requestedTimeSeconds = BigInt(1_800_000_000L)
        val slotConfig = CardanoInfo.mainnet.slotConfig
        val requestedTimeMs = requestedTimeSeconds.toLong * 1000
        val expectedSlot = slotConfig.timeToSlot(requestedTimeMs)
        val expectedSlotStartMs =
            slotConfig.zeroTime + (expectedSlot - slotConfig.zeroSlot) * slotConfig.slotLength

        val (validityInstant, validatorTime) = binocular.util.SlotConfigHelper
            .computeValidityIntervalTime(CardanoInfo.mainnet, Some(requestedTimeSeconds))

        assert(validityInstant.toEpochMilli == expectedSlotStartMs)
        assert(validatorTime == expectedSlotStartMs / 1000)
    }

    test("Evaluate") {
        val block = read[BitcoinBlock](
          Files.readString(
            Path.of("00000000000000000002cfdedd8358532b2284bc157e1352dbc8682b2067fb0c.json")
          )
        )

        val coinbaseTx =
            hex"010000000001010000000000000000000000000000000000000000000000000000000000000000ffffffff4f03d6340d082f5669614254432f2cfabe6d6d7853581d1f2abd53fe30833a1e6b8397b200ec3b658fdf6568edc5f54118f99d10000000000000001003b7b5047d947d88f58877bd2cb73c0000000000ffffffff0342db4a15000000001976a914fb37342f6275b13936799def06f2eb4c0f20151588ac00000000000000002b6a2952534b424c4f434b3aec94a488ba1f588e9cb6bb507c70283e9f10057ef683d2d460d3700900678b710000000000000000266a24aa21a9edb9df509533c6a9526f5180b0c34cc48b86a40cc94abfae661242a596393ef47d0120000000000000000000000000000000000000000000000000000000000000000000000000"
        val coinbase = Bitcoin.makeCoinbaseTxFromByteString(coinbaseTx)
        val coinbaseHash = getCoinbaseTxHash(coinbase)

        val txHashes = block.tx.map(h => ByteString.fromHex(h).reverse)
        val merkleTree = MerkleTree.fromHashes(txHashes)
        val merkleRoot = merkleTree.getMerkleRoot
        val merkleProof = merkleTree.makeMerkleProof(0)

        val coinbaseTxInclusionProof = prelude.List.from(merkleProof).toData

        val blockHeader = BlockHeader(
          bytes =
              hex"000000302b974c15e2ef994183f9806c5be9c61e74abc512a14301000000000000000000aff4af5b1dcc2b8754db824b9911818b65913dc262c295f060abb45c6c1d7ee749f90b67cd0e0317f9cc7dac"
        )
        val hash = blockHeaderHash(blockHeader)
        val bits = hex"17030ecd".reverse
        val target = compactBitsToTarget(bits)
        val timestamp = blockHeader.timestamp

        val prevState = ChainState(
          865493,
          blockHash = hex"0000000000000000000143a112c5ab741ec6e95b6c80f9834199efe2154c972b".reverse,
          currentTarget = bits,
          blockTimestamp = timestamp - 600,
          recentTimestamps = prelude.List(timestamp - 600),
          previousDifficultyAdjustmentTimestamp = timestamp - 600 * DifficultyAdjustmentInterval,
          confirmedBlocksRoot = BitcoinChainState.mpfRootForSingleBlock(
            hex"0000000000000000000143a112c5ab741ec6e95b6c80f9834199efe2154c972b".reverse
          ),
          forksTree = prelude.List.Nil
        )

        // Calculate chainwork for the new block (matching BitcoinValidator logic)
        // When parent is confirmed tip, parent chainwork = calculateBlockProof(target of parent)
        val parentTarget = compactBitsToTarget(prevState.currentTarget)
        val parentChainwork = calculateBlockProof(parentTarget)
        val blockWork = calculateBlockProof(target)
        val newBlockChainwork = parentChainwork + blockWork
        val newBlockHeight = prevState.blockHeight + 1

        // Expected state: block added to forks tree, but NOT promoted (doesn't meet 100 confirmations + 200 min criteria)
        val newBlockSummary = BlockSummary(
          hash = hash,
          height = newBlockHeight,
          chainwork = newBlockChainwork,
          timestamp = timestamp,
          bits = bits,
          addedTime = timestamp
        )
        val newBranch = ForkBranch(
          tipHash = hash,
          tipHeight = newBlockHeight,
          tipChainwork = newBlockChainwork,
          recentBlocks = prelude.List(newBlockSummary)
        )
        val newForksTree = prelude.List(newBranch)

        val newState = ChainState(
          prevState.blockHeight, // Height unchanged - no promotion
          blockHash = prevState.blockHash, // Hash unchanged - no promotion
          currentTarget = prevState.currentTarget, // Target unchanged - no promotion
          blockTimestamp = prevState.blockTimestamp, // Timestamp unchanged - no promotion
          recentTimestamps = prevState.recentTimestamps, // Timestamps unchanged - no promotion
          previousDifficultyAdjustmentTimestamp =
              prevState.previousDifficultyAdjustmentTimestamp, // Unchanged - no promotion
          confirmedBlocksRoot = prevState.confirmedBlocksRoot, // Tree unchanged - no promotion
          forksTree = newForksTree // Block added to forks tree
        )
        println(s"Block prevHash: ${blockHeader.prevBlockHash.toHex}")
        println(s"Expected forksTree size: ${newForksTree.size}")
        println(s"Expected newState.forksTree size: ${newState.forksTree.size}")

        val redeemer = Action
            .UpdateOracle(prelude.List(blockHeader), timestamp, prelude.List.Nil)
            .toData

        println(s"Redeemer size: ${redeemer.toCbor.length}")

        val scriptContext = makeScriptContext(
          timestamp.toLong,
          prevState.toData,
          newState.toData,
          redeemer
        )
        val applied = BitcoinContract.bitcoinProgram $ scriptContext.toData
        println(s"Validator size: ${BitcoinContract.bitcoinProgram.flatEncoded.length}")

        try {
            BitcoinValidator.validate(BitcoinContract.testTxOutRef.toData)(scriptContext.toData)
        } catch {
            case e: Exception =>
                println(s"Validation error: ${e.getMessage}")
                throw e
        }
        applied.evaluateDebug match
            case r: Result.Success => println(r)
            case r: Result.Failure => fail(r.toString)
    }

    def signMessage(claim: ByteString, privateKey: Ed25519PrivateKeyParameters): ByteString =
        val signer = new Ed25519Signer()
        signer.init(true, privateKey)
        signer.update(claim.bytes, 0, claim.size)
        ByteString.fromArray(signer.generateSignature())

    /** Build a ScriptContext for validator tests using Scalus TxBuilder.
      *
      * @param intervalStartSeconds
      *   validity interval start in Unix seconds
      * @param prevState
      *   input datum (previous chain state)
      * @param newState
      *   output datum (new chain state)
      * @param redeemer
      *   the redeemer Data
      * @param inputValue
      *   value locked at the script input (default: 5 ADA)
      * @param outputValue
      *   value sent to the script output (default: 5 ADA)
      */
    def makeScriptContext(
        intervalStartSeconds: Long,
        prevState: Data,
        newState: Data,
        redeemer: Data,
        inputValue: Value = Value.ada(5),
        outputValue: Value = Value.ada(5)
    ): ScriptContext =
        val input = TransactionInput(
          TransactionHash.fromHex(
            "1ab6879fc08345f51dc9571ac4f530bf8673e0d798758c470f9af6f98e2f3982"
          ),
          0
        )
        val utxo = Utxo(
          input,
          Output(testScriptAddr, inputValue, DatumOption.Inline(prevState))
        )
        val utxos: Utxos = Map(utxo.input -> utxo.output)
        val validFrom = Instant.ofEpochSecond(intervalStartSeconds)

        val draft = txBuilder
            .spend(utxo, redeemer, testContract)
            .payTo(testScriptAddr, outputValue, newState)
            .validFrom(validFrom)
            .draft

        draft.getScriptContextV3(utxos, ForSpend(input))

    // ===== TEST INFRASTRUCTURE HELPERS =====

    /** Generate a sequence of valid timestamps for testing */
    def makeTimestampSequence(
        baseTime: BigInt,
        count: Int,
        spacing: BigInt = 600
    ): prelude.List[BigInt] = {
        def buildSeq(remaining: Int, currentTime: BigInt, acc: List[BigInt]): List[BigInt] = {
            if remaining == 0 then acc.reverse
            else buildSeq(remaining - 1, currentTime - spacing, currentTime :: acc)
        }
        prelude.List.from(buildSeq(count, baseTime, Nil))
    }

    /** Calculate accumulated chainwork for a block */
    def computeChainwork(parentChainwork: BigInt, target: BigInt): BigInt = {
        // Chainwork formula: parent_chainwork + 2^256 / (target + 1)
        // Use calculateBlockProof to ensure consistency
        val work = calculateBlockProof(target)
        parentChainwork + work
    }

    /** Build a simple forks tree with linear chain of blocks */
    def buildSimpleForksTree(
        confirmedTip: BlockHash,
        confirmedHeight: BigInt,
        confirmedChainwork: BigInt,
        blockCount: Int,
        baseTimestamp: BigInt,
        bits: CompactBits
    ): prelude.List[ForkBranch] = {
        if blockCount == 0 then return prelude.List.Nil

        val target = compactBitsToTarget(bits)

        // Build chain of blocks
        def buildBlocks(
            remaining: Int,
            height: BigInt,
            chainwork: BigInt,
            timestamp: BigInt,
            blocks: List[BlockSummary]
        ): (List[BlockSummary], BigInt, BigInt) = {
            if remaining == 0 then (blocks.reverse, chainwork, height - 1)
            else
                val blockHash = ByteString.fromArray(Array.fill(32)((height % 256).toByte))
                val newChainwork = computeChainwork(chainwork, target)
                val block = BlockSummary(
                  hash = blockHash,
                  height = height,
                  chainwork = newChainwork,
                  timestamp = timestamp,
                  bits = bits,
                  addedTime = timestamp
                )
                buildBlocks(
                  remaining - 1,
                  height + 1,
                  newChainwork,
                  timestamp + 600,
                  block :: blocks
                )
        }

        val (blocks, finalChainwork, finalHeight) = buildBlocks(
          blockCount,
          confirmedHeight + 1,
          confirmedChainwork,
          baseTimestamp,
          Nil
        )

        // Create a single ForkBranch with all blocks (keep only last 11)
        val recentBlocks = prelude.List.from(blocks.takeRight(11))
        val tipBlock = blocks.last
        val branch = ForkBranch(
          tipHash = tipBlock.hash,
          tipHeight = tipBlock.height,
          tipChainwork = tipBlock.chainwork,
          recentBlocks = recentBlocks
        )

        prelude.List(branch)
    }

    /** Build a test ChainState with populated forks tree */
    def buildTestChainState(
        blockHeight: BigInt,
        blockHash: BlockHash,
        bits: CompactBits,
        baseTimestamp: BigInt,
        forksTreeSize: Int = 0
    ): ChainState = {
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
            else prelude.List.Nil

        ChainState(
          blockHeight = blockHeight,
          blockHash = blockHash,
          currentTarget = bits,
          blockTimestamp = baseTimestamp,
          recentTimestamps = prelude.List(baseTimestamp),
          previousDifficultyAdjustmentTimestamp = baseTimestamp - 600 * 2016,
          confirmedBlocksRoot = BitcoinChainState.mpfRootForSingleBlock(blockHash),
          forksTree = forksTree
        )
    }

    /** Build a single ForkBranch with a chain of blocks (for testing promotion) */
    def buildSingleBranch(
        startHeight: Int,
        endHeight: Int,
        baseChainwork: BigInt,
        baseTimestamp: BigInt,
        bits: CompactBits = hex"1d00ffff".reverse
    ): ForkBranch = {
        val blocks = scala.collection.mutable.ListBuffer[BlockSummary]()
        var chainwork = baseChainwork

        for h <- startHeight to endHeight do {
            val hash = ByteString.fromArray(Array.fill(32)((h % 256).toByte))
            val timestamp = baseTimestamp + (h - startHeight) * 60
            val addedTime =
                baseTimestamp + (h - startHeight) * 60 // Each block added 1 minute apart
            chainwork = chainwork + 1000
            blocks += BlockSummary(
              hash = hash,
              height = h,
              chainwork = chainwork,
              timestamp = timestamp,
              bits = bits,
              addedTime = addedTime
            )
        }

        val recentBlocks = prelude.List.from(blocks.reverse) // All blocks, newest first
        val tipBlock = blocks.last

        ForkBranch(
          tipHash = tipBlock.hash,
          tipHeight = tipBlock.height,
          tipChainwork = tipBlock.chainwork,
          recentBlocks = recentBlocks
        )
    }

    test("validateForkSubmission - accept pure canonical extension") {
        val confirmedTip =
            hex"0000000000000000000143a112c5ab741ec6e95b6c80f9834199efe2154c972b".reverse

        // Block extending confirmed tip
        val blockExtendingTip = BlockHeader(
          hex"000000302b974c15e2ef994183f9806c5be9c61e74abc512a14301000000000000000000aff4af5b1dcc2b8754db824b9911818b65913dc262c295f060abb45c6c1d7ee749f90b67cd0e0317f9cc7dac"
        )

        val forksTree: prelude.List[ForkBranch] = prelude.List.Nil
        val headers = prelude.List(blockExtendingTip)

        // Should NOT throw - pure canonical extension is valid
        BitcoinValidator.validateForkSubmission(headers, forksTree, confirmedTip)
    }

    test("validateForkSubmission - reject pure fork submission") {
        val confirmedTip =
            hex"0000000000000000000143a112c5ab741ec6e95b6c80f9834199efe2154c972b".reverse

        // Block NOT extending confirmed tip (fork)
        val forkBlock = BlockHeader(
          hex"00000020ffffffffffffffffffffffffffffffffffffffffffffffffffffffff00000000aff4af5b1dcc2b8754db824b9911818b65913dc262c295f060abb45c6c1d7ee749f90b67cd0e0317f9cc7dac"
        )

        val forksTree: prelude.List[ForkBranch] = prelude.List.Nil
        val headers = prelude.List(forkBlock)

        // Should throw - fork without canonical extension
        intercept[RuntimeException] {
            BitcoinValidator.validateForkSubmission(headers, forksTree, confirmedTip)
        }
    }

    test("validateForkSubmission - accept canonical + fork") {
        val confirmedTip =
            hex"0000000000000000000143a112c5ab741ec6e95b6c80f9834199efe2154c972b".reverse

        // Block extending confirmed tip (canonical)
        val canonicalBlock = BlockHeader(
          hex"000000302b974c15e2ef994183f9806c5be9c61e74abc512a14301000000000000000000aff4af5b1dcc2b8754db824b9911818b65913dc262c295f060abb45c6c1d7ee749f90b67cd0e0317f9cc7dac"
        )

        // Block NOT extending confirmed tip (fork)
        val forkBlock = BlockHeader(
          hex"00000020ffffffffffffffffffffffffffffffffffffffffffffffffffffffff00000000aff4af5b1dcc2b8754db824b9911818b65913dc262c295f060abb45c6c1d7ee749f90b67cd0e0317f9cc7dac"
        )

        val forksTree: prelude.List[ForkBranch] = prelude.List.Nil
        val headers = prelude.List.from(Seq(canonicalBlock, forkBlock))

        // Should NOT throw - canonical + fork is valid
        BitcoinValidator.validateForkSubmission(headers, forksTree, confirmedTip)
    }

    test("addBlockToForksTree - block extending confirmed tip") {
        val confirmedTip =
            hex"0000000000000000000143a112c5ab741ec6e95b6c80f9834199efe2154c972b".reverse
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
          recentTimestamps = prelude.List(blockTimestamp - 600),
          previousDifficultyAdjustmentTimestamp = blockTimestamp - 600 * 2016,
          confirmedBlocksRoot = BitcoinChainState.mpfRootForSingleBlock(confirmedTip),
          forksTree = prelude.List.Nil
        )

        // Add block to empty forksTree - should succeed
        val updatedForksTree = BitcoinValidator.addBlockToForksTree(
          confirmedState.forksTree,
          newBlockHeader,
          confirmedState,
          currentTime
        )

        val newBlockHash = blockHeaderHash(newBlockHeader)

        // Verify block was added - should find branch with this block as tip
        val branchOpt = BitcoinValidator.findBranch(updatedForksTree, newBlockHash)
        assert(branchOpt.isDefined, "Block should be tip of a branch in forks tree")

        val branch = branchOpt.getOrFail("Branch not found")
        // Verify it's the tip of the branch
        assert(branch.tipHash == newBlockHash)
        assert(branch.tipHeight == BigInt(1001))
    }

    test("forksTree structure - block extending another block in tree") {
        // Test the fork tree structure logic by manually creating a branch with 2 blocks
        val confirmedTip = hex"1000000000000000000000000000000000000000000000000000000000000000"
        val firstBlockHash = hex"1001000000000000000000000000000000000000000000000000000000000000"
        val secondBlockHash = hex"1002000000000000000000000000000000000000000000000000000000000000"

        // Create branch with two blocks
        val firstBlock = BlockSummary(
          hash = firstBlockHash,
          height = 1001,
          chainwork = 1000000,
          timestamp = 1234567890,
          bits = hex"1d00ffff".reverse,
          addedTime = 1234567890
        )

        val secondBlock = BlockSummary(
          hash = secondBlockHash,
          height = 1002,
          chainwork = 2000000,
          timestamp = 1234567900,
          bits = hex"1d00ffff".reverse,
          addedTime = 1234567900
        )

        // Create branch with both blocks (second is tip)
        val branch = ForkBranch(
          tipHash = secondBlockHash,
          tipHeight = 1002,
          tipChainwork = BigInt(2000000),
          recentBlocks = prelude.List.from(Seq(secondBlock, firstBlock)) // newest first
        )

        val forksTree = prelude.List(branch)

        // findBranch only finds by tip hash now (since parents are always tips of their branches)
        // First block is NOT the tip, so findBranch won't find it directly
        assert(
          BitcoinValidator.findBranch(forksTree, firstBlockHash).isEmpty,
          "First block is not a tip, so findBranch returns None"
        )
        assert(
          BitcoinValidator.findBranch(forksTree, secondBlockHash).isDefined,
          "Second block is the tip"
        )

        // Verify second block is at tip
        val foundBranch =
            BitcoinValidator.findBranch(forksTree, secondBlockHash).getOrFail("Branch not found")
        assert(foundBranch.tipHash == secondBlockHash)
        assert(foundBranch.tipHeight == BigInt(1002))

        // First block can be found in the branch's recentBlocks
        assert(
          BitcoinValidator.existsHash(foundBranch.recentBlocks, firstBlockHash),
          "First block should be in branch's recentBlocks"
        )

        // Verify canonical chain selection picks highest chainwork (the branch)
        val canonicalBranch = BitcoinValidator.selectCanonicalChain(forksTree)
        assert(canonicalBranch.map(_.tipHash) == prelude.Option.Some(secondBlockHash))
    }

    test("lookupParentBlock - finds internal block within a branch") {
        val firstBlockHash = hex"1001000000000000000000000000000000000000000000000000000000000000"
        val secondBlockHash = hex"1002000000000000000000000000000000000000000000000000000000000000"

        val firstBlock = BlockSummary(
          hash = firstBlockHash,
          height = 1001,
          chainwork = BigInt(1000000),
          timestamp = BigInt(1234567890),
          bits = hex"1d00ffff".reverse,
          addedTime = BigInt(1234567890)
        )

        val secondBlock = BlockSummary(
          hash = secondBlockHash,
          height = 1002,
          chainwork = BigInt(2000000),
          timestamp = BigInt(1234567900),
          bits = hex"1d00ffff".reverse,
          addedTime = BigInt(1234567900)
        )

        val branch = ForkBranch(
          tipHash = secondBlockHash,
          tipHeight = 1002,
          tipChainwork = BigInt(2000000),
          recentBlocks = prelude.List.from(Seq(secondBlock, firstBlock))
        )

        val forksTree = prelude.List(branch)

        val parentLookup = BitcoinValidator.lookupParentBlock(forksTree, firstBlockHash)
        assert(parentLookup.isDefined, "Internal block should be discoverable for fork creation")

        val (parentBranch, parentBlock, parentIsTip) =
            parentLookup.getOrFail("Parent block should be found")

        assert(parentBranch.tipHash == secondBlockHash)
        assert(parentBlock.hash == firstBlockHash)
        assert(parentBlock.height == 1001)
        assert(!parentIsTip, "Internal block should not be reported as a tip")
    }

    test("expectedNextBitsForParent - non-boundary descendant reuses parent bits") {
        val bits = hex"1d00ffff".reverse
        val confirmedState = buildTestChainState(
          blockHeight = 1000,
          blockHash = ByteString.fromHex("aa" * 32),
          bits = bits,
          baseTimestamp = 1234567890,
          forksTreeSize = 0
        )

        val parentBranch = buildSingleBranch(
          startHeight = 1001,
          endHeight = 1005,
          baseChainwork = 1000000,
          baseTimestamp = 1234568490,
          bits = bits
        )

        val parentBlock = parentBranch.recentBlocks.head

        val expectedBits =
            BitcoinValidator.expectedNextBitsForParent(confirmedState, parentBranch, parentBlock)

        assert(expectedBits == bits)
    }

    test("expectedNextBitsForParent - retarget boundary uses branch history timestamp") {
        val bits = hex"1d00ffff".reverse
        val confirmedState = buildTestChainState(
          blockHeight = 2015,
          blockHash = ByteString.fromHex("bb" * 32),
          bits = bits,
          baseTimestamp = 1_000_000,
          forksTreeSize = 0
        )

        val parentBranch = buildSingleBranch(
          startHeight = 2016,
          endHeight = 4031,
          baseChainwork = 1000000,
          baseTimestamp = 1_000_600,
          bits = bits
        )

        val parentBlock = parentBranch.recentBlocks.head
        val adjustmentStartBlock = parentBranch.recentBlocks.find(_.height == 2016).get

        val expectedBits =
            BitcoinValidator.expectedNextBitsForParent(confirmedState, parentBranch, parentBlock)

        assert(
          expectedBits == getNextWorkRequired(
            parentBlock.height,
            parentBlock.bits,
            parentBlock.timestamp,
            adjustmentStartBlock.timestamp
          )
        )
    }

    test("applyPromotionsToConfirmedState - updates timestamps and difficulty metadata") {
        val initialBits = hex"1d00ffff".reverse
        val promotedBits = hex"1c654657".reverse
        val nextBits = hex"1c654321".reverse

        val confirmedState = ChainState(
          blockHeight = 2015,
          blockHash = ByteString.fromHex("11" * 32),
          currentTarget = initialBits,
          blockTimestamp = 110,
          recentTimestamps = prelude.List.from(Seq[BigInt](110, 100, 90)),
          previousDifficultyAdjustmentTimestamp = 50,
          confirmedBlocksRoot =
              BitcoinChainState.mpfRootForSingleBlock(ByteString.fromHex("11" * 32)),
          forksTree = prelude.List.Nil
        )

        val promoted2016 = BlockSummary(
          hash = ByteString.fromHex("22" * 32),
          height = 2016,
          chainwork = 2000,
          timestamp = 120,
          bits = promotedBits,
          addedTime = 120
        )

        val promoted2017 = BlockSummary(
          hash = ByteString.fromHex("33" * 32),
          height = 2017,
          chainwork = 3000,
          timestamp = 130,
          bits = nextBits,
          addedTime = 130
        )

        val updatedState = BitcoinValidator.applyPromotionsToConfirmedState(
          confirmedState,
          prelude.List.from(Seq(promoted2016, promoted2017))
        )

        assert(updatedState.blockHeight == 2017)
        assert(updatedState.blockHash == promoted2017.hash)
        assert(updatedState.currentTarget == nextBits)
        assert(updatedState.blockTimestamp == 130)
        assert(
          updatedState.recentTimestamps == prelude.List.from(Seq[BigInt](130, 120, 110, 100, 90))
        )
        assert(updatedState.previousDifficultyAdjustmentTimestamp == 120)
    }

    test("selectCanonicalChain - empty forksTree returns None") {
        val emptyForksTree: prelude.List[ForkBranch] = prelude.List.Nil
        val result = BitcoinValidator.selectCanonicalChain(emptyForksTree)

        assert(result.isEmpty)
    }

    test("selectCanonicalChain - single branch returns that branch") {
        val blockHash =
            hex"00000000000000000002cfdedd8358532b2284bc157e1352dbc8682b2067fb0c".reverse

        val blockSummary = BlockSummary(
          hash = blockHash,
          height = 1001,
          chainwork = BigInt(1000000),
          timestamp = BigInt(1234567890),
          bits = hex"1d00ffff".reverse,
          addedTime = BigInt(1234567890)
        )

        val branch = ForkBranch(
          tipHash = blockHash,
          tipHeight = 1001,
          tipChainwork = BigInt(1000000),
          recentBlocks = prelude.List(blockSummary)
        )

        val forksTree = prelude.List(branch)
        val result = BitcoinValidator.selectCanonicalChain(forksTree)

        assert(result.map(_.tipHash) == prelude.Option.Some(blockHash))
    }

    test("selectCanonicalChain - selects highest chainwork") {
        val lowChainworkHash = hex"1111111111111111111111111111111111111111111111111111111111111111"
        val lowChainworkBranch = ForkBranch(
          tipHash = lowChainworkHash,
          tipHeight = 1001,
          tipChainwork = BigInt(1000000),
          recentBlocks = prelude.List(
            BlockSummary(
              hash = lowChainworkHash,
              height = 1001,
              chainwork = BigInt(1000000),
              timestamp = BigInt(1234567890),
              bits = hex"1d00ffff".reverse,
              addedTime = BigInt(1234567890)
            )
          )
        )

        val highChainworkHash =
            hex"2222222222222222222222222222222222222222222222222222222222222222"
        val highChainworkBranch = ForkBranch(
          tipHash = highChainworkHash,
          tipHeight = 1001,
          tipChainwork = BigInt(2000000),
          recentBlocks = prelude.List(
            BlockSummary(
              hash = highChainworkHash,
              height = 1001,
              chainwork = BigInt(2000000),
              timestamp = BigInt(1234567890),
              bits = hex"1d00ffff".reverse,
              addedTime = BigInt(1234567890)
            )
          )
        )

        val forksTree = prelude.List.from(Seq(lowChainworkBranch, highChainworkBranch))

        val result = BitcoinValidator.selectCanonicalChain(forksTree)

        // Should select branch with highest chainwork
        assert(result.map(_.tipHash) == prelude.Option.Some(highChainworkHash))
    }

    test("forksTree after promotion - disconnected forks are valid") {
        // Scenario: After promoting blocks 1001-1003, old fork 1001' remains
        // This fork extends old confirmed tip 1000, not new tip 1003
        // This is a VALID state - garbage collection will clean it up

        // Old fork branch with single block extending block 1000 (no longer confirmed tip)
        val oldForkHash = hex"1001111111111111111111111111111111111111111111111111111111111111"
        val oldForkBranch = ForkBranch(
          tipHash = oldForkHash,
          tipHeight = 1001,
          tipChainwork = BigInt(500000),
          recentBlocks = prelude.List(
            BlockSummary(
              hash = oldForkHash,
              height = 1001,
              chainwork = BigInt(500000),
              timestamp = BigInt(1234567890),
              bits = hex"1d00ffff".reverse,
              addedTime = BigInt(1234567890)
            )
          )
        )

        val forksTree = prelude.List(oldForkBranch)

        // This state is valid even though oldFork doesn't extend newConfirmedTip
        // The fork is "disconnected" but will be removed by garbage collection
        assert(forksTree.size == 1)

        val branch = forksTree.head
        // Verify branch tip
        assert(branch.tipHash == oldForkHash)
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
        val block864801PrevHash = block864800Hash
        val block864801Bits = "17032f14"
        val block864801Timestamp = 1728414644L
        val block864801Nonce = 4254568467L
        val block864801Version = 666378240L
        val block864801Merkleroot =
            "dc2ad1c7a27d394d45961c13a9c36e1d5e6cdbea928c47cb06263bb049a9cd0f"

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
        val computedHash = blockHeaderHash(blockHeader)
        assert(computedHash.reverse.toHex == block864801Hash, "Block header hash mismatch")

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
        val newBlockHash = blockHeaderHash(blockHeader)
        val target = compactBitsToTarget(bits)

        // Parent chainwork: when parent is confirmed tip, calculate proof of parent's target
        val parentTarget = compactBitsToTarget(prevState.currentTarget)
        val parentChainwork = calculateBlockProof(parentTarget)
        val blockWork = calculateBlockProof(target)
        val newChainwork = parentChainwork + blockWork

        val expectedBlockSummary = BlockSummary(
          hash = newBlockHash,
          height = prevState.blockHeight + 1,
          chainwork = newChainwork,
          timestamp = baseTime,
          bits = bits,
          addedTime = baseTime
        )

        val expectedBranch = ForkBranch(
          tipHash = newBlockHash,
          tipHeight = prevState.blockHeight + 1,
          tipChainwork = newChainwork,
          recentBlocks = prelude.List(expectedBlockSummary)
        )

        val expectedForksTree = prelude.List(expectedBranch)

        val expectedState = ChainState(
          blockHeight = prevState.blockHeight,
          blockHash = prevState.blockHash,
          currentTarget = prevState.currentTarget,
          blockTimestamp = prevState.blockTimestamp,
          recentTimestamps = prevState.recentTimestamps,
          previousDifficultyAdjustmentTimestamp = prevState.previousDifficultyAdjustmentTimestamp,
          confirmedBlocksRoot = prevState.confirmedBlocksRoot,
          forksTree = expectedForksTree
        )

        val redeemer = Action
            .UpdateOracle(prelude.List(blockHeader), baseTime, prelude.List.Nil)
            .toData

        // Create script context and evaluate
        val scriptContext = makeScriptContext(
          baseTime.toLong,
          prevState.toData,
          expectedState.toData,
          redeemer
        )

        // Validate - should succeed
        BitcoinValidator.validate(BitcoinContract.testTxOutRef.toData)(scriptContext.toData)

        val result = BitcoinContract.bitcoinProgram $ scriptContext.toData
        val prices = CardanoInfo.mainnet.protocolParams.executionUnitPrices
        result.evaluateDebug match
            case r: Result.Success =>
                println(
                  s"UpdateOracle single block validation succeeded, budget used: ${r.budget.showJson}"
                )
                println(r)
                // Budget changed after migrating confirmedBlocksTree to MPF root
                println(s"Budget: ${r.budget}")
                println(s"Fee: ${r.budget.fee(prices)}")
            case r: Result.Failure =>
                fail(s"Validation failed: $r")
    }

    test("OracleTransactions.applyHeaders matches computeUpdateOracleState semantics") {
        val block864800Hash = "0000000000000000000202c4f7f242ab864a6162b8d1e745de6a86ae979e130b"
        val block864800Height = 864800
        val block864800Timestamp = 1728414569L
        val block864801Bits = "17032f14"
        val block864801Timestamp = 1728414644L
        val block864801Nonce = 4254568467L
        val block864801Version = 666378240L
        val block864801Merkleroot =
            "dc2ad1c7a27d394d45961c13a9c36e1d5e6cdbea928c47cb06263bb049a9cd0f"

        val confirmedTip = ByteString.fromHex(block864800Hash).reverse
        val bits = ByteString.fromHex(block864801Bits).reverse
        val currentTime = BigInt(block864801Timestamp)

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
              confirmedTip ++
              ByteString.fromHex(block864801Merkleroot).reverse ++
              longToLE4Bytes(block864801Timestamp) ++
              ByteString.fromHex(block864801Bits).reverse ++
              longToLE4Bytes(block864801Nonce)
        )

        val prevState = buildTestChainState(
          blockHeight = block864800Height,
          blockHash = confirmedTip,
          bits = bits,
          baseTimestamp = BigInt(block864800Timestamp),
          forksTreeSize = 0
        )

        val headers = prelude.List(blockHeader)

        val expectedState =
            BitcoinValidator.computeUpdateOracleState(
              prevState,
              headers,
              currentTime,
              prelude.List.Nil
            )
        val actualState = OracleTransactions.applyHeaders(prevState, headers, currentTime)

        assert(actualState == expectedState)
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

        val redeemer = Action
            .UpdateOracle(
              prelude.List.empty,
              BigInt(System.currentTimeMillis() / 1000),
              prelude.List.Nil
            )
            .toData

        val scriptContext = makeScriptContext(
          baseTime.toLong * 1000,
          prevState.toData,
          prevState.toData, // Expected state same as previous (no change)
          redeemer
        )

        // Should fail with "Empty block headers list"
        intercept[RuntimeException] {
            BitcoinValidator.validate(BitcoinContract.testTxOutRef.toData)(scriptContext.toData)
        }
    }

    // ===== BLOCK PROMOTION TESTS =====

    test("promoteQualifiedBlocks - empty forks tree returns empty list") {
        val confirmedTip = hex"1000000000000000000000000000000000000000000000000000000000000000"
        val emptyForksTree: prelude.List[ForkBranch] = prelude.List.Nil
        val currentTime = BigInt(System.currentTimeMillis() / 1000)

        val (promotedBlocks, updatedTree) = BitcoinValidator.promoteQualifiedBlocks(
          emptyForksTree,
          confirmedTip,
          confirmedHeight = 1000,
          currentTime
        )

        assert(promotedBlocks.isEmpty, "Should not promote any blocks from empty tree")
        assert(updatedTree.size == BigInt(0), "Tree should remain empty")
    }

    test("promoteQualifiedBlocks - block meets both criteria (100+ conf, 200+ min)") {
        val confirmedTip = hex"1000000000000000000000000000000000000000000000000000000000000000"
        val block1001Hash = ByteString.fromArray(Array.fill(32)((1001 % 256).toByte))
        val currentTime = BigInt(System.currentTimeMillis() / 1000)

        // Build complete chain from 1001 to 1101 (101 blocks, so block 1001 has 100 confirmations)
        // All blocks added 201 minutes ago (meet both criteria)
        val branch = buildSingleBranch(
          startHeight = 1001,
          endHeight = 1101,
          baseChainwork = BigInt(1000000),
          baseTimestamp = currentTime - (201 * 60)
        )
        val forksTree = prelude.List(branch)

        val (promotedBlocks, updatedTree) = BitcoinValidator.promoteQualifiedBlocks(
          forksTree,
          confirmedTip,
          confirmedHeight = 1000,
          currentTime
        )

        assert(promotedBlocks.length == BigInt(1), "Should promote 1 block")
        assert(promotedBlocks.head.hash == block1001Hash, "Should promote the qualified block")

        // Block 1001 should be removed from recentBlocks
        val updatedBranchOpt = updatedTree.headOption
        assert(updatedBranchOpt.isDefined, "Branch should still exist after promotion")
        val updatedBranch = updatedBranchOpt.get
        assert(
          !updatedBranch.recentBlocks.exists(_.hash == block1001Hash),
          "Block 1001 should be removed from recentBlocks"
        )
    }

    test("promoteQualifiedBlocks - reject block with insufficient confirmations") {
        val confirmedTip = hex"1000000000000000000000000000000000000000000000000000000000000000"
        val currentTime = BigInt(System.currentTimeMillis() / 1000)

        // Build chain from 1001 to 1050 (only 49 confirmations, needs 100)
        // All blocks added 201 minutes ago (sufficient age)
        val branch = buildSingleBranch(
          startHeight = 1001,
          endHeight = 1050,
          baseChainwork = BigInt(1000000),
          baseTimestamp = currentTime - (201 * 60)
        )
        val forksTree = prelude.List(branch)

        val (promotedBlocks, updatedTree) = BitcoinValidator.promoteQualifiedBlocks(
          forksTree,
          confirmedTip,
          confirmedHeight = 1000,
          currentTime
        )

        assert(
          promotedBlocks.length ==
              BigInt(0),
          "Should not promote block with insufficient confirmations"
        )
        assert(updatedTree.size == forksTree.size, "Tree should remain unchanged")
    }

    test("promoteQualifiedBlocks - reject block with insufficient age") {
        val confirmedTip = hex"1000000000000000000000000000000000000000000000000000000000000000"
        val currentTime = BigInt(System.currentTimeMillis() / 1000)
        val challengeAgingSeconds = BitcoinValidator.ChallengeAging

        // Build chain from 1001 to 1101 (100+ confirmations - sufficient)
        // Blocks added 1 minute less than ChallengeAging (insufficient age)
        val branch = buildSingleBranch(
          startHeight = 1001,
          endHeight = 1101,
          baseChainwork = BigInt(1000000),
          baseTimestamp = currentTime - challengeAgingSeconds + 60 // 1 minute less than required
        )
        val forksTree = prelude.List(branch)

        val (promotedBlocks, updatedTree) = BitcoinValidator.promoteQualifiedBlocks(
          forksTree,
          confirmedTip,
          confirmedHeight = 1000,
          currentTime
        )

        assert(
          promotedBlocks.length ==
              BigInt(0),
          "Should not promote block with insufficient age"
        )
        assert(updatedTree.size == forksTree.size, "Tree should remain unchanged")
    }

    test("promoteQualifiedBlocks - partial promotion with depth-based cutoff") {
        val confirmedTip = hex"1000000000000000000000000000000000000000000000000000000000000000"
        val currentTime = BigInt(System.currentTimeMillis() / 1000)
        val challengeAgingSeconds = BitcoinValidator.ChallengeAging
        val challengeAgingMinutes = challengeAgingSeconds / 60

        // Build chain from 1001 to 1105
        // Each block is added 1 minute apart (see buildSingleBranch helper)
        // Block 1001: (challengeAging + 1) min old (meets requirement)
        // Block 1002: challengeAging min old (meets requirement, exactly at boundary)
        // Block 1003: (challengeAging - 1) min old (does NOT meet requirement)
        // Only first 2 blocks will meet both criteria:
        // - 100+ confirmations (all blocks 1001-1005 have this)
        // - challengeAging+ age (only blocks 1001-1002 have this)
        val branch = buildSingleBranch(
          startHeight = 1001,
          endHeight = 1105, // Tip at 1105, so block 1005 has exactly 100 confirmations
          baseChainwork = BigInt(1000000),
          baseTimestamp = currentTime - (challengeAgingMinutes + 1) * 60
        )
        val forksTree = prelude.List(branch)

        val (promotedBlocks, updatedTree) = BitcoinValidator.promoteQualifiedBlocks(
          forksTree,
          confirmedTip,
          confirmedHeight = 1000,
          currentTime
        )

        // Should promote blocks with BOTH 100+ confirmations AND challengeAging+ age
        // With blocks added 1 minute apart, only blocks 1001 and 1002 meet both criteria
        assert(
          promotedBlocks.length ==
              BigInt(2),
          "Should promote blocks 1001-1002 (meet both criteria)"
        )

        // Branch should still exist with remaining blocks
        assert(updatedTree.size == BigInt(1), "Branch should still exist")
    }

    // Note: Edge case tests for promoteQualifiedBlocks have been removed
    // as they used the old BlockNode/insertInSortedList API.
    // The new ForkBranch-based tests in ForksTreeStructureTest cover similar scenarios.

    // ===== NFT PRESERVATION TESTS =====

    private val nftPolicyIdScriptHash: ScriptHash =
        OracleConfig.getScriptHash(BitcoinContract.testTxOutRef)

    private def nftValue(lovelace: Long = 5_000_000): Value =
        Value.asset(nftPolicyIdScriptHash, AssetName.empty, 1, Coin(lovelace))

    private def nftPlusExtraValue(lovelace: Long = 5_000_000): Value =
        val extraPolicyId = ScriptHash.fromHex("aa" * 28)
        nftValue(lovelace) + Value.asset(
          extraPolicyId,
          AssetName(ByteString.fromString("extra")),
          100
        )

    /** Build test data for NFT preservation tests (reuses UpdateOracle single block pattern) */
    private def buildNftTestData(): (Data, Data, Data, Long) = {
        val block864800Hash = "0000000000000000000202c4f7f242ab864a6162b8d1e745de6a86ae979e130b"
        val block864800Height = 864800
        val block864800Timestamp = 1728414569L

        val block864801PrevHash = block864800Hash
        val block864801Bits = "17032f14"
        val block864801Timestamp = 1728414644L
        val block864801Nonce = 4254568467L
        val block864801Version = 666378240L
        val block864801Merkleroot =
            "dc2ad1c7a27d394d45961c13a9c36e1d5e6cdbea928c47cb06263bb049a9cd0f"

        val confirmedTip = ByteString.fromHex(block864800Hash).reverse
        val bits = ByteString.fromHex(block864801Bits).reverse
        val baseTime = BigInt(block864801Timestamp)

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

        val prevState = buildTestChainState(
          blockHeight = block864800Height,
          blockHash = confirmedTip,
          bits = bits,
          baseTimestamp = BigInt(block864800Timestamp),
          forksTreeSize = 0
        )

        val newBlockHash = blockHeaderHash(blockHeader)
        val target = compactBitsToTarget(bits)
        val parentTarget = compactBitsToTarget(prevState.currentTarget)
        val parentChainwork = calculateBlockProof(parentTarget)
        val blockWork = calculateBlockProof(target)
        val newChainwork = parentChainwork + blockWork

        val expectedBlockSummary = BlockSummary(
          hash = newBlockHash,
          height = prevState.blockHeight + 1,
          chainwork = newChainwork,
          timestamp = baseTime,
          bits = bits,
          addedTime = baseTime
        )

        val expectedBranch = ForkBranch(
          tipHash = newBlockHash,
          tipHeight = prevState.blockHeight + 1,
          tipChainwork = newChainwork,
          recentBlocks = prelude.List(expectedBlockSummary)
        )

        val expectedState = ChainState(
          blockHeight = prevState.blockHeight,
          blockHash = prevState.blockHash,
          currentTarget = prevState.currentTarget,
          blockTimestamp = prevState.blockTimestamp,
          recentTimestamps = prevState.recentTimestamps,
          previousDifficultyAdjustmentTimestamp = prevState.previousDifficultyAdjustmentTimestamp,
          confirmedBlocksRoot = prevState.confirmedBlocksRoot,
          forksTree = prelude.List(expectedBranch)
        )

        val redeemer = Action
            .UpdateOracle(prelude.List(blockHeader), baseTime, prelude.List.Nil)
            .toData

        (
          prevState.toData,
          expectedState.toData,
          redeemer,
          baseTime.toLong
        )
    }

    test("UpdateOracle succeeds with NFT preserved in input and output") {
        val (prevStateData, newStateData, redeemer, baseTime) = buildNftTestData()

        val scriptContext = makeScriptContext(
          baseTime,
          prevStateData,
          newStateData,
          redeemer,
          inputValue = nftValue(),
          outputValue = nftValue()
        )

        // Should succeed - NFT preserved
        BitcoinValidator.validate(BitcoinContract.testTxOutRef.toData)(scriptContext.toData)

        val result = BitcoinContract.bitcoinProgram $ scriptContext.toData
        result.evaluateDebug match
            case r: Result.Success =>
                println(s"NFT preserved test passed, budget: ${r.budget.showJson}")
            case r: Result.Failure => fail(s"NFT preserved validation failed: $r")
    }

    test("UpdateOracle fails when NFT removed from output") {
        val (prevStateData, newStateData, redeemer, baseTime) = buildNftTestData()

        val scriptContext = makeScriptContext(
          baseTime,
          prevStateData,
          newStateData,
          redeemer,
          inputValue = nftValue(),
          outputValue = Value.ada(5) // No NFT in output
        )

        // Should fail - NFT not preserved
        intercept[RuntimeException] {
            BitcoinValidator.validate(BitcoinContract.testTxOutRef.toData)(scriptContext.toData)
        }
    }

    // ===== MPF PROOF GENERATION TESTS =====

    test("MPF proof generation - off-chain proof validates on-chain insert") {
        import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF
        import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry as OnChainMPF

        val blockHash1 = ByteString.fromHex("aa" * 32)
        val blockHash2 = ByteString.fromHex("bb" * 32)

        // Create off-chain MPF with one block
        val offChainMpf = OffChainMPF.empty.insert(blockHash1, blockHash1)

        // Generate non-membership proof for second block
        val proof = offChainMpf.proveNonMembership(blockHash2)

        // Verify on-chain insert succeeds with this proof
        val onChainMpf = OnChainMPF(offChainMpf.rootHash)
        val newOnChainMpf = onChainMpf.insert(blockHash2, blockHash2, proof)

        // Insert second block into off-chain MPF and verify roots match
        val offChainMpf2 = offChainMpf.insert(blockHash2, blockHash2)
        assert(
          newOnChainMpf.root == offChainMpf2.rootHash,
          "On-chain and off-chain roots should match after insert"
        )
    }

    test("MPF sequential insertions - multiple proofs validate on-chain") {
        import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF
        import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry as OnChainMPF

        val initialHash = ByteString.fromHex("00" * 32)
        val block1 = ByteString.fromHex("11" * 32)
        val block2 = ByteString.fromHex("22" * 32)
        val block3 = ByteString.fromHex("33" * 32)

        // Start with initial block
        var offMpf = OffChainMPF.empty.insert(initialHash, initialHash)
        var onChainRoot = offMpf.rootHash

        // Sequential insertions with proofs
        for blockHash <- Seq(block1, block2, block3) do {
            val proof = offMpf.proveNonMembership(blockHash)

            // Verify on-chain insert
            val newRoot = OnChainMPF(onChainRoot).insert(blockHash, blockHash, proof).root

            // Update off-chain state
            offMpf = offMpf.insert(blockHash, blockHash)
            onChainRoot = newRoot

            // Verify roots match
            assert(
              onChainRoot == offMpf.rootHash,
              s"Roots should match after inserting ${blockHash.toHex}"
            )
        }
    }

    test("MPF applyMpfInserts - validates sequential proofs matching on-chain logic") {
        import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF
        import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry.ProofStep

        val initialHash = ByteString.fromHex("00" * 32)
        val block1Hash = ByteString.fromHex("11" * 32)
        val block2Hash = ByteString.fromHex("22" * 32)

        // Create off-chain MPF with initial block
        var offMpf = OffChainMPF.empty.insert(initialHash, initialHash)
        val initialRoot = offMpf.rootHash

        // Generate proofs for two blocks (mimicking computeUpdateWithProofs)
        val proof1 = offMpf.proveNonMembership(block1Hash)
        offMpf = offMpf.insert(block1Hash, block1Hash)

        val proof2 = offMpf.proveNonMembership(block2Hash)
        offMpf = offMpf.insert(block2Hash, block2Hash)

        val mpfProofs = prelude.List.from(Seq(proof1, proof2))

        // Create mock promoted blocks (only hash matters for MPF)
        val promoted1 = BlockSummary(
          hash = block1Hash,
          height = 1001,
          chainwork = 1000,
          timestamp = 100,
          bits = hex"1d00ffff".reverse,
          addedTime = 100
        )
        val promoted2 = BlockSummary(
          hash = block2Hash,
          height = 1002,
          chainwork = 2000,
          timestamp = 200,
          bits = hex"1d00ffff".reverse,
          addedTime = 200
        )
        val promotedBlocks = prelude.List.from(Seq(promoted1, promoted2))

        // Call the on-chain applyMpfInserts logic directly
        def applyMpfInserts(
            root: ByteString,
            blocks: prelude.List[BlockSummary],
            proofs: prelude.List[prelude.List[ProofStep]]
        ): ByteString =
            blocks match
                case prelude.List.Nil =>
                    proofs match
                        case prelude.List.Nil => root
                        case _                => throw new RuntimeException("proof count mismatch")
                case prelude.List.Cons(block, bTail) =>
                    proofs match
                        case prelude.List.Cons(proof, pTail) =>
                            import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry as MPF
                            val newRoot =
                                MPF(root).insert(block.hash, block.hash, proof).root
                            applyMpfInserts(newRoot, bTail, pTail)
                        case prelude.List.Nil =>
                            throw new RuntimeException("proof count mismatch")

        val finalRoot = applyMpfInserts(initialRoot, promotedBlocks, mpfProofs)

        // Verify the final root matches the off-chain MPF
        assert(
          finalRoot == offMpf.rootHash,
          "On-chain applyMpfInserts result should match off-chain MPF root"
        )
    }

    test("computeUpdateWithProofs matches applyHeaders when no promotion") {
        import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF

        val block864800Hash = "0000000000000000000202c4f7f242ab864a6162b8d1e745de6a86ae979e130b"
        val block864800Height = 864800
        val block864800Timestamp = 1728414569L
        val block864801Bits = "17032f14"
        val block864801Timestamp = 1728414644L
        val block864801Nonce = 4254568467L
        val block864801Version = 666378240L
        val block864801Merkleroot =
            "dc2ad1c7a27d394d45961c13a9c36e1d5e6cdbea928c47cb06263bb049a9cd0f"

        val confirmedTip = ByteString.fromHex(block864800Hash).reverse
        val bits = ByteString.fromHex(block864801Bits).reverse
        val currentTime = BigInt(block864801Timestamp)

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
              confirmedTip ++
              ByteString.fromHex(block864801Merkleroot).reverse ++
              longToLE4Bytes(block864801Timestamp) ++
              ByteString.fromHex(block864801Bits).reverse ++
              longToLE4Bytes(block864801Nonce)
        )

        val prevState = buildTestChainState(
          blockHeight = block864800Height,
          blockHash = confirmedTip,
          bits = bits,
          baseTimestamp = BigInt(block864800Timestamp),
          forksTreeSize = 0
        )

        val headers = prelude.List(blockHeader)
        val offChainMpf = OffChainMPF.empty.insert(confirmedTip, confirmedTip)

        val (newStateWithProofs, proofs, updatedMpf) =
            OracleTransactions.computeUpdateWithProofs(prevState, headers, currentTime, offChainMpf)
        val newStateSimple = OracleTransactions.applyHeaders(prevState, headers, currentTime)

        // No promotion expected, so states should match and proofs should be empty
        assert(newStateWithProofs == newStateSimple, "States should match when no promotion occurs")
        assert(proofs.isEmpty, "No proofs expected when no promotion occurs")
        assert(
          updatedMpf.rootHash == offChainMpf.rootHash,
          "MPF should be unchanged when no promotion occurs"
        )
    }

    test("UpdateOracle fails when extra token added to output (value stuffing)") {
        val (prevStateData, newStateData, redeemer, baseTime) = buildNftTestData()

        val scriptContext = makeScriptContext(
          baseTime,
          prevStateData,
          newStateData,
          redeemer,
          inputValue = nftValue(),
          outputValue = nftPlusExtraValue() // NFT + extra token in output
        )

        // Should fail - extra tokens added
        intercept[RuntimeException] {
            BitcoinValidator.validate(BitcoinContract.testTxOutRef.toData)(scriptContext.toData)
        }
    }

    // ===== BLOCK HEADER THROUGHPUT TEST =====

    test("Block header throughput - max headers per transaction") {
        val baseHeight = 864800
        val pp = CardanoInfo.mainnet.protocolParams
        val prices = pp.executionUnitPrices
        val maxTxCpu = pp.maxTxExecutionUnits.steps
        val maxTxMem = pp.maxTxExecutionUnits.memory
        val maxTxSize = pp.maxTxSize

        // Load the confirmed tip block (864800)
        val (baseFixture, _) = BlockFixture.loadWithHeader(baseHeight)
        val confirmedTip = ByteString.fromHex(baseFixture.hash).reverse
        val bits = ByteString.fromHex(baseFixture.bits).reverse

        // Create initial state with block 864800 as confirmed tip
        val prevState = buildTestChainState(
          blockHeight = baseFixture.height,
          blockHash = confirmedTip,
          bits = bits,
          baseTimestamp = BigInt(baseFixture.timestamp),
          forksTreeSize = 0
        )

        val input = TransactionInput(
          TransactionHash.fromHex(
            "1ab6879fc08345f51dc9571ac4f530bf8673e0d798758c470f9af6f98e2f3982"
          ),
          0
        )

        // Reference script UTxO — script lives here, not in the transaction witness set
        val refScriptInput = TransactionInput(
          TransactionHash.fromHex(
            "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          ),
          0
        )
        val refScriptUtxo = Utxo(
          refScriptInput,
          Output(
            testScriptAddr,
            Value.ada(10),
            None,
            Some(ScriptRef(testContract.script))
          )
        )

        println()
        println("=" * 100)
        println("BLOCK HEADER THROUGHPUT TEST")
        println("=" * 100)
        println(
          f"${"Headers"}%8s | ${"CPU Steps"}%15s | ${"Memory"}%12s | ${"CPU %"}%7s | ${"Mem %"}%7s | ${"Fee (ADA)"}%12s | ${"Tx Size"}%8s | ${"Size %"}%7s | ${"Status"}%6s"
        )
        println("-" * 100)

        // Evaluate incrementally until we exceed any budget/size limit, then stop.
        var count = 0
        var withinLimits = true
        while withinLimits do {
            count += 1
            val headers =
                (1 to count).map(i => BlockFixture.loadWithHeader(baseHeight + i)._2).toList
            val headersScalus = prelude.List.from(headers)
            val lastFixture = BlockFixture.load(baseHeight + count)
            val lastTimestamp = BigInt(lastFixture.timestamp)

            val expectedState = BitcoinValidator.computeUpdateOracleState(
              prevState,
              headersScalus,
              lastTimestamp,
              prelude.List.Nil
            )

            val redeemer = Action
                .UpdateOracle(headersScalus, lastTimestamp, prelude.List.Nil)
                .toData

            val inputValue = Value.ada(5)
            val utxo = Utxo(
              input,
              Output(testScriptAddr, inputValue, DatumOption.Inline(prevState.toData))
            )
            val utxos: Utxos =
                Map(utxo.input -> utxo.output, refScriptUtxo.input -> refScriptUtxo.output)
            val validFrom = Instant.ofEpochSecond(lastTimestamp.toLong)

            // Build a real draft transaction using reference script
            val draft = txBuilder
                .references(refScriptUtxo, testContract)
                .spend(utxo, redeemer)
                .payTo(testScriptAddr, inputValue, expectedState.toData)
                .validFrom(validFrom)
                .draft

            val txSize = draft.toCbor.length

            val scriptContext = draft.getScriptContextV3(utxos, ForSpend(input))

            val result = testContract.program $ scriptContext.toData
            result.evaluateDebug match
                case r: Result.Success =>
                    val cpuPct = r.budget.steps * 100.0 / maxTxCpu
                    val memPct = r.budget.memory * 100.0 / maxTxMem
                    val sizePct = txSize * 100.0 / maxTxSize
                    val feeAda = r.budget.fee(prices).value / 1_000_000.0
                    withinLimits = cpuPct <= 100 && memPct <= 100 && sizePct <= 100
                    val status = if withinLimits then "OK" else "OVER"
                    println(
                      f"$count%8d | ${r.budget.steps}%15d | ${r.budget.memory}%12d | $cpuPct%6.1f%% | $memPct%6.1f%% | $feeAda%12.6f | $txSize%8d | $sizePct%6.1f%% | $status%6s"
                    )
                case r: Result.Failure =>
                    println(f"$count%8d | EVALUATION FAILED: $r")
                    withinLimits = false
        }

        val maxHeadersPerTx = count - 1

        println("-" * 100)
        println(s"Max tx execution budget: CPU=$maxTxCpu steps, Memory=$maxTxMem units")
        println(s"Max tx size: $maxTxSize bytes")
        println(s"Maximum block headers per transaction: $maxHeadersPerTx")
        println("=" * 100)

        assert(
          maxHeadersPerTx == 28,
          s"Expected exactly 28 headers per transaction but got $maxHeadersPerTx" +
              (if maxHeadersPerTx < 28 then " (performance regression)"
               else " (improvement — update this assertion)")
        )
    }
}
