package binocular

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.*
import scalus.cardano.ledger.utils.MinTransactionFee
import scalus.cardano.ledger.*
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry.ProofStep
import scalus.cardano.onchain.plutus.prelude
import scalus.cardano.txbuilder.RedeemerPurpose.ForSpend
import scalus.cardano.txbuilder.txBuilder
import scalus.compiler.Options
import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF
import scalus.testing.kit.ScalusTest
import scalus.testing.kit.TestUtil.getScriptContextV3
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.hex
import scalus.uplc.builtin.Data.toData
import scalus.uplc.eval.*

import java.time.Instant
import binocular.ForkTree.*
import binocular.OracleAction.*
import scalus.cardano.address.{ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.onchain.plutus.v1.PubKeyHash
import scalus.testing.kit.Party

class BitcoinValidatorTest extends AnyFunSuite with ScalusTest with ScalaCheckPropertyChecks {
    private given env: CardanoInfo = CardanoInfo.mainnet

//    override protected def plutusVM: PlutusVM =
//        PlutusVM.makePlutusV3VM(MajorProtocolVersion.vanRossemPV)

    private val testTxOutRef = scalus.cardano.onchain.plutus.v3.TxOutRef(
      scalus.cardano.onchain.plutus.v3.TxId(
        hex"0000000000000000000000000000000000000000000000000000000000000000"
      ),
      BigInt(0)
    )

    private val testOwner = PubKeyHash(Party.Alice.addrKeyHash)

    private val testParams = BitcoinValidatorParams(
      maturationConfirmations = 100,
      challengeAging = 200 * 60, // 200 minutes in seconds
      oneShotTxOutRef = testTxOutRef,
      closureTimeout = 30 * 24 * 60 * 60, // 30 days
      owner = testOwner,
      powLimit = BitcoinHelpers.PowLimit
    )

    private val testContract = {
        given Options = Options.release
//        Options.release.copy(targetProtocolVersion = MajorProtocolVersion.vanRossemPV)
        PlutusV3.compile(BitcoinValidator.validate).withErrorTraces(testParams.toData)
    }
    private val testScriptAddr = testContract.address(env.network)
    private val testScriptHash = testContract.script.scriptHash
    private val testProgram = testContract.program.deBruijnedProgram

    /** Create a Value with ADA + the oracle NFT (1 token at policyId = script hash). */
    private def nftValue(adaAmount: Long): Value =
        Value.asset(testScriptHash, AssetName.empty, 1, Coin.ada(adaAmount))

    private def chainStateWithTree(tree: ForkTree): ChainState =
        ChainState(
          confirmedBlocksRoot = ByteString.unsafeFromArray(Array.fill(32)(0: Byte)),
          ctx = TraversalCtx(
            timestamps = prelude.List.from((0 until 11).map(i => BigInt(1000000 - i * 600)).toList),
            height = 866880,
            currentBits = ByteString.unsafeFromArray(Array.fill(4)(0xff.toByte)),
            prevDiffAdjTimestamp = 1000000,
            lastBlockHash = ByteString.unsafeFromArray(Array.fill(32)(0: Byte))
          ),
          forkTree = tree
        )

    // Helper to create a dummy block summary for bestChainPath tests (only hash matters for identity)
    private def dummyBlock(id: Byte): BlockSummary =
        BlockSummary(
          hash = ByteString.unsafeFromArray(Array.fill(32)(id)),
          timestamp = BigInt(1000000 + id),
          addedTimeSeconds = BigInt(1000000 + id)
        )

    test(
      "bestChainPath - equal chainwork favors left (existing) branch, matching Bitcoin Core first-seen rule"
    ) {
        // Bitcoin Core's CBlockIndexWorkComparator prefers the first-seen chain when chainwork
        // is equal (lower nSequenceId wins). In Binocular, validateAndInsert always places the
        // existing branch left and the new branch right, so bestChainPath's >= tie-break on left
        // achieves the same effect: never reorganize to an equal-work competitor.

        val blockA = dummyBlock(1)
        val blockB = dummyBlock(2)

        // Two branches with identical chainwork
        val equalWorkTree = Fork(
          Blocks(prelude.List(blockA), 100, End),
          Blocks(prelude.List(blockB), 100, End)
        )

        val (cw, depth, path) = BitcoinValidator.bestChainPath(equalWorkTree, 0, 0)
        assert(cw == BigInt(100), "chainwork should be 100")
        assert(depth == BigInt(1), "depth should be 1")
        assert(
          path == prelude.List(BitcoinValidator.LeftFork),
          "equal chainwork must select left (existing) branch"
        )
    }

    test("bestChainPath - higher chainwork wins regardless of position") {
        val blockA = dummyBlock(1)
        val blockB = dummyBlock(2)
        val blockC = dummyBlock(3)

        // Right branch has strictly more work → must win despite being the newer branch
        val rightWinsTree = Fork(
          Blocks(prelude.List(blockA), 100, End),
          Blocks(prelude.List(blockB, blockC), 300, End)
        )

        val (cw, depth, path) = BitcoinValidator.bestChainPath(rightWinsTree, 0, 0)
        assert(cw == BigInt(300), "chainwork should be 300")
        assert(depth == BigInt(2), "depth should be 2")
        assert(path == prelude.List(BitcoinValidator.RightFork), "higher chainwork must win")
    }

    test("bestChainPath - nested forks with equal chainwork favor left at every level") {
        val blockA = dummyBlock(1)
        val blockB = dummyBlock(2)
        val blockC = dummyBlock(3)

        // Shared prefix, then inner fork with equal work
        val nestedTree = Blocks(
          prelude.List(blockA),
          50,
          Fork(
            Blocks(prelude.List(blockB), 100, End),
            Blocks(prelude.List(blockC), 100, End)
          )
        )

        val (cw, depth, path) = BitcoinValidator.bestChainPath(nestedTree, 0, 0)
        assert(cw == BigInt(150), "chainwork should be 50 + 100")
        assert(depth == BigInt(2), "depth should be 2 (prefix + one branch block)")
        assert(
          path == prelude.List(BitcoinValidator.LeftFork),
          "inner equal-work fork must select left"
        )
    }

    test("BitcoinValidator size") {
        given Options =
            Options.release.copy(targetProtocolVersion = MajorProtocolVersion.vanRossemPV)
        val contract = PlutusV3.compile(BitcoinValidator.validate).apply(testParams.toData)
        println(s"Contract size: ${contract.script.script.size}")
//        println(s"Contract size: ${contract.program.showHighlighted}")
//        assert(contract.script.script.size == 7381)
    }

    test("Block header throughput - max headers per transaction") {
        val baseHeight = 866880
        val pp = CardanoInfo.mainnet.protocolParams
        val prices = pp.executionUnitPrices
        val maxTxCpu = pp.maxTxExecutionUnits.steps
        val maxTxMem = pp.maxTxExecutionUnits.memory
        val maxTxSize = pp.maxTxSize

        // Load the confirmed tip block (866880 — exactly at retarget boundary 2016*430)
        val (baseFixture, _) = BlockFixture.loadWithHeader(baseHeight)
        val confirmedTip = ByteString.fromHex(baseFixture.hash).reverse
        val bits = ByteString.fromHex(baseFixture.bits).reverse

        // Build 11 recent timestamps (newest first, reverse-sorted)
        val baseTimestamp = BigInt(baseFixture.timestamp)
        val recentTimestamps =
            prelude.List.from((0 until 11).map(i => baseTimestamp - i * 600).toList)

        // Create initial ChainState with block 866880 as confirmed tip
        // Block 866880 is at retarget boundary, so previousDifficultyAdjustmentTimestamp is its own timestamp
        val prevState = ChainState(
          confirmedBlocksRoot = BitcoinChainState.mpfRootForSingleBlock(confirmedTip),
          ctx = TraversalCtx(
            timestamps = recentTimestamps,
            height = baseFixture.height,
            currentBits = bits,
            prevDiffAdjTimestamp = baseTimestamp,
            lastBlockHash = confirmedTip
          ),
          forkTree = End
        )

        val input = Input(
          TransactionHash.fromHex(
            "1ab6879fc08345f51dc9571ac4f530bf8673e0d798758c470f9af6f98e2f3982"
          ),
          0
        )

        // Reference script UTxO — script lives here, not in the transaction witness set
        val refScriptInput = Input(
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
        println("BLOCK HEADER THROUGHPUT TEST (V2)")
        println("=" * 100)
        println(
          f"${"Headers"}%8s | ${"CPU Steps"}%15s | ${"Memory"}%12s | ${"CPU %"}%7s | ${"Mem %"}%7s | ${"Ex Fee"}%12s | ${"Tx Fee"}%12s | ${"Tx Size"}%8s | ${"Size %"}%7s | ${"Status"}%6s"
        )
        println("-" * 100)

        val maxAvailableHeaders = 195 // fixtures available: 866881..867075
        var count = 0
        var withinLimits = true
        while withinLimits && count < maxAvailableHeaders do {
            count += 1
            val headers =
                (1 to count).map(i => BlockFixture.loadWithHeader(baseHeight + i)._2).toList
            val headersScalus = prelude.List.from(headers)
            val lastFixture = BlockFixture.load(baseHeight + count)
            val lastTimestamp = BigInt(lastFixture.timestamp)

            // Compute expected state using BitcoinValidator.computeUpdate
            val update = UpdateOracle(
              blockHeaders = headersScalus,
              parentPath = prelude.List.Nil, // parent is confirmed tip
              mpfInsertProofs = prelude.List.Nil
            )
            val expectedState =
                BitcoinValidator.computeUpdate(
                  state = prevState,
                  blockHeaders = headersScalus,
                  parentPath = prelude.List.Nil,
                  mpfInsertProofs = prelude.List.Nil,
                  currentTime = lastTimestamp,
                  params = testParams
                )

//            pprint.pprintln(expectedState)

            val redeemer = update.toData
//            pprint.pprintln(redeemer)

            val inputValue = nftValue(5)
            val utxo = Utxo(
              input,
              Output(
                testScriptAddr,
                inputValue,
                DatumOption.Inline(prevState.toData)
              )
            )
            val utxos: Utxos =
                Map(utxo.input -> utxo.output, refScriptUtxo.input -> refScriptUtxo.output)
            val validFrom = Instant.ofEpochSecond(lastTimestamp.toLong)

            val validTo = validFrom.plusSeconds(600)
            val draft = txBuilder
                .references(refScriptUtxo, testContract)
                .spend(utxo, redeemer)
                .payTo(testScriptAddr, inputValue, expectedState.toData)
                .validFrom(validFrom)
                .validTo(validTo)
                .draft

            val txSize = draft.toCbor.length
            val scriptContext = draft.getScriptContextV3(utxos, ForSpend(input))

            val result = testProgram.applyArg(scriptContext.toData).evaluateDebug
            result match
                case r: Result.Success =>
                    val cpuPct = r.budget.steps * 100.0 / maxTxCpu
                    val memPct = r.budget.memory * 100.0 / maxTxMem
                    val sizePct = txSize * 100.0 / maxTxSize
                    val exFeeAda = r.budget.fee(prices).value / 1_000_000.0
                    val txFeeAda = MinTransactionFee
                        .computeMinFee(draft, utxos, pp)
                        .getOrElse(throw new Exception("Failed to compute min fee"))
                        .value / 1_000_000.0
                    withinLimits = cpuPct <= 100 && memPct <= 100 && sizePct <= 100
                    val status = if withinLimits then "OK" else "OVER"
                    println(
                      f"$count%8d | ${r.budget.steps}%15d | ${r.budget.memory}%12d | $cpuPct%6.1f%% | $memPct%6.1f%% | $exFeeAda%12.6f | $txFeeAda%12.6f | $txSize%8d | $sizePct%6.1f%% | $status%6s"
                    )
                case r: Result.Failure =>
                    println(f"$count%8d | EVALUATION FAILED: $r")
                    withinLimits = false

            if !withinLimits then println(result)
        }

        val maxHeadersPerTx = if withinLimits then count else count - 1

        println("-" * 100)
        println(s"Max tx execution budget: CPU=$maxTxCpu steps, Memory=$maxTxMem units")
        println(s"Max tx size: $maxTxSize bytes")
        println(s"Maximum block headers per transaction: $maxHeadersPerTx")
        if withinLimits then println(s"(limited by available test fixtures, not execution budget)")
        println("=" * 100)

        assert(
          maxHeadersPerTx > 0,
          "Should be able to fit at least 1 block header per transaction"
        )
    }

    test("Promotion throughput - 100 blocks in tree + N new blocks triggering N promotions") {
        val baseHeight = 866880
        val preloadCount = 100
        val pp = CardanoInfo.mainnet.protocolParams
        val prices = pp.executionUnitPrices
        val maxTxCpu = pp.maxTxExecutionUnits.steps
        val maxTxMem = pp.maxTxExecutionUnits.memory
        val maxTxSize = pp.maxTxSize

        // Load the confirmed tip block (866880 — retarget boundary 2016*430)
        val (baseFixture, _) = BlockFixture.loadWithHeader(baseHeight)
        val confirmedTip = ByteString.fromHex(baseFixture.hash).reverse
        val bits = ByteString.fromHex(baseFixture.bits).reverse
        val baseTimestamp = BigInt(baseFixture.timestamp)
        val recentTimestamps =
            prelude.List.from((0 until 11).map(i => baseTimestamp - i * 600).toList)

        // Initial confirmed state
        val initialState = ChainState(
          confirmedBlocksRoot = BitcoinChainState.mpfRootForSingleBlock(confirmedTip),
          ctx = TraversalCtx(
            timestamps = recentTimestamps,
            height = baseFixture.height,
            currentBits = bits,
            prevDiffAdjTimestamp = baseTimestamp,
            lastBlockHash = confirmedTip
          ),
          forkTree = End
        )

        // Pre-load 100 blocks into fork tree (866881..866980)
        val preloadHeaders =
            (1 to preloadCount).map(i => BlockFixture.loadWithHeader(baseHeight + i)._2).toList
        val preloadHeadersScalus = prelude.List.from(preloadHeaders)
        val lastPreloadFixture = BlockFixture.load(baseHeight + preloadCount)
        val preloadTime = BigInt(lastPreloadFixture.timestamp)

        val stateWith100Blocks =
            BitcoinValidator.computeUpdate(
              state = initialState,
              blockHeaders = preloadHeadersScalus,
              parentPath = prelude.List.Nil,
              mpfInsertProofs = prelude.List.Nil,
              currentTime = preloadTime,
              params = testParams
            )

        val input = Input(
          TransactionHash.fromHex(
            "1ab6879fc08345f51dc9571ac4f530bf8673e0d798758c470f9af6f98e2f3982"
          ),
          0
        )
        val refScriptInput = Input(
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
        println("=" * 120)
        println(
          "PROMOTION THROUGHPUT TEST (V2) - 100 blocks in fork tree + N new blocks triggering N promotions"
        )
        println("=" * 120)
        println(
          f"${"Headers"}%8s | ${"Promoted"}%8s | ${"CPU Steps"}%15s | ${"Memory"}%12s | ${"CPU %"}%7s | ${"Mem %"}%7s | ${"Ex Fee"}%12s | ${"Tx Fee"}%12s | ${"Tx Size"}%8s | ${"Size %"}%7s | ${"Status"}%6s"
        )
        println("-" * 120)

        val maxNewHeaders = 95 // fixtures available: 866981..867075
        var count = 0
        var withinLimits = true

        while withinLimits && count < maxNewHeaders do {
            count += 1

            // Load N new headers (866981..866980+count)
            val newHeaders =
                (1 to count)
                    .map(i => BlockFixture.loadWithHeader(baseHeight + preloadCount + i)._2)
                    .toList
            val newHeadersScalus = prelude.List.from(newHeaders)

            // currentTime: must satisfy future-time check AND aging >= ChallengeAging
            val lastNewFixture = BlockFixture.load(baseHeight + preloadCount + count)
            val lastNewTimestamp = BigInt(lastNewFixture.timestamp)
            val currentTime = lastNewTimestamp.max(preloadTime + testParams.challengeAging)

            // Parent path: index 99 = last block in the 100-block Blocks node
            val parentPath = prelude.List(BigInt(preloadCount - 1))

            // Determine promoted blocks by running validator logic
            val ctx0 = stateWith100Blocks.ctx
            val newTree = BitcoinValidator.validateAndInsert(
              stateWith100Blocks.forkTree,
              parentPath,
              newHeadersScalus,
              ctx0,
              currentTime,
              testParams
            )
            val (_, bestDepth, bestPath) = BitcoinValidator.bestChainPath(
              newTree,
              stateWith100Blocks.ctx.height,
              BigInt(0)
            )
            val (promoted, _) =
                BitcoinValidator.promoteAndGC(
                  newTree,
                  ctx0,
                  bestPath,
                  bestDepth,
                  currentTime,
                  count,
                  testParams
                )
            val promotedCount = promoted.length

            // Generate MPF proofs for actual promoted blocks
            var mpf = OffChainMPF.empty.insert(confirmedTip, confirmedTip)
            val proofsBuilder =
                scala.collection.mutable.ListBuffer[prelude.List[ProofStep]]()
            promoted.foreach { block =>
                val proof = mpf.proveNonMembership(block.hash)
                proofsBuilder += proof
                mpf = mpf.insert(block.hash, block.hash)
            }
            val mpfProofsScalus = prelude.List.from(proofsBuilder.toList)

            val update = UpdateOracle(
              blockHeaders = newHeadersScalus,
              parentPath = parentPath,
              mpfInsertProofs = mpfProofsScalus
            )
            val expectedState =
                BitcoinValidator.computeUpdate(
                  state = stateWith100Blocks,
                  blockHeaders = newHeadersScalus,
                  parentPath = parentPath,
                  mpfInsertProofs = mpfProofsScalus,
                  currentTime = currentTime,
                  params = testParams
                )

            val redeemer = update.toData
            val inputValue = nftValue(5)
            val utxo = Utxo(
              input,
              Output(
                testScriptAddr,
                inputValue,
                DatumOption.Inline(stateWith100Blocks.toData)
              )
            )
            val utxos: Utxos =
                Map(utxo.input -> utxo.output, refScriptUtxo.input -> refScriptUtxo.output)
            val validFrom = Instant.ofEpochSecond(currentTime.toLong)
            val validTo = validFrom.plusSeconds(600)

            val draft = txBuilder
                .references(refScriptUtxo, testContract)
                .spend(utxo, redeemer)
                .payTo(testScriptAddr, inputValue, expectedState.toData)
                .validFrom(validFrom)
                .validTo(validTo)
                .draft

            val txSize = draft.toCbor.length
            val scriptContext = draft.getScriptContextV3(utxos, ForSpend(input))

            val result = testProgram.applyArg(scriptContext.toData).evaluateDebug
            result match
                case r: Result.Success =>
                    val cpuPct = r.budget.steps * 100.0 / maxTxCpu
                    val memPct = r.budget.memory * 100.0 / maxTxMem
                    val sizePct = txSize * 100.0 / maxTxSize
                    val exFeeAda = r.budget.fee(prices).value / 1_000_000.0
                    val txFeeAda = MinTransactionFee
                        .computeMinFee(draft, utxos, pp)
                        .getOrElse(throw new Exception("Failed to compute min fee"))
                        .value / 1_000_000.0
                    withinLimits = cpuPct <= 100 && memPct <= 100 && sizePct <= 100
                    val status = if withinLimits then "OK" else "OVER"
                    println(
                      f"$count%8d | $promotedCount%8d | ${r.budget.steps}%15d | ${r.budget.memory}%12d | $cpuPct%6.1f%% | $memPct%6.1f%% | $exFeeAda%12.6f | $txFeeAda%12.6f | $txSize%8d | $sizePct%6.1f%% | $status%6s"
                    )
                case r: Result.Failure =>
                    println(f"$count%8d | $promotedCount%8d | EVALUATION FAILED: $r")
                    withinLimits = false

            if !withinLimits then println(result)
        }

        val maxHeadersPerTx = if withinLimits then count else count - 1

        println("-" * 120)
        println(s"Max tx execution budget: CPU=$maxTxCpu steps, Memory=$maxTxMem units")
        println(s"Max tx size: $maxTxSize bytes")
        println(s"Maximum headers with promotion per transaction: $maxHeadersPerTx")
        if withinLimits then println(s"(limited by available test fixtures, not execution budget)")
        println("=" * 120)

        assert(
          maxHeadersPerTx > 0,
          "Should be able to fit at least 1 block header with promotion per transaction"
        )
    }

    test("ForkTree capacity - max blocks in single branch") {
        val pp = CardanoInfo.mainnet.protocolParams
        val maxTxSize = pp.maxTxSize

        // Build a linear fork tree of increasing size
        // and find the maximum number of blocks that fits in a transaction
        def linearTree(n: Int): ForkTree = {
            val blocks = prelude.List.from((1 to n).map(i => dummyBlock(i.toByte)).toList)
            Blocks(blocks, 100, End)
        }

        // Binary search for the max count that fits
        var lo = 1
        var hi = 500
        while lo < hi do {
            val mid = (lo + hi + 1) / 2
            val size = chainStateWithTree(linearTree(mid)).toData.toCbor.length
            if size <= maxTxSize then lo = mid else hi = mid - 1
        }
        val maxBlocks = lo
        val datumSize = chainStateWithTree(linearTree(maxBlocks)).toData.toCbor.length
        val overSize = chainStateWithTree(linearTree(maxBlocks + 1)).toData.toCbor.length

        println(
          f"\nSingle branch: max $maxBlocks blocks ($datumSize%,d bytes, limit $maxTxSize%,d)"
        )

        assert(maxBlocks == 338, s"Expected 338 blocks in single branch, got $maxBlocks")
    }

    test("ForkTree capacity - max forks and minimum griefing attack cost") {
        val pp = CardanoInfo.mainnet.protocolParams
        val maxTxSize = pp.maxTxSize

        // Griefing attack: fill the fork tree so no new block can be added, while keeping
        // every branch shorter than maturationConfirmations (100). Since no branch is deep
        // enough for promotion, blocks can never be removed — the oracle is permanently halted.
        //
        // The cheapest attack uses single-block branches (depth 1, well under 100) to maximize
        // forks per byte. Two tree shapes give different block counts for the same fork count:
        //
        // Left-leaning: Fork(Fork(Fork(..., leaf), leaf), leaf)
        //   Matches how validateAndInsert builds the tree (existing left, new right).
        //   N forks = N+1 blocks. This is the MAXIMUM forks/blocks that fit.
        //
        // Balanced: complete binary tree of depth D
        //   2^D - 1 forks, 2^D blocks. More compact encoding (shared structure) means
        //   fewer blocks fit — this is the MINIMUM blocks needed to halt the oracle.

        def balancedForkTree(depth: Int): ForkTree = {
            if depth <= 0 then Blocks(prelude.List(dummyBlock(1)), 100, End)
            else Fork(balancedForkTree(depth - 1), balancedForkTree(depth - 1))
        }

        // Left-leaning fork tree (matches validateAndInsert: existing branch goes left, new goes right)
        def leftLeaningForkTree(forks: Int): ForkTree = {
            if forks <= 0 then Blocks(prelude.List(dummyBlock(1)), 100, End)
            else
                Fork(
                  leftLeaningForkTree(forks - 1),
                  Blocks(prelude.List(dummyBlock(1)), 100, End)
                )
        }

        // Binary search for max forks (left-leaning)
        var lo = 1
        var hi = 300
        while lo < hi do {
            val mid = (lo + hi + 1) / 2
            val size = chainStateWithTree(leftLeaningForkTree(mid)).toData.toCbor.length
            if size <= maxTxSize then lo = mid else hi = mid - 1
        }
        val maxForks = lo
        val forkDatumSize = chainStateWithTree(leftLeaningForkTree(maxForks)).toData.toCbor.length
        val forkOverSize =
            chainStateWithTree(leftLeaningForkTree(maxForks + 1)).toData.toCbor.length

        // Balanced tree: minimum blocks to fill the datum (griefing lower bound)
        var balancedDepth = 1
        while chainStateWithTree(
              balancedForkTree(balancedDepth + 1)
            ).toData.toCbor.length <= maxTxSize
        do balancedDepth += 1
        val balancedForks = (1 << balancedDepth) - 1 // 2^depth - 1
        val balancedBlocks = 1 << balancedDepth // 2^depth leaf nodes, each with 1 block
        val balancedSize = chainStateWithTree(balancedForkTree(balancedDepth)).toData.toCbor.length

        println(
          f"\nLeft-leaning:  $maxForks forks, ${maxForks + 1} blocks ($forkDatumSize%,d bytes, limit $maxTxSize%,d)"
        )
        println(
          f"Balanced:      $balancedForks forks, $balancedBlocks blocks, depth $balancedDepth ($balancedSize%,d bytes) — min griefing cost"
        )

        assert(maxForks == 256, s"Expected 256 left-leaning forks, got $maxForks")
        assert(balancedDepth == 8, s"Expected balanced depth 8, got $balancedDepth")
    }

    test("Validity interval - unbounded validRange (no validTo) must be rejected") {
        // Without a validTo upper bound, the on-chain time (validFrom) could be
        // arbitrarily stale — the transaction remains valid forever. This weakens
        // the challenge aging guarantee: addedTimeSeconds could be set to a much
        // earlier time than when the transaction actually executes on-chain.
        // The fix requires both validFrom and validTo with a bounded window.
        val baseHeight = 866880

        val (baseFixture, _) = BlockFixture.loadWithHeader(baseHeight)
        val confirmedTip = ByteString.fromHex(baseFixture.hash).reverse
        val bits = ByteString.fromHex(baseFixture.bits).reverse
        val baseTimestamp = BigInt(baseFixture.timestamp)
        val recentTimestamps =
            prelude.List.from((0 until 11).map(i => baseTimestamp - i * 600).toList)

        val prevState = ChainState(
          confirmedBlocksRoot = BitcoinChainState.mpfRootForSingleBlock(confirmedTip),
          ctx = TraversalCtx(
            timestamps = recentTimestamps,
            height = baseFixture.height,
            currentBits = bits,
            prevDiffAdjTimestamp = baseTimestamp,
            lastBlockHash = confirmedTip
          ),
          forkTree = End
        )

        // Add 1 block with a legitimate validFrom
        val header = BlockFixture.loadWithHeader(baseHeight + 1)._2
        val headersScalus = prelude.List(header)
        val fixture = BlockFixture.load(baseHeight + 1)
        val currentTime = BigInt(fixture.timestamp)

        val update = UpdateOracle(
          blockHeaders = headersScalus,
          parentPath = prelude.List.Nil,
          mpfInsertProofs = prelude.List.Nil
        )
        val expectedState =
            BitcoinValidator.computeUpdate(
              state = prevState,
              blockHeaders = headersScalus,
              parentPath = prelude.List.Nil,
              mpfInsertProofs = prelude.List.Nil,
              currentTime = currentTime,
              params = testParams
            )

        val input = Input(
          TransactionHash.fromHex(
            "1ab6879fc08345f51dc9571ac4f530bf8673e0d798758c470f9af6f98e2f3982"
          ),
          0
        )
        val refScriptUtxo = Utxo(
          Input(
            TransactionHash.fromHex(
              "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
            ),
            0
          ),
          Output(
            testScriptAddr,
            Value.ada(10),
            None,
            Some(ScriptRef(testContract.script))
          )
        )
        val inputValue = nftValue(5)
        val utxo = Utxo(
          input,
          Output(testScriptAddr, inputValue, DatumOption.Inline(prevState.toData))
        )
        val utxos: Utxos =
            Map(utxo.input -> utxo.output, refScriptUtxo.input -> refScriptUtxo.output)

        // Transaction with only validFrom, no validTo — unbounded upper end
        val draft = txBuilder
            .references(refScriptUtxo, testContract)
            .spend(utxo, update.toData)
            .payTo(testScriptAddr, inputValue, expectedState.toData)
            .validFrom(Instant.ofEpochSecond(currentTime.toLong))
            .draft

        val scriptContext = draft.getScriptContextV3(utxos, ForSpend(input))
        val result = testProgram.applyArg(scriptContext.toData).evaluateDebug

        assert(
          result.isFailure,
          "Unbounded validity interval (no validTo) must be rejected"
        )
    }

    test("Validity interval - too wide window must be rejected") {
        // Even with validTo set, the window must be bounded to ensure
        // addedTimeSeconds is close to actual wall-clock time.
        val baseHeight = 866880

        val (baseFixture, _) = BlockFixture.loadWithHeader(baseHeight)
        val confirmedTip = ByteString.fromHex(baseFixture.hash).reverse
        val bits = ByteString.fromHex(baseFixture.bits).reverse
        val baseTimestamp = BigInt(baseFixture.timestamp)
        val recentTimestamps =
            prelude.List.from((0 until 11).map(i => baseTimestamp - i * 600).toList)

        val prevState = ChainState(
          confirmedBlocksRoot = BitcoinChainState.mpfRootForSingleBlock(confirmedTip),
          ctx = TraversalCtx(
            timestamps = recentTimestamps,
            height = baseFixture.height,
            currentBits = bits,
            prevDiffAdjTimestamp = baseTimestamp,
            lastBlockHash = confirmedTip
          ),
          forkTree = End
        )

        val header = BlockFixture.loadWithHeader(baseHeight + 1)._2
        val headersScalus = prelude.List(header)
        val fixture = BlockFixture.load(baseHeight + 1)
        val currentTime = BigInt(fixture.timestamp)

        val update = UpdateOracle(
          blockHeaders = headersScalus,
          parentPath = prelude.List.Nil,
          mpfInsertProofs = prelude.List.Nil
        )
        val expectedState =
            BitcoinValidator.computeUpdate(
              state = prevState,
              blockHeaders = headersScalus,
              parentPath = prelude.List.Nil,
              mpfInsertProofs = prelude.List.Nil,
              currentTime = currentTime,
              params = testParams
            )

        val input = Input(
          TransactionHash.fromHex(
            "1ab6879fc08345f51dc9571ac4f530bf8673e0d798758c470f9af6f98e2f3982"
          ),
          0
        )
        val refScriptUtxo = Utxo(
          Input(
            TransactionHash.fromHex(
              "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
            ),
            0
          ),
          Output(
            testScriptAddr,
            Value.ada(10),
            None,
            Some(ScriptRef(testContract.script))
          )
        )
        val inputValue = nftValue(5)
        val utxo = Utxo(
          input,
          Output(testScriptAddr, inputValue, DatumOption.Inline(prevState.toData))
        )
        val utxos: Utxos =
            Map(utxo.input -> utxo.output, refScriptUtxo.input -> refScriptUtxo.output)

        // Transaction with validTo set 1 hour after validFrom (too wide)
        val validFromInstant = Instant.ofEpochSecond(currentTime.toLong)
        val validToInstant = Instant.ofEpochSecond(currentTime.toLong + 3600) // 1 hour window
        val draft = txBuilder
            .references(refScriptUtxo, testContract)
            .spend(utxo, update.toData)
            .payTo(testScriptAddr, inputValue, expectedState.toData)
            .validFrom(validFromInstant)
            .validTo(validToInstant)
            .draft

        val scriptContext = draft.getScriptContextV3(utxos, ForSpend(input))
        val result = testProgram.applyArg(scriptContext.toData).evaluateDebug

        assert(
          result.isFailure,
          "Validity interval wider than MaxValidityWindow must be rejected"
        )
    }

    test("Bifrost scenario - 100 blocks in tree, add 1 header, promote 1 block") {
        val baseHeight = 866880
        val preloadCount = 100
        val pp = CardanoInfo.mainnet.protocolParams
        val prices = pp.executionUnitPrices
        val maxTxCpu = pp.maxTxExecutionUnits.steps
        val maxTxMem = pp.maxTxExecutionUnits.memory
        val maxTxSize = pp.maxTxSize

        val (baseFixture, _) = BlockFixture.loadWithHeader(baseHeight)
        val confirmedTip = ByteString.fromHex(baseFixture.hash).reverse
        val bits = ByteString.fromHex(baseFixture.bits).reverse
        val baseTimestamp = BigInt(baseFixture.timestamp)
        val recentTimestamps =
            prelude.List.from((0 until 11).map(i => baseTimestamp - i * 600).toList)

        val initialState = ChainState(
          confirmedBlocksRoot = BitcoinChainState.mpfRootForSingleBlock(confirmedTip),
          ctx = TraversalCtx(
            timestamps = recentTimestamps,
            height = baseFixture.height,
            currentBits = bits,
            prevDiffAdjTimestamp = baseTimestamp,
            lastBlockHash = confirmedTip
          ),
          forkTree = End
        )

        // Pre-load 100 blocks into fork tree (866881..866980)
        val preloadHeaders =
            (1 to preloadCount).map(i => BlockFixture.loadWithHeader(baseHeight + i)._2).toList
        val preloadHeadersScalus = prelude.List.from(preloadHeaders)
        val lastPreloadFixture = BlockFixture.load(baseHeight + preloadCount)
        val preloadTime = BigInt(lastPreloadFixture.timestamp)

        val stateWith100Blocks =
            BitcoinValidator.computeUpdate(
              state = initialState,
              blockHeaders = preloadHeadersScalus,
              parentPath = prelude.List.Nil,
              mpfInsertProofs = prelude.List.Nil,
              currentTime = preloadTime,
              params = testParams
            )

        // Add 1 new header (866981) and promote 1 block
        val newHeader = BlockFixture.loadWithHeader(baseHeight + preloadCount + 1)._2
        val newHeadersScalus = prelude.List(newHeader)
        val lastNewFixture = BlockFixture.load(baseHeight + preloadCount + 1)
        val lastNewTimestamp = BigInt(lastNewFixture.timestamp)
        val currentTime = lastNewTimestamp.max(preloadTime + testParams.challengeAging)

        val parentPath = prelude.List(BigInt(preloadCount - 1))
        val numPromotions = 1

        // Determine promoted block
        val ctx0 = stateWith100Blocks.ctx
        val newTree = BitcoinValidator.validateAndInsert(
          stateWith100Blocks.forkTree,
          parentPath,
          newHeadersScalus,
          ctx0,
          currentTime,
          testParams
        )
        val (_, bestDepth, bestPath) = BitcoinValidator.bestChainPath(
          newTree,
          stateWith100Blocks.ctx.height,
          BigInt(0)
        )
        val (promoted, _) =
            BitcoinValidator.promoteAndGC(
              newTree,
              ctx0,
              bestPath,
              bestDepth,
              currentTime,
              numPromotions,
              testParams
            )
        assert(
          promoted.length == numPromotions,
          s"Expected $numPromotions promotion, got ${promoted.length}"
        )

        // Generate MPF proof
        var mpf = OffChainMPF.empty.insert(confirmedTip, confirmedTip)
        val proofsBuilder = scala.collection.mutable.ListBuffer[prelude.List[ProofStep]]()
        promoted.foreach { block =>
            val proof = mpf.proveNonMembership(block.hash)
            proofsBuilder += proof
            mpf = mpf.insert(block.hash, block.hash)
        }
        val mpfProofsScalus = prelude.List.from(proofsBuilder.toList)

        val update = UpdateOracle(
          blockHeaders = newHeadersScalus,
          parentPath = parentPath,
          mpfInsertProofs = mpfProofsScalus
        )
        val expectedState =
            BitcoinValidator.computeUpdate(
              state = stateWith100Blocks,
              blockHeaders = newHeadersScalus,
              parentPath = parentPath,
              mpfInsertProofs = mpfProofsScalus,
              currentTime = currentTime,
              params = testParams
            )

        val input = Input(
          TransactionHash.fromHex(
            "1ab6879fc08345f51dc9571ac4f530bf8673e0d798758c470f9af6f98e2f3982"
          ),
          0
        )
        val refScriptInput = Input(
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

        val inputValue = nftValue(5)
        val utxo = Utxo(
          input,
          Output(
            testScriptAddr,
            inputValue,
            DatumOption.Inline(stateWith100Blocks.toData)
          )
        )
        val utxos: Utxos =
            Map(utxo.input -> utxo.output, refScriptUtxo.input -> refScriptUtxo.output)
        val validFrom = Instant.ofEpochSecond(currentTime.toLong)
        val validTo = validFrom.plusSeconds(600)

        val draft = txBuilder
            .references(refScriptUtxo, testContract)
            .spend(utxo, update.toData)
            .payTo(testScriptAddr, inputValue, expectedState.toData)
            .validFrom(validFrom)
            .validTo(validTo)
            .draft

        val txSize = draft.toCbor.length
        val scriptContext = draft.getScriptContextV3(utxos, ForSpend(input))

        val result = testProgram.applyArg(scriptContext.toData).evaluateDebug
        result match
            case r: Result.Success =>
                val cpuPct = r.budget.steps * 100.0 / maxTxCpu
                val memPct = r.budget.memory * 100.0 / maxTxMem
                val sizePct = txSize * 100.0 / maxTxSize
                val exFeeAda = r.budget.fee(prices).value / 1_000_000.0
                val txFeeAda = MinTransactionFee
                    .computeMinFee(draft, utxos, pp)
                    .getOrElse(throw new Exception("Failed to compute min fee"))
                    .value / 1_000_000.0

                println(result)
                println()
                println("=" * 80)
                println("BIFROST SCENARIO: 100 blocks in tree + 1 header + 1 promotion")
                println("=" * 80)
                println(f"  CPU Steps:    ${r.budget.steps}%,15d  ($cpuPct%5.1f%%)")
                println(f"  Memory:       ${r.budget.memory}%,15d  ($memPct%5.1f%%)")
                println(f"  Tx Size:      $txSize%,15d  ($sizePct%5.1f%%)")
                println(f"  Ex Fee:       $exFeeAda%15.6f ADA")
                println(f"  Tx Fee:       $txFeeAda%15.6f ADA")
                println("=" * 80)

                assert(cpuPct <= 100, "CPU budget exceeded")
                assert(memPct <= 100, "Memory budget exceeded")
                assert(sizePct <= 100, "Tx size exceeded")
            case r: Result.Failure =>
                fail(s"Evaluation failed: $r")
    }

    // =========================================================================
    // Oracle UTxO safety tests
    // =========================================================================

    /** Shared setup for safety tests: creates prevState, update, expectedState, input,
      * refScriptUtxo for a single-header submission. Returns everything needed to build a
      * transaction.
      */
    private def safetyTestSetup() = {
        val baseHeight = 866880
        val (baseFixture, _) = BlockFixture.loadWithHeader(baseHeight)
        val confirmedTip = ByteString.fromHex(baseFixture.hash).reverse
        val bits = ByteString.fromHex(baseFixture.bits).reverse
        val baseTimestamp = BigInt(baseFixture.timestamp)
        val recentTimestamps =
            prelude.List.from((0 until 11).map(i => baseTimestamp - i * 600).toList)

        val prevState = ChainState(
          confirmedBlocksRoot = BitcoinChainState.mpfRootForSingleBlock(confirmedTip),
          ctx = TraversalCtx(
            timestamps = recentTimestamps,
            height = baseFixture.height,
            currentBits = bits,
            prevDiffAdjTimestamp = baseTimestamp,
            lastBlockHash = confirmedTip
          ),
          forkTree = End
        )

        val header = BlockFixture.loadWithHeader(baseHeight + 1)._2
        val headersScalus = prelude.List(header)
        val fixture = BlockFixture.load(baseHeight + 1)
        val currentTime = BigInt(fixture.timestamp)

        val update = UpdateOracle(
          blockHeaders = headersScalus,
          parentPath = prelude.List.Nil,
          mpfInsertProofs = prelude.List.Nil
        )
        val expectedState =
            BitcoinValidator.computeUpdate(
              state = prevState,
              blockHeaders = headersScalus,
              parentPath = prelude.List.Nil,
              mpfInsertProofs = prelude.List.Nil,
              currentTime = currentTime,
              params = testParams
            )

        val input = Input(
          TransactionHash.fromHex(
            "1ab6879fc08345f51dc9571ac4f530bf8673e0d798758c470f9af6f98e2f3982"
          ),
          0
        )
        val refScriptUtxo = Utxo(
          Input(
            TransactionHash.fromHex(
              "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
            ),
            0
          ),
          Output(
            testScriptAddr,
            Value.ada(10),
            None,
            Some(ScriptRef(testContract.script))
          )
        )

        (prevState, update, expectedState, currentTime, input, refScriptUtxo)
    }

    test("Safety - staking address cannot change during oracle updates") {
        // The continuing output is found by address match + oracle NFT.
        // If someone tries to redirect the output to an address WITH a staking credential
        // (same script hash but different staking part), the address check rejects it.
        val (prevState, update, expectedState, currentTime, input, refScriptUtxo) =
            safetyTestSetup()

        val inputValue = nftValue(5)
        val utxo = Utxo(
          input,
          Output(testScriptAddr, inputValue, DatumOption.Inline(prevState.toData))
        )
        val utxos: Utxos =
            Map(utxo.input -> utxo.output, refScriptUtxo.input -> refScriptUtxo.output)

        // Build an output address with same script hash but WITH a staking credential
        val fakeStakeKeyHash =
            StakeKeyHash.fromByteString(ByteString.unsafeFromArray(Array.fill(28)(0xab.toByte)))
        val addrWithStaking = ShelleyAddress(
          env.network,
          ShelleyPaymentPart.Script(testContract.script.scriptHash),
          ShelleyDelegationPart.Key(fakeStakeKeyHash)
        )

        val validFrom = Instant.ofEpochSecond(currentTime.toLong)
        val validTo = validFrom.plusSeconds(600)
        val draft = txBuilder
            .references(refScriptUtxo, testContract)
            .spend(utxo, update.toData)
            .payTo(addrWithStaking, inputValue, expectedState.toData)
            .validFrom(validFrom)
            .validTo(validTo)
            .draft

        val scriptContext = draft.getScriptContextV3(utxos, ForSpend(input))
        val result = testProgram.applyArg(scriptContext.toData).evaluateDebug

        assert(
          result.isFailure,
          "Output to a different address (with staking credential) must be rejected"
        )
    }

    test("Safety - ADA value can only increase during oracle updates") {
        // The oracle UTxO's ADA value must not decrease during updates.
        // This prevents draining the oracle UTxO.
        val (prevState, update, expectedState, currentTime, input, refScriptUtxo) =
            safetyTestSetup()

        val inputValue = nftValue(10)
        val decreasedValue = nftValue(5) // less ADA than input

        val utxo = Utxo(
          input,
          Output(testScriptAddr, inputValue, DatumOption.Inline(prevState.toData))
        )
        val utxos: Utxos =
            Map(utxo.input -> utxo.output, refScriptUtxo.input -> refScriptUtxo.output)

        val validFrom = Instant.ofEpochSecond(currentTime.toLong)
        val validTo = validFrom.plusSeconds(600)
        val draft = txBuilder
            .references(refScriptUtxo, testContract)
            .spend(utxo, update.toData)
            .payTo(testScriptAddr, decreasedValue, expectedState.toData)
            .validFrom(validFrom)
            .validTo(validTo)
            .draft

        val scriptContext = draft.getScriptContextV3(utxos, ForSpend(input))
        val result = testProgram.applyArg(scriptContext.toData).evaluateDebug

        assert(
          result.isFailure,
          "Decreasing ADA value must be rejected"
        )
    }

    // =========================================================================
    // CloseOracle tests
    // =========================================================================

    /** Shared setup for CloseOracle tests. Creates a stale oracle with recentTimestamps.head set to
      * `lastBlockTimestamp`. Returns everything needed to build a close transaction.
      */
    private def closeOracleSetup(lastBlockTimestamp: BigInt) = {
        val prevState = ChainState(
          confirmedBlocksRoot = ByteString.unsafeFromArray(Array.fill(32)(0: Byte)),
          ctx = TraversalCtx(
            timestamps =
                prelude.List.from((0 until 11).map(i => lastBlockTimestamp - i * 600).toList),
            height = 866880,
            currentBits = ByteString.unsafeFromArray(Array.fill(4)(0xff.toByte)),
            prevDiffAdjTimestamp = lastBlockTimestamp - 11 * 600,
            lastBlockHash = ByteString.unsafeFromArray(Array.fill(32)(0: Byte))
          ),
          forkTree = ForkTree.End
        )

        val input = Input(
          TransactionHash.fromHex(
            "1ab6879fc08345f51dc9571ac4f530bf8673e0d798758c470f9af6f98e2f3982"
          ),
          0
        )
        val refScriptUtxo = Utxo(
          Input(
            TransactionHash.fromHex(
              "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
            ),
            0
          ),
          Output(
            testScriptAddr,
            Value.ada(10),
            None,
            Some(ScriptRef(testContract.script))
          )
        )

        (prevState, input, refScriptUtxo)
    }

    test("CloseOracle succeeds when oracle is stale and signed by owner") {
        // Oracle's last confirmed block timestamp is 60 days ago
        val lastBlockTimestamp = BigInt(1700000000)
        val currentTime = lastBlockTimestamp + 60 * 24 * 60 * 60 // 60 days later

        val (prevState, input, refScriptUtxo) = closeOracleSetup(lastBlockTimestamp)

        val inputValue = nftValue(5)
        val utxo = Utxo(
          input,
          Output(testScriptAddr, inputValue, DatumOption.Inline(prevState.toData))
        )
        val utxos: Utxos =
            Map(utxo.input -> utxo.output, refScriptUtxo.input -> refScriptUtxo.output)

        val redeemer = OracleAction.CloseOracle.toData
        val validFrom = Instant.ofEpochSecond(currentTime.toLong)
        val validTo = validFrom.plusSeconds(600)

        val draft = txBuilder
            .references(refScriptUtxo, testContract)
            .spend(utxo, redeemer)
            .mint(testContract, Map(AssetName.empty -> -1L), redeemer)
            .requireSignature(
              Party.Alice.addrKeyHash
            )
            .validFrom(validFrom)
            .validTo(validTo)
            .draft

        val scriptContext = draft.getScriptContextV3(utxos, ForSpend(input))
        val result = testProgram.applyArg(scriptContext.toData).evaluateDebug

        assert(
          result.isSuccess,
          s"CloseOracle should succeed when stale + signed + NFT burned, but got: $result"
        )
    }

    test("CloseOracle fails when oracle is not stale") {
        // Oracle's last confirmed block timestamp is recent (only 1 day ago)
        val lastBlockTimestamp = BigInt(1700000000)
        val currentTime = lastBlockTimestamp + 1 * 24 * 60 * 60 // only 1 day later (< 30 days)

        val (prevState, input, refScriptUtxo) = closeOracleSetup(lastBlockTimestamp)

        val inputValue = nftValue(5)
        val utxo = Utxo(
          input,
          Output(testScriptAddr, inputValue, DatumOption.Inline(prevState.toData))
        )
        val utxos: Utxos =
            Map(utxo.input -> utxo.output, refScriptUtxo.input -> refScriptUtxo.output)

        val redeemer = OracleAction.CloseOracle.toData
        val validFrom = Instant.ofEpochSecond(currentTime.toLong)
        val validTo = validFrom.plusSeconds(600)

        val draft = txBuilder
            .references(refScriptUtxo, testContract)
            .spend(utxo, redeemer)
            .mint(testContract, Map(AssetName.empty -> -1L), redeemer)
            .requireSignature(
              Party.Alice.addrKeyHash
            )
            .validFrom(validFrom)
            .validTo(validTo)
            .draft

        val scriptContext = draft.getScriptContextV3(utxos, ForSpend(input))
        val result = testProgram.applyArg(scriptContext.toData).evaluateDebug

        assert(
          result.isFailure,
          "CloseOracle should fail when oracle is not stale"
        )
    }

    test("CloseOracle fails when not signed by owner") {
        val lastBlockTimestamp = BigInt(1700000000)
        val currentTime = lastBlockTimestamp + 60 * 24 * 60 * 60

        val (prevState, input, refScriptUtxo) = closeOracleSetup(lastBlockTimestamp)

        val inputValue = nftValue(5)
        val utxo = Utxo(
          input,
          Output(testScriptAddr, inputValue, DatumOption.Inline(prevState.toData))
        )
        val utxos: Utxos =
            Map(utxo.input -> utxo.output, refScriptUtxo.input -> refScriptUtxo.output)

        val redeemer = OracleAction.CloseOracle.toData
        val validFrom = Instant.ofEpochSecond(currentTime.toLong)
        val validTo = validFrom.plusSeconds(600)

        // Use a wrong signer (different key hash)
        val wrongSigner = scalus.cardano.ledger.AddrKeyHash(
          hex"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
        )

        val draft = txBuilder
            .references(refScriptUtxo, testContract)
            .spend(utxo, redeemer)
            .mint(testContract, Map(AssetName.empty -> -1L), redeemer)
            .requireSignature(wrongSigner)
            .validFrom(validFrom)
            .validTo(validTo)
            .draft

        val scriptContext = draft.getScriptContextV3(utxos, ForSpend(input))
        val result = testProgram.applyArg(scriptContext.toData).evaluateDebug

        assert(
          result.isFailure,
          "CloseOracle should fail when not signed by oracle owner"
        )
    }

    test("CloseOracle fails when NFT is not burned") {
        val lastBlockTimestamp = BigInt(1700000000)
        val currentTime = lastBlockTimestamp + 60 * 24 * 60 * 60

        val (prevState, input, refScriptUtxo) = closeOracleSetup(lastBlockTimestamp)

        val inputValue = nftValue(5)
        val utxo = Utxo(
          input,
          Output(testScriptAddr, inputValue, DatumOption.Inline(prevState.toData))
        )
        val utxos: Utxos =
            Map(utxo.input -> utxo.output, refScriptUtxo.input -> refScriptUtxo.output)

        val redeemer = OracleAction.CloseOracle.toData
        val validFrom = Instant.ofEpochSecond(currentTime.toLong)
        val validTo = validFrom.plusSeconds(600)

        // No mint/burn — NFT is not burned
        val draft = txBuilder
            .references(refScriptUtxo, testContract)
            .spend(utxo, redeemer)
            .requireSignature(
              Party.Alice.addrKeyHash
            )
            .validFrom(validFrom)
            .validTo(validTo)
            .draft

        val scriptContext = draft.getScriptContextV3(utxos, ForSpend(input))
        val result = testProgram.applyArg(scriptContext.toData).evaluateDebug

        assert(
          result.isFailure,
          "CloseOracle should fail when NFT is not burned"
        )
    }

    test("Safety - ADA value increase is allowed during oracle updates") {
        // Positive test: increasing ADA should succeed.
        val (prevState, update, expectedState, currentTime, input, refScriptUtxo) =
            safetyTestSetup()

        val inputValue = nftValue(5)
        val increasedValue = nftValue(10) // more ADA than input

        val utxo = Utxo(
          input,
          Output(testScriptAddr, inputValue, DatumOption.Inline(prevState.toData))
        )
        val utxos: Utxos =
            Map(utxo.input -> utxo.output, refScriptUtxo.input -> refScriptUtxo.output)

        val validFrom = Instant.ofEpochSecond(currentTime.toLong)
        val validTo = validFrom.plusSeconds(600)
        val draft = txBuilder
            .references(refScriptUtxo, testContract)
            .spend(utxo, update.toData)
            .payTo(testScriptAddr, increasedValue, expectedState.toData)
            .validFrom(validFrom)
            .validTo(validTo)
            .draft

        val scriptContext = draft.getScriptContextV3(utxos, ForSpend(input))
        val result = testProgram.applyArg(scriptContext.toData).evaluateDebug

        assert(
          result.isSuccess,
          s"Increasing ADA value must be allowed, but got: $result"
        )
    }

    // =========================================================================
    // Full chain validation across all test fixtures
    // =========================================================================

    /** Validate a contiguous run of blocks using BitcoinValidator.validateBlock.
      *
      * Checks PoW, difficulty, MTP, future-time, and parent-hash for every block. At retarget
      * boundaries (height % 2016 == 0) the difficulty transition is validated against the real
      * Bitcoin chain.
      *
      * @param startHeight
      *   confirmed tip height (first block in the run, used as initial state)
      * @param endHeight
      *   last block height to validate (inclusive)
      * @param prevDiffAdjTimestamp
      *   timestamp of the epoch-start block for the epoch containing startHeight
      */
    private def validateBlockRange(
        startHeight: Int,
        endHeight: Int,
        prevDiffAdjTimestamp: BigInt
    ): Int = {
        val (startFixture, _) = BlockFixture.loadWithHeader(startHeight)
        val startHash = ByteString.fromHex(startFixture.hash).reverse
        val startBits = ByteString.fromHex(startFixture.bits).reverse
        val startTimestamp = BigInt(startFixture.timestamp)

        // Build recent timestamps: real tip timestamp + 10 synthetic predecessors.
        // Only used for MTP of the first ~10 blocks; real timestamps accumulate quickly.
        val recentTimestamps = prelude.List.from(
          (0 until 11).map(i => startTimestamp - i * 600).toList
        )

        var ctx = TraversalCtx(
          timestamps = recentTimestamps,
          height = BigInt(startHeight),
          currentBits = startBits,
          prevDiffAdjTimestamp = prevDiffAdjTimestamp,
          lastBlockHash = startHash
        )

        val totalBlocks = endHeight - startHeight
        var validatedCount = 0

        for height <- (startHeight + 1) to endHeight do {
            val (fixture, header) = BlockFixture.loadWithHeader(height)
            val currentTime = BigInt(fixture.timestamp)

            val (summary, newCtx, blockProof) =
                BitcoinValidator.validateBlock(header, ctx, currentTime, testParams)

            assert(
              summary.hash.reverse.toHex == fixture.hash,
              s"Hash mismatch at height $height"
            )

            ctx = newCtx
            validatedCount += 1
        }

        assert(validatedCount == totalBlocks, s"Expected $totalBlocks blocks, got $validatedCount")
        validatedCount
    }

    test(
      "Full chain validation - 205 fixture blocks pass Bitcoin consensus (PoW, difficulty, MTP)"
    ) {
        // Validates 866870-867075: 205 real Bitcoin mainnet blocks crossing the retarget
        // boundary at 866880 (= 2016 * 430). Each block is checked for: proof-of-work,
        // difficulty, median-time-past, future-time, and parent-hash continuity.

        val startHeight = 866870
        val endHeight = 867075

        // prevDiffAdjTimestamp = timestamp of block 864864 (= 2016 * 429), the epoch-start block.
        // Real Bitcoin mainnet value fetched from mempool.space API.
        val prevDiffAdj = BigInt(1728456399) // block 864864 timestamp

        val validated = validateBlockRange(startHeight, endHeight, prevDiffAdj)
        println(s"  Validated $validated blocks ($startHeight-$endHeight)")

        assert(validated == 205, s"Expected 205 validated blocks, got $validated")
    }

}
