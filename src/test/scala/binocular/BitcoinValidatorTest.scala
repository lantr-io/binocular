package binocular

import binocular.ForkTree.*
import binocular.OracleAction.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.*
import scalus.cardano.address.{ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.utils.MinTransactionFee
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry.ProofStep
import scalus.cardano.onchain.plutus.prelude
import scalus.cardano.onchain.plutus.v1.PubKeyHash
import scalus.cardano.txbuilder.RedeemerPurpose.ForSpend
import scalus.cardano.txbuilder.txBuilder
import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF
import scalus.testing.kit.TestUtil.getScriptContextV3
import scalus.testing.kit.{Party, ScalusTest}
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.hex
import scalus.uplc.builtin.Data.toData
import scalus.uplc.eval.*

import java.time.Instant

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

    private val testParams = BitcoinValidatorParams.makeMainnet(testTxOutRef, testOwner)

    private val testContract = {
//        given Options = Options.release
//        Options.release.copy(targetProtocolVersion = MajorProtocolVersion.vanRossemPV)
//        PlutusV3.compile(BitcoinValidator.validate).withErrorTraces(testParams.toData)
        BitcoinContract.makeContract(testParams).withErrorTraces
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
          addedTimeDelta = 0
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

    test("ForkTree pretty print - various tree shapes") {
        import binocular.ForkTreePretty.*

        // Use realistic-ish timestamps
        val baseTime = BigInt(1700000000L) // ~Nov 2023
        def block(id: Byte, heightOffset: Int): BlockSummary =
            BlockSummary(
              hash = ByteString.unsafeFromArray(Array.fill(32)(id)),
              timestamp = baseTime + heightOffset * 600,
              addedTimeDelta = 3600
            )

        // 1. Empty tree
        val emptyState = chainStateWithTree(End)
        println("=== Empty tree ===")
        println(emptyState.forkTree.pretty(emptyState.ctx.height))

        // 2. Linear chain
        val linearBlocks = prelude.List.from(
          (1 to 5).map(i => block(i.toByte, i)).toList
        )
        val linearTree = Blocks(linearBlocks, 500, End)
        println("=== Linear chain (5 blocks) ===")
        println(linearTree.pretty(866880))

        // 3. Simple fork
        val forkTree = Blocks(
          prelude.List.from((1 to 3).map(i => block(i.toByte, i)).toList),
          300,
          Fork(
            Blocks(
              prelude.List.from((4 to 6).map(i => block(i.toByte, i)).toList),
              400,
              End
            ),
            Blocks(
              prelude.List.from((7 to 8).map(i => block((i + 10).toByte, i - 3)).toList),
              200,
              End
            )
          )
        )
        println("=== Simple fork ===")
        println(forkTree.pretty(866880))

        // 4. Nested forks
        val nestedTree = Blocks(
          prelude.List.from((1 to 2).map(i => block(i.toByte, i)).toList),
          200,
          Fork(
            Blocks(
              prelude.List.from((3 to 4).map(i => block(i.toByte, i)).toList),
              250,
              Fork(
                Blocks(prelude.List(block(5, 5)), 150, End),
                Blocks(prelude.List(block(6, 5)), 100, End)
              )
            ),
            Blocks(prelude.List(block(7, 3)), 100, End)
          )
        )
        println("=== Nested forks ===")
        println(nestedTree.pretty(866880))

        // 5. With current time + confirmed blocks count
        val currentTime = baseTime + 250 * 60 // 250 minutes later
        println("=== With aging info + MPF size ===")
        println(
          forkTree.pretty(866880, currentTime = Some(currentTime), confirmedBlocks = Some(100_000))
        )

        // 6. Large linear chain
        val bigBlocks = prelude.List.from(
          (1 to 320).map(i => block((i % 256).toByte, i)).toList
        )
        val bigTree = Blocks(bigBlocks, 50000, End)
        println("=== Large linear chain (320 blocks) ===")
        println(bigTree.pretty(866880))

        // 7. No ANSI colors
        println("=== No colors ===")
        println(forkTree.pretty(866880, ansi = false))
    }

    test("BitcoinValidator size") {
        val contract = BitcoinContract.makeContract(testParams)
        info(s"Contract size: ${contract.script.script.size}")
//        println(contract.program.showHighlighted)
        assert(contract.script.script.size == 8207)
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

        val maxAvailableHeaders = 195 // fixtures available: 866881..867075
        var count = 0
        var withinLimits = true
        var lastFittingLine = ""
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

            val redeemer = update.toData

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
            // Validator reads validRange.to as currentTime; align validTo with the off-chain
            // currentTime used to compute expectedState.
            val validTo = Instant.ofEpochSecond(lastTimestamp.toLong)
            val validFrom = validTo.minusSeconds(600)
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
                    val line =
                        f"$count%8d | ${r.budget.steps}%15d | ${r.budget.memory}%12d | $cpuPct%6.1f%% | $memPct%6.1f%% | $exFeeAda%12.6f | $txFeeAda%12.6f | $txSize%8d | $sizePct%6.1f%% | $status%6s"
                    if withinLimits then lastFittingLine = line
                case r: Result.Failure =>
                    withinLimits = false
        }

        val maxHeadersPerTx = if withinLimits then count else count - 1

        info(s"BLOCK HEADER THROUGHPUT: max $maxHeadersPerTx headers/tx")
        if lastFittingLine.nonEmpty then
            info(
              f"${"Headers"}%8s | ${"CPU Steps"}%15s | ${"Memory"}%12s | ${"CPU %"}%7s | ${"Mem %"}%7s | ${"Ex Fee"}%12s | ${"Tx Fee"}%12s | ${"Tx Size"}%8s | ${"Size %"}%7s | ${"Status"}%6s"
            )
            info(lastFittingLine)

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

        val maxNewHeaders = 95 // fixtures available: 866981..867075
        var count = 0
        var withinLimits = true
        var lastFittingLine = ""

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
            // Validator reads validRange.to as currentTime; align validTo with the off-chain
            // currentTime used to compute expectedState.
            val validTo = Instant.ofEpochSecond(currentTime.toLong)
            val validFrom = validTo.minusSeconds(600)

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
                    val line =
                        f"$count%8d | $promotedCount%8d | ${r.budget.steps}%15d | ${r.budget.memory}%12d | $cpuPct%6.1f%% | $memPct%6.1f%% | $exFeeAda%12.6f | $txFeeAda%12.6f | $txSize%8d | $sizePct%6.1f%% | $status%6s"
                    if withinLimits then lastFittingLine = line
                case r: Result.Failure =>
                    withinLimits = false
        }

        val maxHeadersPerTx = if withinLimits then count else count - 1

        info(s"PROMOTION THROUGHPUT: max $maxHeadersPerTx headers+promotions/tx")
        if lastFittingLine.nonEmpty then
            info(
              f"${"Headers"}%8s | ${"Promoted"}%8s | ${"CPU Steps"}%15s | ${"Memory"}%12s | ${"CPU %"}%7s | ${"Mem %"}%7s | ${"Ex Fee"}%12s | ${"Tx Fee"}%12s | ${"Tx Size"}%8s | ${"Size %"}%7s | ${"Status"}%6s"
            )
            info(lastFittingLine)

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

        info(
          f"Single branch: max $maxBlocks blocks ($datumSize%,d bytes, limit $maxTxSize%,d)"
        )

        assert(maxBlocks == 368, s"Expected 368 blocks in single branch, got $maxBlocks")
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

        info(
          f"Left-leaning:  $maxForks forks, ${maxForks + 1} blocks ($forkDatumSize%,d bytes, limit $maxTxSize%,d)"
        )
        info(
          f"Balanced:      $balancedForks forks, $balancedBlocks blocks, depth $balancedDepth ($balancedSize%,d bytes) — min griefing cost"
        )

        assert(maxForks == 274, s"Expected 274 left-leaning forks, got $maxForks")
        assert(balancedDepth == 8, s"Expected balanced depth 8, got $balancedDepth")
        assert(
          testParams.maxBlocksInForkTree <= balancedBlocks,
          s"maxBlocksInForkTree (${testParams.maxBlocksInForkTree}) must be <= balanced tree capacity ($balancedBlocks)"
        )
    }

    test("ForkTree capacity with promotion - max blocks that allow promoting 1 block") {
        val pp = CardanoInfo.mainnet.protocolParams
        val maxTxSize = pp.maxTxSize

        // Build an MPF with 100,000 entries to get realistic proof sizes
        println("\nBuilding MPF with 100,000 entries...")
        var mpf = OffChainMPF.empty
        val numMpfEntries = 100_000
        for i <- 0 until numMpfEntries do {
            val key = ByteString.unsafeFromArray {
                val arr = Array.fill(32)(0: Byte)
                arr(0) = (i >> 24).toByte
                arr(1) = (i >> 16).toByte
                arr(2) = (i >> 8).toByte
                arr(3) = i.toByte
                arr
            }
            mpf = mpf.insert(key, key)
        }
        println(s"MPF built with root: ${mpf.rootHash.toHex}")

        // Generate a non-membership proof for a new key (simulates promoting 1 block)
        val newBlockHash = ByteString.unsafeFromArray {
            val arr = Array.fill(32)(0xff.toByte)
            arr(0) = 0xab.toByte
            arr
        }
        val proof = mpf.proveNonMembership(newBlockHash)
        val proofData = prelude.List.from(List(proof)).toData
        val proofSize = proofData.toCbor.length
        println(s"MPF proof for 1 block: $proofSize bytes (${proof.length} steps)")

        // Build a dummy UpdateOracle redeemer with:
        //   - 0 new block headers (promotion-only tx)
        //   - empty parent path
        //   - 1 MPF insert proof
        val redeemer = UpdateOracle(
          prelude.List.Nil, // no new headers
          prelude.List.Nil, // empty parent path
          prelude.List.from(List(proof))
        )
        val redeemerSize = redeemer.toData.toCbor.length
        println(s"Redeemer size (0 headers, 1 promotion proof): $redeemerSize bytes")

        // Measure transaction overhead with a reference script UTxO
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

        val oracleInput = Input(
          TransactionHash.fromHex(
            "1ab6879fc08345f51dc9571ac4f530bf8673e0d798758c470f9af6f98e2f3982"
          ),
          0
        )

        def buildTxDraft(forkTreeBlocks: Int): (Int, Int, Int) = {
            // Build input ChainState with the fork tree
            val inputState = chainStateWithTree(
              Blocks(
                prelude.List.from(
                  (1 to forkTreeBlocks).map(i => dummyBlock(i.toByte)).toList
                ),
                100,
                End
              )
            )

            // Output state: 1 block promoted (removed from tree head), rest remain
            val outputTree =
                if forkTreeBlocks > 1 then
                    Blocks(
                      prelude.List.from(
                        (2 to forkTreeBlocks).map(i => dummyBlock(i.toByte)).toList
                      ),
                      100,
                      End
                    )
                else End

            val outputState = inputState.copy(
              ctx = inputState.ctx.copy(height = inputState.ctx.height + 1),
              forkTree = outputTree
            )

            val inputValue = nftValue(5)
            val utxo = Utxo(
              oracleInput,
              Output(testScriptAddr, inputValue, DatumOption.Inline(inputState.toData))
            )

            val validFrom = Instant.ofEpochSecond(1000000L)
            val validTo = validFrom.plusSeconds(600)

            val draft = txBuilder
                .references(refScriptUtxo, testContract)
                .spend(utxo, redeemer)
                .payTo(testScriptAddr, inputValue, outputState.toData)
                .validFrom(validFrom)
                .validTo(validTo)
                .draft

            val txSize = draft.toCbor.length
            val inputDatumSize = inputState.toData.toCbor.length
            val outputDatumSize = outputState.toData.toCbor.length
            (txSize, inputDatumSize, outputDatumSize)
        }

        // Binary search for the max fork tree blocks that fit with promotion
        var lo = 1
        var hi = 368 // known max without promotion overhead
        while lo < hi do {
            val mid = (lo + hi + 1) / 2
            val (txSize, _, _) = buildTxDraft(mid)
            if txSize <= maxTxSize then lo = mid else hi = mid - 1
        }
        val maxBlocks = lo
        val (txSize, inputDatumSize, outputDatumSize) = buildTxDraft(maxBlocks)
        val (overTxSize, _, _) = buildTxDraft(maxBlocks + 1)

        info(f"Max fork tree blocks with 1-block promotion (100K MPF): $maxBlocks")
        info(f"  Transaction size:  $txSize%,d bytes (limit: $maxTxSize%,d)")
        info(f"  Input datum size:  $inputDatumSize%,d bytes")
        info(f"  Output datum size: $outputDatumSize%,d bytes")
        info(f"  Redeemer size:     $redeemerSize%,d bytes")
        info(
          f"  Overhead:          ${txSize - inputDatumSize - outputDatumSize - redeemerSize}%,d bytes"
        )
        info(f"  maxBlocks+1 tx:    $overTxSize%,d bytes (over limit)")

        // The max should be significantly less than 368 (no-promotion max)
        assert(
          maxBlocks < 368,
          s"Expected fewer blocks than 368 to fit with promotion overhead, got $maxBlocks"
        )
        assert(
          maxBlocks > 100,
          s"Expected at least 100 blocks to fit with promotion, got $maxBlocks"
        )
    }

    test("Validity interval - unbounded validRange (no validTo) must be rejected") {
        // Without a validTo upper bound, the on-chain time (validFrom) could be
        // arbitrarily stale — the transaction remains valid forever. This weakens
        // the challenge aging guarantee: addedTimeDelta could be computed from a much
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
        // addedTimeDelta is computed from a time close to actual wall-clock time.
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
        val maxTxSize = CardanoInfo.mainnet.protocolParams.maxTxSize

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
        val feePayerUtxo = Utxo(
          Input(
            TransactionHash.fromHex(
              "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
            ),
            0
          ),
          Output(Party.Alice.address, Value.ada(50))
        )
        val collateralUtxo = Utxo(
          Input(
            TransactionHash.fromHex(
              "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
            ),
            0
          ),
          Output(Party.Alice.address, Value.ada(5))
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
        // Validator reads validRange.to as currentTime; align validTo with the off-chain
        // currentTime used to compute expectedState.
        val validTo = Instant.ofEpochSecond(currentTime.toLong)
        val validFrom = validTo.minusSeconds(600)

        val built = txBuilder
            .spend(feePayerUtxo)
            .collaterals(collateralUtxo)
            .references(refScriptUtxo, testContract)
            .spend(utxo, update.toData)
            .payTo(testScriptAddr, inputValue, expectedState.toData)
            .validFrom(validFrom)
            .validTo(validTo)
            .build(changeTo = Party.Alice.address)

        val pp = CardanoInfo.mainnet.protocolParams
        val tx = built.transaction
        val txSize = tx.toCbor.length
        val txFeeAda = tx.body.value.fee.value / 1_000_000.0
        val exUnits = tx.witnessSet.redeemers.map(_.value.totalExUnits).getOrElse(ExUnits.zero)
        val exFeeAda = exUnits.fee(pp.executionUnitPrices).value / 1_000_000.0
        val maxTxCpu = pp.maxTxExecutionUnits.steps
        val maxTxMem = pp.maxTxExecutionUnits.memory

        info("BIFROST SCENARIO: 100 blocks in tree + 1 header + 1 promotion")
        info(f"  CPU Steps:    ${exUnits.steps}%,15d  (${exUnits.steps * 100.0 / maxTxCpu}%5.1f%%)")
        info(
          f"  Memory:       ${exUnits.memory}%,15d  (${exUnits.memory * 100.0 / maxTxMem}%5.1f%%)"
        )
        info(f"  Tx Size:      $txSize%,15d  (${txSize * 100.0 / maxTxSize}%5.1f%%)")
        info(f"  Ex Fee:       $exFeeAda%15.6f ADA")
        info(f"  Tx Fee:       $txFeeAda%15.6f ADA")

        assert(txSize <= maxTxSize, "Tx size exceeded")
        assert(
          tx.body.value.fee == Coin(943423),
          s"Tx fee ${tx.body.value.fee} != 943423 lovelace"
        )
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

        // Validator reads validRange.to as currentTime; align validTo with the off-chain
        // currentTime used to compute expectedState.
        val validTo = Instant.ofEpochSecond(currentTime.toLong)
        val validFrom = validTo.minusSeconds(600)
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

        // Validator reads validRange.to as currentTime; align validTo with the off-chain
        // currentTime used to compute expectedState.
        val validTo = Instant.ofEpochSecond(currentTime.toLong)
        val validFrom = validTo.minusSeconds(600)
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
        // Validator reads validRange.to as currentTime; align validTo with the off-chain
        // currentTime used to compute expectedState.
        val validTo = Instant.ofEpochSecond(currentTime.toLong)
        val validFrom = validTo.minusSeconds(600)

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
        // Validator reads validRange.to as currentTime; align validTo with the off-chain
        // currentTime used to compute expectedState.
        val validTo = Instant.ofEpochSecond(currentTime.toLong)
        val validFrom = validTo.minusSeconds(600)

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
        // Validator reads validRange.to as currentTime; align validTo with the off-chain
        // currentTime used to compute expectedState.
        val validTo = Instant.ofEpochSecond(currentTime.toLong)
        val validFrom = validTo.minusSeconds(600)

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
        // Validator reads validRange.to as currentTime; align validTo with the off-chain
        // currentTime used to compute expectedState.
        val validTo = Instant.ofEpochSecond(currentTime.toLong)
        val validFrom = validTo.minusSeconds(600)

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

        // Validator reads validRange.to as currentTime; align validTo with the off-chain
        // currentTime used to compute expectedState.
        val validTo = Instant.ofEpochSecond(currentTime.toLong)
        val validFrom = validTo.minusSeconds(600)
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

    test("Duplicate block insertion must be rejected") {
        val baseHeight = 866880

        // Load confirmed tip (866880)
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

        // Insert block 866881
        val (_, header881) = BlockFixture.loadWithHeader(baseHeight + 1)
        val fixture881 = BlockFixture.load(baseHeight + 1)
        val headersScalus = prelude.List(header881)

        val stateAfterInsert = BitcoinValidator.computeUpdate(
          state = initialState,
          blockHeaders = headersScalus,
          parentPath = prelude.List.Nil,
          mpfInsertProofs = prelude.List.Nil,
          currentTime = BigInt(fixture881.timestamp),
          params = testParams
        )

        // Attempt to insert block 866881 again — same parentPath (confirmed tip) triggers
        // a root fork where existsAsChild detects the duplicate
        val ex = intercept[Exception] {
            BitcoinValidator.computeUpdate(
              state = stateAfterInsert,
              blockHeaders = headersScalus,
              parentPath = prelude.List.Nil,
              mpfInsertProofs = prelude.List.Nil,
              currentTime = BigInt(fixture881.timestamp),
              params = testParams
            )
        }
        assert(
          ex.getMessage.contains("Block already exists"),
          s"Unexpected error: ${ex.getMessage}"
        )
    }

    test("validateBlock rejects headers with incorrect nBits encoding") {
        val startHeight = 866880
        val (startFixture, _) = BlockFixture.loadWithHeader(startHeight)
        val startHash = ByteString.fromHex(startFixture.hash).reverse
        val startBits = ByteString.fromHex(startFixture.bits).reverse
        val startTimestamp = BigInt(startFixture.timestamp)
        val recentTimestamps = prelude.List.from(
          (0 until 11).map(i => startTimestamp - i * 600).toList
        )

        val ctx = TraversalCtx(
          timestamps = recentTimestamps,
          height = BigInt(startHeight),
          currentBits = startBits,
          prevDiffAdjTimestamp = startTimestamp,
          lastBlockHash = startHash
        )

        val (fixture, header) = BlockFixture.loadWithHeader(startHeight + 1)
        val wrongBits = ByteString.fromHex("1d00ffff").reverse
        val mutatedHeader =
            binocular.BlockHeader(
              header.bytes.slice(0, 72) ++ wrongBits ++ header.bytes.slice(76, 4)
            )

        val ex = intercept[Exception] {
            BitcoinValidator.validateBlock(
              mutatedHeader,
              ctx,
              BigInt(fixture.timestamp),
              testParams
            )
        }

        assert(
          ex.getMessage.contains("Incorrect difficulty bits"),
          s"Unexpected error: ${ex.getMessage}"
        )
    }

    test("validateBlock rejects non-80-byte headers") {
        val startHeight = 866880
        val (startFixture, _) = BlockFixture.loadWithHeader(startHeight)
        val startHash = ByteString.fromHex(startFixture.hash).reverse
        val startBits = ByteString.fromHex(startFixture.bits).reverse
        val startTimestamp = BigInt(startFixture.timestamp)
        val recentTimestamps = prelude.List.from(
          (0 until 11).map(i => startTimestamp - i * 600).toList
        )

        val ctx = TraversalCtx(
          timestamps = recentTimestamps,
          height = BigInt(startHeight),
          currentBits = startBits,
          prevDiffAdjTimestamp = startTimestamp,
          lastBlockHash = startHash
        )

        val (fixture, header) = BlockFixture.loadWithHeader(startHeight + 1)
        val extendedHeader = binocular.BlockHeader(header.bytes ++ hex"00010203")

        val ex = intercept[Exception] {
            BitcoinValidator.validateBlock(
              extendedHeader,
              ctx,
              BigInt(fixture.timestamp),
              testParams
            )
        }

        assert(
          ex.getMessage.contains("Invalid block header length"),
          s"Unexpected error: ${ex.getMessage}"
        )
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
