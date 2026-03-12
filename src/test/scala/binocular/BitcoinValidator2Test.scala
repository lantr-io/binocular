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

class BitcoinValidator2Test extends AnyFunSuite with ScalusTest with ScalaCheckPropertyChecks {
    private given env: CardanoInfo = CardanoInfo.mainnet

    private val testTxOutRef = scalus.cardano.onchain.plutus.v3.TxOutRef(
      scalus.cardano.onchain.plutus.v3.TxId(
        hex"0000000000000000000000000000000000000000000000000000000000000000"
      ),
      BigInt(0)
    )

    private val testContract = {
        given Options = Options.release
        PlutusV3.compile(BitcoinValidator2.validate).withErrorTraces(testTxOutRef.toData)
    }
    private val testScriptAddr = testContract.address(env.network)
    private val testProgram = testContract.program.deBruijnedProgram

    test("BitcoinValidator2 size") {
        given Options = Options.release
        val contract = PlutusV3.compile(BitcoinValidator2.validate)
        println(s"Contract size: ${contract.script.script.size}")
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

        // Create initial ChainState2 with block 866880 as confirmed tip
        // Block 866880 is at retarget boundary, so previousDifficultyAdjustmentTimestamp is its own timestamp
        val prevState = ChainState2(
          blockHeight = baseFixture.height,
          blockHash = confirmedTip,
          currentTarget = bits,
          recentTimestamps = recentTimestamps,
          previousDifficultyAdjustmentTimestamp = baseTimestamp,
          confirmedBlocksRoot = BitcoinChainState.mpfRootForSingleBlock(confirmedTip),
          forksTree = ForkTree.End
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

            // Compute expected state using BitcoinValidator2.computeUpdate
            val update = UpdateOracle2(
              blockHeaders = headersScalus,
              parentPath = prelude.List.Nil, // parent is confirmed tip
              mpfInsertProofs = prelude.List.Nil
            )
            val expectedState =
                BitcoinValidator2.computeUpdate(prevState, update, lastTimestamp)

//            pprint.pprintln(expectedState)

            val redeemer = update.toData
//            pprint.pprintln(redeemer)

            val inputValue = Value.ada(5)
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

            val draft = txBuilder
                .references(refScriptUtxo, testContract)
                .spend(utxo, redeemer)
                .payTo(testScriptAddr, inputValue, expectedState.toData)
                .validFrom(validFrom)
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
        val initialState = ChainState2(
          blockHeight = baseFixture.height,
          blockHash = confirmedTip,
          currentTarget = bits,
          recentTimestamps = recentTimestamps,
          previousDifficultyAdjustmentTimestamp = baseTimestamp,
          confirmedBlocksRoot = BitcoinChainState.mpfRootForSingleBlock(confirmedTip),
          forksTree = ForkTree.End
        )

        // Pre-load 100 blocks into fork tree (866881..866980)
        val preloadHeaders =
            (1 to preloadCount).map(i => BlockFixture.loadWithHeader(baseHeight + i)._2).toList
        val preloadHeadersScalus = prelude.List.from(preloadHeaders)
        val lastPreloadFixture = BlockFixture.load(baseHeight + preloadCount)
        val preloadTime = BigInt(lastPreloadFixture.timestamp)

        val preloadUpdate = UpdateOracle2(
          blockHeaders = preloadHeadersScalus,
          parentPath = prelude.List.Nil,
          mpfInsertProofs = prelude.List.Nil
        )
        val stateWith100Blocks =
            BitcoinValidator2.computeUpdate(initialState, preloadUpdate, preloadTime)

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
            val currentTime = lastNewTimestamp.max(preloadTime + BitcoinValidator2.ChallengeAging)

            // Parent path: index 99 = last block in the 100-block Blocks node
            val parentPath = prelude.List(BigInt(preloadCount - 1))

            // Determine promoted blocks by running validator logic
            val ctx0 = BitcoinValidator2.initCtx(stateWith100Blocks)
            val newTree = BitcoinValidator2.validateAndInsert(
              stateWith100Blocks.forksTree,
              parentPath,
              newHeadersScalus,
              ctx0,
              currentTime
            )
            val (_, bestDepth, bestPath) = BitcoinValidator2.bestChainPath(
              newTree,
              stateWith100Blocks.blockHeight,
              BigInt(0)
            )
            val (promoted, _) =
                BitcoinValidator2.promoteAndGC(
                  newTree,
                  ctx0,
                  bestPath,
                  bestDepth,
                  currentTime,
                  count
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

            val update = UpdateOracle2(
              blockHeaders = newHeadersScalus,
              parentPath = parentPath,
              mpfInsertProofs = mpfProofsScalus
            )
            val expectedState =
                BitcoinValidator2.computeUpdate(stateWith100Blocks, update, currentTime)

            val redeemer = update.toData
            val inputValue = Value.ada(5)
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

            val draft = txBuilder
                .references(refScriptUtxo, testContract)
                .spend(utxo, redeemer)
                .payTo(testScriptAddr, inputValue, expectedState.toData)
                .validFrom(validFrom)
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

        val initialState = ChainState2(
          blockHeight = baseFixture.height,
          blockHash = confirmedTip,
          currentTarget = bits,
          recentTimestamps = recentTimestamps,
          previousDifficultyAdjustmentTimestamp = baseTimestamp,
          confirmedBlocksRoot = BitcoinChainState.mpfRootForSingleBlock(confirmedTip),
          forksTree = ForkTree.End
        )

        // Pre-load 100 blocks into fork tree (866881..866980)
        val preloadHeaders =
            (1 to preloadCount).map(i => BlockFixture.loadWithHeader(baseHeight + i)._2).toList
        val preloadHeadersScalus = prelude.List.from(preloadHeaders)
        val lastPreloadFixture = BlockFixture.load(baseHeight + preloadCount)
        val preloadTime = BigInt(lastPreloadFixture.timestamp)

        val preloadUpdate = UpdateOracle2(
          blockHeaders = preloadHeadersScalus,
          parentPath = prelude.List.Nil,
          mpfInsertProofs = prelude.List.Nil
        )
        val stateWith100Blocks =
            BitcoinValidator2.computeUpdate(initialState, preloadUpdate, preloadTime)

        // Add 1 new header (866981) and promote 1 block
        val newHeader = BlockFixture.loadWithHeader(baseHeight + preloadCount + 1)._2
        val newHeadersScalus = prelude.List(newHeader)
        val lastNewFixture = BlockFixture.load(baseHeight + preloadCount + 1)
        val lastNewTimestamp = BigInt(lastNewFixture.timestamp)
        val currentTime = lastNewTimestamp.max(preloadTime + BitcoinValidator2.ChallengeAging)

        val parentPath = prelude.List(BigInt(preloadCount - 1))
        val numPromotions = 1

        // Determine promoted block
        val ctx0 = BitcoinValidator2.initCtx(stateWith100Blocks)
        val newTree = BitcoinValidator2.validateAndInsert(
          stateWith100Blocks.forksTree,
          parentPath,
          newHeadersScalus,
          ctx0,
          currentTime
        )
        val (_, bestDepth, bestPath) = BitcoinValidator2.bestChainPath(
          newTree,
          stateWith100Blocks.blockHeight,
          BigInt(0)
        )
        val (promoted, _) =
            BitcoinValidator2.promoteAndGC(
              newTree,
              ctx0,
              bestPath,
              bestDepth,
              currentTime,
              numPromotions
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

        val update = UpdateOracle2(
          blockHeaders = newHeadersScalus,
          parentPath = parentPath,
          mpfInsertProofs = mpfProofsScalus
        )
        val expectedState =
            BitcoinValidator2.computeUpdate(stateWith100Blocks, update, currentTime)

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

        val inputValue = Value.ada(5)
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

        val draft = txBuilder
            .references(refScriptUtxo, testContract)
            .spend(utxo, update.toData)
            .payTo(testScriptAddr, inputValue, expectedState.toData)
            .validFrom(validFrom)
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

}
