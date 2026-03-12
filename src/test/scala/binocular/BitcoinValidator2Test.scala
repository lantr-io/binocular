package binocular

import binocular.BitcoinHelpers.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.*
import scalus.cardano.ledger.{CardanoInfo, DatumOption, TransactionHash, TransactionInput, TransactionOutput, Utxo, Utxos, Value}
import scalus.cardano.onchain.plutus.prelude
import scalus.cardano.txbuilder.RedeemerPurpose.ForSpend
import scalus.cardano.txbuilder.txBuilder
import scalus.compiler.Options
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
        assert(contract.script.script.size == 7381)
    }

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

        // Build 11 recent timestamps (newest first, reverse-sorted)
        val baseTimestamp = BigInt(baseFixture.timestamp)
        val recentTimestamps =
            prelude.List.from((0 until 11).map(i => baseTimestamp - i * 600).toList)

        // Create initial ChainState2 with block 864800 as confirmed tip
        val prevState = ChainState2(
          blockHeight = baseFixture.height,
          blockHash = confirmedTip,
          currentTarget = bits,
          recentTimestamps = recentTimestamps,
          previousDifficultyAdjustmentTimestamp =
              BigInt(baseFixture.timestamp) - 600 * DifficultyAdjustmentInterval,
          confirmedBlocksRoot = BitcoinChainState.mpfRootForSingleBlock(confirmedTip),
          forksTree = ForkTree.End
        )

        val input = TransactionInput(
          TransactionHash.fromHex(
            "1ab6879fc08345f51dc9571ac4f530bf8673e0d798758c470f9af6f98e2f3982"
          ),
          0
        )

        println()
        println("=" * 100)
        println("BLOCK HEADER THROUGHPUT TEST (V2)")
        println("=" * 100)
        println(
          f"${"Headers"}%8s | ${"CPU Steps"}%15s | ${"Memory"}%12s | ${"CPU %"}%7s | ${"Mem %"}%7s | ${"Fee (ADA)"}%12s | ${"Tx Size"}%8s | ${"Size %"}%7s | ${"Status"}%6s"
        )
        println("-" * 100)

        var count = 0
        var withinLimits = true
        while withinLimits do {
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

            pprint.pprintln(expectedState)

            val redeemer = update.toData
//            pprint.pprintln(redeemer)

            val inputValue = Value.ada(5)
            val utxo = Utxo(
              input,
              TransactionOutput(
                testScriptAddr,
                inputValue,
                DatumOption.Inline(prevState.toData)
              )
            )
            val utxos: Utxos = Map(utxo.input -> utxo.output)
            val validFrom = Instant.ofEpochSecond(lastTimestamp.toLong)

            val draft = txBuilder
                .spend(utxo, redeemer, testContract)
                .payTo(testScriptAddr, inputValue, expectedState.toData)
                .validFrom(validFrom)
                .draft

            val txSize = draft.toCbor.length
            val scriptContext = draft.getScriptContextV3(utxos, ForSpend(input))

            val result = testProgram $ scriptContext.toData
            result.evaluateDebug match
                case r: Result.Success =>
                    val cpuPct = r.budget.steps * 100.0 / maxTxCpu
                    val memPct = r.budget.memory * 100.0 / maxTxMem
                    val sizePct = txSize * 100.0 / maxTxSize
                    val feeAda = r.budget.fee(prices).value / 1_000_000.0
                    withinLimits = cpuPct <= 100 && memPct <= 100 && sizePct <= 100
                    val status = if withinLimits then "OK" else {
                        println(r)
                        "OVER"
                    }
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
          maxHeadersPerTx > 0,
          "Should be able to fit at least 1 block header per transaction"
        )
    }
}
