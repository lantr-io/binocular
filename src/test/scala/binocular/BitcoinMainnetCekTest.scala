package binocular

import org.scalatest.Tag
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{AssetName, CardanoInfo, Coin, DatumOption, Input, Output, ScriptRef, TransactionHash, Utxos, Value}
import scalus.cardano.ledger.Utxo as CardanoUtxo
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
import scalus.uplc.eval.Result

import java.time.Instant
import binocular.OracleAction.*
import scalus.cardano.onchain.plutus.v1.PubKeyHash
import scalus.testing.kit.Party

import scala.concurrent.ExecutionContext
import scalus.utils.await
import scalus.utils.Hex.hexToBytes

object ManualTest extends Tag("binocular.ManualTest")

class BitcoinMainnetCekTest extends AnyFunSuite with ScalusTest {
    private given ec: ExecutionContext = ExecutionContext.global
    private given env: CardanoInfo = CardanoInfo.mainnet

    private val testTxOutRef = scalus.cardano.onchain.plutus.v3.TxOutRef(
      scalus.cardano.onchain.plutus.v3.TxId(
        hex"0000000000000000000000000000000000000000000000000000000000000000"
      ),
      BigInt(0)
    )
    private val testOwner = PubKeyHash(Party.Alice.addrKeyHash)

    private val testParams = BitcoinValidatorParams.makeMainnet(testTxOutRef, testOwner)

    private val testContract = {
        given Options = Options.release
        PlutusV3.compile(BitcoinValidator.validate).withErrorTraces(testParams.toData)
    }
    private val testScriptAddr = testContract.address(env.network)
    private val testScriptHash = testContract.script.scriptHash
    private val testProgram = testContract.program.deBruijnedProgram

    private def nftValue(adaAmount: Long): Value =
        Value.asset(testScriptHash, AssetName.empty, 1, Coin.ada(adaAmount))

    private val oracleInput = Input(
      TransactionHash.fromHex(
        "1ab6879fc08345f51dc9571ac4f530bf8673e0d798758c470f9af6f98e2f3982"
      ),
      0
    )
    private val refScriptUtxo = CardanoUtxo(
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
    private val inputValue = nftValue(5)

    /** Fetch stale tips from getchaintips that fall within our height range. */
    private def fetchStaleForks(
        rpc: SimpleBitcoinRpc,
        minHeight: Int,
        maxHeight: Int
    ): Map[Int, Seq[String]] = {
        val tips = rpc.getChainTips().await()
        val staleTips = tips.filter { tip =>
            (tip.status == "valid-fork" || tip.status == "headers-only" || tip.status == "valid-headers") &&
            tip.height >= minHeight && tip.height <= maxHeight
        }

        val orphansByForkHeight = scala.collection.mutable.Map[Int, Seq[String]]()
        for tip <- staleTips do {
            val forkPointHeight = tip.height - tip.branchlen
            if forkPointHeight >= minHeight then {
                val headers = scala.collection.mutable.ListBuffer[String]()
                var hash = tip.hash
                for _ <- 0 until tip.branchlen do {
                    headers.prepend(hash)
                    val hdr = rpc.getBlockHeader(hash).await()
                    hash = hdr.previousblockhash.getOrElse("")
                }
                orphansByForkHeight(forkPointHeight) = headers.toSeq
            }
        }
        orphansByForkHeight.toMap
    }

    /** CEK-evaluate a single oracle update and return (newState, updatedMpf, result). */
    private def cekEvaluateUpdate(
        prevState: ChainState,
        headers: prelude.List[BlockHeader],
        parentPath: prelude.List[BigInt],
        currentTime: BigInt,
        mpf: OffChainMPF
    ): (ChainState, OffChainMPF, Result) = {
        val (expectedState, mpfProofs, updatedMpf) =
            OracleTransactions.computeUpdateWithProofs(
              prevState,
              headers,
              parentPath,
              currentTime,
              mpf,
              testParams
            )

        val update = UpdateOracle(
          blockHeaders = headers,
          parentPath = parentPath,
          mpfInsertProofs = mpfProofs
        )

        val utxo = CardanoUtxo(
          oracleInput,
          Output(testScriptAddr, inputValue, DatumOption.Inline(prevState.toData))
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

        val scriptContext = draft.getScriptContextV3(utxos, ForSpend(oracleInput))
        val result = testProgram.applyArg(scriptContext.toData).evaluateDebug

        (expectedState, updatedMpf, result)
    }

    private def prettyTree(tree: ForkTree, height: BigInt): String =
        ForkTreePretty.pretty(tree)(height, ansi = false)

    /** Convert RPC big-endian hash hex to internal little-endian ByteString. */
    private def rpcHashToInternal(hashHex: String): ByteString =
        ByteString.fromArray(hashHex.hexToBytes.reverse)

    /** Parse RPC chainwork hex (big-endian, no 0x prefix) to BigInt. */
    private def parseChainwork(hex: String): BigInt =
        BigInt(hex, 16)

    /** Find the path to a block with the given hash in the fork tree.
      *
      * Returns the insertion path (as used by validateAndInsert) to the block matching
      * `targetHash`, or None if the hash is not found.
      */
    private def findPathToHash(
        tree: ForkTree,
        targetHash: BlockHash
    ): Option[prelude.List[BigInt]] = {
        tree match {
            case ForkTree.Blocks(blocks, _, next) =>
                val blocksList = blocks.toScalaList
                blocksList.zipWithIndex.find(_._1.hash == targetHash) match {
                    case Some((_, idx)) => Some(prelude.List(BigInt(idx)))
                    case None =>
                        findPathToHash(next, targetHash) match {
                            case Some(subPath) =>
                                Some(prelude.List.Cons(BigInt(blocksList.size), subPath))
                            case None => None
                        }
                }
            case ForkTree.Fork(left, right) =>
                findPathToHash(left, targetHash) match {
                    case Some(subPath) => Some(prelude.List.Cons(BigInt(0), subPath))
                    case None =>
                        findPathToHash(right, targetHash) match {
                            case Some(subPath) => Some(prelude.List.Cons(BigInt(1), subPath))
                            case None          => None
                        }
                }
            case ForkTree.End => None
        }
    }

    /** Collect blocks along the best chain path only (excludes orphan/stale branches). */
    private def bestChainBlocks(tree: ForkTree): scala.List[BlockSummary] = {
        tree match {
            case ForkTree.Blocks(blocks, _, next) =>
                blocks.toScalaList ++ bestChainBlocks(next)
            case ForkTree.Fork(left, right) =>
                val (leftWork, _, _) = BitcoinValidator.bestChainPath(left, 0, 0)
                val (rightWork, _, _) = BitcoinValidator.bestChainPath(right, 0, 0)
                if leftWork >= rightWork then bestChainBlocks(left)
                else bestChainBlocks(right)
            case ForkTree.End => scala.Nil
        }
    }

    /** Verify oracle state fields against Bitcoin RPC data.
      *
      * Checks confirmed tip (hash, bits, timestamp, timestamps, prevDiffAdjTimestamp) and fork tree
      * tip (hash, chainwork).
      */
    private def verifyStateAgainstRpc(
        state: ChainState,
        rpc: SimpleBitcoinRpc,
        batchLabel: String,
        fullCheck: Boolean
    ): Unit = {
        val confirmedHeight = state.ctx.height.toInt

        // --- Confirmed tip verification ---
        val confirmedHashHex = rpc.getBlockHash(confirmedHeight).await()
        val confirmedHeader = rpc.getBlockHeader(confirmedHashHex).await()

        // 1. Confirmed tip hash
        val expectedHash = rpcHashToInternal(confirmedHeader.hash)
        assert(
          state.ctx.lastBlockHash == expectedHash,
          s"[$batchLabel] Confirmed tip hash mismatch at height $confirmedHeight: " +
              s"state=${state.ctx.lastBlockHash.toHex} rpc=${expectedHash.toHex}"
        )

        // 2. Confirmed tip bits
        val expectedBits = BitcoinChainState.rpcBitsToCompactBits(confirmedHeader.bits)
        assert(
          state.ctx.currentBits == expectedBits,
          s"[$batchLabel] Confirmed tip bits mismatch at height $confirmedHeight: " +
              s"state=${state.ctx.currentBits.toHex} rpc=${expectedBits.toHex}"
        )

        // 3. Newest timestamp matches confirmed tip
        assert(
          state.ctx.timestamps.head == BigInt(confirmedHeader.time),
          s"[$batchLabel] Newest timestamp mismatch at height $confirmedHeight: " +
              s"state=${state.ctx.timestamps.head} rpc=${confirmedHeader.time}"
        )

        // 4. prevDiffAdjTimestamp matches the last difficulty adjustment block
        val interval = BitcoinHelpers.DifficultyAdjustmentInterval.toInt
        val adjHeight = confirmedHeight - (confirmedHeight % interval)
        val adjHashHex = rpc.getBlockHash(adjHeight).await()
        val adjHeader = rpc.getBlockHeader(adjHashHex).await()
        assert(
          state.ctx.prevDiffAdjTimestamp == BigInt(adjHeader.time),
          s"[$batchLabel] prevDiffAdjTimestamp mismatch: " +
              s"state=${state.ctx.prevDiffAdjTimestamp} rpc=${adjHeader.time} (adj block #$adjHeight)"
        )

        // --- Fork tree tip verification ---
        val (bestChainwork, bestDepth, _) =
            BitcoinValidator.bestChainPath(state.forkTree, confirmedHeight, 0)
        val bestTipHeight = bestDepth.toInt

        if state.forkTree != ForkTree.End then {
            val tipHashHex = rpc.getBlockHash(bestTipHeight).await()
            val tipHeader = rpc.getBlockHeader(tipHashHex).await()

            // 5. Fork tree best-chain tip hash
            val bestBlocks = bestChainBlocks(state.forkTree)
            if bestBlocks.nonEmpty then {
                val treeTipHash = bestBlocks.last.hash
                val expectedTipHash = rpcHashToInternal(tipHeader.hash)
                assert(
                  treeTipHash == expectedTipHash,
                  s"[$batchLabel] Fork tree tip hash mismatch at height $bestTipHeight: " +
                      s"tree=${treeTipHash.toHex} rpc=${expectedTipHash.toHex}"
                )
            }

            // 6. Chainwork: fork tree best-path chainwork == rpc(tip) - rpc(confirmed)
            for {
                tipCwHex <- tipHeader.chainwork
                confirmedCwHex <- confirmedHeader.chainwork
            } {
                val expectedCw = parseChainwork(tipCwHex) - parseChainwork(confirmedCwHex)
                assert(
                  bestChainwork == expectedCw,
                  s"[$batchLabel] Chainwork mismatch: tree=$bestChainwork " +
                      s"expected=$expectedCw (rpc tip=$tipCwHex confirmed=$confirmedCwHex)"
                )
            }
        }

        // --- Full checks (less frequent) ---
        if fullCheck then {
            // 7. All 11 MTP timestamps
            val timestamps = state.ctx.timestamps.toScalaList
            for (ts, i) <- timestamps.zipWithIndex do {
                val h = confirmedHeight - i
                if h >= 0 then {
                    val hashHex = rpc.getBlockHash(h).await()
                    val hdr = rpc.getBlockHeader(hashHex).await()
                    assert(
                      ts == BigInt(hdr.time),
                      s"[$batchLabel] Timestamp[$i] mismatch at height $h: " +
                          s"state=$ts rpc=${hdr.time}"
                    )
                }
            }

            // 8. All best-chain fork tree block hashes
            val bestTreeBlocks = bestChainBlocks(state.forkTree)
            for (block, i) <- bestTreeBlocks.zipWithIndex do {
                val blockHeight = confirmedHeight + 1 + i
                val hashHex = rpc.getBlockHash(blockHeight.toInt).await()
                val expectedBlockHash = rpcHashToInternal(hashHex)
                assert(
                  block.hash == expectedBlockHash,
                  s"[$batchLabel] Fork tree block hash mismatch at height $blockHeight: " +
                      s"tree=${block.hash.toHex} rpc=${expectedBlockHash.toHex}"
                )
            }
        }
    }

    test("CEK validation of real Bitcoin mainnet blocks from RPC", ManualTest) {
        val config = BinocularConfig.load()
        assume(config.bitcoinNode.url.nonEmpty, "Bitcoin RPC not configured — skipping")

        val rpc = new SimpleBitcoinRpc(config.bitcoinNode)
        val startHeight = 0
        val batchSize = 50
        val tipHeight = rpc.getBlockchainInfo().await().blocks
        val totalBlocks =
            sys.env.get("BINOCULAR_CEK_BLOCK_COUNT").map(_.toInt).getOrElse(tipHeight - startHeight)

        println(s"=== Bitcoin Mainnet CEK Test ===")
        println(s"Start height: $startHeight, batch size: $batchSize, total blocks: $totalBlocks")

        // Fetch stale forks in our range
        val staleForks =
            try {
                fetchStaleForks(rpc, startHeight, startHeight + totalBlocks)
            } catch {
                case _: Exception =>
                    println("  (getchaintips not available — skipping orphan detection)")
                    Map.empty[Int, Seq[String]]
            }
        if staleForks.nonEmpty then {
            println(s"Found ${staleForks.size} stale fork(s):")
            staleForks.foreach { case (forkHeight, hashes) =>
                println(s"  Fork at height $forkHeight: ${hashes.size} orphan block(s)")
            }
        } else {
            println("No stale forks found in range")
        }

        // Initialize state
        val initialState =
            BitcoinChainState.getInitialChainState(rpc, startHeight).await()
        var state = initialState
        var mpf = OffChainMPF.empty.insert(
          state.ctx.lastBlockHash,
          state.ctx.lastBlockHash
        )
        var prevCurrentTime = state.ctx.timestamps.head

        val testStartMs = System.currentTimeMillis()
        var totalCekMs = 0L
        var batchCount = 0
        var orphanCount = 0

        val batches = (0 until totalBlocks).grouped(batchSize).toList
        for batch <- batches do {
            batchCount += 1
            val batchStartHeight = startHeight + batch.head + 1
            val batchEndHeight = startHeight + batch.last + 1

            // 1. Fetch main chain headers
            val headerHexes = batch.map { offset =>
                val h = startHeight + offset + 1
                val hash = rpc.getBlockHash(h).await()
                rpc.getBlockHeaderRaw(hash).await()
            }
            val headers = headerHexes.map(hex => BlockHeader(ByteString.fromHex(hex)))
            val headersScalus = prelude.List.from(headers.toList)

            // Compute currentTime: must exceed challengeAging for promotions
            val lastHeaderTimestamp = headers.last.timestamp
            val currentTime =
                (lastHeaderTimestamp max prevCurrentTime) + testParams.challengeAging + 1

            // 2. Compute parentPath from the first header's prevBlockHash
            val firstPrevHash = headers.head.prevBlockHash
            val parentPath =
                if firstPrevHash == state.ctx.lastBlockHash then prelude.List.Nil
                else
                    findPathToHash(state.forkTree, firstPrevHash).getOrElse {
                        fail(
                          s"Cannot find parent hash ${firstPrevHash.toHex} in fork tree for batch $batchCount"
                        )
                        prelude.List.Nil
                    }

            // 3. CEK evaluate main chain batch
            val cekStartMs = System.currentTimeMillis()
            val (newState, newMpf, result) =
                cekEvaluateUpdate(state, headersScalus, parentPath, currentTime, mpf)
            val cekElapsedMs = System.currentTimeMillis() - cekStartMs
            totalCekMs += cekElapsedMs

            result match {
                case r: Result.Success =>
                    val budget = r.budget
                    val promotedCount =
                        newState.ctx.height.toInt - state.ctx.height.toInt
                    println(
                      f"Batch $batchCount%3d | #$batchStartHeight%6d..#$batchEndHeight%6d | " +
                          f"CEK ${cekElapsedMs}%5dms | " +
                          f"steps ${budget.steps}%,12d mem ${budget.memory}%,12d | " +
                          f"promoted $promotedCount%3d | " +
                          f"tree ${newState.forkTree.blockCount}%3d blocks | " +
                          f"confirmed height ${newState.ctx.height}"
                    )
                case r: Result.Failure =>
                    println(
                      s"FAILED batch $batchCount (#$batchStartHeight..#$batchEndHeight): ${r.exception.getMessage}"
                    )
                    println(s"Fork tree:\n${prettyTree(state.forkTree, state.ctx.height)}")
                    fail(s"CEK evaluation failed at batch $batchCount: ${r.exception.getMessage}")
            }

            state = newState
            mpf = newMpf
            prevCurrentTime = currentTime

            // 4. Verify state against RPC (full check every 10 batches)
            verifyStateAgainstRpc(
              state,
              rpc,
              s"batch $batchCount",
              fullCheck = batchCount % 10 == 0
            )

            // 5. Check for orphan blocks to submit at heights in this batch
            for {
                forkHeight <- batchStartHeight to batchEndHeight
                orphanHashes <- staleForks.get(forkHeight)
            } {
                orphanCount += orphanHashes.size
                println(
                  s"  >> Submitting ${orphanHashes.size} orphan block(s) forking at height $forkHeight"
                )

                val orphanHeaderHexes = orphanHashes.map { hash =>
                    rpc.getBlockHeaderRaw(hash).await()
                }
                val orphanHeaders =
                    orphanHeaderHexes.map(hex => BlockHeader(ByteString.fromHex(hex)))
                val orphanHeadersScalus = prelude.List.from(orphanHeaders.toList)

                // Compute correct parent path: fork point block is the parent
                val confirmedHeight = state.ctx.height.toInt
                val forkPointHash = rpcHashToInternal(rpc.getBlockHash(forkHeight).await())
                val orphanParentPath =
                    if forkPointHash == state.ctx.lastBlockHash then
                        prelude.List.Nil // parent is confirmed tip
                    else
                        findPathToHash(state.forkTree, forkPointHash) match {
                            case Some(path) => path
                            case None =>
                                println(
                                  s"  >> Skipping orphan: fork point #$forkHeight not in fork tree (already confirmed)"
                                )
                                null // sentinel to skip
                        }

                if orphanParentPath != null then {
                    val orphanCekStart = System.currentTimeMillis()
                    val (orphanState, orphanMpf, orphanResult) =
                        cekEvaluateUpdate(
                          state,
                          orphanHeadersScalus,
                          orphanParentPath,
                          currentTime,
                          mpf
                        )
                    val orphanCekMs = System.currentTimeMillis() - orphanCekStart
                    totalCekMs += orphanCekMs

                    orphanResult match {
                        case r: Result.Success =>
                            println(
                              f"  >> Orphan OK | CEK ${orphanCekMs}%5dms | " +
                                  f"steps ${r.budget.steps}%,12d mem ${r.budget.memory}%,12d"
                            )
                            println(
                              s"  >> Fork tree after orphan:\n${prettyTree(orphanState.forkTree, orphanState.ctx.height)}"
                            )
                        case r: Result.Failure =>
                            println(s"  >> Orphan FAILED: ${r.exception.getMessage}")
                            println(
                              s"  >> Fork tree:\n${prettyTree(state.forkTree, state.ctx.height)}"
                            )
                            fail(
                              s"CEK evaluation failed for orphan at height $forkHeight: ${r.exception.getMessage}"
                            )
                    }

                    state = orphanState
                    mpf = orphanMpf
                }
            }
        }

        val totalElapsedMs = System.currentTimeMillis() - testStartMs
        val finalHeight = state.ctx.height
        println()
        println(s"=== Summary ===")
        println(s"Blocks processed: $totalBlocks main + $orphanCount orphan(s)")
        println(s"Confirmed height: $finalHeight")
        println(f"Total time: ${totalElapsedMs / 1000.0}%.1fs (CEK: ${totalCekMs / 1000.0}%.1fs)")
        println(s"Final fork tree:\n${prettyTree(state.forkTree, state.ctx.height)}")
    }
}
