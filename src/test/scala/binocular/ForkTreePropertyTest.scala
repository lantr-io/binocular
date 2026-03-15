package binocular

import binocular.BitcoinHelpers.*
import binocular.ForkTree.*
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.cardano.onchain.plutus.prelude
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.cardano.onchain.plutus.prelude.List.{Cons, Nil as PNil}
import scalus.compiler.Options
import scalus.uplc.{Constant, DeBruijnedProgram, PlutusV3, Term}
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.{to, toData}
import scalus.uplc.eval.{PlutusVM, Result}

class ForkTreePropertyTest
    extends AnyFunSuite
    with ScalaCheckPropertyChecks
    with BitcoinValidatorGenerators {

    // Increase minSuccessful for better coverage on most tests
    implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
        PropertyCheckConfiguration(minSuccessful = 100)

    // ============================================================================
    // CEK testing infrastructure
    // ============================================================================

    private given Options = Options.release
    private given PlutusVM = PlutusVM.makePlutusV3VM()

    // Use Term.evaluateDebug (not DeBruijnedProgram.evaluateDebug) because
    // script-level evaluation enforces Unit return, but our test programs return Data.
    private def evalCEK(program: DeBruijnedProgram, args: Data*): Result = {
        val applied = args.foldLeft(program)(_ $ _)
        applied.term.evaluateDebug
    }

    private def assertCEKSuccess(program: DeBruijnedProgram, args: Data*): Unit =
        evalCEK(program, args*) match
            case _: Result.Success => ()
            case f: Result.Failure =>
                fail(s"CEK evaluation failed: ${f.exception}\nlogs: ${f.logs.mkString("\n")}")

    private def assertCEKData(program: DeBruijnedProgram, args: Data*)(
        expectedData: Data
    ): Unit =
        evalCEK(program, args*) match
            case Result.Success(term, _, _, _) =>
                term match
                    case Term.Const(Constant.Data(data), _) =>
                        assert(data == expectedData, "CEK result doesn't match JVM")
                    case other =>
                        fail(s"CEK returned non-Data term: $other")
            case f: Result.Failure =>
                fail(s"CEK evaluation failed: ${f.exception}\nlogs: ${f.logs.mkString("\n")}")

    // Compiled programs for CEK evaluation
    private lazy val insertAscendingCEK = PlutusV3
        .compile { (d1: Data, d2: Data) =>
            BitcoinValidator.insertAscending(d1.to[BigInt], d2.to[PList[BigInt]]).toData
        }
        .program
        .deBruijnedProgram

    private lazy val insertionSortCEK = PlutusV3
        .compile { (d: Data) =>
            BitcoinValidator.insertionSort(d.to[PList[BigInt]]).toData
        }
        .program
        .deBruijnedProgram

    private lazy val compactBitsToTargetCEK = PlutusV3
        .compile { (d: Data) =>
            compactBitsToTarget(d.to[CompactBits]).toData
        }
        .program
        .deBruijnedProgram

    private lazy val targetToCompactByteStringCEK = PlutusV3
        .compile { (d: Data) =>
            targetToCompactByteString(d.to[BigInt]).toData
        }
        .program
        .deBruijnedProgram

    private lazy val calculateBlockProofCEK = PlutusV3
        .compile { (d: Data) =>
            calculateBlockProof(d.to[BigInt]).toData
        }
        .program
        .deBruijnedProgram

    private lazy val accumulateBlockCEK = PlutusV3
        .compile { (d1: Data, d2: Data) =>
            BitcoinValidator.accumulateBlock(d1.to[TraversalCtx], d2.to[BlockSummary]).toData
        }
        .program
        .deBruijnedProgram

    private lazy val bestChainPathCEK = PlutusV3
        .compile { (d1: Data, d2: Data, d3: Data) =>
            BitcoinValidator.bestChainPath(d1.to[ForkTree], d2.to[BigInt], d3.to[BigInt])
        }
        .program
        .deBruijnedProgram

    private lazy val computeUpdateCEK = PlutusV3
        .compile { (d1: Data, d2: Data, d3: Data, d4: Data) =>
            BitcoinValidator
                .computeUpdate(
                  d1.to[ChainState],
                  d2.to[UpdateOracle],
                  d3.to[BigInt],
                  d4.to[BitcoinValidatorParams]
                )
                .toData
        }
        .program
        .deBruijnedProgram

    // ============================================================================
    // insertAscending properties
    // ============================================================================

    test("insertAscending - result is always sorted ascending") {
        val genSortedList = Gen.listOf(Gen.choose(-1000L, 1000L).map(BigInt(_))).map { xs =>
            PList.from(xs.sorted)
        }
        forAll(genSortedList, Gen.choose(-1000L, 1000L).map(BigInt(_))) { (sorted, x) =>
            val result = BitcoinValidator.insertAscending(x, sorted)
            val resultScala = result.toScalaList
            assert(resultScala == resultScala.sorted)
            // CEK
            assertCEKData(insertAscendingCEK, x.toData, sorted.toData)(result.toData)
        }
    }

    test("insertAscending - length increases by 1") {
        val genSortedList = Gen.listOf(Gen.choose(-1000L, 1000L).map(BigInt(_))).map { xs =>
            PList.from(xs.sorted)
        }
        forAll(genSortedList, Gen.choose(-1000L, 1000L).map(BigInt(_))) { (sorted, x) =>
            val result = BitcoinValidator.insertAscending(x, sorted)
            assert(result.length == sorted.length + 1)
            // CEK
            assertCEKData(insertAscendingCEK, x.toData, sorted.toData)(result.toData)
        }
    }

    test("insertAscending - all original elements + x present in result") {
        val genSortedList = Gen.listOf(Gen.choose(-1000L, 1000L).map(BigInt(_))).map { xs =>
            PList.from(xs.sorted)
        }
        forAll(genSortedList, Gen.choose(-1000L, 1000L).map(BigInt(_))) { (sorted, x) =>
            val result = BitcoinValidator.insertAscending(x, sorted)
            val resultMultiset = result.toScalaList.groupBy(identity).view.mapValues(_.size).toMap
            val expectedMultiset =
                (sorted.toScalaList :+ x).groupBy(identity).view.mapValues(_.size).toMap
            assert(resultMultiset == expectedMultiset)
            // CEK
            assertCEKData(insertAscendingCEK, x.toData, sorted.toData)(result.toData)
        }
    }

    test("insertAscending - inserting into already-sorted produces sorted") {
        forAll(Gen.listOf(Gen.choose(-1000L, 1000L).map(BigInt(_)))) { xs =>
            val sorted = PList.from(xs.sorted)
            forAll(Gen.choose(-1000L, 1000L).map(BigInt(_))) { x =>
                val result = BitcoinValidator.insertAscending(x, sorted)
                val resultScala = result.toScalaList
                assert(resultScala == resultScala.sorted)
                // CEK
                assertCEKData(insertAscendingCEK, x.toData, sorted.toData)(result.toData)
            }
        }
    }

    test("insertAscending - commutativity of elements") {
        val genSortedList = Gen.listOf(Gen.choose(-100L, 100L).map(BigInt(_))).map { xs =>
            PList.from(xs.sorted)
        }
        forAll(
          genSortedList,
          Gen.choose(-100L, 100L).map(BigInt(_)),
          Gen.choose(-100L, 100L).map(BigInt(_))
        ) { (sorted, a, b) =>
            val result1 =
                BitcoinValidator.insertAscending(b, BitcoinValidator.insertAscending(a, sorted))
            val result2 =
                BitcoinValidator.insertAscending(a, BitcoinValidator.insertAscending(b, sorted))
            assert(result1.toScalaList.sorted == result2.toScalaList.sorted)
            // CEK - verify both orderings produce same result
            assertCEKData(insertAscendingCEK, a.toData, sorted.toData)(
              BitcoinValidator.insertAscending(a, sorted).toData
            )
            assertCEKData(insertAscendingCEK, b.toData, sorted.toData)(
              BitcoinValidator.insertAscending(b, sorted).toData
            )
        }
    }

    // ============================================================================
    // insertionSort properties
    // ============================================================================

    test("insertionSort - output is ascending-sorted") {
        forAll(Gen.listOfN(11, Gen.choose(-1000L, 1000L).map(BigInt(_)))) { xs =>
            val input = PList.from(xs)
            val result = BitcoinValidator.insertionSort(input)
            val resultScala = result.toScalaList
            assert(resultScala == resultScala.sorted)
            // CEK
            assertCEKData(insertionSortCEK, input.toData)(result.toData)
        }
    }

    test("insertionSort - length preserved") {
        forAll(Gen.listOf(Gen.choose(-1000L, 1000L).map(BigInt(_)))) { xs =>
            val input = PList.from(xs)
            val result = BitcoinValidator.insertionSort(input)
            assert(result.length == input.length)
            // CEK
            assertCEKData(insertionSortCEK, input.toData)(result.toData)
        }
    }

    test("insertionSort - output is permutation of input") {
        forAll(Gen.listOf(Gen.choose(-1000L, 1000L).map(BigInt(_)))) { xs =>
            val input = PList.from(xs)
            val result = BitcoinValidator.insertionSort(input)
            assert(result.toScalaList.sorted == input.toScalaList.sorted)
            // CEK
            assertCEKData(insertionSortCEK, input.toData)(result.toData)
        }
    }

    test("insertionSort - agrees with Scala sorted") {
        forAll(Gen.listOf(Gen.choose(-1000L, 1000L).map(BigInt(_)))) { xs =>
            val input = PList.from(xs)
            val result = BitcoinValidator.insertionSort(input)
            assert(result.toScalaList == xs.sorted)
            // CEK
            assertCEKData(insertionSortCEK, input.toData)(result.toData)
        }
    }

    // ============================================================================
    // compactBitsToTarget / targetToCompactBits properties
    // ============================================================================

    test("compactBitsToTarget/targetToCompactBits - roundtrip for representable targets") {
        forAll(genCompactBits) { bits =>
            val target = compactBitsToTarget(bits)
            val roundtripped = targetToCompactByteString(target)
            val targetBack = compactBitsToTarget(roundtripped)
            assert(
              target == targetBack,
              s"bits=${bits.toHex}, target=$target, roundtripped=${roundtripped.toHex}"
            )
            // CEK
            assertCEKData(compactBitsToTargetCEK, bits.toData)(target.toData)
            assertCEKData(targetToCompactByteStringCEK, target.toData)(roundtripped.toData)
        }
    }

    test("compactBitsToTarget - target bounded by PowLimit") {
        forAll(genCompactBits) { bits =>
            val target = compactBitsToTarget(bits)
            assert(target <= PowLimit)
            // CEK
            assertCEKData(compactBitsToTargetCEK, bits.toData)(target.toData)
        }
    }

    test("compactBitsToTarget - target is non-negative") {
        forAll(genCompactBits) { bits =>
            val target = compactBitsToTarget(bits)
            assert(target >= 0)
            // CEK
            assertCEKData(compactBitsToTargetCEK, bits.toData)(target.toData)
        }
    }

    // ============================================================================
    // calculateBlockProof properties
    // ============================================================================

    test("calculateBlockProof - higher target yields lower proof") {
        forAll(genCompactBits, genCompactBits) { (bits1, bits2) =>
            val target1 = compactBitsToTarget(bits1)
            val target2 = compactBitsToTarget(bits2)
            whenever(target1 > 0 && target2 > 0 && target1 != target2) {
                val proof1 = calculateBlockProof(target1)
                val proof2 = calculateBlockProof(target2)
                if target1 > target2 then assert(proof1 < proof2)
                else assert(proof1 > proof2)
                // CEK
                assertCEKData(calculateBlockProofCEK, target1.toData)(proof1.toData)
                assertCEKData(calculateBlockProofCEK, target2.toData)(proof2.toData)
            }
        }
    }

    test("calculateBlockProof - always positive") {
        forAll(genCompactBits) { bits =>
            val target = compactBitsToTarget(bits)
            whenever(target > 0) {
                val proof = calculateBlockProof(target)
                assert(proof > 0)
                // CEK
                assertCEKData(calculateBlockProofCEK, target.toData)(proof.toData)
            }
        }
    }

    test("calculateBlockProof - matches formula TwoTo256 / (target + 1)") {
        forAll(genCompactBits) { bits =>
            val target = compactBitsToTarget(bits)
            whenever(target > 0) {
                val proof = calculateBlockProof(target)
                val expected = TwoTo256 / (target + 1)
                assert(proof == expected)
                // CEK
                assertCEKData(calculateBlockProofCEK, target.toData)(proof.toData)
            }
        }
    }

    // ============================================================================
    // accumulateBlock properties
    // ============================================================================

    test("accumulateBlock - height increments by 1") {
        forAll(genNonRetargetCtx) { ctx =>
            val block = genBlockSummaryWithId(1, 1700000000L, 1700000000L)
            val newCtx = BitcoinValidator.accumulateBlock(ctx, block)
            assert(newCtx.height == ctx.height + 1)
            // CEK
            assertCEKData(accumulateBlockCEK, ctx.toData, block.toData)(newCtx.toData)
        }
    }

    test("accumulateBlock - lastBlockHash updated to block.hash") {
        forAll(genNonRetargetCtx) { ctx =>
            val block = genBlockSummaryWithId(42, 1700000000L, 1700000000L)
            val newCtx = BitcoinValidator.accumulateBlock(ctx, block)
            assert(newCtx.lastBlockHash == block.hash)
            // CEK
            assertCEKData(accumulateBlockCEK, ctx.toData, block.toData)(newCtx.toData)
        }
    }

    test("accumulateBlock - block.timestamp prepended to timestamps") {
        forAll(genNonRetargetCtx) { ctx =>
            val block = genBlockSummaryWithId(1, 1700000000L, 1700000000L)
            val newCtx = BitcoinValidator.accumulateBlock(ctx, block)
            assert(newCtx.timestamps.head == block.timestamp)
            // CEK
            assertCEKData(accumulateBlockCEK, ctx.toData, block.toData)(newCtx.toData)
        }
    }

    test("accumulateBlock - at retarget boundary: prevDiffAdjTimestamp updated") {
        // Find a height where (height+1) % 2016 == 0
        forAll(Gen.choose(0, 500).map(n => BigInt(n * 2016 - 1))) { retargetHeight =>
            whenever(retargetHeight >= 0 && (retargetHeight + 1) % 2016 == 0) {
                val ctx = TraversalCtx(
                  timestamps =
                      PList.from((0 until 11).map(i => BigInt(1700000000 - i * 600)).toList),
                  height = retargetHeight,
                  currentBits = integerToByteString(false, 4, BigInt(0x1d00ffff)),
                  prevDiffAdjTimestamp = BigInt(1600000000),
                  lastBlockHash = genUniqueBlockHash(0)
                )
                val block = genBlockSummaryWithId(1, 1700001000L, 1700001000L)
                val newCtx = BitcoinValidator.accumulateBlock(ctx, block)
                assert(newCtx.prevDiffAdjTimestamp == block.timestamp)
                // CEK
                assertCEKData(accumulateBlockCEK, ctx.toData, block.toData)(newCtx.toData)
            }
        }
    }

    test("accumulateBlock - not at retarget: prevDiffAdjTimestamp preserved") {
        forAll(genNonRetargetCtx) { ctx =>
            val block = genBlockSummaryWithId(1, 1700000000L, 1700000000L)
            val newCtx = BitcoinValidator.accumulateBlock(ctx, block)
            assert(newCtx.prevDiffAdjTimestamp == ctx.prevDiffAdjTimestamp)
            // CEK
            assertCEKData(accumulateBlockCEK, ctx.toData, block.toData)(newCtx.toData)
        }
    }

    test("accumulateBlock - non-monotonic timestamps accepted") {
        forAll(genNonRetargetCtx) { ctx =>
            // Block with timestamp less than the head of ctx.timestamps
            val earlierTimestamp = ctx.timestamps.head - 100
            val block = BlockSummary(
              hash = genUniqueBlockHash(99),
              timestamp = earlierTimestamp,
              addedTimeSeconds = BigInt(1700000000)
            )
            // Should not throw — accumulateBlock doesn't check MTP
            val newCtx = BitcoinValidator.accumulateBlock(ctx, block)
            assert(newCtx.height == ctx.height + 1)
            // CEK
            assertCEKData(accumulateBlockCEK, ctx.toData, block.toData)(newCtx.toData)
        }
    }

    // ============================================================================
    // bestChainPath properties
    // ============================================================================

    test("bestChainPath - empty tree returns passthrough") {
        forAll(
          Gen.choose(0L, 1000000L).map(BigInt(_)),
          Gen.choose(0L, 1000000L).map(BigInt(_))
        ) { (h, cw) =>
            val (resCw, resH, resPath) = BitcoinValidator.bestChainPath(End, h, cw)
            assert(resCw == cw)
            assert(resH == h)
            assert(resPath == PNil)
            // CEK
            assertCEKSuccess(bestChainPathCEK, End.toData, h.toData, cw.toData)
        }
    }

    test("bestChainPath - linear tree: chainwork = segment cw, depth = h + blocks.length") {
        forAll(genLinearForkTree(10), Gen.choose(100L, 1000L).map(BigInt(_))) { (tree, baseH) =>
            tree match
                case Blocks(blocks, cw, End) =>
                    val (resCw, resDepth, resPath) = BitcoinValidator.bestChainPath(tree, baseH, 0)
                    assert(resCw == cw)
                    assert(resDepth == baseH + blocks.length)
                    assert(resPath == PNil) // no forks → empty path
                    // CEK
                    assertCEKSuccess(bestChainPathCEK, tree.toData, baseH.toData, BigInt(0).toData)
                case _ => // skip if generator produced something unexpected
        }
    }

    test("bestChainPath - left wins ties (>=)") {
        forAll(
          genBlockSummarySeq(2),
          Gen.choose(100L, 10000L).map(BigInt(_))
        ) { (blocks, cw) =>
            whenever(blocks.size == 2) {
                val tree = Fork(
                  Blocks(PList(blocks.head), cw, End),
                  Blocks(PList(blocks(1)), cw, End)
                )
                val (_, _, path) = BitcoinValidator.bestChainPath(tree, 0, 0)
                assert(path.head == BitcoinValidator.LeftFork)
                // CEK
                assertCEKSuccess(bestChainPathCEK, tree.toData, BigInt(0).toData, BigInt(0).toData)
            }
        }
    }

    test("bestChainPath - right wins strictly higher chainwork") {
        forAll(
          genBlockSummarySeq(2),
          Gen.choose(100L, 10000L).map(BigInt(_))
        ) { (blocks, cw) =>
            whenever(blocks.size == 2) {
                val tree = Fork(
                  Blocks(PList(blocks.head), cw, End),
                  Blocks(PList(blocks(1)), cw + 1, End)
                )
                val (_, _, path) = BitcoinValidator.bestChainPath(tree, 0, 0)
                assert(path.head == BitcoinValidator.RightFork)
                // CEK
                assertCEKSuccess(bestChainPathCEK, tree.toData, BigInt(0).toData, BigInt(0).toData)
            }
        }
    }

    test("bestChainPath - path length = fork count along best path") {
        forAll(genForkTree(3)) { tree =>
            val (_, _, path) = BitcoinValidator.bestChainPath(tree, 0, 0)
            // Count how many Fork nodes are in the tree along the best path
            def countForksOnPath(t: ForkTree, p: PList[BigInt]): Int = t match
                case Fork(left, right) =>
                    p match
                        case Cons(dir, rest) =>
                            1 + (if dir == BigInt(0) then countForksOnPath(left, rest)
                                 else countForksOnPath(right, rest))
                        case _ => 0 // path exhausted
                case Blocks(_, _, next) => countForksOnPath(next, p)
                case End                => 0

            assert(path.length == countForksOnPath(tree, path))
            // CEK
            assertCEKSuccess(bestChainPathCEK, tree.toData, BigInt(0).toData, BigInt(0).toData)
        }
    }

    test("bestChainPath - consistent with highestHeight") {
        forAll(genForkTree(3), Gen.choose(100L, 1000L).map(BigInt(_))) { (tree, baseH) =>
            val (_, depth, _) = BitcoinValidator.bestChainPath(tree, baseH, 0)
            assert(depth == tree.highestHeight(baseH))
            // CEK
            assertCEKSuccess(bestChainPathCEK, tree.toData, baseH.toData, BigInt(0).toData)
        }
    }

    test("bestChainPath - deterministic") {
        forAll(genForkTree(3), Gen.choose(0L, 1000L).map(BigInt(_))) { (tree, h) =>
            val r1 = BitcoinValidator.bestChainPath(tree, h, 0)
            val r2 = BitcoinValidator.bestChainPath(tree, h, 0)
            assert(r1 == r2)
            // CEK
            assertCEKSuccess(bestChainPathCEK, tree.toData, h.toData, BigInt(0).toData)
        }
    }

    // ============================================================================
    // ForkTree helper properties
    // ============================================================================

    test("ForkTree - blockCount == toBlockList.size") {
        forAll(genForkTree(3)) { tree =>
            assert(tree.blockCount == tree.toBlockList.size)
        }
    }

    test("ForkTree - nonEmpty iff tree is not End") {
        forAll(genForkTree(3)) { tree =>
            // nonEmpty checks structural non-emptiness (not End), not block count.
            // Fork(End, End) is nonEmpty but has blockCount == 0.
            assert(tree.nonEmpty == (tree != End))
        }
    }

    test("ForkTree - existsHash finds every block in toBlockList") {
        forAll(genNonEmptyForkTree) { tree =>
            tree.toBlockList.foreach { block =>
                assert(tree.existsHash(block.hash), s"existsHash failed for ${block.hash.toHex}")
            }
        }
    }

    test("ForkTree - existsHash rejects random hash not in tree") {
        forAll(genForkTree(3), genBlockHash) { (tree, randomHash) =>
            val allHashes = tree.toBlockList.map(_.hash).toSet
            whenever(!allHashes.contains(randomHash)) {
                assert(!tree.existsHash(randomHash))
            }
        }
    }

    test("ForkTree - oldestBlockTime matches toBlockList min") {
        forAll(genForkTree(3)) { tree =>
            val blockList = tree.toBlockList
            val expected =
                if blockList.isEmpty then scala.None
                else scala.Some(blockList.map(_.addedTimeSeconds).min)
            assert(tree.oldestBlockTime == expected)
        }
    }

    // ============================================================================
    // splitPromotable properties
    // ============================================================================

    private def makeSplitCtx(height: BigInt): TraversalCtx =
        TraversalCtx(
          timestamps = PList.from((0 until 11).map(i => BigInt(1700000000 - i * 600)).toList),
          height = height,
          currentBits = integerToByteString(false, 4, BigInt(0x1d00ffff)),
          prevDiffAdjTimestamp = BigInt(1699990000),
          lastBlockHash = genUniqueBlockHash(0)
        )

    test("splitPromotable - promoted ++ remaining == original blocks") {
        forAll(genBlockSummarySeq(5), Gen.choose(200L, 300L).map(BigInt(_))) {
            (blocks, bestDepth) =>
                whenever(blocks.nonEmpty) {
                    val ctx = makeSplitCtx(bestDepth - 110) // ensure some blocks are deep enough
                    val currentTime = blocks.head.addedTimeSeconds + 200 * 60 + 1000
                    val blocksPL = PList.from(blocks)
                    val params = testParams()
                    val (promoted, remaining, _) =
                        BitcoinValidator.splitPromotable(
                          blocksPL,
                          ctx,
                          bestDepth,
                          currentTime,
                          100,
                          params
                        )
                    assert(promoted.toScalaList ++ remaining.toScalaList == blocks)
                }
        }
    }

    test("splitPromotable - all promoted blocks satisfy depth + age conditions") {
        forAll(genBlockSummarySeq(5), Gen.choose(200L, 300L).map(BigInt(_))) {
            (blocks, bestDepth) =>
                whenever(blocks.nonEmpty) {
                    val ctx = makeSplitCtx(bestDepth - 110)
                    val currentTime = blocks.head.addedTimeSeconds + 200 * 60 + 1000
                    val blocksPL = PList.from(blocks)
                    val params = testParams()
                    val (promoted, _, _) =
                        BitcoinValidator.splitPromotable(
                          blocksPL,
                          ctx,
                          bestDepth,
                          currentTime,
                          100,
                          params
                        )
                    var h = ctx.height
                    promoted.foreach { block =>
                        h = h + 1
                        val depth = bestDepth - h
                        val age = currentTime - block.addedTimeSeconds
                        assert(depth >= params.maturationConfirmations)
                        assert(age >= params.challengeAging)
                    }
                }
        }
    }

    test("splitPromotable - first remaining block fails at least one condition") {
        forAll(genBlockSummarySeq(5), Gen.choose(200L, 300L).map(BigInt(_))) {
            (blocks, bestDepth) =>
                whenever(blocks.nonEmpty) {
                    val ctx = makeSplitCtx(bestDepth - 110)
                    val currentTime = blocks.head.addedTimeSeconds + 200 * 60 + 1000
                    val blocksPL = PList.from(blocks)
                    val params = testParams()
                    val (promoted, remaining, finalCtx) =
                        BitcoinValidator.splitPromotable(
                          blocksPL,
                          ctx,
                          bestDepth,
                          currentTime,
                          100,
                          params
                        )
                    remaining match
                        case Cons(firstRemaining, _) =>
                            val blockHeight = finalCtx.height + 1
                            val depth = bestDepth - blockHeight
                            val age = currentTime - firstRemaining.addedTimeSeconds
                            assert(
                              depth < params.maturationConfirmations || age < params.challengeAging,
                              "First remaining block should fail at least one condition"
                            )
                        case _ => // remaining empty, all promoted — ok
                }
        }
    }

    test("splitPromotable - promoted.length <= maxPromotions") {
        forAll(
          genBlockSummarySeq(10),
          Gen.choose(0, 5),
          Gen.choose(300L, 500L).map(BigInt(_))
        ) { (blocks, maxProm, bestDepth) =>
            whenever(blocks.nonEmpty) {
                val ctx = makeSplitCtx(bestDepth - 200)
                val currentTime = blocks.head.addedTimeSeconds + 200 * 60 + 10000
                val blocksPL = PList.from(blocks)
                val params = testParams()
                val (promoted, _, _) =
                    BitcoinValidator.splitPromotable(
                      blocksPL,
                      ctx,
                      bestDepth,
                      currentTime,
                      maxProm,
                      params
                    )
                assert(promoted.length <= maxProm)
            }
        }
    }

    test("splitPromotable - maxPromotions=0 returns (Nil, blocks, ctx)") {
        forAll(genBlockSummarySeq(3)) { blocks =>
            whenever(blocks.nonEmpty) {
                val ctx = makeSplitCtx(1000)
                val currentTime = BigInt(2000000000)
                val blocksPL = PList.from(blocks)
                val params = testParams()
                val (promoted, remaining, retCtx) =
                    BitcoinValidator.splitPromotable(blocksPL, ctx, 1000, currentTime, 0, params)
                assert(promoted == PNil)
                assert(remaining.toScalaList == blocks)
                assert(retCtx == ctx)
            }
        }
    }

    test("splitPromotable - empty input returns (Nil, Nil, ctx)") {
        val ctx = makeSplitCtx(1000)
        val params = testParams()
        val (promoted, remaining, retCtx) =
            BitcoinValidator.splitPromotable(PNil, ctx, 1000, BigInt(2000000000), 100, params)
        assert(promoted == PNil)
        assert(remaining == PNil)
        assert(retCtx == ctx)
    }

    // ============================================================================
    // promoteAndGC properties
    // ============================================================================

    test("promoteAndGC - empty tree returns (Nil, End)") {
        val ctx = makeSplitCtx(1000)
        val params = testParams()
        val (promoted, cleaned) =
            BitcoinValidator.promoteAndGC(End, ctx, PNil, 1100, BigInt(2000000000), 10, params)
        assert(promoted == PNil)
        assert(cleaned == End)
    }

    test("promoteAndGC - promoted.size + cleanedTree.blockCount <= tree.blockCount") {
        forAll(genNonEmptyForkTree) { tree =>
            val ctx = makeSplitCtx(tree.blockCount + 200)
            val bestDepth = BigInt(tree.blockCount + 200)
            val currentTime = BigInt(2000000000)
            val (_, _, bestPath) = BitcoinValidator.bestChainPath(tree, ctx.height, 0)
            val params = testParams()
            val (promoted, cleaned) =
                BitcoinValidator.promoteAndGC(
                  tree,
                  ctx,
                  bestPath,
                  bestDepth,
                  currentTime,
                  100,
                  params
                )
            assert(promoted.length.toInt + cleaned.blockCount <= tree.blockCount)
        }
    }

    test("promoteAndGC - all blocks too young: promoted is empty") {
        forAll(genNonEmptyForkTree) { tree =>
            // Set currentTime so that no block is old enough
            val ctx = makeSplitCtx(tree.blockCount + 200)
            val bestDepth = BigInt(tree.blockCount + 200)
            // Set addedTimeSeconds very recent, currentTime barely after
            val currentTime = BigInt(1700000010) // blocks have addedTime around 1.5-1.7 billion
            val (_, _, bestPath) = BitcoinValidator.bestChainPath(tree, ctx.height, 0)
            val params = testParams()
            val (promoted, _) =
                BitcoinValidator.promoteAndGC(
                  tree,
                  ctx,
                  bestPath,
                  bestDepth,
                  currentTime,
                  100,
                  params
                )
            // If currentTime - addedTimeSeconds < challengeAging for all blocks, promoted is empty
            val allBlockTimes = tree.toBlockList.map(_.addedTimeSeconds)
            val oldestBlock = if allBlockTimes.nonEmpty then allBlockTimes.min else BigInt(0)
            if currentTime - oldestBlock < params.challengeAging then assert(promoted == PNil)
        }
    }

    // ============================================================================
    // computeUpdate end-to-end properties (testingMode)
    // ============================================================================

    test("computeUpdate - header-only preserves confirmed state fields") {
        forAll(Gen.choose(1, 5)) { n =>
            val state = makeTestChainState()
            val ctx = BitcoinValidator.initCtx(state)
            val headers = genFakeHeaderChain(n, ctx, state.recentTimestamps.head + 1000).sample.get
            val headersPL = PList.from(headers)
            val update = UpdateOracle(
              blockHeaders = headersPL,
              parentPath = PNil,
              mpfInsertProofs = PNil
            )
            val currentTime = state.recentTimestamps.head + 1000
            val newState =
                BitcoinValidator.computeUpdate(state, update, currentTime, testingParams)
            // Confirmed state fields unchanged
            assert(newState.blockHeight == state.blockHeight)
            assert(newState.blockHash == state.blockHash)
            assert(newState.currentTarget == state.currentTarget)
            assert(newState.recentTimestamps == state.recentTimestamps)
            assert(
              newState.previousDifficultyAdjustmentTimestamp == state.previousDifficultyAdjustmentTimestamp
            )
            assert(newState.confirmedBlocksRoot == state.confirmedBlocksRoot)
            // CEK
            assertCEKData(
              computeUpdateCEK,
              state.toData,
              update.toData,
              currentTime.toData,
              testingParams.toData
            )(newState.toData)
        }
    }

    test("computeUpdate - header-only only changes forkTree") {
        forAll(Gen.choose(1, 5)) { n =>
            val state = makeTestChainState()
            val ctx = BitcoinValidator.initCtx(state)
            val headers = genFakeHeaderChain(n, ctx, state.recentTimestamps.head + 1000).sample.get
            val headersPL = PList.from(headers)
            val update = UpdateOracle(
              blockHeaders = headersPL,
              parentPath = PNil,
              mpfInsertProofs = PNil
            )
            val currentTime = state.recentTimestamps.head + 1000
            val newState =
                BitcoinValidator.computeUpdate(state, update, currentTime, testingParams)
            assert(newState.forkTree != state.forkTree || n == 0)
            assert(newState.forkTree.nonEmpty)
            // CEK
            assertCEKData(
              computeUpdateCEK,
              state.toData,
              update.toData,
              currentTime.toData,
              testingParams.toData
            )(newState.toData)
        }
    }

    test("computeUpdate - new blocks' hashes exist in resulting forkTree") {
        forAll(Gen.choose(1, 5)) { n =>
            val state = makeTestChainState()
            val ctx = BitcoinValidator.initCtx(state)
            val currentTime = state.recentTimestamps.head + 1000
            val headers = genFakeHeaderChain(n, ctx, currentTime).sample.get
            val headersPL = PList.from(headers)
            val update = UpdateOracle(
              blockHeaders = headersPL,
              parentPath = PNil,
              mpfInsertProofs = PNil
            )
            val newState =
                BitcoinValidator.computeUpdate(state, update, currentTime, testingParams)
            headers.foreach { header =>
                val hash = blockHeaderHash(header)
                assert(
                  newState.forkTree.existsHash(hash),
                  s"hash ${hash.toHex} not found in resulting tree"
                )
            }
            // CEK
            assertCEKData(
              computeUpdateCEK,
              state.toData,
              update.toData,
              currentTime.toData,
              testingParams.toData
            )(newState.toData)
        }
    }

    test("computeUpdate - fork ordering: existing tree goes left, new branch goes right") {
        val state = makeTestChainState()
        val ctx = BitcoinValidator.initCtx(state)
        val currentTime = state.recentTimestamps.head + 1000

        // Insert first batch
        val headers1 = genFakeHeaderChain(2, ctx, currentTime).sample.get
        val update1 = UpdateOracle(
          blockHeaders = PList.from(headers1),
          parentPath = PNil,
          mpfInsertProofs = PNil
        )
        val state1 = BitcoinValidator.computeUpdate(state, update1, currentTime, testingParams)

        // Insert second batch branching from confirmed tip (creates a fork)
        val headers2 = genFakeHeaderChain(2, ctx, currentTime).sample.get
        val update2 = UpdateOracle(
          blockHeaders = PList.from(headers2),
          parentPath = PNil,
          mpfInsertProofs = PNil
        )
        val state2 = BitcoinValidator.computeUpdate(state1, update2, currentTime, testingParams)

        // The result should be Fork(existing, new)
        state2.forkTree match
            case Fork(left, right) =>
                // Left should contain headers from first batch
                headers1.foreach { h =>
                    assert(left.existsHash(blockHeaderHash(h)))
                }
                // Right should contain headers from second batch
                headers2.foreach { h =>
                    assert(right.existsHash(blockHeaderHash(h)))
                }
            case other => fail(s"Expected Fork, got: $other")
    }

    test("computeUpdate - empty update: state unchanged") {
        val state = makeTestChainState()
        val update = UpdateOracle(
          blockHeaders = PNil,
          parentPath = PNil,
          mpfInsertProofs = PNil
        )
        val currentTime = state.recentTimestamps.head + 1000
        val newState =
            BitcoinValidator.computeUpdate(state, update, currentTime, testingParams)
        assert(newState == state)
        // CEK
        assertCEKData(
          computeUpdateCEK,
          state.toData,
          update.toData,
          currentTime.toData,
          testingParams.toData
        )(newState.toData)
    }

    // ============================================================================
    // Operational scenario tests (long chains, forks, promotions)
    // ============================================================================

    test("operational - build 100-block linear chain and extend with 1-5 new headers") {
        forAll(Gen.choose(1, 5)) { numNew =>
            val (stateWith100, newHeaders) = buildLongChainAndExtension(100, numNew)
            assert(stateWith100.forkTree.blockCount == 100)

            // Extend at the tip: path = [99] (last block index)
            val parentPath = PList(BigInt(99))
            val currentTime = stateWith100.recentTimestamps.head + 1100
            val update = UpdateOracle(
              blockHeaders = PList.from(newHeaders),
              parentPath = parentPath,
              mpfInsertProofs = PNil
            )
            val newState =
                BitcoinValidator.computeUpdate(stateWith100, update, currentTime, testingParams)

            // Tree should still be linear with 100 + numNew blocks
            assert(newState.forkTree.blockCount == 100 + numNew)
            newState.forkTree match
                case Blocks(blocks, _, End) =>
                    assert(blocks.length == 100 + numNew)
                case other =>
                    fail(s"Expected linear Blocks, got: ${classifyTreeShape(other)}")

            // Confirmed state unchanged (header-only, no proofs)
            assert(newState.blockHeight == stateWith100.blockHeight)
            assert(newState.blockHash == stateWith100.blockHash)
        }
    }

    test("operational - 100-block chain, fork near tip creates proper split") {
        forAll(Gen.choose(90, 98)) { forkPoint =>
            val (stateWith100, _) = buildLongChainState(100)

            // Build a competing branch from block at forkPoint
            val ctx0 = BitcoinValidator.initCtx(stateWith100)
            val allBlocks = stateWith100.forkTree.toBlockList
            val ctxAtForkPoint =
                allBlocks.take(forkPoint + 1).foldLeft(ctx0)(BitcoinValidator.accumulateBlock)
            val currentTime = BigInt(1700000100)
            val forkHeaders = genFakeHeaderChain(2, ctxAtForkPoint, currentTime).sample.get

            val parentPath = PList(BigInt(forkPoint))
            val update = UpdateOracle(
              blockHeaders = PList.from(forkHeaders),
              parentPath = parentPath,
              mpfInsertProofs = PNil
            )
            val newState =
                BitcoinValidator.computeUpdate(stateWith100, update, currentTime, testingParams)

            // Tree should have a fork: prefix [0..forkPoint] then Fork(suffix, newBranch)
            newState.forkTree match
                case Blocks(prefix, _, Fork(left, right)) =>
                    // Prefix = forkPoint+1 blocks (0..forkPoint inclusive)
                    assert(
                      prefix.length == forkPoint + 1,
                      s"Expected prefix of ${forkPoint + 1}, got ${prefix.length}"
                    )
                    // Left (existing) has the remaining blocks from the original chain
                    assert(left.blockCount == 100 - forkPoint - 1)
                    // Right (new) has the fork headers
                    assert(right.blockCount == 2)
                    // All original hashes still present
                    allBlocks.foreach { b =>
                        assert(newState.forkTree.existsHash(b.hash))
                    }
                    // New fork hashes present
                    forkHeaders.foreach { h =>
                        assert(newState.forkTree.existsHash(blockHeaderHash(h)))
                    }
                case other =>
                    fail(s"Expected Blocks(.., Fork(..)), got: ${classifyTreeShape(other)}")
        }
    }

    test("operational - 100-block chain, extend then fork at second-to-last creates mid-split") {
        // Use a currentTime far enough from addedTimeBase so MTP never catches up
        val addedTimeBase = BigInt(1700000000)
        val (stateWith100, _) = buildLongChainState(100, addedTimeBase)

        val ctx0 = BitcoinValidator.initCtx(stateWith100)
        val allBlocks = stateWith100.forkTree.toBlockList
        val ctxAtTip = allBlocks.foldLeft(ctx0)(BitcoinValidator.accumulateBlock)
        // Use a currentTime well ahead of block timestamps to avoid MTP issues
        val currentTime = addedTimeBase + 50000

        // First extend to 101 blocks
        val extensionHeaders = genFakeHeaderChain(1, ctxAtTip, currentTime).sample.get
        val extUpdate = UpdateOracle(
          blockHeaders = PList.from(extensionHeaders),
          parentPath = PList(BigInt(99)),
          mpfInsertProofs = PNil
        )
        val stateWith101 =
            BitcoinValidator.computeUpdate(stateWith100, extUpdate, currentTime, testingParams)
        assert(stateWith101.forkTree.blockCount == 101)

        // Now fork at block 95 in the 101-block chain (mid-split)
        val ctxAt95 = allBlocks.take(96).foldLeft(ctx0)(BitcoinValidator.accumulateBlock)
        val forkHeaders = genFakeHeaderChain(2, ctxAt95, currentTime).sample.get
        val update = UpdateOracle(
          blockHeaders = PList.from(forkHeaders),
          parentPath = PList(BigInt(95)),
          mpfInsertProofs = PNil
        )
        val forkedState =
            BitcoinValidator.computeUpdate(stateWith101, update, currentTime, testingParams)

        // Should be: Blocks(96 blocks, .., Fork(Blocks(5), Blocks(2)))
        forkedState.forkTree match
            case Blocks(prefix, _, Fork(left, right)) =>
                assert(
                  prefix.length == 96,
                  s"Expected prefix of 96, got ${prefix.length}"
                )
                assert(
                  left.blockCount == 5,
                  s"Left should have 5 suffix blocks, got ${left.blockCount}"
                )
                assert(
                  right.blockCount == 2,
                  s"Right should have 2 fork blocks, got ${right.blockCount}"
                )
            case other =>
                fail(s"Expected Blocks(96, Fork(5, 2)), got: ${classifyTreeShape(other)}")
    }

    test("operational - bestChainPath selects longer branch in 100-block chain with short fork") {
        forAll(Gen.choose(80, 95)) { forkPoint =>
            val (stateWith100, _) = buildLongChainState(100)

            val ctx0 = BitcoinValidator.initCtx(stateWith100)
            val allBlocks = stateWith100.forkTree.toBlockList
            val ctxAtFork =
                allBlocks.take(forkPoint + 1).foldLeft(ctx0)(BitcoinValidator.accumulateBlock)
            val currentTime = BigInt(1700000100)
            // Short fork: 1 block (will have less chainwork than the main chain's 100 - forkPoint - 1 remaining)
            val forkHeaders = genFakeHeaderChain(1, ctxAtFork, currentTime).sample.get

            val parentPath = PList(BigInt(forkPoint))
            val update = UpdateOracle(
              blockHeaders = PList.from(forkHeaders),
              parentPath = parentPath,
              mpfInsertProofs = PNil
            )
            val forkedState =
                BitcoinValidator.computeUpdate(stateWith100, update, currentTime, testingParams)

            // Best chain should follow the left (original/longer) branch
            val (bestCw, bestDepth, bestPath) =
                BitcoinValidator.bestChainPath(forkedState.forkTree, stateWith100.blockHeight, 0)
            assert(bestDepth == stateWith100.blockHeight + 100)
            assert(bestPath.head == BitcoinValidator.LeftFork)
        }
    }

    test("operational - promotion from 101-block chain with sufficient depth and age") {
        // Build 101 blocks, all added at addedTimeBase. Set currentTime far enough
        // in the future that oldest blocks satisfy both depth (100) and age (12000s).
        val addedTimeBase = BigInt(1700000000)
        val (stateWith101, _) = buildLongChainState(101, addedTimeBase)
        assert(stateWith101.forkTree.blockCount == 101)

        val ctx0 = BitcoinValidator.initCtx(stateWith101)
        val (bestCw, bestDepth, bestPath) =
            BitcoinValidator.bestChainPath(stateWith101.forkTree, stateWith101.blockHeight, 0)

        // currentTime must satisfy: currentTime - addedTimeBase >= challengeAging (12000s)
        val currentTime = addedTimeBase + 200 * 60 + 1000 // 13000s after blocks were added

        val params = testParams()
        val (promoted, cleaned) =
            BitcoinValidator.promoteAndGC(
              stateWith101.forkTree,
              ctx0,
              bestPath,
              bestDepth,
              currentTime,
              10, // allow up to 10 promotions
              params
            )

        // First block has depth = 101 (bestDepth - (confirmedHeight+1)) = 101 confirmations >= 100 ✓
        // Age = 13000s >= 12000s ✓
        // Second block has depth = 100 >= 100 ✓ still promotable
        assert(promoted.length >= 1, s"Expected at least 1 promotion, got ${promoted.length}")
        assert(promoted.length <= 10, "Promotion count should not exceed maxPromotions")

        // Promoted blocks should come from the beginning of the chain
        val allBlocks = stateWith101.forkTree.toBlockList
        promoted.toScalaList.zipWithIndex.foreach { case (p, i) =>
            assert(p.hash == allBlocks(i).hash, s"Promoted block $i should match original block $i")
        }

        // Cleaned tree should have fewer blocks
        assert(cleaned.blockCount == 101 - promoted.length.toInt)
    }

    test("operational - promotion + GC removes short competing fork") {
        val addedTimeBase = BigInt(1700000000)
        // Build 103 blocks so first blocks have depth >= 100
        val (stateWith103, _) = buildLongChainState(103, addedTimeBase)

        // Create a 2-block fork at block 2. This splits the tree into:
        // Blocks([b0,b1,b2], prefCw, Fork(Blocks([b3..b102], suffCw, End), Blocks([f0,f1], forkCw, End)))
        // Prefix has only 3 blocks — all promotable since depth >= 100.
        val ctx0 = BitcoinValidator.initCtx(stateWith103)
        val allBlocks = stateWith103.forkTree.toBlockList
        val ctxAt2 = allBlocks.take(3).foldLeft(ctx0)(BitcoinValidator.accumulateBlock)
        val currentTime = addedTimeBase + 50000
        val forkHeaders = genFakeHeaderChain(2, ctxAt2, currentTime).sample.get

        val forkUpdate = UpdateOracle(
          blockHeaders = PList.from(forkHeaders),
          parentPath = PList(BigInt(2)),
          mpfInsertProofs = PNil
        )
        val forkedState =
            BitcoinValidator.computeUpdate(stateWith103, forkUpdate, currentTime, testingParams)

        // Verify: 3 prefix + Fork(100 main, 2 fork) = 105 blocks total
        assert(forkedState.forkTree.blockCount == 105)

        // Promote with aged blocks. All 3 prefix blocks have:
        //   depth >= 100 (they have depth 103, 102, 101) ✓
        //   age >= challengeAging (13000s) ✓
        // After promoting all 3 prefix blocks, promoteAndGC reaches the Fork.
        // At the Fork, the right branch (fork headers) gets GC'd.
        val promotionTime = addedTimeBase + 200 * 60 + 1000
        val ctx0_2 = BitcoinValidator.initCtx(forkedState)
        val (_, bestDepth, bestPath) =
            BitcoinValidator.bestChainPath(forkedState.forkTree, forkedState.blockHeight, 0)

        val params = testParams()
        val (promoted, cleaned) =
            BitcoinValidator.promoteAndGC(
              forkedState.forkTree,
              ctx0_2,
              bestPath,
              bestDepth,
              promotionTime,
              5,
              params
            )

        // All 3 prefix blocks should be promoted (plus possibly 1-2 from the left branch)
        assert(promoted.length >= 3, s"Expected at least 3 promotions, got ${promoted.length}")

        // The fork headers should be GC'd — they were on the non-best (right) branch
        forkHeaders.foreach { h =>
            assert(
              !cleaned.existsHash(blockHeaderHash(h)),
              "Fork headers should be GC'd after promotion along main chain"
            )
        }

        // Cleaned tree should have fewer blocks (promoted removed + fork GC'd)
        assert(
          cleaned.blockCount < forkedState.forkTree.blockCount,
          "Cleaned tree should have fewer blocks after promotion + GC"
        )
    }

    test("operational - sequential operations: build → extend → fork → extend main → promote") {
        val addedTimeBase = BigInt(1700000000)

        // Step 1: Build 99-block chain
        val (state99, _) = buildLongChainState(99, addedTimeBase)
        assert(state99.forkTree.blockCount == 99)

        // Use generous time offsets to stay well ahead of accumulated MTP
        val ctx0 = BitcoinValidator.initCtx(state99)

        // Step 2: Extend with 4 more headers (→ 103 blocks)
        // Need 103+ main chain blocks so all 3 prefix blocks have depth >= 100
        val blocks99 = state99.forkTree.toBlockList
        val ctxAtTip99 = blocks99.foldLeft(ctx0)(BitcoinValidator.accumulateBlock)
        val time2 = addedTimeBase + 50000
        val ext2Headers = genFakeHeaderChain(4, ctxAtTip99, time2).sample.get
        val ext2Update = UpdateOracle(
          blockHeaders = PList.from(ext2Headers),
          parentPath = PList(BigInt(98)),
          mpfInsertProofs = PNil
        )
        val state103 = BitcoinValidator.computeUpdate(state99, ext2Update, time2, testingParams)
        assert(state103.forkTree.blockCount == 103)

        // Step 3: Create a short fork at block 2 (early fork for easy GC)
        val blocks103 = state103.forkTree.toBlockList
        val ctxAt2 = blocks103.take(3).foldLeft(ctx0)(BitcoinValidator.accumulateBlock)
        val time3 = addedTimeBase + 50100
        val forkHeaders = genFakeHeaderChain(1, ctxAt2, time3).sample.get
        val forkUpdate = UpdateOracle(
          blockHeaders = PList.from(forkHeaders),
          parentPath = PList(BigInt(2)),
          mpfInsertProofs = PNil
        )
        val stateForked = BitcoinValidator.computeUpdate(state103, forkUpdate, time3, testingParams)
        assert(stateForked.forkTree.blockCount == 104)

        // Step 4: Extend main chain by 1 more (→ 104 in main branch)
        val ctxAtTip103 = blocks103.foldLeft(ctx0)(BitcoinValidator.accumulateBlock)
        val time4 = addedTimeBase + 50200

        // Tree after fork at block 2: Blocks(3, .., Fork(Blocks(100,..,End), Blocks(1,..,End)))
        // To extend main branch tip: pass through 3, enter left fork, go to last block (index 99)
        val extHeaders = genFakeHeaderChain(1, ctxAtTip103, time4).sample.get
        val extPath = PList.from(scala.List(BigInt(3), BigInt(0), BigInt(99)))
        val extUpdate = UpdateOracle(
          blockHeaders = PList.from(extHeaders),
          parentPath = extPath,
          mpfInsertProofs = PNil
        )
        val stateExtended =
            BitcoinValidator.computeUpdate(stateForked, extUpdate, time4, testingParams)
        assert(stateExtended.forkTree.blockCount == 105) // 104 main + 1 fork

        // Step 5: Promote with aged blocks
        // Promote 3+ blocks to get past the 3-block prefix and trigger GC at the Fork
        val promotionTime = addedTimeBase + 200 * 60 + 5000
        val ctx0_final = BitcoinValidator.initCtx(stateExtended)
        val (_, bestDepth, bestPath) =
            BitcoinValidator.bestChainPath(stateExtended.forkTree, stateExtended.blockHeight, 0)
        val params = testParams()
        val (promoted, cleaned) =
            BitcoinValidator.promoteAndGC(
              stateExtended.forkTree,
              ctx0_final,
              bestPath,
              bestDepth,
              promotionTime,
              5, // promote enough to pass the 3-block prefix and reach the Fork
              params
            )

        // Should promote at least 3 blocks (the prefix), triggering GC
        assert(promoted.length >= 3, s"Should promote at least 3 blocks, got ${promoted.length}")
        assert(promoted.length <= 5, "Should not exceed maxPromotions")

        // GC should remove the fork branch (right branch at the Fork node)
        forkHeaders.foreach { h =>
            assert(!cleaned.existsHash(blockHeaderHash(h)), "Fork should be GC'd")
        }

        // Main chain blocks beyond promoted should still be in cleaned tree
        val promotedHashes = promoted.toScalaList.map(_.hash).toSet
        val mainBlocks = stateExtended.forkTree.toBlockList
        mainBlocks.foreach { b =>
            if !promotedHashes.contains(b.hash) then
                val isForkBlock = forkHeaders.exists(h => blockHeaderHash(h) == b.hash)
                if !isForkBlock then
                    assert(cleaned.existsHash(b.hash), s"Main chain block should survive GC")
        }
    }

    test("operational - varying chain lengths 99-101 all work correctly") {
        forAll(Gen.choose(99, 101)) { chainLen =>
            val addedTimeBase = BigInt(1700000000)
            val (state, _) = buildLongChainState(chainLen, addedTimeBase)
            assert(state.forkTree.blockCount == chainLen)

            // Verify tree is linear
            state.forkTree match
                case Blocks(blocks, cw, End) =>
                    assert(blocks.length == chainLen)
                    assert(cw > 0)
                case _ => fail("Expected linear tree")

            // bestChainPath should report correct depth
            val (_, depth, path) =
                BitcoinValidator.bestChainPath(state.forkTree, state.blockHeight, 0)
            assert(depth == state.blockHeight + chainLen)
            assert(path == PNil) // linear → no forks in path
        }
    }

    test("computeUpdate - backward-timestamp blocks accepted in testingMode") {
        val state = makeTestChainState()
        val ctx = BitcoinValidator.initCtx(state)
        val currentTime = state.recentTimestamps.head + 5000

        // Generate multiple chains and check that backward timestamps work
        var hasBackward = false
        var attempts = 0
        while !hasBackward && attempts < 20 do {
            attempts += 1
            val headers = genFakeHeaderChain(5, ctx, currentTime).sample.get
            val timestamps = headers.map(_.timestamp)
            val backward = timestamps.sliding(2).exists {
                case scala.List(a, b) => b < a
                case _                => false
            }
            if backward then hasBackward = true

            // Should always succeed regardless of backward timestamps
            val update = UpdateOracle(
              blockHeaders = PList.from(headers),
              parentPath = PNil,
              mpfInsertProofs = PNil
            )
            val newState =
                BitcoinValidator.computeUpdate(state, update, currentTime, testingParams)
            assert(newState.forkTree.blockCount == headers.size)
        }
        // Our generator produces backward timestamps ~30% of the time,
        // so 20 attempts should be more than enough
        assert(hasBackward, "Expected at least one chain with backward timestamps in 20 attempts")
    }
}
