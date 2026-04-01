package binocular

import binocular.BitcoinHelpers.*
import binocular.ForkTree.*
import org.scalacheck.Gen
import scalus.cardano.onchain.plutus.prelude
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.cardano.onchain.plutus.prelude.List.Cons
import scalus.cardano.onchain.plutus.v1.PubKeyHash
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}
import scalus.testing.kit.Party
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.hex

trait BitcoinValidatorGenerators extends scalus.uplc.test.ArbitraryInstances {

    // ============================================================================
    // Primitive generators
    // ============================================================================

    /** Random 32-byte hash */
    val genBlockHash: Gen[ByteString] = genByteStringOfN(32)

    /** Deterministic unique hash from an integer id */
    def genUniqueBlockHash(id: Int): ByteString = {
        val bytes = Array.fill(32)(0.toByte)
        bytes(0) = ((id >> 24) & 0xff).toByte
        bytes(1) = ((id >> 16) & 0xff).toByte
        bytes(2) = ((id >> 8) & 0xff).toByte
        bytes(3) = (id & 0xff).toByte
        ByteString.unsafeFromArray(bytes)
    }

    /** Valid 4-byte compact bits that decode to a target in [1, PowLimit]. PowLimit compact is
      * 0x1d00ffff. We generate exponents 3..29, then cap the coefficient for exponent 29 to stay
      * within PowLimit.
      */
    val genCompactBits: Gen[CompactBits] =
        for {
            exponent <- Gen.choose(3, 29)
            maxCoeff = if exponent >= 29 then 0x00ffff else 0x7fffff
            coefficient <- Gen.choose(1, maxCoeff)
        } yield {
            val compact = BigInt(coefficient) + (BigInt(exponent) * BigInt(0x1000000))
            integerToByteString(false, 4, compact)
        }

    /** Bitcoin-era timestamp (2009–2040) */
    val genTimestamp: Gen[BigInt] =
        Gen.choose(1231006505L, 2208988800L).map(BigInt(_))

    /** n timestamps in a Scalus list, newest-first, with 5–15 min gaps */
    def genTimestampListDesc(n: Int): Gen[PList[BigInt]] =
        for
            base <- Gen.choose(1500000000L, 1700000000L).map(BigInt(_))
            gaps <- Gen.listOfN(n - 1, Gen.choose(300L, 900L).map(BigInt(_)))
        yield
            val timestamps = gaps.scanLeft(base) { (ts, gap) => ts - gap }
            PList.from(timestamps)

    // ============================================================================
    // Domain generators
    // ============================================================================

    /** Random BlockSummary with given id for uniqueness */
    def genBlockSummaryWithId(id: Int, baseTimestamp: Long, addedTime: Long): BlockSummary =
        BlockSummary(
          hash = genUniqueBlockHash(id),
          timestamp = BigInt(baseTimestamp),
          addedTimeDelta = BigInt(addedTime - baseTimestamp)
        )

    /** n BlockSummaries with unique hashes. Bitcoin timestamps may go backward (jitter ±300s around
      * base), but added time (Cardano observation time) increases monotonically. addedTimeDelta =
      * addedTime - timestamp.
      */
    def genBlockSummarySeq(n: Int): Gen[scala.List[BlockSummary]] =
        for {
            baseTimestamp <- Gen.choose(1500000000L, 1700000000L)
            jitters <- Gen.listOfN(n, Gen.choose(-300, 300))
            addedBase <- Gen.choose(1500000000L, 1700000000L)
        } yield {
            (0 until n).map { i =>
                val ts = BigInt(baseTimestamp + i * 600L + jitters(i))
                val addedTime = BigInt(addedBase + i * 10L) // monotonically increasing
                BlockSummary(
                  hash = genUniqueBlockHash(i + 1),
                  timestamp = ts,
                  addedTimeDelta = addedTime - ts
                )
            }.toList
        }

    /** Random ForkTree with bounded depth */
    def genForkTree(maxDepth: Int): Gen[ForkTree] =
        if maxDepth <= 0 then Gen.const(End)
        else
            Gen.frequency(
              2 -> Gen.const(End),
              5 -> (for {
                  n <- Gen.choose(1, 5)
                  blocks <- genBlockSummarySeq(n)
                  cw <- Gen.choose(1L, 10000L).map(BigInt(_))
                  next <- genForkTree(maxDepth - 1)
              } yield Blocks(PList.from(blocks), cw, next)),
              3 -> (for {
                  left <- genForkTree(maxDepth - 1)
                  right <- genForkTree(maxDepth - 1)
              } yield Fork(left, right))
            )

    /** Non-empty ForkTree */
    val genNonEmptyForkTree: Gen[ForkTree] =
        genForkTree(4).suchThat(_.nonEmpty)

    /** Linear ForkTree (no forks, single Blocks node) */
    def genLinearForkTree(maxBlocks: Int): Gen[ForkTree] =
        for {
            n <- Gen.choose(1, maxBlocks)
            blocks <- genBlockSummarySeq(n)
            cw <- Gen.choose(1L, 10000L).map(BigInt(_))
        } yield Blocks(PList.from(blocks), cw, End)

    /** Random TraversalCtx with 11 timestamps (not necessarily sorted — reflects real Bitcoin) */
    val genTraversalCtx: Gen[TraversalCtx] =
        for {
            timestamps <- genTimestampListDesc(11)
            height <- Gen.choose(1000L, 900000L).map(BigInt(_))
            bits <- genCompactBits
            hash <- genBlockHash
        } yield TraversalCtx(
          timestamps = timestamps,
          height = height,
          currentBits = bits,
          prevDiffAdjTimestamp = timestamps.last,
          lastBlockHash = hash
        )

    /** TraversalCtx at a height where (height+1) % 2016 != 0 */
    val genNonRetargetCtx: Gen[TraversalCtx] =
        genTraversalCtx.suchThat { ctx =>
            (ctx.height + 1) % DifficultyAdjustmentInterval != BigInt(0)
        }

    // ============================================================================
    // Fake header generators (for testingMode)
    // ============================================================================

    /** Build a fake 80-byte block header that passes MTP/future-time/chain-link checks. Requires
      * testingMode=true to skip PoW+difficulty checks.
      *
      * Timestamp is chosen in [mtp+1, currentTime+MaxFutureBlockTime]. When currentTime is close to
      * MTP (happens after many blocks), the "normal" range automatically shifts upward, and the
      * "backward" range narrows — both stay valid.
      */
    def genFakeBlockHeader(
        prevHash: BlockHash,
        timestamps: PList[BigInt],
        currentTime: BigInt,
        bits: CompactBits
    ): Gen[BlockHeader] = {
        val sortedTimestamps =
            BitcoinValidator.insertionSort(timestamps.take(MedianTimeSpan))
        val mtp = sortedTimestamps.at(5)
        val minTimestamp = mtp + 1
        // Blocks can be up to 2 hours in the future relative to currentTime
        val maxTimestamp = currentTime + MaxFutureBlockTime

        // Normal case: near currentTime but always >= minTimestamp
        val normalLow = minTimestamp.max(currentTime - 300)
        val normalHigh = minTimestamp.max(currentTime)
        // Backward case: just above MTP (may equal normal when MTP ≈ currentTime)
        val backwardHigh = minTimestamp.max(mtp + 600).min(maxTimestamp)

        for {
            timestamp <- Gen.frequency(
              7 -> Gen.choose(normalLow.toLong, normalHigh.toLong),
              3 -> Gen.choose(minTimestamp.toLong, backwardHigh.toLong)
            )
            merkleRoot <- Gen.listOfN(32, Gen.choose(0, 255).map(_.toByte))
            nonce <- Gen.choose(0L, 0xffffffffL)
        } yield {
            val version = integerToByteString(false, 4, BigInt(0x20000000))
            val merkleRootBs = ByteString.unsafeFromArray(merkleRoot.toArray)
            val timestampBs = integerToByteString(false, 4, BigInt(timestamp))
            val nonceBs = integerToByteString(false, 4, BigInt(nonce))
            BlockHeader(version ++ prevHash ++ merkleRootBs ++ timestampBs ++ bits ++ nonceBs)
        }
    }

    /** Chain N fake headers, each linking to previous via hash */
    def genFakeHeaderChain(
        n: Int,
        startCtx: TraversalCtx,
        currentTime: BigInt
    ): Gen[scala.List[BlockHeader]] = {
        if n <= 0 then Gen.const(scala.List.empty)
        else
            def loop(
                remaining: Int,
                ctx: TraversalCtx,
                acc: scala.List[BlockHeader]
            ): Gen[scala.List[BlockHeader]] = {
                if remaining <= 0 then Gen.const(acc.reverse)
                else
                    genFakeBlockHeader(
                      ctx.lastBlockHash,
                      ctx.timestamps,
                      currentTime,
                      ctx.currentBits
                    ).flatMap { header =>
                        val hash = blockHeaderHash(header)
                        val newTimestamps = Cons(header.timestamp, ctx.timestamps)
                        val newHeight = ctx.height + 1
                        val newCtx = TraversalCtx(
                          timestamps = newTimestamps,
                          height = newHeight,
                          currentBits = ctx.currentBits, // testingMode: bits don't change
                          prevDiffAdjTimestamp = ctx.prevDiffAdjTimestamp,
                          lastBlockHash = hash
                        )
                        loop(remaining - 1, newCtx, header :: acc)
                    }
            }
            loop(n, startCtx, scala.List.empty)
    }

    // ============================================================================
    // Test helpers
    // ============================================================================

    val testTxOutRef: TxOutRef = TxOutRef(
      TxId(hex"0000000000000000000000000000000000000000000000000000000000000000"),
      BigInt(0)
    )

    val testOwner: PubKeyHash = PubKeyHash(Party.Alice.addrKeyHash)

    def testParams(testingMode: Boolean = false): BitcoinValidatorParams =
        BitcoinValidatorParams(
          maturationConfirmations = 100,
          challengeAging = 200 * 60,
          oneShotTxOutRef = testTxOutRef,
          closureTimeout = 30 * 24 * 60 * 60,
          owner = testOwner,
          powLimit = BitcoinHelpers.PowLimit,
          maxBlocksInForkTree = BitcoinContract.DefaultMaxBlocksInForkTree,
          testingMode = testingMode
        )

    val testingParams: BitcoinValidatorParams = testParams(testingMode = true)

    /** Build a ChainState suitable for property tests in testingMode */
    def makeTestChainState(
        height: BigInt = 866880,
        bits: CompactBits = integerToByteString(false, 4, BigInt(0x1d00ffff)),
        tree: ForkTree = End
    ): ChainState = {
        val hash = genUniqueBlockHash(0)
        val baseTimestamp = BigInt(1700000000)
        val timestamps = PList.from((0 until 11).map(i => baseTimestamp - i * 600).toList)
        ChainState(
          confirmedBlocksRoot = ByteString.unsafeFromArray(Array.fill(32)(0.toByte)),
          ctx = TraversalCtx(
            timestamps = timestamps,
            height = height,
            currentBits = bits,
            prevDiffAdjTimestamp = baseTimestamp - 11 * 600,
            lastBlockHash = hash
          ),
          forkTree = tree
        )
    }

    // ============================================================================
    // Operational scenario helpers (long chains)
    // ============================================================================

    /** Build a ChainState with N blocks in the fork tree by inserting fake headers via
      * computeUpdate in testingMode. Returns (state, headers) so tests can reference the inserted
      * headers.
      */
    def buildLongChainState(
        numBlocks: Int,
        addedTimeBase: BigInt = BigInt(1700000000)
    ): (ChainState, scala.List[BlockHeader]) = {
        val state0 = makeTestChainState()
        val ctx0 = state0.ctx
        // currentTime must be > MTP of timestamps and allow future-time check.
        // addedTimeBase is used as the Cardano observation time (= currentTime for each batch).
        val currentTime = addedTimeBase
        val headers = genFakeHeaderChain(numBlocks, ctx0, currentTime).sample.get
        val state =
            BitcoinValidator.computeUpdate(
              state = state0,
              blockHeaders = PList.from(headers),
              parentPath = PList.Nil,
              mpfInsertProofs = PList.Nil,
              currentTime = currentTime,
              params = testingParams
            )
        (state, headers)
    }

    /** Build a chain state with N blocks, then generate M more fake headers that extend the tip.
      * Returns (stateWithN, extensionHeaders, ctx at tip of N blocks).
      */
    def buildLongChainAndExtension(
        numExisting: Int,
        numNew: Int,
        addedTimeBase: BigInt = BigInt(1700000000)
    ): (ChainState, scala.List[BlockHeader]) = {
        val (stateN, existingHeaders) = buildLongChainState(numExisting, addedTimeBase)
        // Build ctx at the tip of the existing chain by replaying through validateBlock
        val ctx0 = stateN.ctx
        // We need the ctx after all N blocks. Since the blocks are in the fork tree,
        // we can accumulate them.
        val allBlocks = stateN.forkTree.toBlockList
        val ctxAtTip = allBlocks.foldLeft(ctx0)((c, b) =>
            BitcoinValidator.accumulateBlock(c, b, BitcoinHelpers.PowLimit)
        )
        val currentTime = addedTimeBase + 100 // slightly later for the extension
        val newHeaders = genFakeHeaderChain(numNew, ctxAtTip, currentTime).sample.get
        (stateN, newHeaders)
    }

    // ============================================================================
    // Classify helpers
    // ============================================================================

    def classifyTreeShape(tree: ForkTree): String = tree match
        case End                      => "empty"
        case Blocks(_, _, End)        => "single-segment"
        case Blocks(_, _, Fork(_, _)) => "segment-then-fork"
        case Fork(_, _)               => "root-fork"
        case _                        => "complex"

    def classifyBlockCount(tree: ForkTree): String = {
        val count = tree.blockCount
        if count == 0 then "empty"
        else if count <= 3 then "small(1-3)"
        else if count <= 10 then "medium(4-10)"
        else "large(11+)"
    }

    def classifyForkCount(tree: ForkTree): String = {
        def countForks(t: ForkTree): Int = t match
            case Fork(l, r)      => 1 + countForks(l) + countForks(r)
            case Blocks(_, _, n) => countForks(n)
            case End             => 0
        val count = countForks(tree)
        if count == 0 then "linear(0)"
        else if count == 1 then "single(1)"
        else if count <= 3 then "few(2-3)"
        else "many(4+)"
    }

    def classifyTimestampOrdering(blocks: scala.List[BlockSummary]): String = {
        val hasBackward = blocks.sliding(2).exists {
            case scala.List(a, b) => b.timestamp < a.timestamp
            case _                => false
        }
        if hasBackward then "has-backward-timestamps" else "monotonic-increasing"
    }
}
