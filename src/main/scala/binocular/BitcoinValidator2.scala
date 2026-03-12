package binocular

import scalus.uplc.builtin.*
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.Data.{toData, FromData, ToData}
import scalus.cardano.onchain.plutus.v1.{Address, Credential, PolicyId, PosixTime}
import scalus.cardano.onchain.plutus.v2.{IntervalBoundType, OutputDatum}
import scalus.cardano.onchain.plutus.v3.{DataParameterizedValidator, Datum, TxInfo, TxOut, TxOutRef}
import scalus.cardano.onchain.plutus.prelude.{List, *}
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry as MPF
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry.ProofStep
import scalus.compiler.Compile
import scalus.{show as _, *}

type PosixTimeSeconds = BigInt

case class BlockSummary2(
    hash: BlockHash, // Block hash
    timestamp: PosixTime, // Bitcoin block timestamp (for median-time-past)
    addedTimeSeconds: PosixTimeSeconds // Cardano time when this block was added to forksTree
) derives FromData,
      ToData

enum ForkTree derives FromData, ToData {
    case Blocks(blocks: List[BlockSummary2], next: ForkTree)
    case Fork(left: ForkTree, right: ForkTree)
    case End
}

case class ChainState2(
    blockHeight: BigInt,
    blockHash: BlockHash,
    currentTarget: CompactBits,
    recentTimestamps: List[PosixTime],
    previousDifficultyAdjustmentTimestamp: PosixTime,
    confirmedBlocksRoot: ByteString,
    forksTree: ForkTree
) derives FromData,
      ToData

type PathElement = BigInt
type Path = List[PathElement]

case class UpdateOracle2(
    blockHeaders: List[BlockHeader],
    parentPath: Path,
    mpfInsertProofs: List[List[ProofStep]]
) derives FromData,
      ToData

case class TraversalCtx(
    timestamps: List[BigInt], // newest-first (prepended during accumulation)
    height: BigInt,
    currentBits: CompactBits,
    prevDiffAdjTimestamp: BigInt,
    lastBlockHash: BlockHash,
    chainwork: BigInt // cumulative relative to confirmed tip
) derives FromData,
      ToData

@Compile
object BitcoinValidator2 extends DataParameterizedValidator {
    import BitcoinHelpers.*

    // Binocular protocol parameters
    val MaturationConfirmations: BigInt = 100
    val ChallengeAging: BigInt = 200 * 60 // 200 minutes in seconds

    // ============================================================================
    // TraversalCtx helpers
    // ============================================================================

    /** Initialize traversal context from confirmed state. chainwork starts at 0 (relative). */
    def initCtx(state: ChainState2): TraversalCtx =
        TraversalCtx(
          timestamps = state.recentTimestamps,
          height = state.blockHeight,
          currentBits = state.currentTarget,
          prevDiffAdjTimestamp = state.previousDifficultyAdjustmentTimestamp,
          lastBlockHash = state.blockHash,
          chainwork = BigInt(0)
        )

    /** Accumulate one existing block into the traversal context. */
    def accumulateBlock(ctx: TraversalCtx, block: BlockSummary2): TraversalCtx = {
        val newHeight = ctx.height + 1
        val newTimestamps = List.Cons(block.timestamp, ctx.timestamps)
        val newBits = getNextWorkRequired(
          ctx.height,
          ctx.currentBits,
          newTimestamps.head,
          ctx.prevDiffAdjTimestamp
        )
        val newChainwork =
            ctx.chainwork + calculateBlockProof(compactBitsToTarget(newBits))
        val newPrevDiffAdjTimestamp =
            if newHeight % DifficultyAdjustmentInterval == BigInt(0) then block.timestamp
            else ctx.prevDiffAdjTimestamp
        TraversalCtx(
          timestamps = newTimestamps,
          height = newHeight,
          currentBits = newBits,
          prevDiffAdjTimestamp = newPrevDiffAdjTimestamp,
          lastBlockHash = block.hash,
          chainwork = newChainwork
        )
    }

    /** Accumulate exactly `count` blocks from the head of `blocks` (oldest-first). */
    def accumulateN(
        ctx: TraversalCtx,
        blocks: List[BlockSummary2],
        count: BigInt
    ): TraversalCtx = {
        if count == BigInt(0) then ctx
        else
            blocks match
                case List.Nil => fail("accumulateN: not enough blocks")
                case List.Cons(head, tail) =>
                    accumulateN(accumulateBlock(ctx, head), tail, count - 1)
    }

    /** Accumulate all blocks in the list. */
    def accumulateCtx(ctx: TraversalCtx, blocks: List[BlockSummary2]): TraversalCtx = {
        blocks match
            case List.Nil              => ctx
            case List.Cons(head, tail) => accumulateCtx(accumulateBlock(ctx, head), tail)
    }

    // ============================================================================
    // Block validation
    // ============================================================================

    /** Validate a single new block header against the traversal context.
      * Returns the BlockSummary2 and updated context.
      */
    def validateBlock(
        header: BlockHeader,
        ctx: TraversalCtx,
        currentTime: BigInt
    ): (BlockSummary2, TraversalCtx) = {
        val hash = blockHeaderHash(header)
        val hashInt = byteStringToInteger(false, hash)

        // PoW validation
        val target = compactBitsToTarget(header.bits)
        require(hashInt <= target, "Invalid proof-of-work")
        require(target <= PowLimit, "Target exceeds PowLimit")

        // Difficulty validation
        val expectedBits = getNextWorkRequired(
          ctx.height,
          ctx.currentBits,
          ctx.timestamps.head,
          ctx.prevDiffAdjTimestamp
        )
        require(header.bits == expectedBits, "Invalid difficulty")

        // MTP validation
        val sortedTimestamps = ctx.timestamps.take(MedianTimeSpan).quicksort
        val medianTimePast = sortedTimestamps !! (BigInt(5))
        require(header.timestamp > medianTimePast, "Block timestamp not greater than MTP")

        // Future time validation
        require(
          header.timestamp <= currentTime + MaxFutureBlockTime,
          "Block timestamp too far in future"
        )

        // Version validation
        require(header.version >= 4, "Outdated block version")

        // Parent hash validation
        require(header.prevBlockHash == ctx.lastBlockHash, "Parent hash mismatch")

        // Build new context
        val newHeight = ctx.height + 1
        val newTimestamps = List.Cons(header.timestamp, ctx.timestamps)
        val newChainwork = ctx.chainwork + calculateBlockProof(target)
        val newPrevDiffAdjTimestamp =
            if newHeight % DifficultyAdjustmentInterval == BigInt(0) then header.timestamp
            else ctx.prevDiffAdjTimestamp

        val summary = BlockSummary2(
          hash = hash,
          timestamp = header.timestamp,
          addedTimeSeconds = currentTime
        )

        val newCtx = TraversalCtx(
          timestamps = newTimestamps,
          height = newHeight,
          currentBits = expectedBits,
          prevDiffAdjTimestamp = newPrevDiffAdjTimestamp,
          lastBlockHash = hash,
          chainwork = newChainwork
        )

        (summary, newCtx)
    }

    /** Validate a list of headers (oldest-first), returning summaries and final context. */
    def validateAndCollectBlocks(
        headers: List[BlockHeader],
        ctx: TraversalCtx,
        currentTime: BigInt
    ): (List[BlockSummary2], TraversalCtx) = {
        def loop(
            remaining: List[BlockHeader],
            acc: List[BlockSummary2],
            currentCtx: TraversalCtx
        ): (List[BlockSummary2], TraversalCtx) = {
            remaining match
                case List.Nil => (acc.reverse, currentCtx)
                case List.Cons(header, tail) =>
                    val (summary, newCtx) = validateBlock(header, currentCtx, currentTime)
                    loop(tail, List.Cons(summary, acc), newCtx)
        }
        loop(headers, List.Nil, ctx)
    }

    // ============================================================================
    // Tree insertion
    // ============================================================================

    /** Insert validated blocks into the fork tree following the given path. */
    def insertBlocks(
        tree: ForkTree,
        path: Path,
        headers: List[BlockHeader],
        ctx: TraversalCtx,
        currentTime: BigInt
    ): ForkTree = {
        require(!path.isEmpty, "Path is empty")
        val pathHead = path.head
        val pathTail = path.tail

        tree match
            case ForkTree.Blocks(blocks, next) =>
                if pathHead == blocks.size then
                    // Pass through: accumulate all blocks, recurse into next
                    val newCtx = accumulateCtx(ctx, blocks)
                    ForkTree.Blocks(blocks, insertBlocks(next, pathTail, headers, newCtx, currentTime))
                else
                    // Parent is block at index pathHead
                    val parentCtx = accumulateN(ctx, blocks, pathHead + 1)
                    require(
                      headers.head.prevBlockHash == parentCtx.lastBlockHash,
                      "Parent hash mismatch at insertion point"
                    )
                    val (newSummaries, _) =
                        validateAndCollectBlocks(headers, parentCtx, currentTime)
                    val isLast = pathHead == blocks.size - 1

                    if isLast then
                        next match
                            case ForkTree.End =>
                                ForkTree.Blocks(blocks ++ newSummaries, ForkTree.End)
                            case _ =>
                                ForkTree.Blocks(
                                  blocks,
                                  ForkTree.Fork(next, ForkTree.Blocks(newSummaries, ForkTree.End))
                                )
                    else
                        val prefix = blocks.take(pathHead + 1)
                        val suffix = blocks.drop(pathHead + 1)
                        ForkTree.Blocks(
                          prefix,
                          ForkTree.Fork(
                            ForkTree.Blocks(suffix, next),
                            ForkTree.Blocks(newSummaries, ForkTree.End)
                          )
                        )

            case ForkTree.Fork(left, right) =>
                if pathHead == BigInt(0) then
                    ForkTree.Fork(
                      insertBlocks(left, pathTail, headers, ctx, currentTime),
                      right
                    )
                else
                    ForkTree.Fork(
                      left,
                      insertBlocks(right, pathTail, headers, ctx, currentTime)
                    )

            case ForkTree.End =>
                fail("Path leads to End")
    }

    // ============================================================================
    // Best chain selection
    // ============================================================================

    /** Find the best (highest chainwork) chain path through the tree.
      * Returns (chainwork, depth, path-to-best).
      * Single full traversal of the tree.
      */
    def bestChainPath(
        tree: ForkTree,
        ctx: TraversalCtx
    ): (BigInt, BigInt, Path) = {
        tree match
            case ForkTree.Blocks(blocks, next) =>
                val newCtx = accumulateCtx(ctx, blocks)
                bestChainPath(next, newCtx)

            case ForkTree.Fork(left, right) =>
                val (leftWork, leftDepth, leftPath) = bestChainPath(left, ctx)
                val (rightWork, rightDepth, rightPath) = bestChainPath(right, ctx)
                if leftWork >= rightWork then
                    (leftWork, leftDepth, List.Cons(BigInt(0), leftPath))
                else (rightWork, rightDepth, List.Cons(BigInt(1), rightPath))

            case ForkTree.End =>
                (ctx.chainwork, ctx.height, List.Nil)
    }

    // ============================================================================
    // Promotion and GC
    // ============================================================================

    /** Split promotable blocks from a Blocks node.
      * Walks oldest→newest. A block is eligible if:
      *   bestDepth - blockHeight >= MaturationConfirmations AND
      *   currentTime - addedTimeSeconds >= ChallengeAging
      *
      * Returns (promoted oldest-first, remaining oldest-first).
      */
    def splitPromotable(
        blocks: List[BlockSummary2],
        ctx: TraversalCtx,
        bestDepth: BigInt,
        currentTime: BigInt
    ): (List[BlockSummary2], List[BlockSummary2], TraversalCtx) = {
        blocks match
            case List.Nil => (List.Nil, List.Nil, ctx)
            case List.Cons(block, tail) =>
                val blockHeight = ctx.height + 1
                val depth = bestDepth - blockHeight
                val age = currentTime - block.addedTimeSeconds
                if depth >= MaturationConfirmations && age >= ChallengeAging then
                    val newCtx = accumulateBlock(ctx, block)
                    val (morePromoted, remaining, finalCtx) =
                        splitPromotable(tail, newCtx, bestDepth, currentTime)
                    (List.Cons(block, morePromoted), remaining, finalCtx)
                else
                    // This block not eligible, so neither are any following (they are newer/shallower)
                    (List.Nil, blocks, ctx)
    }

    /** Promote eligible blocks and GC dead forks along the best path.
      * Returns (promoted blocks oldest-first, cleaned tree).
      */
    def promoteAndGC(
        tree: ForkTree,
        ctx: TraversalCtx,
        bestPath: Path,
        bestDepth: BigInt,
        currentTime: BigInt
    ): (List[BlockSummary2], ForkTree) = {
        tree match
            case ForkTree.Blocks(blocks, next) =>
                val (promoted, remaining, newCtx) =
                    splitPromotable(blocks, ctx, bestDepth, currentTime)
                if promoted.isEmpty then
                    // No promotion here — accumulate and recurse into next for GC
                    val fullCtx = accumulateCtx(ctx, blocks)
                    val (nextPromoted, cleanedNext) =
                        promoteAndGC(next, fullCtx, bestPath, bestDepth, currentTime)
                    (nextPromoted, ForkTree.Blocks(blocks, cleanedNext))
                else if remaining.isEmpty then
                    // All blocks promoted — recurse into next for more promotion
                    val (nextPromoted, cleanedNext) =
                        promoteAndGC(next, newCtx, bestPath, bestDepth, currentTime)
                    (promoted ++ nextPromoted, cleanedNext)
                else
                    // Partial promotion — return remaining as new Blocks node
                    (promoted, ForkTree.Blocks(remaining, next))

            case ForkTree.Fork(left, right) =>
                require(!bestPath.isEmpty, "Best path exhausted at Fork")
                val direction = bestPath.head
                val pathTail = bestPath.tail
                if direction == BigInt(0) then
                    // Follow left, drop right (GC)
                    val (promoted, cleanedLeft) =
                        promoteAndGC(left, ctx, pathTail, bestDepth, currentTime)
                    (promoted, cleanedLeft)
                else
                    // Follow right, drop left (GC)
                    val (promoted, cleanedRight) =
                        promoteAndGC(right, ctx, pathTail, bestDepth, currentTime)
                    (promoted, cleanedRight)

            case ForkTree.End =>
                (List.Nil, ForkTree.End)
    }

    // ============================================================================
    // Apply promotions to confirmed state
    // ============================================================================

    /** Apply promoted blocks to the confirmed state, updating MPF root. */
    def applyPromotions(
        state: ChainState2,
        promoted: List[BlockSummary2],
        mpfProofs: List[List[ProofStep]]
    ): ChainState2 = {
        def loop(
            blocks: List[BlockSummary2],
            proofs: List[List[ProofStep]],
            ctx: TraversalCtx,
            mpfRoot: ByteString
        ): (TraversalCtx, ByteString) = {
            blocks match
                case List.Nil =>
                    proofs match
                        case List.Nil => (ctx, mpfRoot)
                        case _        => fail("MPF proof count mismatch")
                case List.Cons(block, bTail) =>
                    proofs match
                        case List.Cons(proof, pTail) =>
                            val newCtx = accumulateBlock(ctx, block)
                            val newRoot =
                                MPF(mpfRoot).insert(block.hash, block.hash, proof).root
                            loop(bTail, pTail, newCtx, newRoot)
                        case List.Nil => fail("MPF proof count mismatch")
        }

        val ctx0 = initCtx(state)
        val (finalCtx, finalRoot) =
            loop(promoted, mpfProofs, ctx0, state.confirmedBlocksRoot)

        ChainState2(
          blockHeight = finalCtx.height,
          blockHash = finalCtx.lastBlockHash,
          currentTarget = finalCtx.currentBits,
          recentTimestamps = finalCtx.timestamps.take(MedianTimeSpan),
          previousDifficultyAdjustmentTimestamp = finalCtx.prevDiffAdjTimestamp,
          confirmedBlocksRoot = finalRoot,
          forksTree = state.forksTree // placeholder, will be overwritten by caller
        )
    }

    // ============================================================================
    // Main compute function
    // ============================================================================

    /** Compute the new ChainState2 after applying an update. */
    def computeUpdate2(
        prevState: ChainState2,
        update: UpdateOracle2,
        currentTime: BigInt
    ): ChainState2 = {
        val headers = update.blockHeaders
        val ctx0 = initCtx(prevState)

        // Step 1: Insert new blocks into tree
        val newTree =
            if headers.isEmpty then prevState.forksTree
            else if headers.head.prevBlockHash == prevState.blockHash then
                // Parent is confirmed tip — validate and attach directly
                val (newSummaries, _) =
                    validateAndCollectBlocks(headers, ctx0, currentTime)
                val newBranch = ForkTree.Blocks(newSummaries, ForkTree.End)
                prevState.forksTree match
                    case ForkTree.End => newBranch
                    case existing     => ForkTree.Fork(existing, newBranch)
            else
                insertBlocks(
                  prevState.forksTree,
                  update.parentPath,
                  headers,
                  ctx0,
                  currentTime
                )

        // Step 2: Find best chain (single full traversal)
        val (bestWork, bestDepth, bestPath) = bestChainPath(newTree, ctx0)

        // Step 3: Promote eligible blocks + GC dead forks (single traversal along bestPath)
        val (promoted, cleanedTree) =
            promoteAndGC(newTree, ctx0, bestPath, bestDepth, currentTime)

        // Step 4: Build result
        if promoted.isEmpty then
            ChainState2(
              blockHeight = prevState.blockHeight,
              blockHash = prevState.blockHash,
              currentTarget = prevState.currentTarget,
              recentTimestamps = prevState.recentTimestamps,
              previousDifficultyAdjustmentTimestamp =
                  prevState.previousDifficultyAdjustmentTimestamp,
              confirmedBlocksRoot = prevState.confirmedBlocksRoot,
              forksTree = cleanedTree
            )
        else
            val updatedState =
                applyPromotions(prevState, promoted, update.mpfInsertProofs)
            ChainState2(
              blockHeight = updatedState.blockHeight,
              blockHash = updatedState.blockHash,
              currentTarget = updatedState.currentTarget,
              recentTimestamps = updatedState.recentTimestamps,
              previousDifficultyAdjustmentTimestamp =
                  updatedState.previousDifficultyAdjustmentTimestamp,
              confirmedBlocksRoot = updatedState.confirmedBlocksRoot,
              forksTree = cleanedTree
            )
    }

    // ============================================================================
    // Spend entry point
    // ============================================================================

    def findUniqueOutputFrom(outputs: List[TxOut], scriptAddress: Address): TxOut = {
        val matchingOutputs = outputs.filter(out => out.address === scriptAddress)
        require(matchingOutputs.size == BigInt(1), "There must be exactly one continuing output")
        matchingOutputs.head
    }

    inline override def spend(
        param: Data,
        datum: Option[Datum],
        redeemer: Datum,
        tx: TxInfo,
        outRef: TxOutRef
    ): Unit = {
        val update = redeemer.to[UpdateOracle2]

        val intervalStartInSeconds = tx.validRange.from.boundType match
            case IntervalBoundType.Finite(time) => time / 1000
            case _                              => fail("Must have finite interval start")

        val inputs = tx.inputs
        val outputs = tx.outputs

        // Find own input
        val ownInput = inputs
            .find(_.outRef === outRef)
            .getOrFail("Input not found")
            .resolved

        val prevState = ownInput.datum match
            case OutputDatum.OutputDatum(d) => d.to[ChainState2]
            case _                          => fail("No inline datum")

        // Compute expected new state
        val computedState = computeUpdate2(prevState, update, intervalStartInSeconds)

        // Find continuing output
        val continuingOutput = findUniqueOutputFrom(outputs, ownInput.address)

        // NFT preservation
        require(
          ownInput.value.withoutLovelace === continuingOutput.value.withoutLovelace,
          "Non-ADA tokens must be preserved"
        )

        // Verify output datum matches computed state
        val providedOutputDatum = continuingOutput.datum match
            case OutputDatum.OutputDatum(d) => d
            case _                          => fail("Continuing output must have inline datum")

        require(
          computedState.toData == providedOutputDatum,
          "Computed state does not match provided output datum"
        )
    }
}
