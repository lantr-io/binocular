package binocular

import binocular.ForkTree.{Blocks, End, Fork}
import scalus.uplc.builtin.*
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.Data.{toData, FromData, ToData}
import scalus.cardano.onchain.plutus.v1.{Address, Credential, PolicyId, PosixTime}
import scalus.cardano.onchain.plutus.v2.{IntervalBoundType, OutputDatum}
import scalus.cardano.onchain.plutus.v3.{DataParameterizedValidator, Datum, TxInfo, TxOut, TxOutRef}
import scalus.cardano.onchain.plutus.prelude.{List, *}
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry as MPF
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry.ProofStep
import scalus.cardano.onchain.plutus.prelude.List.*
import scalus.compiler.Compile
import scalus.{show as _, *}

type PosixTimeSeconds = BigInt
type Chainwork = BigInt
type NonEmptyList[A] = List[A]
type List11Timestamps = List[PosixTimeSeconds]
type MPFRoot = ByteString

case class BlockSummary2(
    hash: BlockHash, // Block hash
    timestamp: PosixTime, // Bitcoin block timestamp (for median-time-past)
    addedTimeSeconds: PosixTimeSeconds // Cardano time when this block was added to forksTree
) derives FromData,
      ToData

enum ForkTree derives FromData, ToData {
    case Blocks(blocks: NonEmptyList[BlockSummary2], chainwork: Chainwork, next: ForkTree)
    case Fork(left: ForkTree, right: ForkTree)
    case End
}

case class ChainState2(
    blockHeight: BigInt,
    blockHash: BlockHash,
    currentTarget: CompactBits,
    recentTimestamps: List11Timestamps,
    previousDifficultyAdjustmentTimestamp: PosixTime,
    confirmedBlocksRoot: MPFRoot,
    forksTree: ForkTree
) derives FromData,
      ToData

/** Path in [[ForkTree]]
  *
  *   - empty mean no blocks in fork tree, take parent from confirmed state
  *   - non-negative number means index in Blocks node (0-based, oldest-first)
  *   - 0 in Fork means go left, 1 means go right
  */
type PathElement = BigInt
type Path = List[PathElement]

case class UpdateOracle2(
    blockHeaders: List[BlockHeader],
    parentPath: Path,
    mpfInsertProofs: List[List[ProofStep]]
) derives FromData,
      ToData

case class TraversalCtx(
    timestamps: NonEmptyList[PosixTimeSeconds], // newest-first (prepended during accumulation)
    height: BigInt,
    currentBits: CompactBits,
    prevDiffAdjTimestamp: PosixTimeSeconds,
    lastBlockHash: BlockHash
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

    /** Initialize traversal context from the confirmed state.
      *
      * The context starts at the confirmed tip, with height, bits, timestamps, and hash matching the
      * confirmed chain. All tree traversal (accumulation, validation, chainwork) builds on top of
      * this starting point.
      */
    def initCtx(state: ChainState2): TraversalCtx =
        TraversalCtx(
          timestamps = state.recentTimestamps,
          height = state.blockHeight,
          currentBits = state.currentTarget,
          prevDiffAdjTimestamp = state.previousDifficultyAdjustmentTimestamp,
          lastBlockHash = state.blockHash
        )

    /** Accumulate one already-validated block into the traversal context.
      *
      * Replays difficulty computation without re-validating PoW or timestamps. Used when walking
      * existing blocks in the tree (e.g. to build ctx before validating new headers, or to compute
      * chainwork for a prefix during splits).
      *
      * At retarget boundaries (newHeight % 2016 == 0), updates `prevDiffAdjTimestamp` to this
      * block's timestamp — it becomes `nFirstBlockTime` for the next retarget calculation.
      */
    def accumulateBlock(ctx: TraversalCtx, block: BlockSummary2): TraversalCtx = {
        val newHeight = ctx.height + 1
        // Compute expected bits for block at newHeight (same as getNextWorkRequired in Bitcoin Core)
        val newBits = getNextWorkRequired(
          ctx.height,
          ctx.currentBits,
          ctx.timestamps.head,
          ctx.prevDiffAdjTimestamp
        )
        val newTimestamps = Cons(block.timestamp, ctx.timestamps)
        // At retarget boundary: record this block's timestamp as the period start for next retarget
        val newPrevDiffAdjTimestamp =
            if newHeight % DifficultyAdjustmentInterval == BigInt(0) then block.timestamp
            else ctx.prevDiffAdjTimestamp
        TraversalCtx(
          timestamps = newTimestamps,
          height = newHeight,
          currentBits = newBits,
          prevDiffAdjTimestamp = newPrevDiffAdjTimestamp,
          lastBlockHash = block.hash
        )
    }

    // ============================================================================
    // Block validation
    // ============================================================================

    /** Insert element into an ascending-sorted list, maintaining ascending order. */
    def insertAscending(x: BigInt, sorted: List[BigInt]): List[BigInt] = sorted match
        case Nil                  => Cons(x, Nil)
        case Cons(h, t) if x <= h => Cons(x, sorted)
        case Cons(h, t)           => Cons(h, insertAscending(x, t))

    /** Sort a list of BigInts in ascending order using insertion sort. Efficient for small
      * fixed-size lists (e.g. 11 timestamps for MTP calculation).
      */
    def insertionSort(xs: List[BigInt]): List[BigInt] =
        xs.foldLeft(List.empty[BigInt])((sorted, x) => insertAscending(x, sorted))

    /** Validate a single new block header against the traversal context. Returns the BlockSummary2,
      * updated context, and block proof (for chainwork accumulation).
      */
    def validateBlock(
        header: BlockHeader,
        ctx: TraversalCtx,
        currentTime: PosixTimeSeconds
    ): (BlockSummary2, TraversalCtx, BigInt) = {
        val hash = blockHeaderHash(header)
        val hashInt = byteStringToInteger(false, hash)

        // PoW validation — target is reused for block proof below
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
        val sortedTimestamps = insertionSort(ctx.timestamps.take(MedianTimeSpan))
        val medianTimePast = sortedTimestamps.at(5)
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
        val newTimestamps = Cons(header.timestamp, ctx.timestamps)
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
          lastBlockHash = hash
        )

        // Block proof from target already computed for PoW validation
        val blockProof = calculateBlockProof(target)

        (summary, newCtx, blockProof)
    }

    /** Compute segment chainwork for a list of blocks by replaying difficulty from a
      * [[TraversalCtx]].
      *
      * Only used in rare cases where stored chainwork must be recomputed:
      *   - '''Split insertion''': a Blocks node is split into prefix + suffix, and we need the
      *     prefix's chainwork (suffix = original - prefix).
      *   - '''Partial promotion''': some blocks are promoted from a Blocks node, and we need the
      *     promoted blocks' chainwork (remaining = original - promoted).
      *
      * Not used in the common append case, where chainwork is simply `originalCw + newCw`.
      */
    def computeChainwork(
        blocks: NonEmptyList[BlockSummary2],
        ctx: TraversalCtx,
        chainwork: Chainwork
    ): BigInt = blocks match
        case Nil => chainwork
        case Cons(block, tail) =>
            val bits = getNextWorkRequired(
              ctx.height,
              ctx.currentBits,
              ctx.timestamps.head,
              ctx.prevDiffAdjTimestamp
            )
            val newCtx = accumulateBlock(ctx, block)
            computeChainwork(
              tail,
              newCtx,
              chainwork + calculateBlockProof(compactBitsToTarget(bits))
            )

    /** Validate a list of headers (oldest-first), returning validated summaries and segment
      * chainwork.
      */
    def validateAndCollectBlocks(
        headers: List[BlockHeader],
        ctx: TraversalCtx,
        currentTime: BigInt
    ): (List[BlockSummary2], BigInt) = {
        val (acc, _, cw) = headers.foldLeft((Nil: List[BlockSummary2], ctx, BigInt(0))) {
            case ((acc, currentCtx, chainwork), header) =>
                val (summary, newCtx, blockProof) =
                    validateBlock(header, currentCtx, currentTime)
                (Cons(summary, acc), newCtx, chainwork + blockProof)
        }
        (acc.reverse, cw)
    }

    // ============================================================================
    // Tree navigation, validation, and insertion (single traversal)
    // ============================================================================

    /** Navigate the fork tree along `path`, validate `headers`, and insert the resulting block
      * summaries — all in a single traversal.
      *
      * Path semantics (one element consumed per tree node):
      *   - '''Empty path''' (`Nil`): the parent is the confirmed tip (stored in `ctx`). Validate
      *     headers and attach as a new branch at the tree root.
      *   - '''At `Blocks(blocks, next)`''': `pathHead` is an index into `blocks`. If
      *     `pathHead == blocks.length`, all blocks are accumulated and traversal continues into
      *     `next`. Otherwise `blocks(pathHead)` is the parent block where new headers branch off.
      *   - '''At `Fork(left, right)`''': `pathHead` selects a branch (`0` = left, `1` = right).
      *
      * @param tree
      *   current fork tree (or subtree during recursion)
      * @param path
      *   navigation path to the parent block
      * @param headers
      *   new block headers to validate and insert (oldest first)
      * @param ctx
      *   accumulated traversal context up to the current tree node
      * @param currentTime
      *   current wall-clock time (seconds) for validation
      * @return
      *   updated fork tree with new blocks inserted
      */
    def validateAndInsert(
        tree: ForkTree,
        path: Path,
        headers: List[BlockHeader],
        ctx: TraversalCtx,
        currentTime: BigInt
    ): ForkTree = {
        path match
            case Nil =>
                // Path is empty → parent is the confirmed tip (stored in ctx).
                // Validate headers and attach as a new branch at the tree root.
                val (newBlocks, newCw) =
                    validateAndCollectBlocks(headers, ctx, currentTime)
                val newBranch = Blocks(newBlocks, newCw, End)
                tree match
                    // First-ever insertion into an empty tree:
                    //   End → Blocks([H1,H2], cw, End)
                    case End => newBranch
                    // Tree already has blocks from confirmed tip → create a fork:
                    //   Blocks([A,B], ..) → Fork(Blocks([A,B], ..), Blocks([H1,H2], cw, End))
                    case existing => Fork(existing, newBranch)

            case Cons(pathHead, pathTail) =>
                tree match
                    case Blocks(blocks, originalCw, next) =>
                        // Walk the blocks list to find the insertion point at index pathHead.
                        // `count` tracks position, `prefix` accumulates blocks before pathHead
                        // (in reverse), `newCtx` accumulates traversal context.
                        def loop(
                            count: BigInt,
                            remaining: List[BlockSummary2],
                            prefix: List[BlockSummary2],
                            newCtx: TraversalCtx
                        ): ForkTree = {
                            remaining match
                                case Nil if count == pathHead =>
                                    // Pass-through: pathHead == blocks.length, all blocks consumed.
                                    // Continue navigation into the subtree `next`.
                                    // Example: Blocks([A,B,C], cw, Fork(..)), path=[3, 0, ..]
                                    //   → accumulate A,B,C, recurse into Fork(..) with path=[0, ..]
                                    Blocks(
                                      blocks,
                                      originalCw,
                                      validateAndInsert(
                                        next,
                                        pathTail,
                                        headers,
                                        newCtx,
                                        currentTime
                                      )
                                    )
                                case Cons(block, tail) if count == pathHead =>
                                    // Found insertion point: block at index pathHead is the parent.
                                    val parentCtx = accumulateBlock(newCtx, block)
                                    val (newBlocks, newCw) =
                                        validateAndCollectBlocks(
                                          headers,
                                          parentCtx,
                                          currentTime
                                        )
                                    val fullPrefix = prefix.reverse.prepended(block)
                                    tail match
                                        case Nil =>
                                            next match
                                                case End =>
                                                    // Append: parent is last block, no subtree.
                                                    // Blocks([A,B,C], cw, End), path=[2]
                                                    //   → Blocks([A,B,C,H1,H2], cw+newCw, End)
                                                    Blocks(
                                                      fullPrefix ++ newBlocks,
                                                      originalCw + newCw,
                                                      End
                                                    )
                                                case _ =>
                                                    // Fork at end: parent is last block but subtree
                                                    // exists. Must split chainwork.
                                                    // Blocks([A,B,C], cw, Fork(..)), path=[2]
                                                    //   → Blocks([A,B,C], prefCw,
                                                    //       Fork(Fork(..), Blocks([H1], newCw, End)))
                                                    val prefixCw =
                                                        computeChainwork(fullPrefix, ctx, BigInt(0))
                                                    Blocks(
                                                      fullPrefix,
                                                      prefixCw,
                                                      Fork(
                                                        next,
                                                        Blocks(newBlocks, newCw, End)
                                                      )
                                                    )
                                        case _ =>
                                            // Mid-split: parent is in the middle of the block list.
                                            // The Blocks node is split into prefix and suffix.
                                            // Blocks([A,B,C,D,E], cw, End), path=[2]
                                            //   → Blocks([A,B,C], prefCw,
                                            //       Fork(Blocks([D,E], cw-prefCw, End),
                                            //            Blocks([H1,H2], newCw, End)))
                                            val prefixCw =
                                                computeChainwork(fullPrefix, ctx, BigInt(0))
                                            Blocks(
                                              fullPrefix,
                                              prefixCw,
                                              Fork(
                                                Blocks(tail, originalCw - prefixCw, next),
                                                Blocks(newBlocks, newCw, End)
                                              )
                                            )
                                case Cons(block, tail) =>
                                    // Not at pathHead yet — accumulate block and advance counter.
                                    loop(
                                      count + 1,
                                      tail,
                                      Cons(block, prefix),
                                      accumulateBlock(newCtx, block)
                                    )
                                case _ => fail("Path index out of bounds")
                        }
                        loop(0, blocks, Nil, ctx)

                    case Fork(left, right) =>
                        // Consume path element: 0 → recurse left, else → recurse right.
                        if pathHead == BigInt(0) then
                            Fork(
                              validateAndInsert(
                                left,
                                pathTail,
                                headers,
                                ctx,
                                currentTime
                              ),
                              right
                            )
                        else
                            Fork(
                              left,
                              validateAndInsert(
                                right,
                                pathTail,
                                headers,
                                ctx,
                                currentTime
                              )
                            )

                    case End =>
                        fail("Path leads to End")
    }

    // ============================================================================
    // Best chain selection
    // ============================================================================

    /** Find the best (highest cumulative chainwork) chain path through the tree.
      *
      * Returns `(chainwork, depth, path)` where:
      *   - `chainwork` — total proof-of-work of the best chain (relative to confirmed tip)
      *   - `depth` — height of the best chain's tip (confirmed height + unconfirmed blocks)
      *   - `path` — navigation path to the best chain tip, used by [[promoteAndGC]]
      *
      * The path contains one element per Fork node encountered (0 = left, 1 = right). Blocks nodes
      * do not consume or produce path elements — they simply add their stored chainwork and block
      * count, then recurse into `next`. Ties favor the left branch (`>=`).
      *
      * Example: `Fork(Blocks([A,B], 200, End), Blocks([C], 150, End))` with confirmed height 100
      *   - Left: chainwork=200, depth=102, path=[]
      *   - Right: chainwork=150, depth=101, path=[]
      *   - Result: (200, 102, [0])
      */
    def bestChainPath(tree: ForkTree, height: BigInt, chainwork: BigInt): (BigInt, BigInt, Path) = {
        tree match
            case Blocks(blocks, cw, next) =>
                // Accumulate segment chainwork and block count, recurse into subtree.
                bestChainPath(next, height + blocks.length, chainwork + cw)

            case Fork(left, right) =>
                // Recurse both branches, pick higher chainwork, prepend direction to path.
                val (leftWork, leftDepth, leftPath) = bestChainPath(left, height, chainwork)
                val (rightWork, rightDepth, rightPath) = bestChainPath(right, height, chainwork)
                if leftWork >= rightWork then (leftWork, leftDepth, Cons(0, leftPath))
                else (rightWork, rightDepth, Cons(1, rightPath))

            case End =>
                // Leaf — return accumulated totals, empty path.
                (chainwork, height, Nil)
    }

    // ============================================================================
    // Promotion and GC
    // ============================================================================

    /** Split a block list into promotable prefix and remaining suffix.
      *
      * Walks oldest→newest. A block is eligible for promotion if:
      *   - `bestDepth - blockHeight >= MaturationConfirmations` (100 confirmations)
      *   - `currentTime - addedTimeSeconds >= ChallengeAging` (200 minutes on-chain)
      *   - `maxPromotions > 0` (caller-imposed limit from number of MPF proofs provided)
      *
      * Stops at the first ineligible block. This is safe because blocks are ordered oldest→newest:
      * each subsequent block has strictly less depth and same-or-less age, so if one fails either
      * condition, all following blocks will too.
      *
      * @return
      *   (promoted oldest-first, remaining oldest-first, ctx after promoted blocks)
      */
    def splitPromotable(
        blocks: List[BlockSummary2],
        ctx: TraversalCtx,
        bestDepth: BigInt,
        currentTime: PosixTimeSeconds,
        maxPromotions: BigInt
    ): (List[BlockSummary2], List[BlockSummary2], TraversalCtx) = {
        blocks match
            case Nil => (Nil, Nil, ctx)
            case Cons(block, tail) =>
                // Promotion limit reached — treat remaining blocks as non-promotable.
                if maxPromotions <= BigInt(0) then (Nil, blocks, ctx)
                else
                    val blockHeight = ctx.height + 1
                    val depth = bestDepth - blockHeight
                    val age = currentTime - block.addedTimeSeconds
                    if depth >= MaturationConfirmations && age >= ChallengeAging then
                        // Block eligible — promote it and check next block.
                        val newCtx = accumulateBlock(ctx, block)
                        val (morePromoted, remaining, finalCtx) =
                            splitPromotable(
                              tail,
                              newCtx,
                              bestDepth,
                              currentTime,
                              maxPromotions - 1
                            )
                        (Cons(block, morePromoted), remaining, finalCtx)
                    else
                        // Block not eligible — stop. All following blocks are newer/shallower,
                        // so they can't be eligible either.
                        (Nil, blocks, ctx)
    }

    /** Promote eligible blocks and garbage-collect dead forks along the best chain path.
      *
      * Walks the tree following `bestPath` (from [[bestChainPath]]). At each node:
      *   - '''Blocks''': tries to promote eligible blocks (up to `numPromotions`), then recurses
      *     into the subtree for more promotions and/or GC.
      *   - '''Fork''': follows the best branch (per `bestPath`), drops the other branch (GC).
      *     The Fork node itself is eliminated — the surviving branch replaces it.
      *   - '''End''': leaf, nothing to do.
      *
      * `bestPath` is consumed only at Fork nodes (matching [[bestChainPath]] which produces path
      * elements only at Fork nodes). Blocks nodes pass `bestPath` through unchanged.
      *
      * @param numPromotions
      *   maximum blocks to promote (= number of MPF proofs provided by submitter)
      * @return
      *   (promoted blocks oldest-first, cleaned tree with promoted blocks removed and dead forks
      *   dropped)
      */
    def promoteAndGC(
        tree: ForkTree,
        ctx: TraversalCtx,
        bestPath: Path,
        bestDepth: BigInt,
        currentTime: PosixTimeSeconds,
        numPromotions: BigInt
    ): (List[BlockSummary2], ForkTree) = {
        tree match
            case Blocks(blocks, cw, next) =>
                val (promoted, remaining, newCtx) =
                    splitPromotable(blocks, ctx, bestDepth, currentTime, numPromotions)
                log("splitPromotable 2")
                if promoted.isEmpty then
                    // No promotion in this node (blocks too young/shallow, or limit reached).
                    // Accumulate all blocks and recurse into subtree for GC.
                    // Example: Blocks([A,B], cw, Fork(..)) where A,B are not yet eligible
                    //   → keep Blocks([A,B], cw, <cleaned subtree>)
                    val fullCtx = blocks.foldLeft(ctx)(accumulateBlock)
                    log("promoteAndGC fullCtx")
                    val (nextPromoted, cleanedNext) =
                        promoteAndGC(
                          next,
                          fullCtx,
                          bestPath,
                          bestDepth,
                          currentTime,
                          numPromotions
                        )
                    (nextPromoted, Blocks(blocks, cw, cleanedNext))
                else if remaining.isEmpty then
                    // All blocks in this node promoted. Consume node entirely and recurse
                    // into subtree for more promotions (with decremented limit).
                    // Example: Blocks([A,B,C], cw, Blocks([D,E], ..)) where all A,B,C eligible
                    //   → promoted=[A,B,C], recurse into Blocks([D,E], ..) with limit-=3
                    val (nextPromoted, cleanedNext) =
                        promoteAndGC(
                          next,
                          newCtx,
                          bestPath,
                          bestDepth,
                          currentTime,
                          numPromotions - promoted.length
                        )
                    (promoted ++ nextPromoted, cleanedNext)
                else
                    // Partial promotion: some blocks promoted, rest remain. Stop recursion —
                    // remaining blocks and subtree stay as-is (subtree GC deferred to future tx).
                    // Example: Blocks([A,B,C,D,E], cw, End) where A,B eligible but C is not
                    //   → promoted=[A,B], tree=Blocks([C,D,E], cw-promotedCw, End)
                    val promotedCw = computeChainwork(promoted, ctx, 0)
                    log("promoteAndGC computeChainwork")
                    (promoted, Blocks(remaining, cw - promotedCw, next))

            case Fork(left, right) =>
                // GC: follow the best branch, drop the other entirely.
                // The Fork node is eliminated — the surviving subtree replaces it.
                // Example: Fork(Blocks([A,B], ..), Blocks([C], ..)), bestPath=[0]
                //   → recurse into left=Blocks([A,B], ..), drop right=Blocks([C], ..)
                //   → result is cleaned left subtree (no Fork wrapper)
                require(!bestPath.isEmpty, "Best path exhausted at Fork")
                val direction = bestPath.head
                val pathTail = bestPath.tail
                if direction == BigInt(0) then
                    val (promoted, cleanedLeft) =
                        promoteAndGC(left, ctx, pathTail, bestDepth, currentTime, numPromotions)
                    (promoted, cleanedLeft)
                else
                    val (promoted, cleanedRight) =
                        promoteAndGC(right, ctx, pathTail, bestDepth, currentTime, numPromotions)
                    (promoted, cleanedRight)

            case End =>
                (Nil, End)
    }

    // ============================================================================
    // Apply promotions to confirmed state
    // ============================================================================

    /** Apply promoted blocks to the confirmed state, updating the MPF root and building the new
      * [[ChainState2]].
      *
      * Walks `promoted` and `mpfProofs` in lockstep (oldest-first). For each block:
      *   1. Accumulates the block into `ctx` (advancing height, timestamps, difficulty)
      *   2. Inserts `block.hash` into the MPF trie using the corresponding non-membership proof
      *
      * The lists must have equal length — fails if one runs out before the other.
      *
      * The final `ChainState2` reflects the new confirmed tip (from `finalCtx`) with
      * `recentTimestamps` trimmed to 11 entries and the updated MPF root.
      */
    def applyPromotions(
        state: ChainState2,
        promoted: List[BlockSummary2],
        mpfProofs: List[List[ProofStep]],
        ctx0: TraversalCtx,
        cleanedTree: ForkTree
    ): ChainState2 = {
        def loop(
            blocks: List[BlockSummary2],
            proofs: List[List[ProofStep]],
            ctx: TraversalCtx,
            mpfRoot: ByteString
        ): (TraversalCtx, ByteString) = {
            blocks match
                case Nil =>
                    proofs match
                        case Nil => (ctx, mpfRoot)
                        case _   => fail("MPF proof count mismatch")
                case Cons(block, bTail) =>
                    proofs match
                        case Cons(proof, pTail) =>
                            val newCtx = accumulateBlock(ctx, block)
                            val newRoot =
                                MPF(mpfRoot).insert(block.hash, block.hash, proof).root
                            loop(bTail, pTail, newCtx, newRoot)
                        case Nil => fail("MPF proof count mismatch")
        }

        val (finalCtx, finalRoot) =
            loop(promoted, mpfProofs, ctx0, state.confirmedBlocksRoot)

        ChainState2(
          blockHeight = finalCtx.height,
          blockHash = finalCtx.lastBlockHash,
          currentTarget = finalCtx.currentBits,
          recentTimestamps = finalCtx.timestamps.take(MedianTimeSpan),
          previousDifficultyAdjustmentTimestamp = finalCtx.prevDiffAdjTimestamp,
          confirmedBlocksRoot = finalRoot,
          forksTree = cleanedTree
        )
    }

    // ============================================================================
    // Main compute function
    // ============================================================================

    /** Compute the new [[ChainState2]] after applying an [[UpdateOracle2]].
      *
      * Four phases:
      *   1. '''Insert''': validate new block headers and insert into the fork tree
      *   1. '''Best chain''': find the highest-chainwork path (single full tree traversal)
      *   1. '''Promote + GC''': promote eligible blocks to confirmed state and drop dead forks
      *      (single traversal along best path)
      *   1. '''Apply''': update confirmed state with promoted blocks and MPF proofs
      *
      * Steps 2-4 are skipped when no MPF proofs are provided (header-only submission).
      * When proofs are provided, the number of promoted blocks must match exactly.
      */
    def computeUpdate(
        state: ChainState2,
        update: UpdateOracle2,
        currentTime: BigInt
    ): ChainState2 = {
        val headers = update.blockHeaders
        val ctx0 = initCtx(state)

        // Step 1: Validate and insert new blocks into tree (or keep tree if no headers)
        val newTree = headers match
            case Nil => state.forksTree
            case _ =>
                validateAndInsert(
                  state.forksTree,
                  update.parentPath,
                  headers,
                  ctx0,
                  currentTime
                )
        log("validateAndInsert")

        val promotionProofs = update.mpfInsertProofs
        val numBlocksToPromote = promotionProofs.length
        if numBlocksToPromote > BigInt(0) then
            // Step 2: Find best chain by chainwork (single full traversal)
            val (_, bestDepth, bestPath) = bestChainPath(newTree, state.blockHeight, BigInt(0))
            log("bestChainPath")

            // Step 3: Promote eligible blocks + GC dead forks (single traversal along bestPath)
            val (promoted, cleanedTree) =
                promoteAndGC(newTree, ctx0, bestPath, bestDepth, currentTime, numBlocksToPromote)
            log("promoteAndGC")
            require(promoted.length == numBlocksToPromote, "Promoted block count mismatch")

            // Step 4: Apply promotions and set cleaned tree
            val updatedState =
                applyPromotions(state, promoted, promotionProofs, ctx0, cleanedTree)
            log("applyPromotions")
            updatedState
        else
            // No proofs → header-only submission, skip promotion and GC
            state.copy(forksTree = newTree)
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
        val computedState = computeUpdate(prevState, update, intervalStartInSeconds)

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

        log("spend: providedOutputDatum")
        require(
          computedState.toData == providedOutputDatum,
          "Computed state does not match provided output datum"
        )
        log("spend: computedState.toData == providedOutputDatum")
    }

    import StrictLookups.*
    // One-shot NFT minting policy
    // param: TxOutRef that must be consumed to mint (one-shot guarantee)
    // redeemer: BigInt index of the oracle output in tx.outputs
    inline override def mint(
        oneShotTxOutRef: Data,
        redeemer: Data,
        policyId: PolicyId,
        tx: TxInfo
    ): Unit = {
        val minted = tx.mint.toSortedMap.lookupOrFail(policyId).toData
        if minted == SortedMap.singleton(ByteString.empty, BigInt(1)).toData then
            // ensure we spend the one-shot TxOutRef
            tx.inputs.findOrFail(_.outRef.toData == oneShotTxOutRef)
            // Verify oracle output contains the NFT at the specified index
            val outputIndex = redeemer.to[BigInt]
            val oracleOutput = tx.outputs !! outputIndex
            require(
              oracleOutput.value.existingQuantityOf(policyId, ByteString.empty) == BigInt(1),
              "Oracle output must contain NFT"
            )
            // Verify oracle output goes to this script's address (policyId == script hash)
            require(
              oracleOutput.address.credential.toData == Credential
                  .ScriptCredential(policyId)
                  .toData,
              "Oracle output must go to script address"
            )
        else
            require(
              minted == SortedMap.singleton(ByteString.empty, BigInt(-1)).toData,
              "can only mint 1 or burn 1 SP NFT"
            )
    }
}
