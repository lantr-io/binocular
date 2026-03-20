package binocular

import binocular.ForkTree.{Blocks, End, Fork}
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry as MPF
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry.ProofStep
import scalus.cardano.onchain.plutus.prelude.List.*
import scalus.cardano.onchain.plutus.prelude.{List, *}
import scalus.cardano.onchain.plutus.v1.{Address, Credential, PolicyId, PosixTime, PubKeyHash}
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.{DataParameterizedValidator, *}
import scalus.compiler.Compile
import scalus.uplc.builtin.*
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString.*
import scalus.uplc.builtin.Data.{toData, FromData, ToData}
import scalus.{show as _, *}

extension (a: ByteString) def reverse: ByteString = ByteString.fromArray(a.bytes.reverse)

// Type aliases for semantic clarity
type BlockHash = ByteString // 32-byte SHA256d hash of block header
type TxHash = ByteString // 32-byte SHA256d hash of transaction
type MerkleRoot = ByteString // 32-byte Merkle tree root hash
type CompactBits = ByteString // 4-byte compact difficulty target representation
type BlockHeaderBytes = ByteString // 80-byte raw Bitcoin block header

type PosixTimeSeconds = BigInt
type Chainwork = BigInt
type NonEmptyList[A] = List[A]
type List11Timestamps = List[PosixTimeSeconds]
type MPFRoot = ByteString

case class BlockHeader(bytes: ByteString) derives FromData, ToData {
    inline def version: BigInt =
        byteStringToInteger(false, bytes.slice(0, 4))

    inline def prevBlockHash: BlockHash = bytes.slice(4, 32)

    inline def bits: CompactBits = bytes.slice(72, 4)

    inline def merkleRoot: MerkleRoot = bytes.slice(36, 32)

    inline def timestamp: BigInt = byteStringToInteger(false, bytes.slice(68, 4))
}

case class BlockSummary(
    hash: BlockHash, // Block hash
    timestamp: PosixTime, // Bitcoin block timestamp (for median-time-past)
    addedTimeSeconds: PosixTimeSeconds // Cardano time when this block was added to forkTree
) derives FromData,
      ToData

/** Binary tree of unconfirmed block segments.
  *
  * ## Fork ordering invariant:
  *
  * `Fork(left = existing, right = new)`.
  *
  * Every fork-creating operation in [[BitcoinValidator.validateAndInsert]] places the pre-existing
  * subtree on the left and the newly submitted branch on the right. This mirrors Bitcoin Core's
  * first-seen preference: `CBlockIndexWorkComparator` (`blockstorage.cpp`) breaks equal-chainwork
  * ties by `nSequenceId`, favoring whichever chain tip was received first.
  *
  * Since [[BitcoinValidator.bestChainPath]] uses `>=` when comparing left vs right chainwork, the
  * left (existing/older) branch wins ties — achieving the same "never reorg to an equal-work
  * competitor" behavior as Bitcoin Core.
  */
enum ForkTree derives FromData, ToData {
    case Blocks(blocks: NonEmptyList[BlockSummary], chainwork: Chainwork, next: ForkTree)
    case Fork(left: ForkTree, right: ForkTree)
    case End

    /** Total number of blocks in the tree. */
    def blockCount: Int = this match
        case Blocks(blocks, _, next) => blocks.size.toInt + next.blockCount
        case Fork(left, right)       => left.blockCount + right.blockCount
        case End                     => 0

    def nonEmpty: Boolean = this != End

    /** Check if a block hash exists anywhere in the tree. */
    def existsHash(hash: BlockHash): Boolean = this match
        case Blocks(blocks, _, next) =>
            blocks.exists(_.hash == hash) || next.existsHash(hash)
        case Fork(left, right) =>
            left.existsHash(hash) || right.existsHash(hash)
        case End => false

    /** Height of the best chain tip (confirmed height + unconfirmed blocks). */
    def highestHeight(baseHeight: BigInt): BigInt = {
        val (_, depth, _) = BitcoinValidator.bestChainPath(this, baseHeight, 0)
        depth
    }

    /** Earliest addedTimeSeconds among all blocks in the tree, or None if empty. */
    def oldestBlockTime: scala.Option[BigInt] = this match
        case Blocks(blocks, _, next) =>
            var min: scala.Option[BigInt] = scala.None
            blocks.foreach { b =>
                min = min match
                    case scala.Some(v) => scala.Some(v.min(b.addedTimeSeconds))
                    case scala.None    => scala.Some(b.addedTimeSeconds)
            }
            val minInNext = next.oldestBlockTime
            (min, minInNext) match
                case (scala.Some(a), scala.Some(b)) => scala.Some(a.min(b))
                case (a, scala.None)                => a
                case (scala.None, b)                => b
        case Fork(left, right) =>
            (left.oldestBlockTime, right.oldestBlockTime) match
                case (scala.Some(a), scala.Some(b)) => scala.Some(a.min(b))
                case (a, scala.None)                => a
                case (scala.None, b)                => b
        case End => scala.None

    /** Flatten all blocks in the tree into a single list (depth-first order). */
    def toBlockList: scala.collection.immutable.List[BlockSummary] = this match
        case Blocks(blocks, _, next) =>
            blocks.toScalaList ++ next.toBlockList
        case Fork(left, right) =>
            left.toBlockList ++ right.toBlockList
        case End => scala.collection.immutable.Nil

    /** Compute the insertion path to the tip of the best chain.
      *
      * Produces a [[Path]] (not a [[BestPath]]). At each node:
      *   - `Blocks(bs, _, next)` with `next == End`: emit `bs.length - 1` (last block is the
      *     parent)
      *   - `Blocks(bs, _, next)` with `next != End`: emit `bs.length` (pass-through) and recurse
      *   - `Fork(left, right)`: pick highest-chainwork branch (left wins ties), emit 0/1 and
      *     recurse
      *   - `End`: emit nothing (empty tree — parent is the confirmed tip)
      */
    def findTipPath: List[BigInt] = this match
        case Blocks(blocks, _, next) =>
            next match
                case End =>
                    // Last Blocks node — tip is the last block
                    List(blocks.length - 1)
                case _ =>
                    // Pass through all blocks, recurse into subtree
                    List.Cons(blocks.length, next.findTipPath)
        case Fork(left, right) =>
            val (leftWork, _, _) = BitcoinValidator.bestChainPath(left, 0, 0)
            val (rightWork, _, _) = BitcoinValidator.bestChainPath(right, 0, 0)
            if leftWork >= rightWork then List.Cons(BigInt(0), left.findTipPath)
            else List.Cons(BigInt(1), right.findTipPath)
        case End => List.Nil

    /** Display tree structure for debugging/CLI output. */
    def displayTree(baseHeight: BigInt, indent: String = ""): String = this match
        case Blocks(blocks, cw, next) =>
            val first = blocks.head
            val last = blocks.last
            val firstHeight = baseHeight + 1
            val lastHeight = baseHeight + blocks.size.toInt
            val line =
                s"${indent}Blocks[${blocks.size.toInt}] heights $firstHeight..$lastHeight, chainwork=$cw, tip=${last.hash.toHex.take(8)}..."
            val nextStr = next.displayTree(lastHeight, indent)
            if nextStr.isEmpty then line else line + "\n" + nextStr
        case Fork(left, right) =>
            val leftStr = left.displayTree(baseHeight, indent + "  L ")
            val rightStr = right.displayTree(baseHeight, indent + "  R ")
            s"${indent}Fork:\n$leftStr\n$rightStr"
        case End => ""
}

case class ChainState(
    blockHeight: BigInt,
    blockHash: BlockHash,
    currentTarget: CompactBits,
    recentTimestamps: List11Timestamps,
    previousDifficultyAdjustmentTimestamp: PosixTime,
    confirmedBlocksRoot: MPFRoot,
    forkTree: ForkTree
) derives FromData,
      ToData

/** Path element in [[ForkTree]] navigation. Interpretation depends on path type. */
type PathElement = BigInt

/** Insertion path — navigates to a specific block within [[ForkTree]].
  *
  * Used by [[BitcoinValidator.validateAndInsert]] to find the parent block for new headers. One
  * element is consumed per tree node:
  *   - '''At `Blocks`''': element is a 0-based index into the block list. If index ==
  *     blocks.length, all blocks are accumulated and traversal continues into `next`.
  *   - '''At `Fork`''': 0 = left, 1 = right.
  *   - '''Empty''': parent is the confirmed tip (no blocks in fork tree).
  */
type Path = List[PathElement]

/** Best-chain path — navigates through [[ForkTree]] forks to identify the winning chain.
  *
  * Produced by [[BitcoinValidator.bestChainPath]], consumed by [[BitcoinValidator.promoteAndGC]].
  * Elements are produced/consumed '''only at `Fork` nodes''' (0 = left, 1 = right). `Blocks` nodes
  * do not produce or consume path elements — `promoteAndGC` processes entire `Blocks` nodes
  * (promoting eligible blocks), so block-level indexing is unnecessary.
  */
type BestPath = List[PathElement]

enum OracleAction derives FromData, ToData {
    case UpdateOracle(
        blockHeaders: List[BlockHeader],
        parentPath: Path,
        mpfInsertProofs: List[List[ProofStep]]
    )
    case CloseOracle
}

case class TraversalCtx(
    timestamps: NonEmptyList[PosixTimeSeconds], // newest-first (prepended during accumulation)
    height: BigInt,
    currentBits: CompactBits,
    prevDiffAdjTimestamp: PosixTimeSeconds,
    lastBlockHash: BlockHash
) derives FromData,
      ToData

case class BitcoinValidatorParams(
    maturationConfirmations: BigInt,
    challengeAging: BigInt,
    oneShotTxOutRef: TxOutRef,
    closureTimeout: BigInt,
    owner: PubKeyHash,
    powLimit: BigInt,
    testingMode: Boolean = false
) derives FromData,
      ToData

@Compile
object BitcoinValidator extends DataParameterizedValidator {
    import BitcoinHelpers.*

    // Maximum allowed width of the transaction validity interval (in milliseconds).
    // Ensures validFrom is close to actual wall-clock time, so addedTimeSeconds
    // is trustworthy for the challenge aging check.
    // 10 minutes = 600 seconds = 600_000 milliseconds
    val MaxValidityWindow: BigInt = 600_000

    // Fork path directions
    val LeftFork: BigInt = 0
    val RightFork: BigInt = 1

    // ============================================================================
    // TraversalCtx helpers
    // ============================================================================

    /** Initialize traversal context from the confirmed state.
      *
      * The context starts at the confirmed tip, with height, bits, timestamps, and hash matching
      * the confirmed chain. All tree traversal (accumulation, validation, chainwork) builds on top
      * of this starting point.
      */
    def initCtx(state: ChainState): TraversalCtx =
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
      * Inlines the logic of `GetNextWorkRequired()` (pow.cpp:14-48) to handle difficulty retarget
      * and `prevDiffAdjTimestamp` update in a single branch.
      *
      * At retarget boundaries (height % 2016 == 0):
      *   - Computes new difficulty via `CalculateNextWorkRequired()` (pow.cpp:50-84) using
      *     `ctx.timestamps.head` as `pindexLast->GetBlockTime()` (the block *before* the boundary,
      *     not the new block being accumulated)
      *   - Records this block's timestamp as `nFirstBlockTime` for the next retarget period
      */
    def accumulateBlock(ctx: TraversalCtx, block: BlockSummary, powLimit: BigInt): TraversalCtx = {
        val newHeight = ctx.height + 1
        val timestamp = block.timestamp
        val hash = block.hash
        val newTimestamps = Cons(timestamp, ctx.timestamps)

        if newHeight % DifficultyAdjustmentInterval == BigInt(0) then
            // Retarget boundary — matches CalculateNextWorkRequired() in pow.cpp:50-84
            // nActualTimespan = pindexLast->GetBlockTime() - nFirstBlockTime
            //                 = ctx.timestamps.head      - ctx.prevDiffAdjTimestamp
            val newTarget = calculateNextWorkRequired(
              ctx.currentBits,
              ctx.timestamps.head, // pindexLast->GetBlockTime()
              ctx.prevDiffAdjTimestamp, // nFirstBlockTime
              powLimit
            )
            val newBits = targetToCompactByteString(newTarget)
            TraversalCtx(
              timestamps = newTimestamps,
              height = newHeight,
              currentBits = newBits,
              prevDiffAdjTimestamp = timestamp, // becomes nFirstBlockTime for next period
              lastBlockHash = hash
            )
        else
            // No retarget — matches GetNextWorkRequired() returning current target unchanged
            // pow.cpp:46 `return pindex->nBits;`
            ctx.copy(
              timestamps = newTimestamps,
              height = newHeight,
              lastBlockHash = hash
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

    /** Validate a single new block header and accumulate it into the traversal context.
      *
      * Matches `ContextualCheckBlockHeader()` in validation.cpp:
      *   - PoW: `CheckProofOfWork()` in pow.cpp:140-163 — hash ≤ target
      *   - MTP: `GetMedianTimePast()` in chain.h:278-290 — timestamp > median of last 11
      *   - Future time: validation.cpp — timestamp ≤ currentTime + MAX_FUTURE_BLOCK_TIME
      *   - Chain continuity: prevBlockHash must match ctx.lastBlockHash
      *
      * Difficulty is enforced structurally: PoW is checked against `ctx.currentBits` (the expected
      * target maintained by [[accumulateBlock]]), so blocks must satisfy the correct difficulty
      * even though `header.bits` is not explicitly compared. Context update (height, timestamps,
      * difficulty retarget) is delegated to [[accumulateBlock]].
      */
    def validateBlock(
        header: BlockHeader,
        ctx: TraversalCtx,
        currentTime: PosixTimeSeconds,
        params: BitcoinValidatorParams
    ): (BlockSummary, TraversalCtx, BigInt) = {
        val hash = blockHeaderHash(header)
        val hashInt = byteStringToInteger(false, hash)

        // PoW validation — matches CheckProofOfWork() in pow.cpp:140-163
        // Target derived from ctx.currentBits (expected difficulty), reused for block proof below
        val target = compactBitsToTarget(ctx.currentBits)

        if !params.testingMode then
            require(hashInt <= target, "Invalid proof-of-work")
            require(target <= params.powLimit, "Target exceeds PowLimit")

        // MTP validation — matches GetMedianTimePast() in chain.h:278-290
        val sortedTimestamps = insertionSort(ctx.timestamps.take(MedianTimeSpan))
        val medianTimePast = sortedTimestamps.at(5)
        val timestamp = header.timestamp
        require(timestamp > medianTimePast, "Block timestamp not greater than MTP")

        // Future time validation — matches MAX_FUTURE_BLOCK_TIME check in validation.cpp
        require(
          timestamp <= currentTime + MaxFutureBlockTime,
          "Block timestamp too far in future"
        )

        // Chain continuity — prevBlockHash must link to the tip of the traversed chain
        require(header.prevBlockHash == ctx.lastBlockHash, "Parent hash mismatch")

        val summary = BlockSummary(
          hash = hash,
          timestamp = timestamp,
          addedTimeSeconds = currentTime
        )

        // Delegate context update (timestamps, height, difficulty retarget) to accumulateBlock
        val newCtx = accumulateBlock(ctx, summary, params.powLimit)

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
        blocks: NonEmptyList[BlockSummary],
        ctx: TraversalCtx,
        chainwork: Chainwork,
        powLimit: BigInt
    ): BigInt = blocks match
        case Nil => chainwork
        case Cons(block, tail) =>
            val bits = getNextWorkRequired(
              ctx.height,
              ctx.currentBits,
              ctx.timestamps.head,
              ctx.prevDiffAdjTimestamp,
              powLimit
            )
            val newCtx = accumulateBlock(ctx, block, powLimit)
            computeChainwork(
              tail,
              newCtx,
              chainwork + calculateBlockProof(compactBitsToTarget(bits)),
              powLimit
            )

    /** Validate a list of headers (oldest-first), returning validated summaries and segment
      * chainwork.
      */
    def validateAndCollectBlocks(
        headers: List[BlockHeader],
        ctx: TraversalCtx,
        currentTime: BigInt,
        chainwork: BigInt,
        acc: List[BlockSummary],
        params: BitcoinValidatorParams
    ): (List[BlockSummary], BigInt) = headers match
        case Nil => (acc.reverse, chainwork)
        case Cons(header, tail) =>
            val (summary, newCtx, blockProof) =
                validateBlock(header, ctx, currentTime, params)
            validateAndCollectBlocks(
              tail,
              newCtx,
              currentTime,
              chainwork + blockProof,
              Cons(summary, acc),
              params
            )

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
        currentTime: PosixTimeSeconds,
        params: BitcoinValidatorParams
    ): ForkTree = {
        path match
            case Nil =>
                // Path is empty → parent is the confirmed tip (stored in ctx).
                // Validate headers and attach as a new branch at the tree root.
                val (newBlocks, newCw) =
                    validateAndCollectBlocks(headers, ctx, currentTime, 0, Nil, params)
                val newBranch = Blocks(newBlocks, newCw, End)
                tree match
                    // First-ever insertion into an empty tree:
                    //   End → Blocks([H1,H2], cw, End)
                    case End => newBranch
                    // Tree already has blocks from confirmed tip → create a fork.
                    // Existing branch goes left, new branch goes right (Fork ordering invariant).
                    //   Blocks([A,B], ..) → Fork(Blocks([A,B], ..), Blocks([H1,H2], cw, End))
                    case existing => Fork(existing, newBranch)

            case Cons(pathHead, pathTail) =>
                validateAndInsertInPath(
                  tree,
                  headers,
                  ctx,
                  currentTime,
                  pathHead,
                  pathTail,
                  params
                )
    }

    // ============================================================================
    // Best chain selection
    // ============================================================================

    private inline def validateAndInsertInPath(
        tree: ForkTree,
        headers: List[BlockHeader],
        ctx: TraversalCtx,
        currentTime: PosixTimeSeconds,
        pathHead: PathElement,
        pathTail: Path,
        params: BitcoinValidatorParams
    ) = {
        tree match
            case Blocks(blocks, originalCw, next) =>
                // Walk the blocks list to find the insertion point at index pathHead.
                // `count` tracks position, `prefix` accumulates blocks before pathHead
                // (in reverse), `newCtx` accumulates traversal context.
                def loop(
                    count: BigInt,
                    remaining: List[BlockSummary],
                    prefix: List[BlockSummary],
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
                                currentTime,
                                params
                              )
                            )
                        case Cons(block, tail) if count == pathHead =>
                            // Found insertion point: block at index pathHead is the parent.
                            val parentCtx = accumulateBlock(newCtx, block, params.powLimit)
                            val (newBlocks, newCw) =
                                validateAndCollectBlocks(
                                  headers,
                                  parentCtx,
                                  currentTime,
                                  0,
                                  Nil,
                                  params
                                )
                            val fullPrefix = Cons(block, prefix).reverse
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
                                            // Existing subtree goes left, new branch right
                                            // (Fork ordering invariant).
                                            // Blocks([A,B,C], cw, Fork(..)), path=[2]
                                            //   → Blocks([A,B,C], prefCw,
                                            //       Fork(Fork(..), Blocks([H1], newCw, End)))
                                            val prefixCw =
                                                computeChainwork(
                                                  fullPrefix,
                                                  ctx,
                                                  BigInt(0),
                                                  params.powLimit
                                                )
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
                                    // Existing suffix goes left, new branch right
                                    // (Fork ordering invariant).
                                    // Blocks([A,B,C,D,E], cw, End), path=[2]
                                    //   → Blocks([A,B,C], prefCw,
                                    //       Fork(Blocks([D,E], cw-prefCw, End),
                                    //            Blocks([H1,H2], newCw, End)))
                                    val prefixCw =
                                        computeChainwork(
                                          fullPrefix,
                                          ctx,
                                          BigInt(0),
                                          params.powLimit
                                        )
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
                              accumulateBlock(newCtx, block, params.powLimit)
                            )
                        case _ => fail("Path index out of bounds")
                }

                loop(0, blocks, Nil, ctx)

            case Fork(left, right) =>
                // Consume path element: 0 → recurse left, else → recurse right.
                if pathHead == LeftFork then
                    Fork(
                      validateAndInsert(
                        left,
                        pathTail,
                        headers,
                        ctx,
                        currentTime,
                        params
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
                        currentTime,
                        params
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
      *   - `path` — [[BestPath]] to the best chain tip, used by [[promoteAndGC]]
      *
      * The path contains one element per Fork node encountered (0 = left, 1 = right). Blocks nodes
      * do not consume or produce path elements — they simply add their stored chainwork and block
      * count, then recurse into `next`.
      *
      * '''Tie-breaking: left (existing) branch wins (`>=`).''' This matches Bitcoin Core's
      * `CBlockIndexWorkComparator` in `blockstorage.cpp`, which uses `nSequenceId` to prefer the
      * first-seen chain when chainwork is equal. Since [[validateAndInsert]] always places existing
      * branches left and new branches right, `>=` ensures the existing chain is never displaced by
      * an equal-work competitor — consistent with Bitcoin Core never reorganizing to a same-work
      * alternative.
      *
      * Example: `Fork(Blocks([A,B], 200, End), Blocks([C], 150, End))` with confirmed height 100
      *   - Left: chainwork=200, depth=102, path=[]
      *   - Right: chainwork=150, depth=101, path=[]
      *   - Result: (200, 102, [0])
      */
    def bestChainPath(
        tree: ForkTree,
        height: BigInt,
        chainwork: BigInt
    ): (BigInt, BigInt, BestPath) = {
        tree match
            case Blocks(blocks, cw, next) =>
                // Accumulate segment chainwork and block count, recurse into subtree.
                bestChainPath(next, height + blocks.length, chainwork + cw)

            case Fork(left, right) =>
                // Recurse both branches, pick higher chainwork, prepend direction to path.
                // >= means left (existing) wins ties — matches Bitcoin Core's first-seen rule
                // (CBlockIndexWorkComparator in blockstorage.cpp).
                val (leftWork, leftDepth, leftPath) = bestChainPath(left, height, chainwork)
                val (rightWork, rightDepth, rightPath) = bestChainPath(right, height, chainwork)
                if leftWork >= rightWork then (leftWork, leftDepth, Cons(LeftFork, leftPath))
                else (rightWork, rightDepth, Cons(RightFork, rightPath))

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
      *   - `bestDepth - blockHeight >= params.maturationConfirmations` (e.g. 100 confirmations)
      *   - `currentTime - addedTimeSeconds >= params.challengeAging` (e.g. 200 minutes on-chain)
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
        blocks: List[BlockSummary],
        ctx: TraversalCtx,
        bestDepth: BigInt,
        currentTime: PosixTimeSeconds,
        maxPromotions: BigInt,
        params: BitcoinValidatorParams
    ): (List[BlockSummary], List[BlockSummary], TraversalCtx) = {
        blocks match
            case Nil               => (Nil, Nil, ctx)
            case Cons(block, tail) =>
                // Promotion limit reached — treat remaining blocks as non-promotable.
                if maxPromotions <= BigInt(0) then (Nil, blocks, ctx)
                else
                    val blockHeight = ctx.height + 1
                    val depth = bestDepth - blockHeight
                    val age = currentTime - block.addedTimeSeconds
                    if depth >= params.maturationConfirmations && age >= params.challengeAging then
                        // Block eligible — promote it and check next block.
                        val newCtx = accumulateBlock(ctx, block, params.powLimit)
                        val (morePromoted, remaining, finalCtx) =
                            splitPromotable(
                              tail,
                              newCtx,
                              bestDepth,
                              currentTime,
                              maxPromotions - 1,
                              params
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
      *   - '''Fork''': follows the best branch (per `bestPath`), drops the other branch (GC). The
      *     Fork node itself is eliminated — the surviving branch replaces it.
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
        bestPath: BestPath,
        bestDepth: BigInt,
        currentTime: PosixTimeSeconds,
        numPromotions: BigInt,
        params: BitcoinValidatorParams
    ): (List[BlockSummary], ForkTree) = {
        tree match
            case Blocks(blocks, cw, next) =>
                val (promoted, remaining, newCtx) =
                    splitPromotable(blocks, ctx, bestDepth, currentTime, numPromotions, params)
//                log("splitPromotable 2")
                if promoted.isEmpty then
                    // No promotion in this node (blocks too young/shallow, or limit reached).
                    // Accumulate all blocks and recurse into subtree for GC.
                    val fullCtx =
                        blocks.foldLeft(ctx)((c, b) => accumulateBlock(c, b, params.powLimit))
//                    log("promoteAndGC fullCtx")
                    val (nextPromoted, cleanedNext) =
                        promoteAndGC(
                          next,
                          fullCtx,
                          bestPath,
                          bestDepth,
                          currentTime,
                          numPromotions,
                          params
                        )
                    (nextPromoted, Blocks(blocks, cw, cleanedNext))
                else if remaining.isEmpty then
                    // All blocks in this node promoted. Consume node entirely and recurse
                    // into subtree for more promotions (with decremented limit).
                    val (nextPromoted, cleanedNext) =
                        promoteAndGC(
                          next,
                          newCtx,
                          bestPath,
                          bestDepth,
                          currentTime,
                          numPromotions - promoted.length,
                          params
                        )
                    (promoted ++ nextPromoted, cleanedNext)
                else
                    // Partial promotion: some blocks promoted, rest remain. Stop recursion —
                    // remaining blocks and subtree stay as-is (subtree GC deferred to future tx).
                    val promotedCw = computeChainwork(promoted, ctx, 0, params.powLimit)
//                    log("promoteAndGC computeChainwork")
                    (promoted, Blocks(remaining, cw - promotedCw, next))

            case Fork(left, right) =>
                // GC: follow the best branch, drop the other entirely.
                require(!bestPath.isEmpty, "Best path exhausted at Fork")
                val direction = bestPath.head
                val pathTail = bestPath.tail
                if direction == LeftFork then
                    val (promoted, cleanedLeft) =
                        promoteAndGC(
                          left,
                          ctx,
                          pathTail,
                          bestDepth,
                          currentTime,
                          numPromotions,
                          params
                        )
                    (promoted, cleanedLeft)
                else
                    val (promoted, cleanedRight) =
                        promoteAndGC(
                          right,
                          ctx,
                          pathTail,
                          bestDepth,
                          currentTime,
                          numPromotions,
                          params
                        )
                    (promoted, cleanedRight)

            case End =>
                (Nil, End)
    }

    // ============================================================================
    // Apply promotions to confirmed state
    // ============================================================================

    /** Apply promoted blocks to the confirmed state, updating the MPF root and building the new
      * [[ChainState]].
      */
    def applyPromotions(
        state: ChainState,
        promoted: List[BlockSummary],
        mpfProofs: List[List[ProofStep]],
        ctx0: TraversalCtx,
        cleanedTree: ForkTree,
        powLimit: BigInt
    ): ChainState = {
        def loop(
            blocks: List[BlockSummary],
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
                            val newCtx = accumulateBlock(ctx, block, powLimit)
                            val newRoot =
                                MPF(mpfRoot).insert(block.hash, block.hash, proof).root
                            loop(bTail, pTail, newCtx, newRoot)
                        case Nil => fail("MPF proof count mismatch")
        }

        val (finalCtx, finalRoot) =
            loop(promoted, mpfProofs, ctx0, state.confirmedBlocksRoot)

        ChainState(
          blockHeight = finalCtx.height,
          blockHash = finalCtx.lastBlockHash,
          currentTarget = finalCtx.currentBits,
          recentTimestamps = finalCtx.timestamps.take(MedianTimeSpan),
          previousDifficultyAdjustmentTimestamp = finalCtx.prevDiffAdjTimestamp,
          confirmedBlocksRoot = finalRoot,
          forkTree = cleanedTree
        )
    }

    // ============================================================================
    // Main compute function
    // ============================================================================

    /** Compute the new [[ChainState]] after applying an [[UpdateOracle]].
      *
      * Four phases:
      *   1. '''Insert''': validate new block headers and insert into the fork tree
      *   1. '''Best chain''': find the highest-chainwork path (single full tree traversal)
      *   1. '''Promote + GC''': promote eligible blocks to confirmed state and drop dead forks
      *      (single traversal along best path)
      *   1. '''Apply''': update confirmed state with promoted blocks and MPF proofs
      *
      * Steps 2-4 are skipped when no MPF proofs are provided (header-only submission). When proofs
      * are provided, the number of promoted blocks must match exactly.
      */
    def computeUpdate(
        state: ChainState,
        blockHeaders: List[BlockHeader],
        parentPath: Path,
        mpfInsertProofs: List[List[ProofStep]],
        currentTime: BigInt,
        params: BitcoinValidatorParams
    ): ChainState = {
        val ctx0 = initCtx(state)

        // Step 1: Validate and insert new blocks into tree (or keep tree if no headers)
        val newTree = blockHeaders match
            case Nil => state.forkTree
            case _ =>
                validateAndInsert(
                  state.forkTree,
                  parentPath,
                  blockHeaders,
                  ctx0,
                  currentTime,
                  params
                )
//        log("validateAndInsert")

        val promotionProofs = mpfInsertProofs
        val numBlocksToPromote = promotionProofs.length
        if numBlocksToPromote > BigInt(0) then
            // Step 2: Find best chain by chainwork (single full traversal)
            val (_, bestDepth, bestPath) = bestChainPath(newTree, state.blockHeight, BigInt(0))
//            log("bestChainPath")

            // Step 3: Promote eligible blocks + GC dead forks (single traversal along bestPath)
            val (promoted, cleanedTree) =
                promoteAndGC(
                  newTree,
                  ctx0,
                  bestPath,
                  bestDepth,
                  currentTime,
                  numBlocksToPromote,
                  params
                )
//            log("promoteAndGC")
            require(promoted.length == numBlocksToPromote, "Promoted block count mismatch")

            // Step 4: Apply promotions and set cleaned tree
            val updatedState =
                applyPromotions(
                  state,
                  promoted,
                  promotionProofs,
                  ctx0,
                  cleanedTree,
                  params.powLimit
                )
//            log("applyPromotions")
            updatedState
        else
            // No proofs → header-only submission, skip promotion and GC
            state.copy(forkTree = newTree)
    }

    // ============================================================================
    // Spend entry point
    // ============================================================================

    inline override def spend(
        param: Data,
        datum: Option[Datum],
        redeemer: Datum,
        tx: TxInfo,
        outRef: TxOutRef
    ): Unit = {
        val params = param.to[BitcoinValidatorParams]
        val action = redeemer.to[OracleAction]

        val intervalStartMs = tx.validRange.from.finiteOrFail("Must have finite interval start")
        val intervalEndMs = tx.validRange.to.finiteOrFail("Must have finite interval end")

        require(intervalEndMs - intervalStartMs <= MaxValidityWindow, "Validity interval too wide")

        val intervalStartInSeconds = intervalStartMs / 1000

        val inputs = tx.inputs
        val outputs = tx.outputs

        // Find own input
        val ownInput = inputs.find(_.outRef === outRef).getOrFail("Input not found").resolved

        // Extract policy ID (= script hash) for oracle NFT lookup
        val Credential.ScriptCredential(policyId) = ownInput.address.credential: @unchecked

        val chainState = ownInput.datum match
            case OutputDatum.OutputDatum(d) => d.to[ChainState]
            case _                          => fail("No inline datum")

        action match
            case OracleAction.UpdateOracle(blockHeaders, parentPath, mpfInsertProofs) =>
                // Compute expected new chainState
                val computedState =
                    computeUpdate(
                      chainState,
                      blockHeaders,
                      parentPath,
                      mpfInsertProofs,
                      intervalStartInSeconds,
                      params
                    )

                // Find continuing output: address match + oracle NFT
                val continuingOutput = outputs
                    .find(out =>
                        out.address.toData == ownInput.address.toData
                            && out.value.quantityOf(policyId, ByteString.empty) == BigInt(1)
                    )
                    .getOrFail("No continuing output with oracle NFT found")

                // NFT preservation
                require(
                  ownInput.value.withoutLovelace.toData == continuingOutput.value.withoutLovelace.toData,
                  "Non-ADA tokens must be preserved"
                )

                // ADA value can only increase (prevents draining oracle UTxO)
                require(
                  continuingOutput.value.lovelaceAmount >= ownInput.value.lovelaceAmount,
                  "ADA value can only increase"
                )

                // Verify output datum matches computed chainState
                val providedOutputDatum = continuingOutput.datum.toData
                val expectedOutputDatum = OutputDatum.OutputDatum(computedState.toData).toData
                require(
                  providedOutputDatum == expectedOutputDatum,
                  "Computed state does not match provided output datum"
                )

            case OracleAction.CloseOracle =>
                // 1. Staleness check: last confirmed block timestamp must be > closureTimeout ago
                require(
                  intervalStartInSeconds - chainState.recentTimestamps.head > params.closureTimeout,
                  "Oracle is not stale"
                )
                // 2. Owner authorization
                require(tx.isSignedBy(params.owner), "Not signed by oracle owner")
                // 3. NFT must be burned
                require(
                  tx.mint.tokens(policyId).toData == SortedMap
                      .singleton(ByteString.empty, BigInt(-1))
                      .toData,
                  "Must burn oracle NFT"
                )
    }

    import StrictLookups.*
    // One-shot NFT minting policy
    // param: BitcoinValidatorParams containing the TxOutRef that must be consumed to mint
    // redeemer: BigInt index of the oracle output in tx.outputs
    inline override def mint(
        param: Data,
        redeemer: Data,
        policyId: PolicyId,
        tx: TxInfo
    ): Unit = {
        val params = param.to[BitcoinValidatorParams]
        val minted = tx.mint.tokens(policyId).toData
        if minted == SortedMap.singleton(ByteString.empty, BigInt(1)).toData then
            // ensure we spend the one-shot TxOutRef
            tx.inputs.findOrFail(_.outRef.toData == params.oneShotTxOutRef.toData)
            // Verify oracle output contains the NFT at the specified index
            val outputIndex = redeemer.to[BigInt]
            val oracleOutput = tx.outputs.at(outputIndex)
            val expectedValue = Value.unsafeFromList(List((policyId, List(ByteString.empty -> 1))))
            require(
              oracleOutput.value.withoutLovelace.toData == expectedValue.toData,
              "Oracle output must contain NFT and no other tokens"
            )
            // Verify oracle output goes to this script's address (policyId == script hash)
            val expectedAddress = Address(Credential.ScriptCredential(policyId), Option.None)
            require(
              oracleOutput.address.toData == expectedAddress.toData,
              "Oracle output must go to script address without staking"
            )
        else
            require(
              minted == SortedMap.singleton(ByteString.empty, BigInt(-1)).toData,
              "can only mint 1 or burn 1 SP NFT"
            )
    }
}
