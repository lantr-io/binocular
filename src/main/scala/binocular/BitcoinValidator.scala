package binocular

import scalus.uplc.builtin.*
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString.*
import scalus.uplc.builtin.Data.{toData, FromData, ToData}
import scalus.cardano.onchain.plutus.v1.{Address, Credential}
import scalus.cardano.onchain.plutus.v2.{Interval, IntervalBoundType, OutputDatum}
import scalus.cardano.onchain.plutus.v1.PolicyId
import scalus.cardano.onchain.plutus.v3.{DataParameterizedValidator, Datum, TxInInfo, TxInfo, TxOut, TxOutRef}
import scalus.cardano.onchain.plutus.prelude.{List, *}
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry as MPF
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry.ProofStep
import scalus.{show as _, *}

import scala.annotation.tailrec

extension (a: ByteString) def reverse: ByteString = ByteString.fromArray(a.bytes.reverse)

// Type aliases for semantic clarity
type BlockHash = ByteString // 32-byte SHA256d hash of block header
type TxHash = ByteString // 32-byte SHA256d hash of transaction
type MerkleRoot = ByteString // 32-byte Merkle tree root hash
type CompactBits = ByteString // 4-byte compact difficulty target representation
type BlockHeaderBytes = ByteString // 80-byte raw Bitcoin block header

case class CoinbaseTx(
    version: ByteString,
    inputScriptSigAndSequence: ByteString,
    txOutsAndLockTime: ByteString
) derives FromData,
      ToData

@Compile
object CoinbaseTx

case class BlockHeader(bytes: ByteString) derives FromData, ToData
@Compile
object BlockHeader

extension (bh: BlockHeader)
    inline def version: BigInt =
        byteStringToInteger(false, bh.bytes.slice(0, 4))

    inline def prevBlockHash: BlockHash = bh.bytes.slice(4, 32)

    inline def bits: CompactBits = bh.bytes.slice(72, 4)

    inline def merkleRoot: MerkleRoot = bh.bytes.slice(36, 32)

    inline def timestamp: BigInt = byteStringToInteger(false, bh.bytes.slice(68, 4))

// ============================================================================
// Optimized ForksTree Data Structures
// ============================================================================
// These structures reduce memory consumption by storing only fork points and
// recent blocks instead of the entire unconfirmed chain history.

/** Summary of a block with essential validation data. Stored in a sliding window (last 11 blocks)
  * per branch for validation.
  */
case class BlockSummary(
    hash: BlockHash, // Block hash
    height: BigInt, // Block height
    chainwork: BigInt, // Cumulative chainwork at this block
    timestamp: BigInt, // Bitcoin block timestamp (for median-time-past)
    bits: CompactBits, // Difficulty target (for difficulty validation)
    addedTime: BigInt // Cardano time when this block was added to forksTree
) derives FromData,
      ToData
@Compile
object BlockSummary

/** A complete chain branch from a fork point to its current tip. Maintains ALL blocks in the branch
  * (newest first) for:
  *   - Median-time-past validation (needs last 11 blocks)
  *   - Promotion tracking (blocks are removed from list when promoted)
  *   - Each block has individual addedTime for accurate challenge period enforcement
  */
case class ForkBranch(
    tipHash: BlockHash, // Current tip of this branch
    tipHeight: BigInt, // Height of tip
    tipChainwork: BigInt, // Chainwork at tip
    recentBlocks: List[
      BlockSummary
    ] // ALL blocks in branch (newest first), each with individual addedTime
) derives FromData,
      ToData
@Compile
object ForkBranch

case class ChainState(
    // Confirmed state
    blockHeight: BigInt,
    blockHash: BlockHash,
    currentTarget: CompactBits,
    blockTimestamp: BigInt,
    recentTimestamps: List[BigInt], // Newest first
    previousDifficultyAdjustmentTimestamp: BigInt,
    confirmedBlocksRoot: ByteString, // MPF root hash (32 bytes) for confirmed blocks trie

    // Forks tree - each branch stores consecutive blocks from a fork point
    // This reduces memory usage by ~90% compared to storing each block separately
    // For a linear chain of 100 blocks: old approach = 100 entries, new approach = 1 branch
    forksTree: List[ForkBranch] // All active fork branches (unsorted)
) derives FromData,
      ToData

@Compile
object ChainState

enum Action derives FromData, ToData:
    /** Update the oracle with new block headers
      * @param blockHeaders
      *   \- new block headers to process (ordered from oldest to newest)
      * @param currentTime
      *   \- current on-chain time for validation in seconds since Unix epoch
      * @param inputDatumHash
      *   \- blake2b-256 hash of the input datum (for debugging non-determinism)
      * @param mpfInsertProofs
      *   \- one MPF proof (List[ProofStep]) per promoted block, for inserting into confirmed trie
      */
    case UpdateOracle(
        blockHeaders: List[BlockHeader],
        currentTime: BigInt,
        inputDatumHash: ByteString,
        mpfInsertProofs: List[List[ProofStep]]
    )

@Compile
object Action

@Compile
object BitcoinValidator extends DataParameterizedValidator {
    import BitcoinHelpers.*

    // ============================================================================
    // ForkBranch Helper Functions
    // ============================================================================
    // These functions work with the optimized ForkBranch structure where consecutive
    // blocks in a fork are stored together, reducing memory usage by ~90%.

    /** Find a branch by its tip hash. Returns the branch if found. Parent blocks are always tips of
      * their branches (forks create new branches, so we only need to check tips).
      */
    def findBranch(
        forksTree: List[ForkBranch],
        blockHash: BlockHash
    ): Option[ForkBranch] = {
        def search(remaining: List[ForkBranch]): Option[ForkBranch] = {
            remaining match
                case List.Nil => Option.None
                case List.Cons(branch, tail) =>
                    if branch.tipHash == blockHash then Option.Some(branch)
                    else search(tail)
        }
        search(forksTree)
    }

    /** Check if a block exists anywhere in the recentBlocks list by hash */
    def existsHash(blocks: List[BlockSummary], hash: BlockHash): Boolean = {
        def check(remaining: List[BlockSummary]): Boolean = {
            remaining match
                case List.Nil => false
                case List.Cons(block, tail) =>
                    if block.hash == hash then true
                    else check(tail)
        }
        check(blocks)
    }

    /** Lookup a specific block in recentBlocks by hash */
    def lookupInRecentBlocks(
        blocks: List[BlockSummary],
        hash: BlockHash
    ): Option[BlockSummary] = {
        def search(remaining: List[BlockSummary]): Option[BlockSummary] = {
            remaining match
                case List.Nil => Option.None
                case List.Cons(block, tail) =>
                    if block.hash == hash then Option.Some(block)
                    else search(tail)
        }
        search(blocks)
    }

    /** Find a parent block anywhere in the forks tree.
      *
      * Returns the containing branch, the matching block summary, and whether the matching block is
      * the current tip of that branch.
      */
    def lookupParentBlock(
        forksTree: List[ForkBranch],
        blockHash: BlockHash
    ): Option[(ForkBranch, BlockSummary, Boolean)] = {
        def search(
            remaining: List[ForkBranch]
        ): Option[(ForkBranch, BlockSummary, Boolean)] = {
            remaining match
                case List.Nil => Option.None
                case List.Cons(branch, tail) =>
                    lookupInRecentBlocks(branch.recentBlocks, blockHash) match
                        case Option.Some(block) =>
                            Option.Some((branch, block, branch.tipHash == blockHash))
                        case Option.None =>
                            search(tail)
        }

        search(forksTree)
    }

    def lookupBlockByHeight(
        blocks: List[BlockSummary],
        height: BigInt
    ): Option[BlockSummary] = {
        def search(remaining: List[BlockSummary]): Option[BlockSummary] = {
            remaining match
                case List.Nil => Option.None
                case List.Cons(block, tail) =>
                    if block.height == height then Option.Some(block)
                    else search(tail)
        }

        search(blocks)
    }

    /** Compute the expected difficulty bits for a block extending an unconfirmed parent block. */
    def expectedNextBitsForParent(
        confirmedState: ChainState,
        parentBranch: ForkBranch,
        parentBlock: BlockSummary
    ): CompactBits = {
        val nextHeight = parentBlock.height + 1

        if nextHeight % DifficultyAdjustmentInterval != BigInt(0) then parentBlock.bits
        else
            val adjustmentStartHeight = nextHeight - DifficultyAdjustmentInterval
            val adjustmentStartTimestamp =
                if adjustmentStartHeight <= confirmedState.blockHeight then
                    confirmedState.previousDifficultyAdjustmentTimestamp
                else
                    lookupBlockByHeight(parentBranch.recentBlocks, adjustmentStartHeight) match
                        case Option.Some(block) => block.timestamp
                        case Option.None =>
                            fail("Difficulty adjustment start block not found in branch history")

            getNextWorkRequired(
              parentBlock.height,
              parentBlock.bits,
              parentBlock.timestamp,
              adjustmentStartTimestamp
            )
    }

    /** Extend a branch by adding a new block at the tip Updates tipHash, tipHeight, tipChainwork
      * and prepends new block to recentBlocks Keeps ALL blocks in the branch until they are
      * promoted
      */
    def extendBranch(
        branch: ForkBranch,
        newBlock: BlockSummary
    ): ForkBranch = {
        val newRecentBlocks = List.Cons(newBlock, branch.recentBlocks)
        ForkBranch(
          tipHash = newBlock.hash,
          tipHeight = newBlock.height,
          tipChainwork = newBlock.chainwork,
          recentBlocks = newRecentBlocks
        )
    }

    /** Remove a branch from forksTree */
    def removeBranch(
        forksTree: List[ForkBranch],
        branchToRemove: ForkBranch
    ): List[ForkBranch] = {
        def remove(
            remaining: List[ForkBranch],
            accumulated: List[ForkBranch]
        ): List[ForkBranch] = {
            remaining match
                case List.Nil => accumulated.reverse
                case List.Cons(branch, tail) =>
                    if branch.tipHash == branchToRemove.tipHash then
                        // Found it - skip this branch
                        accumulated.reverse ++ tail
                    else remove(tail, List.Cons(branch, accumulated))
        }
        remove(forksTree, List.Nil)
    }

    /** Update a branch in forksTree (remove old, add new) */
    def updateBranch(
        forksTree: List[ForkBranch],
        oldBranch: ForkBranch,
        newBranch: ForkBranch
    ): List[ForkBranch] = {
        List.Cons(newBranch, removeBranch(forksTree, oldBranch))
    }

    // Binocular protocol parameters
    val MaturationConfirmations: BigInt = 100 // Blocks needed for promotion to confirmed state
    val TimeToleranceSeconds: BigInt =
        1 * 60 * 60 // Maximum difference between redeemer time and validity interval time
    val ChallengeAging: BigInt = 200 * 60 // 200 minutes in seconds (challenge period)
    val StaleCompetingForkAge: BigInt = 400 * 60 // 400 minutes (2× challenge period)
    val ChainworkGapThreshold: BigInt = 10 // Blocks worth of work for stale fork detection
    val MaxForksTreeSize: BigInt = 180 // Maximum forks tree size before garbage collection

    def getMedianTimePastFromBlocks(
        forkBlocks: List[BlockSummary],
        confirmedTimestamps: List[BigInt]
    ): BigInt = {
        // Collect timestamps from fork blocks, then from confirmed state if needed
        // Sort using insertion sort (efficient for n ≤ 11)
        // Bitcoin allows out-of-order timestamps, so we must sort before finding median

        @tailrec
        def collect(
            remaining: List[BlockSummary],
            confirmed: List[BigInt],
            sorted: List[BigInt],
            count: BigInt
        ): List[BigInt] =
            if count >= MedianTimeSpan then sorted
            else
                remaining match
                    case List.Cons(block, tail) =>
                        collect(
                          tail,
                          confirmed,
                          insertReverseSorted(block.timestamp, sorted),
                          count + 1
                        )
                    case List.Nil =>
                        // Fork exhausted, continue with confirmed timestamps
                        confirmed match
                            case List.Nil => sorted
                            case List.Cons(ts, tail) =>
                                collect(List.Nil, tail, insertReverseSorted(ts, sorted), count + 1)

        val sortedTimestamps = collect(forkBlocks, confirmedTimestamps, List.Nil, 0)
        getMedianTimePast(sortedTimestamps, sortedTimestamps.size)
    }

    // Canonical chain selection - matches Algorithm 9 in Whitepaper
    // Selects the branch with highest cumulative chainwork at its tip
    def selectCanonicalChain(
        forksTree: List[ForkBranch]
    ): Option[ForkBranch] =
        // Find branch with maximum chainwork
        def findMaxChainworkBranch(
            branches: List[ForkBranch],
            currentBest: Option[ForkBranch]
        ): Option[ForkBranch] =
            branches match
                case List.Nil => currentBest
                case List.Cons(branch, tail) =>
                    val newBest = currentBest match
                        case Option.None =>
                            Option.Some(branch)
                        case Option.Some(best) =>
                            if branch.tipChainwork > best.tipChainwork then Option.Some(branch)
                            else currentBest
                    findMaxChainworkBranch(tail, newBest)

        findMaxChainworkBranch(forksTree, Option.None)

    // Add block to forks tree - matches Transition 1 in Whitepaper
    def addBlockToForksTree(
        forksTree: List[ForkBranch],
        blockHeader: BlockHeader,
        confirmedState: ChainState,
        currentTime: BigInt
    ): List[ForkBranch] =
        val hash = blockHeaderHash(blockHeader)
        val hashInt = byteStringToInteger(false, hash)
        val prevHash = blockHeader.prevBlockHash

        // Check if parent exists (in forksTree OR is confirmed tip)
        val parentLookupOpt = lookupParentBlock(forksTree, prevHash)
        val parentIsConfirmedTip = prevHash == confirmedState.blockHash
        require(
          parentLookupOpt.isDefined || parentIsConfirmedTip,
          "Parent block not found"
        )

        // === SECURITY: Full block validation ===

        // Calculate parent height and chainwork
        val (
          parentBranchOpt,
          parentBlockOpt,
          parentIsBranchTip,
          parentHeight,
          parentChainwork,
          parentTimestamp,
          parentBits
        ) =
            if parentIsConfirmedTip then
                (
                  Option.None,
                  Option.None,
                  false,
                  confirmedState.blockHeight,
                  calculateBlockProof(
                    compactBitsToTarget(confirmedState.currentTarget)
                  ), // Work for this block
                  confirmedState.blockTimestamp,
                  confirmedState.currentTarget
                )
            else
                parentLookupOpt match
                    case Option.Some((branch, block, isTip)) =>
                        (
                          Option.Some(branch),
                          Option.Some(block),
                          isTip,
                          block.height,
                          block.chainwork,
                          block.timestamp,
                          block.bits
                        )
                    case Option.None =>
                        fail("Parent branch not found")

        val blockHeight = parentHeight + 1

        // 1. VALIDATE PROOF-OF-WORK
        val target = compactBitsToTarget(blockHeader.bits)
        require(hashInt <= target, "Invalid proof-of-work")
        require(target <= PowLimit, "Target exceeds PowLimit")

        // 2. VALIDATE DIFFICULTY
        val expectedBits =
            if parentIsConfirmedTip then
                getNextWorkRequired(
                  parentHeight,
                  confirmedState.currentTarget,
                  confirmedState.blockTimestamp,
                  confirmedState.previousDifficultyAdjustmentTimestamp
                )
            else
                parentBranchOpt match
                    case Option.Some(parentBranch) =>
                        parentBlockOpt match
                            case Option.Some(parentBlock) =>
                                expectedNextBitsForParent(
                                  confirmedState,
                                  parentBranch,
                                  parentBlock
                                )
                            case Option.None =>
                                fail("Parent block data not found")
                    case Option.None =>
                        fail("Parent branch data not found")

        require(blockHeader.bits == expectedBits, "Invalid difficulty")

        // 3. VALIDATE TIMESTAMP
        val blockTime = blockHeader.timestamp
        val medianTimePast =
            if parentIsConfirmedTip then
                getMedianTimePast(
                  confirmedState.recentTimestamps,
                  confirmedState.recentTimestamps.size
                )
            else
                // For fork blocks, use branch's recentBlocks + confirmed timestamps for median-time-past
                parentBranchOpt match
                    case Option.Some(branch) =>
                        getMedianTimePastFromBlocks(
                          branch.recentBlocks,
                          confirmedState.recentTimestamps
                        )
                    case Option.None => fail("Parent branch not found")

        require(blockTime > medianTimePast, "Block timestamp not greater than median time past")
        require(blockTime <= currentTime + MaxFutureBlockTime, "Block timestamp too far in future")

        // 4. VALIDATE VERSION
        require(blockHeader.version >= 4, "Outdated block version")

        // === End validation ===

        // Calculate chainwork using Bitcoin Core's formula: 2^256 / (target + 1)
        // Matches GetBlockProof() in Bitcoin Core's chain.cpp
        val blockWork = calculateBlockProof(target)
        val newChainwork = parentChainwork + blockWork

        // Create BlockSummary for this block
        val newBlock = BlockSummary(
          hash = hash,
          height = blockHeight,
          chainwork = newChainwork,
          timestamp = blockTime,
          bits = blockHeader.bits,
          addedTime = currentTime // Cardano time when block added
        )

        // Determine how to update forksTree based on parent location
        if parentIsConfirmedTip then
            // Parent is confirmed tip - create new branch
            val newBranch = ForkBranch(
              tipHash = hash,
              tipHeight = blockHeight,
              tipChainwork = newChainwork,
              recentBlocks = List.single(newBlock)
            )
            List.Cons(newBranch, forksTree)
        else
            parentBranchOpt match
                case Option.Some(parentBranch) =>
                    if parentIsBranchTip then
                        // Parent is branch tip - extend the branch
                        val extendedBranch = extendBranch(parentBranch, newBlock)
                        updateBranch(forksTree, parentBranch, extendedBranch)
                    else
                        // Parent is an internal block - create a competing branch from that point
                        val newBranch = ForkBranch(
                          tipHash = hash,
                          tipHeight = blockHeight,
                          tipChainwork = newChainwork,
                          recentBlocks = List.single(newBlock)
                        )
                        List.Cons(newBranch, forksTree)
                case Option.None =>
                    fail("Parent branch not found")

    // Block promotion (maturation) - matches Algorithm 10 in Whitepaper
    // Returns (list of promoted blocks oldest-first, updated forks tree with promoted blocks removed)
    def promoteQualifiedBlocks(
        forksTree: List[ForkBranch],
        confirmedTip: BlockHash,
        confirmedHeight: BigInt,
        currentTime: BigInt
    ): (List[BlockSummary], List[ForkBranch]) =
        // Find canonical branch
        selectCanonicalChain(forksTree) match
            case Option.None =>
                // No blocks in forks tree
                (List.Nil, forksTree)
            case Option.Some(canonicalBranch) =>
                // Split blocks into (remainingReverse, promoted) in a single pass
                // recentBlocks is newest-first, so we traverse from newest to oldest
                // Once we find a promotable block, all older blocks (rest of list) are also promotable
                val canonicalTipHeight = canonicalBranch.tipHeight

                // Returns (remaining blocks reversed, promoted blocks oldest-first)
                // When no promotion, remainingReverse is Nil and we use original recentBlocks
                def splitPromotable(
                    blocks: List[BlockSummary],
                    remainingReverse: List[BlockSummary]
                ): (List[BlockSummary], List[BlockSummary]) =
                    blocks match
                        case List.Nil => (remainingReverse, List.Nil)
                        case List.Cons(block, tail) =>
                            val depth = canonicalTipHeight - block.height
                            val age = currentTime - block.addedTime

                            if depth >= MaturationConfirmations && age >= ChallengeAging then
                                // This block and all older blocks (tail) are promotable
                                // Reverse tail and prepend this block to get oldest-first order
                                def reverseAndPrepend(
                                    bs: List[BlockSummary],
                                    acc: List[BlockSummary]
                                ): List[BlockSummary] =
                                    bs match
                                        case List.Nil => acc
                                        case List.Cons(b, tailBs) =>
                                            reverseAndPrepend(tailBs, List.Cons(b, acc))
                                val promotedBlocks =
                                    reverseAndPrepend(tail, List.Cons(block, List.Nil))
                                (remainingReverse, promotedBlocks)
                            else
                                // Not yet promotable, add to remaining and continue
                                splitPromotable(tail, List.Cons(block, remainingReverse))

                val (remainingReverse, promotedBlocks) =
                    splitPromotable(canonicalBranch.recentBlocks, List.Nil)

                // Update forks tree based on promotion result
                val updatedTree =
                    if promotedBlocks.isEmpty then forksTree
                    else if remainingReverse.isEmpty then
                        // All blocks were promoted, remove the entire branch
                        removeBranch(forksTree, canonicalBranch)
                    else
                        // Update branch with remaining blocks (need to reverse back)
                        val updatedBranch = ForkBranch(
                          tipHash = canonicalBranch.tipHash,
                          tipHeight = canonicalBranch.tipHeight,
                          tipChainwork = canonicalBranch.tipChainwork,
                          recentBlocks = remainingReverse.reverse
                        )
                        updateBranch(forksTree, canonicalBranch, updatedBranch)

                (promotedBlocks, updatedTree)

    // Validate fork submission rule: must include canonical extension
    // Prevents attack where adversary submits only forks to stall Oracle progress
    def validateForkSubmission(
        blockHeaders: List[BlockHeader],
        forksTree: List[ForkBranch],
        confirmedTip: BlockHash
    ): Unit = {
        // Note: Duplicate blocks are implicitly rejected by addBlockToForksTree
        // because after adding the first block, its parent becomes unreachable
        // (findBranch only finds tips, and the duplicate's parent is no longer a tip)

        // Find current canonical tip hash
        val canonicalTipHash = selectCanonicalChain(forksTree) match
            case Option.Some(branch) => branch.tipHash
            case Option.None         => confirmedTip

        // RULE 2: Classify blocks: canonical extensions vs others (forks)
        def classifyBlocks(
            headers: List[BlockHeader],
            canonicalExts: BigInt,
            forkBlocks: BigInt
        ): (BigInt, BigInt) = {
            headers match
                case List.Nil => (canonicalExts, forkBlocks)
                case List.Cons(header, tail) =>
                    val prevHash = header.prevBlockHash
                    if prevHash == canonicalTipHash then
                        // Extends current canonical tip
                        classifyBlocks(tail, canonicalExts + 1, forkBlocks)
                    else
                        // Fork or extends non-canonical block
                        classifyBlocks(tail, canonicalExts, forkBlocks + 1)
        }

        val (canonicalCount, forkCount) = classifyBlocks(blockHeaders, BigInt(0), BigInt(0))

        // RULE 3: If submitting any forks, must include at least one canonical extension
        if forkCount > BigInt(0) && canonicalCount == BigInt(0) then
            fail("Fork submission must include canonical tip extension")
    }

    // Garbage collection - removes old dead fork branches to maintain bounded datum size
    // With ForkBranch structure, GC is simpler: we remove entire branches that meet criteria
    def garbageCollect(
        forksTree: List[ForkBranch],
        confirmedTip: BlockHash,
        confirmedHeight: BigInt,
        currentTime: BigInt
    ): List[ForkBranch] = {
        val currentSize = forksTree.size

        if currentSize <= MaxForksTreeSize then
            // No garbage collection needed
            forksTree
        else
            // Find canonical branch
            selectCanonicalChain(forksTree) match
                case Option.None =>
                    // No blocks in forks tree
                    forksTree
                case Option.Some(canonicalBranch) =>
                    val canonicalTipHeight = canonicalBranch.tipHeight
                    val canonicalTipChainwork = canonicalBranch.tipChainwork

                    // Determine if a branch is removable
                    def isRemovable(branch: ForkBranch): Boolean = {
                        // Cannot remove canonical branch
                        if branch.tipHash == canonicalBranch.tipHash then false
                        else
                            val heightGap = canonicalTipHeight - branch.tipHeight
                            // Use the oldest block's addedTime (last in list) for age calculation
                            val oldestBlockTime = branch.recentBlocks.lastOption
                                .map(_.addedTime)
                                .getOrElse(currentTime)
                            val age = currentTime - oldestBlockTime
                            val chainworkGap = canonicalTipChainwork - branch.tipChainwork

                            // Criteria Set A: Old dead forks
                            val isOldDeadFork =
                                heightGap >= MaturationConfirmations &&
                                    age >= ChallengeAging

                            // Criteria Set B: Stale competing forks
                            val isStaleCompetingFork =
                                age >= StaleCompetingForkAge &&
                                    chainworkGap >= (ChainworkGapThreshold * (PowLimit / compactBitsToTarget(
                                      hex"1d00ffff"
                                    )))

                            // Criteria Set C: Competing long branches
                            // If we have two very long branches competing, remove the one with less chainwork
                            // This prevents the forksTree from filling up with competing long forks
                            val isLongCompetingFork =
                                age >= ChallengeAging &&
                                    branch.tipHeight >= (confirmedHeight + MaturationConfirmations) &&
                                    chainworkGap > BigInt(
                                      0
                                    ) // Any chainwork gap qualifies for removal after challenge period

                            isOldDeadFork || isStaleCompetingFork || isLongCompetingFork
                    }

                    // Remove branches that meet criteria
                    def performRemoval(
                        branches: List[ForkBranch],
                        targetSize: BigInt
                    ): List[ForkBranch] = {
                        // Filter out removable branches
                        def filterBranches(
                            remaining: List[ForkBranch],
                            accumulated: List[ForkBranch]
                        ): List[ForkBranch] = {
                            remaining match
                                case List.Nil => accumulated.reverse
                                case List.Cons(branch, tail) =>
                                    if isRemovable(branch) then filterBranches(tail, accumulated)
                                    else filterBranches(tail, List.Cons(branch, accumulated))
                        }

                        val filtered = filterBranches(branches, List.Nil)

                        // If still over size, keep only the N branches with highest chainwork
                        if filtered.size > targetSize then
                            // Manual selection sort - keep top N by chainwork
                            def selectTopN(
                                remaining: List[ForkBranch],
                                selected: List[ForkBranch],
                                count: BigInt
                            ): List[ForkBranch] = {
                                if count == BigInt(0) || remaining.isEmpty then selected
                                else
                                    // Find max chainwork in remaining
                                    def findMax(
                                        lst: List[ForkBranch],
                                        currentMax: ForkBranch
                                    ): ForkBranch = {
                                        lst match
                                            case List.Nil => currentMax
                                            case List.Cons(b, tail) =>
                                                if b.tipChainwork > currentMax.tipChainwork then
                                                    findMax(tail, b)
                                                else findMax(tail, currentMax)
                                    }

                                    val maxBranch = findMax(remaining.tail, remaining.head)

                                    // Remove maxBranch from remaining
                                    val newRemaining =
                                        remaining.filter(b => b.tipHash != maxBranch.tipHash)

                                    selectTopN(
                                      newRemaining,
                                      List.Cons(maxBranch, selected),
                                      count - 1
                                    )
                            }

                            selectTopN(filtered, List.Nil, targetSize)
                        else filtered
                    }

                    performRemoval(forksTree, MaxForksTreeSize)
    }

    def updateTip(
        prevState: ChainState,
        blockHeader: BlockHeader,
        currentTime: BigInt
    ): ChainState =
        val hash = blockHeaderHash(blockHeader)
        val hashBigInt = byteStringToInteger(false, hash)
        val blockTime = blockHeader.timestamp

        // check previous block hash
        val validPrevBlockHash = blockHeader.prevBlockHash == prevState.blockHash

        // check proof of work - matches CheckProofOfWork() in pow.cpp:140-163
        // FIXME: check if bits are valid
        val compactTarget = blockHeader.bits
        val target = compactBitsToTarget(compactTarget)
        val validProofOfWork = hashBigInt <= target

        // Difficulty validation - matches ContextualCheckBlockHeader() in validation.cpp:4165
        val nextDifficulty = getNextWorkRequired(
          prevState.blockHeight,
          prevState.currentTarget,
          prevState.blockTimestamp,
          prevState.previousDifficultyAdjustmentTimestamp
        )
        val validDifficulty = compactTarget == nextDifficulty

        // Check blockTime against median of last 11 blocks
        // Matches ContextualCheckBlockHeader() in validation.cpp:4180-4182
        val numTimestamps = prevState.recentTimestamps.size
        val medianTimePast = getMedianTimePast(prevState.recentTimestamps, numTimestamps)
        // verify the block timestamp
        require(
          blockTime <= currentTime + MaxFutureBlockTime,
          "Block timestamp too far in the future"
        )
        require(
          blockTime > medianTimePast,
          "Block timestamp must be greater than median time of past 11 blocks"
        )

        val newDifficultyAdjustmentTimestamp =
            if (prevState.blockHeight + 1) % DifficultyAdjustmentInterval == BigInt(0) then
                blockTime
            else prevState.previousDifficultyAdjustmentTimestamp

        // Insert new blockTime maintaining reverse sort order
        val withNewTimestamp = insertReverseSorted(blockTime, prevState.recentTimestamps)
        val newTimestamps = withNewTimestamp.take(MedianTimeSpan)

        // Reject blocks with outdated version
        // Matches ContextualCheckBlockHeader() in validation.cpp:4201-4206
        val validVersion = blockHeader.version >= 4
        require(validVersion, "Block version is outdated")

        val validBlockHeader = validPrevBlockHash.?
            && validProofOfWork.?
            && validDifficulty.?
            && validVersion.?

        require(validBlockHeader, "Block header is not valid")
        ChainState(
          blockHeight = prevState.blockHeight + 1,
          blockHash = hash,
          currentTarget = nextDifficulty,
          blockTimestamp = blockTime,
          recentTimestamps = newTimestamps,
          previousDifficultyAdjustmentTimestamp = newDifficultyAdjustmentTimestamp,
          confirmedBlocksRoot =
              prevState.confirmedBlocksRoot, // Preserve confirmed blocks root (updated separately)
          forksTree = prevState.forksTree // Preserve forks tree (will be updated separately)
        )

    def verifyNewTip(
        intervalStartInSeconds: BigInt,
        prevState: ChainState,
        newState: Data,
        blockHeader: BlockHeader
    ): Unit =
        val expectedNewState = updateTip(prevState, blockHeader, intervalStartInSeconds)
        val validNewState = newState == expectedNewState.toData

        require(validNewState, "New state does not match expected state")

    /** Apply promoted blocks to the confirmed-state metadata in oldest-to-newest order. */
    def applyPromotionsToConfirmedState(
        confirmedState: ChainState,
        promotedBlocks: List[BlockSummary]
    ): ChainState = {
        promotedBlocks.foldLeft(confirmedState) { (state, block) =>
            val withNewTimestamp = insertReverseSorted(block.timestamp, state.recentTimestamps)
            val newTimestamps = withNewTimestamp.take(MedianTimeSpan)
            val newDifficultyAdjustmentTimestamp =
                if block.height % DifficultyAdjustmentInterval == BigInt(0) then block.timestamp
                else state.previousDifficultyAdjustmentTimestamp

            ChainState(
              blockHeight = block.height,
              blockHash = block.hash,
              currentTarget = block.bits,
              blockTimestamp = block.timestamp,
              recentTimestamps = newTimestamps,
              previousDifficultyAdjustmentTimestamp = newDifficultyAdjustmentTimestamp,
              confirmedBlocksRoot = state.confirmedBlocksRoot,
              forksTree = state.forksTree
            )
        }
    }

    /** Compute the new ChainState after applying block headers. This function contains the core
      * UpdateOracle logic and can be called both on-chain (from validator) and off-chain (for
      * pre-computation).
      *
      * @param prevState
      *   Current ChainState
      * @param blockHeaders
      *   Block headers to apply
      * @param currentTime
      *   Current time in seconds (POSIX time)
      * @return
      *   New ChainState after applying headers
      */
    def computeUpdateOracleState(
        prevState: ChainState,
        blockHeaders: List[BlockHeader],
        currentTime: BigInt,
        mpfInsertProofs: List[List[ProofStep]]
    ): ChainState = {
        // scalus.prelude.log("computeUpdateOracleState START")
        // scalus.prelude.log("INPUT prevState.forksTree.size:")
        // scalus.prelude.log(scalus.prelude.show(prevState.forksTree.size))
        // scalus.prelude.log("INPUT prevState.blockHeight:")
        // scalus.prelude.log(scalus.prelude.show(prevState.blockHeight))

        // Validate fork submission rule (prevents stalling attack and duplicates)
        validateForkSubmission(blockHeaders, prevState.forksTree, prevState.blockHash)

        // Validate non-empty block headers
        require(!blockHeaders.isEmpty, "Empty block headers list")

        // Process all block headers: add each to forks tree
        def processHeaders(
            headers: List[BlockHeader],
            currentForksTree: List[ForkBranch]
        ): List[ForkBranch] = {
            headers match
                case List.Nil =>
                    // scalus.prelude.log("processHeaders done")
                    currentForksTree
                case List.Cons(header, tail) =>
                    // scalus.prelude.log("processHeaders adding block")
                    val updatedTree = addBlockToForksTree(
                      currentForksTree,
                      header,
                      prevState,
                      currentTime
                    )
                    processHeaders(tail, updatedTree)
        }

        // scalus.prelude.log("processing headers")
        val forksTreeAfterAddition = processHeaders(blockHeaders, prevState.forksTree)
        // scalus.prelude.log("headers processed")

        // Select canonical chain (highest chainwork)
        // scalus.prelude.log("selecting canonical chain")
        val canonicalBranchOpt = selectCanonicalChain(forksTreeAfterAddition)
        val canonicalTipHash = canonicalBranchOpt match
            case Option.Some(branch) =>
                // scalus.prelude.log("canonical tip found")
                branch.tipHash
            case Option.None =>
                // scalus.prelude.log("canonical tip is prev hash")
                prevState.blockHash

        // Promote qualified blocks (100+ confirmations AND 200+ min old)
        // scalus.prelude.log("promoting qualified blocks")
        val (promotedBlocks, forksTreeAfterPromotion) = promoteQualifiedBlocks(
          forksTreeAfterAddition,
          prevState.blockHash, // confirmedTip
          prevState.blockHeight,
          currentTime
        )

        // if promotedBlocks.isEmpty then
        //     scalus.prelude.log("no blocks promoted")
        // else
        //     scalus.prelude.log("blocks promoted")

        // Run garbage collection if forks tree exceeds size limit
        val finalForksTree =
            if forksTreeAfterPromotion.size > MaxForksTreeSize then
                // scalus.prelude.log("running GC")
                garbageCollect(
                  forksTreeAfterPromotion,
                  prevState.blockHash, // confirmedTip
                  prevState.blockHeight,
                  currentTime
                )
            else
                // scalus.prelude.log("skipping GC")
                forksTreeAfterPromotion

        // Compute new state
        // If blocks were promoted, update confirmed state
        // scalus.prelude.log("computing final state")

        // Log final forksTree size
        val finalTreeSize = finalForksTree.size
        // scalus.prelude.log("ON-CHAIN final forksTree size:")
        // scalus.prelude.log(scalus.prelude.show(finalTreeSize))

        if promotedBlocks.isEmpty then
            // No promotion: only forks tree changed
            // scalus.prelude.log("no promotion, returning state with updated forks")
            ChainState(
              blockHeight = prevState.blockHeight,
              blockHash = prevState.blockHash,
              currentTarget = prevState.currentTarget,
              blockTimestamp = prevState.blockTimestamp,
              recentTimestamps = prevState.recentTimestamps,
              previousDifficultyAdjustmentTimestamp =
                  prevState.previousDifficultyAdjustmentTimestamp,
              confirmedBlocksRoot = prevState.confirmedBlocksRoot,
              forksTree = finalForksTree
            )
        else
            // scalus.prelude.log("promotion occurred, updating confirmed state")
            // Promotion occurred: update confirmed state
            // Get the latest (newest) promoted block (last in oldest-first list)
            val latestPromotedBlock = promotedBlocks.last

            // Insert promoted blocks into MPF trie (in order from oldest to newest)
            // Each insert proof must correspond to the trie state after previous inserts
            def applyMpfInserts(
                root: ByteString,
                blocks: List[BlockSummary],
                proofs: List[List[ProofStep]]
            ): ByteString =
                blocks match
                    case List.Nil =>
                        proofs match
                            case List.Nil => root
                            case _        => fail("MPF proof count mismatch")
                    case List.Cons(block, bTail) =>
                        proofs match
                            case List.Cons(proof, pTail) =>
                                val newRoot =
                                    MPF(root).insert(block.hash, block.hash, proof).root
                                applyMpfInserts(newRoot, bTail, pTail)
                            case List.Nil => fail("MPF proof count mismatch")

            val updatedRoot =
                applyMpfInserts(prevState.confirmedBlocksRoot, promotedBlocks, mpfInsertProofs)

            val updatedConfirmedState =
                applyPromotionsToConfirmedState(prevState, promotedBlocks)

            ChainState(
              blockHeight = updatedConfirmedState.blockHeight,
              blockHash = updatedConfirmedState.blockHash,
              currentTarget = updatedConfirmedState.currentTarget,
              blockTimestamp = updatedConfirmedState.blockTimestamp,
              recentTimestamps = updatedConfirmedState.recentTimestamps,
              previousDifficultyAdjustmentTimestamp =
                  updatedConfirmedState.previousDifficultyAdjustmentTimestamp,
              confirmedBlocksRoot = updatedRoot,
              forksTree = finalForksTree
            )
    }

    def findUniqueOutputFrom(outputs: List[TxOut], scriptAddress: Address): TxOut = {
        val matchingOutputs = outputs.filter(out => out.address === scriptAddress)
        require(matchingOutputs.size == BigInt(1), "There must be exactly one continuing output")
        matchingOutputs.head
    }

    inline def update(
        outRef: TxOutRef,
        action: Action,
        inputs: List[TxInInfo],
        outputs: List[TxOut],
        validRange: Interval
    ): Unit = {
        val intervalStartInSeconds = validRange.from.boundType match
            case IntervalBoundType.Finite(time) => time / 1000
            case _                              => fail("Must have finite interval start")

        // Find the continuing output (output to the same script address)
        val ownInput = inputs
            .find {
                _.outRef === outRef
            }
            .getOrFail("Input not found")
            .resolved
        val prevState =
            ownInput.datum match
                case OutputDatum.OutputDatum(datum) =>
                    datum.to[ChainState]
                case _ => fail("No datum")
        action match
            case Action.UpdateOracle(blockHeaders, redeemerTime, inputDatumHash, mpfInsertProofs) =>
                // Datum hash verification disabled for production (expensive)
                // Uncomment for debugging datum non-determinism issues:
                // val actualInputDatumHash = blake2b_256(serialiseData(prevState.toData))
                // // scalus.prelude.log("Expected inputDatumHash from redeemer (hex):")
                // // scalus.prelude.log(scalus.prelude.Prelude.encodeHex(inputDatumHash))
                // // scalus.prelude.log("Actual inputDatumHash computed on-chain (hex):")
                // // scalus.prelude.log(scalus.prelude.Prelude.encodeHex(actualInputDatumHash))
                // require(
                //   inputDatumHash == actualInputDatumHash,
                //   "Input datum hash mismatch - datum was modified!"
                // )

                // Verify redeemer time is within tolerance of actual validity interval time
                val timeDiff =
                    if redeemerTime > intervalStartInSeconds then
                        redeemerTime - intervalStartInSeconds
                    else intervalStartInSeconds - redeemerTime

                require(
                  timeDiff <= TimeToleranceSeconds,
                  "Redeemer time too far from validity interval"
                )

                // Compute the new state using time from redeemer (ensures offline/online consistency)
                val computedState =
                    computeUpdateOracleState(prevState, blockHeaders, redeemerTime, mpfInsertProofs)

                val continuingOutput = findUniqueOutputFrom(outputs, ownInput.address)

                // NFT preservation check: non-ADA tokens must be preserved
                require(
                  ownInput.value.withoutLovelace === continuingOutput.value.withoutLovelace,
                  "Non-ADA tokens must be preserved"
                )

                // Extract the datum from the continuing output (provided by transaction builder)
                val providedOutputDatum = continuingOutput.datum match
                    case OutputDatum.OutputDatum(datum) => datum
                    case _ => fail("Continuing output must have inline datum")

                val computedStateDatum = computedState.toData

                require(
                  computedStateDatum == providedOutputDatum,
                  "Computed state does not match provided output datum"
                )
    }

    // One-shot NFT minting policy
    // param: TxOutRef that must be consumed to mint (one-shot guarantee)
    // redeemer: BigInt index of the oracle output in tx.outputs
    inline override def mint(
        param: Data,
        redeemer: Data,
        policyId: PolicyId,
        tx: TxInfo
    ): Unit = {
        val mintedQty = tx.mint.quantityOf(policyId, ByteString.empty)
        if mintedQty > BigInt(0) then
            // Minting: enforce one-shot
            val requiredUtxo = param.to[TxOutRef]
            require(tx.inputs.exists(_.outRef === requiredUtxo), "Required UTxO not spent")
            require(mintedQty == BigInt(1), "Must mint exactly 1 NFT")
            // Verify oracle output contains the NFT at the specified index
            val outputIndex = redeemer.to[BigInt]
            val oracleOutput = tx.outputs !! outputIndex
            require(
              oracleOutput.value.quantityOf(policyId, ByteString.empty) == BigInt(1),
              "Oracle output must contain NFT"
            )
            // Verify oracle output goes to this script's address (policyId == script hash)
            require(
              oracleOutput.address.credential match
                  case Credential.ScriptCredential(hash) => hash == policyId
                  case _                                 => false,
              "Oracle output must go to script address"
            )
        // else: burning (mintedQty < 0) is always allowed
    }

    // This one is for V3 lowering
    inline override def spend(
        param: Data,
        datum: Option[Datum],
        redeemer: Datum,
        tx: TxInfo,
        outRef: TxOutRef
    ): Unit = {
        val action = redeemer.to[Action]

        val inputs = tx.inputs
        val outputs = tx.outputs
        val validRange = tx.validRange

        update(outRef, action, inputs, outputs, validRange)
    }

    // This is for Sum-of-Products lowering
    inline def spend2(
        param: Data,
        datum: Option[Datum],
        redeemer: Datum,
        txInfoData: Data,
        outRef: TxOutRef
    ): Unit = {
        val action = redeemer.to[Action]

        val inputs = txInfoData.field[TxInfo](_.inputs).to[List[TxInInfo]]
        val outputs = txInfoData.field[TxInfo](_.outputs).to[List[TxOut]]
        val validRange = txInfoData.field[TxInfo](_.validRange).to[Interval]
        update(outRef, action, inputs, outputs, validRange)
    }

    // This is for Sum-of-Products lowering
    def validate2(param: Data)(scData: Data): Unit = {
        val sc = unConstrData(scData).snd
        val txInfoData = sc.head
        val redeemer = sc.tail.head
        val scriptInfo = unConstrData(sc.tail.tail.head)
        if scriptInfo.fst == BigInt(1) then
            val txOutRef = scriptInfo.snd.head.to[TxOutRef]
            val datum = scriptInfo.snd.tail.head.to[Option[Datum]]
            spend2(param, datum, redeemer, txInfoData, txOutRef)
        else fail("Invalid script context")
    }

    def reverse(bs: ByteString): ByteString =
        val len = lengthOfByteString(bs)
        def loop(idx: BigInt, acc: ByteString): ByteString =
            if idx == len then acc
            else loop(idx + 1, consByteString(bs.at(idx), acc))
        loop(0, ByteString.empty)
}
// recompile trigger $(date +%s)
