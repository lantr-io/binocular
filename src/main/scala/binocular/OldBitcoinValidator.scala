package binocular

import scalus.uplc.builtin.*
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString.*
import scalus.uplc.builtin.Data.{toData, FromData, ToData}
import scalus.cardano.onchain.plutus.v1.{Address, Credential, PolicyId, TokenName}
import scalus.cardano.onchain.plutus.v2.{Interval, IntervalBoundType, OutputDatum}
import scalus.cardano.onchain.plutus.v3.{DataParameterizedValidator, Datum, TxInInfo, TxInfo, TxOut, TxOutRef, Value}
import scalus.cardano.onchain.plutus.prelude.{List, *}
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry as MPF
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry.ProofStep
import scalus.cardano.onchain.plutus.prelude.PairList.{PairCons, PairNil}
import scalus.compiler.Compile
import scalus.{show as _, *}

import scala.annotation.tailrec

// ============================================================================
// Old ForksTree Data Structures (preserved for backward compatibility with old tests)
// Shared types (BlockHeader, CoinbaseTx, type aliases, StrictLookups) are in BitcoinValidator.scala
// ============================================================================

case class OldBlockSummary(
    hash: BlockHash,
    height: BigInt,
    chainwork: BigInt,
    timestamp: BigInt,
    bits: CompactBits,
    addedTime: BigInt
) derives FromData,
      ToData
@Compile
object OldBlockSummary

case class OldForkBranch(
    tipHash: BlockHash,
    tipHeight: BigInt,
    tipChainwork: BigInt,
    recentBlocks: List[OldBlockSummary]
) derives FromData,
      ToData
@Compile
object OldForkBranch

case class OldChainState(
    blockHeight: BigInt,
    blockHash: BlockHash,
    currentTarget: CompactBits,
    blockTimestamp: BigInt,
    recentTimestamps: List[BigInt],
    previousDifficultyAdjustmentTimestamp: BigInt,
    confirmedBlocksRoot: ByteString,
    forksTree: List[OldForkBranch]
) derives FromData,
      ToData

@Compile
object OldChainState

enum OldAction derives FromData, ToData:
    case UpdateOracle(
        blockHeaders: List[BlockHeader],
        currentTime: BigInt,
        mpfInsertProofs: List[List[ProofStep]]
    )

@Compile
object OldAction

@Compile
object OldBitcoinValidator extends DataParameterizedValidator {
    import BitcoinHelpers.*

    def findBranch(
        forksTree: List[OldForkBranch],
        blockHash: BlockHash
    ): Option[OldForkBranch] = {
        def search(remaining: List[OldForkBranch]): Option[OldForkBranch] = {
            remaining match
                case List.Nil => Option.None
                case List.Cons(branch, tail) =>
                    if branch.tipHash == blockHash then Option.Some(branch)
                    else search(tail)
        }
        search(forksTree)
    }

    def existsHash(blocks: List[OldBlockSummary], hash: BlockHash): Boolean = {
        def check(remaining: List[OldBlockSummary]): Boolean = {
            remaining match
                case List.Nil => false
                case List.Cons(block, tail) =>
                    if block.hash == hash then true
                    else check(tail)
        }
        check(blocks)
    }

    def lookupInRecentBlocks(
        blocks: List[OldBlockSummary],
        hash: BlockHash
    ): Option[OldBlockSummary] = {
        def search(remaining: List[OldBlockSummary]): Option[OldBlockSummary] = {
            remaining match
                case List.Nil => Option.None
                case List.Cons(block, tail) =>
                    if block.hash == hash then Option.Some(block)
                    else search(tail)
        }
        search(blocks)
    }

    def lookupParentBlock(
        forksTree: List[OldForkBranch],
        blockHash: BlockHash
    ): Option[(OldForkBranch, OldBlockSummary, Boolean)] = {
        def search(
            remaining: List[OldForkBranch]
        ): Option[(OldForkBranch, OldBlockSummary, Boolean)] = {
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
        blocks: List[OldBlockSummary],
        height: BigInt
    ): Option[OldBlockSummary] = {
        def search(remaining: List[OldBlockSummary]): Option[OldBlockSummary] = {
            remaining match
                case List.Nil => Option.None
                case List.Cons(block, tail) =>
                    if block.height == height then Option.Some(block)
                    else search(tail)
        }

        search(blocks)
    }

    def expectedNextBitsForParent(
        confirmedState: OldChainState,
        parentBranch: OldForkBranch,
        parentBlock: OldBlockSummary
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

    def extendBranch(
        branch: OldForkBranch,
        newBlock: OldBlockSummary
    ): OldForkBranch = {
        val newRecentBlocks = List.Cons(newBlock, branch.recentBlocks)
        OldForkBranch(
          tipHash = newBlock.hash,
          tipHeight = newBlock.height,
          tipChainwork = newBlock.chainwork,
          recentBlocks = newRecentBlocks
        )
    }

    def removeBranch(
        forksTree: List[OldForkBranch],
        branchToRemove: OldForkBranch
    ): List[OldForkBranch] = {
        def remove(
            remaining: List[OldForkBranch],
            accumulated: List[OldForkBranch]
        ): List[OldForkBranch] = {
            remaining match
                case List.Nil => accumulated.reverse
                case List.Cons(branch, tail) =>
                    if branch.tipHash == branchToRemove.tipHash then accumulated.reverse ++ tail
                    else remove(tail, List.Cons(branch, accumulated))
        }
        remove(forksTree, List.Nil)
    }

    def updateBranch(
        forksTree: List[OldForkBranch],
        oldBranch: OldForkBranch,
        newBranch: OldForkBranch
    ): List[OldForkBranch] = {
        List.Cons(newBranch, removeBranch(forksTree, oldBranch))
    }

    val MaturationConfirmations: BigInt = 100
    val TimeToleranceSeconds: BigInt = 1 * 60 * 60
    val ChallengeAging: BigInt = 200 * 60
    val StaleCompetingForkAge: BigInt = 400 * 60
    val ChainworkGapThreshold: BigInt = 10
    val MaxForksTreeSize: BigInt = 180

    def getMedianTimePastFromBlocks(
        forkBlocks: List[OldBlockSummary],
        confirmedTimestamps: List[BigInt]
    ): BigInt = {
        @tailrec
        def collect(
            remaining: List[OldBlockSummary],
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
                        confirmed match
                            case List.Nil => sorted
                            case List.Cons(ts, tail) =>
                                collect(List.Nil, tail, insertReverseSorted(ts, sorted), count + 1)

        val sortedTimestamps = collect(forkBlocks, confirmedTimestamps, List.Nil, 0)
        getMedianTimePast(sortedTimestamps, sortedTimestamps.size)
    }

    def selectCanonicalChain(
        forksTree: List[OldForkBranch]
    ): Option[OldForkBranch] =
        def findMaxChainworkBranch(
            branches: List[OldForkBranch],
            currentBest: Option[OldForkBranch]
        ): Option[OldForkBranch] =
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

    def addBlockToForksTree(
        forksTree: List[OldForkBranch],
        blockHeader: BlockHeader,
        confirmedState: OldChainState,
        currentTime: BigInt
    ): List[OldForkBranch] =
        val hash = blockHeaderHash(blockHeader)
        val hashInt = byteStringToInteger(false, hash)
        val prevHash = blockHeader.prevBlockHash

        val parentLookupOpt = lookupParentBlock(forksTree, prevHash)
        val parentIsConfirmedTip = prevHash == confirmedState.blockHash
        require(
          parentLookupOpt.isDefined || parentIsConfirmedTip,
          "Parent block not found"
        )

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
                  ),
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

        val target = compactBitsToTarget(blockHeader.bits)
        require(hashInt <= target, "Invalid proof-of-work")
        require(target <= PowLimit, "Target exceeds PowLimit")

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

        val blockTime = blockHeader.timestamp
        val medianTimePast =
            if parentIsConfirmedTip then
                getMedianTimePast(
                  confirmedState.recentTimestamps,
                  confirmedState.recentTimestamps.size
                )
            else
                parentBranchOpt match
                    case Option.Some(branch) =>
                        getMedianTimePastFromBlocks(
                          branch.recentBlocks,
                          confirmedState.recentTimestamps
                        )
                    case Option.None => fail("Parent branch not found")

        require(blockTime > medianTimePast, "Block timestamp not greater than median time past")
        require(blockTime <= currentTime + MaxFutureBlockTime, "Block timestamp too far in future")

        require(blockHeader.version >= 4, "Outdated block version")

        val blockWork = calculateBlockProof(target)
        val newChainwork = parentChainwork + blockWork

        val newBlock = OldBlockSummary(
          hash = hash,
          height = blockHeight,
          chainwork = newChainwork,
          timestamp = blockTime,
          bits = blockHeader.bits,
          addedTime = currentTime
        )

        if parentIsConfirmedTip then
            val newBranch = OldForkBranch(
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
                        val extendedBranch = extendBranch(parentBranch, newBlock)
                        updateBranch(forksTree, parentBranch, extendedBranch)
                    else
                        val newBranch = OldForkBranch(
                          tipHash = hash,
                          tipHeight = blockHeight,
                          tipChainwork = newChainwork,
                          recentBlocks = List.single(newBlock)
                        )
                        List.Cons(newBranch, forksTree)
                case Option.None =>
                    fail("Parent branch not found")

    def promoteQualifiedBlocks(
        forksTree: List[OldForkBranch],
        confirmedTip: BlockHash,
        confirmedHeight: BigInt,
        currentTime: BigInt
    ): (List[OldBlockSummary], List[OldForkBranch]) =
        selectCanonicalChain(forksTree) match
            case Option.None =>
                (List.Nil, forksTree)
            case Option.Some(canonicalBranch) =>
                val canonicalTipHeight = canonicalBranch.tipHeight

                def splitPromotable(
                    blocks: List[OldBlockSummary],
                    remainingReverse: List[OldBlockSummary]
                ): (List[OldBlockSummary], List[OldBlockSummary]) =
                    blocks match
                        case List.Nil => (remainingReverse, List.Nil)
                        case List.Cons(block, tail) =>
                            val depth = canonicalTipHeight - block.height
                            val age = currentTime - block.addedTime

                            if depth >= MaturationConfirmations && age >= ChallengeAging then
                                def reverseAndPrepend(
                                    bs: List[OldBlockSummary],
                                    acc: List[OldBlockSummary]
                                ): List[OldBlockSummary] =
                                    bs match
                                        case List.Nil => acc
                                        case List.Cons(b, tailBs) =>
                                            reverseAndPrepend(tailBs, List.Cons(b, acc))
                                val promotedBlocks =
                                    reverseAndPrepend(tail, List.Cons(block, List.Nil))
                                (remainingReverse, promotedBlocks)
                            else splitPromotable(tail, List.Cons(block, remainingReverse))

                val (remainingReverse, promotedBlocks) =
                    splitPromotable(canonicalBranch.recentBlocks, List.Nil)

                val updatedTree =
                    if promotedBlocks.isEmpty then forksTree
                    else if remainingReverse.isEmpty then removeBranch(forksTree, canonicalBranch)
                    else
                        val updatedBranch = OldForkBranch(
                          tipHash = canonicalBranch.tipHash,
                          tipHeight = canonicalBranch.tipHeight,
                          tipChainwork = canonicalBranch.tipChainwork,
                          recentBlocks = remainingReverse.reverse
                        )
                        updateBranch(forksTree, canonicalBranch, updatedBranch)

                (promotedBlocks, updatedTree)

    def validateForkSubmission(
        blockHeaders: List[BlockHeader],
        forksTree: List[OldForkBranch],
        confirmedTip: BlockHash
    ): Unit = {
        val canonicalTipHash = selectCanonicalChain(forksTree) match
            case Option.Some(branch) => branch.tipHash
            case Option.None         => confirmedTip

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
                        classifyBlocks(tail, canonicalExts + 1, forkBlocks)
                    else classifyBlocks(tail, canonicalExts, forkBlocks + 1)
        }

        val (canonicalCount, forkCount) = classifyBlocks(blockHeaders, BigInt(0), BigInt(0))

        if forkCount > BigInt(0) && canonicalCount == BigInt(0) then
            fail("Fork submission must include canonical tip extension")
    }

    def garbageCollect(
        forksTree: List[OldForkBranch],
        confirmedTip: BlockHash,
        confirmedHeight: BigInt,
        currentTime: BigInt
    ): List[OldForkBranch] = {
        val currentSize = forksTree.size

        if currentSize <= MaxForksTreeSize then forksTree
        else
            selectCanonicalChain(forksTree) match
                case Option.None =>
                    forksTree
                case Option.Some(canonicalBranch) =>
                    val canonicalTipHeight = canonicalBranch.tipHeight
                    val canonicalTipChainwork = canonicalBranch.tipChainwork

                    def isRemovable(branch: OldForkBranch): Boolean = {
                        if branch.tipHash == canonicalBranch.tipHash then false
                        else
                            val heightGap = canonicalTipHeight - branch.tipHeight
                            val oldestBlockTime = branch.recentBlocks.lastOption
                                .map(_.addedTime)
                                .getOrElse(currentTime)
                            val age = currentTime - oldestBlockTime
                            val chainworkGap = canonicalTipChainwork - branch.tipChainwork

                            val isOldDeadFork =
                                heightGap >= MaturationConfirmations &&
                                    age >= ChallengeAging

                            val isStaleCompetingFork =
                                age >= StaleCompetingForkAge &&
                                    chainworkGap >= (ChainworkGapThreshold * (PowLimit / compactBitsToTarget(
                                      hex"1d00ffff"
                                    )))

                            val isLongCompetingFork =
                                age >= ChallengeAging &&
                                    branch.tipHeight >= (confirmedHeight + MaturationConfirmations) &&
                                    chainworkGap > BigInt(0)

                            isOldDeadFork || isStaleCompetingFork || isLongCompetingFork
                    }

                    def performRemoval(
                        branches: List[OldForkBranch],
                        targetSize: BigInt
                    ): List[OldForkBranch] = {
                        def filterBranches(
                            remaining: List[OldForkBranch],
                            accumulated: List[OldForkBranch]
                        ): List[OldForkBranch] = {
                            remaining match
                                case List.Nil => accumulated.reverse
                                case List.Cons(branch, tail) =>
                                    if isRemovable(branch) then filterBranches(tail, accumulated)
                                    else filterBranches(tail, List.Cons(branch, accumulated))
                        }

                        val filtered = filterBranches(branches, List.Nil)

                        if filtered.size > targetSize then
                            def selectTopN(
                                remaining: List[OldForkBranch],
                                selected: List[OldForkBranch],
                                count: BigInt
                            ): List[OldForkBranch] = {
                                if count == BigInt(0) || remaining.isEmpty then selected
                                else
                                    def findMax(
                                        lst: List[OldForkBranch],
                                        currentMax: OldForkBranch
                                    ): OldForkBranch = {
                                        lst match
                                            case List.Nil => currentMax
                                            case List.Cons(b, tail) =>
                                                if b.tipChainwork > currentMax.tipChainwork then
                                                    findMax(tail, b)
                                                else findMax(tail, currentMax)
                                    }

                                    val maxBranch = findMax(remaining.tail, remaining.head)

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
        prevState: OldChainState,
        blockHeader: BlockHeader,
        currentTime: BigInt
    ): OldChainState =
        val hash = blockHeaderHash(blockHeader)
        val hashBigInt = byteStringToInteger(false, hash)
        val blockTime = blockHeader.timestamp

        val validPrevBlockHash = blockHeader.prevBlockHash == prevState.blockHash

        val compactTarget = blockHeader.bits
        val target = compactBitsToTarget(compactTarget)
        val validProofOfWork = hashBigInt <= target

        val nextDifficulty = getNextWorkRequired(
          prevState.blockHeight,
          prevState.currentTarget,
          prevState.blockTimestamp,
          prevState.previousDifficultyAdjustmentTimestamp
        )
        val validDifficulty = compactTarget == nextDifficulty

        val numTimestamps = prevState.recentTimestamps.size
        val medianTimePast = getMedianTimePast(prevState.recentTimestamps, numTimestamps)
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

        val withNewTimestamp = insertReverseSorted(blockTime, prevState.recentTimestamps)
        val newTimestamps = withNewTimestamp.take(MedianTimeSpan)

        val validVersion = blockHeader.version >= 4
        require(validVersion, "Block version is outdated")

        val validBlockHeader = validPrevBlockHash.?
            && validProofOfWork.?
            && validDifficulty.?
            && validVersion.?

        require(validBlockHeader, "Block header is not valid")
        OldChainState(
          blockHeight = prevState.blockHeight + 1,
          blockHash = hash,
          currentTarget = nextDifficulty,
          blockTimestamp = blockTime,
          recentTimestamps = newTimestamps,
          previousDifficultyAdjustmentTimestamp = newDifficultyAdjustmentTimestamp,
          confirmedBlocksRoot = prevState.confirmedBlocksRoot,
          forksTree = prevState.forksTree
        )

    def verifyNewTip(
        intervalStartInSeconds: BigInt,
        prevState: OldChainState,
        newState: Data,
        blockHeader: BlockHeader
    ): Unit =
        val expectedNewState = updateTip(prevState, blockHeader, intervalStartInSeconds)
        val validNewState = newState == expectedNewState.toData

        require(validNewState, "New state does not match expected state")

    def applyPromotionsToConfirmedState(
        confirmedState: OldChainState,
        promotedBlocks: List[OldBlockSummary]
    ): OldChainState = {
        promotedBlocks.foldLeft(confirmedState) { (state, block) =>
            val withNewTimestamp = insertReverseSorted(block.timestamp, state.recentTimestamps)
            val newTimestamps = withNewTimestamp.take(MedianTimeSpan)
            val newDifficultyAdjustmentTimestamp =
                if block.height % DifficultyAdjustmentInterval == BigInt(0) then block.timestamp
                else state.previousDifficultyAdjustmentTimestamp

            OldChainState(
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

    def computeUpdateOracleState(
        prevState: OldChainState,
        blockHeaders: List[BlockHeader],
        currentTime: BigInt,
        mpfInsertProofs: List[List[ProofStep]]
    ): OldChainState = {
        validateForkSubmission(blockHeaders, prevState.forksTree, prevState.blockHash)

        require(!blockHeaders.isEmpty, "Empty block headers list")

        def processHeaders(
            headers: List[BlockHeader],
            currentForksTree: List[OldForkBranch]
        ): List[OldForkBranch] = {
            headers match
                case List.Nil =>
                    currentForksTree
                case List.Cons(header, tail) =>
                    val updatedTree = addBlockToForksTree(
                      currentForksTree,
                      header,
                      prevState,
                      currentTime
                    )
                    processHeaders(tail, updatedTree)
        }

        val forksTreeAfterAddition = processHeaders(blockHeaders, prevState.forksTree)

        val canonicalBranchOpt = selectCanonicalChain(forksTreeAfterAddition)
        val canonicalTipHash = canonicalBranchOpt match
            case Option.Some(branch) => branch.tipHash
            case Option.None         => prevState.blockHash

        val (promotedBlocks, forksTreeAfterPromotion) = promoteQualifiedBlocks(
          forksTreeAfterAddition,
          prevState.blockHash,
          prevState.blockHeight,
          currentTime
        )

        val finalForksTree =
            if forksTreeAfterPromotion.size > MaxForksTreeSize then
                garbageCollect(
                  forksTreeAfterPromotion,
                  prevState.blockHash,
                  prevState.blockHeight,
                  currentTime
                )
            else forksTreeAfterPromotion

        val finalTreeSize = finalForksTree.size

        if promotedBlocks.isEmpty then
            OldChainState(
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
            val latestPromotedBlock = promotedBlocks.last

            def applyMpfInserts(
                root: ByteString,
                blocks: List[OldBlockSummary],
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

            OldChainState(
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
        action: OldAction,
        inputs: List[TxInInfo],
        outputs: List[TxOut],
        validRange: Interval
    ): Unit = {
        val intervalStartInSeconds = validRange.from.boundType match
            case IntervalBoundType.Finite(time) => time / 1000
            case _                              => fail("Must have finite interval start")

        val ownInput = inputs
            .find {
                _.outRef === outRef
            }
            .getOrFail("Input not found")
            .resolved
        val prevState =
            ownInput.datum match
                case OutputDatum.OutputDatum(datum) =>
                    datum.to[OldChainState]
                case _ => fail("No datum")
        action match
            case OldAction.UpdateOracle(blockHeaders, redeemerTime, mpfInsertProofs) =>

                val timeDiff =
                    if redeemerTime > intervalStartInSeconds then
                        redeemerTime - intervalStartInSeconds
                    else intervalStartInSeconds - redeemerTime

                require(
                  timeDiff <= TimeToleranceSeconds,
                  "Redeemer time too far from validity interval"
                )

                val computedState =
                    computeUpdateOracleState(prevState, blockHeaders, redeemerTime, mpfInsertProofs)

                val continuingOutput = findUniqueOutputFrom(outputs, ownInput.address)

                require(
                  ownInput.value.withoutLovelace === continuingOutput.value.withoutLovelace,
                  "Non-ADA tokens must be preserved"
                )

                val providedOutputDatum = continuingOutput.datum match
                    case OutputDatum.OutputDatum(datum) => datum
                    case _ => fail("Continuing output must have inline datum")

                val computedStateDatum = computedState.toData

                require(
                  computedStateDatum == providedOutputDatum,
                  "Computed state does not match provided output datum"
                )
    }

    import OldStrictLookups.*
    inline override def mint(
        oneShotTxOutRef: Data,
        redeemer: Data,
        policyId: PolicyId,
        tx: TxInfo
    ): Unit = {
        val minted = tx.mint.toSortedMap.lookupOrFail(policyId).toData
        if minted == SortedMap.singleton(ByteString.empty, BigInt(1)).toData then
            tx.inputs.findOrFail(_.outRef.toData == oneShotTxOutRef)
            val outputIndex = redeemer.to[BigInt]
            val oracleOutput = tx.outputs !! outputIndex
            require(
              oracleOutput.value.existingQuantityOf(policyId, ByteString.empty) == BigInt(1),
              "Oracle output must contain NFT"
            )
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

    inline override def spend(
        param: Data,
        datum: Option[Datum],
        redeemer: Datum,
        tx: TxInfo,
        outRef: TxOutRef
    ): Unit = {
        val action = redeemer.to[OldAction]

        val inputs = tx.inputs
        val outputs = tx.outputs
        val validRange = tx.validRange

        update(outRef, action, inputs, outputs, validRange)
    }

    def reverse(bs: ByteString): ByteString =
        val len = lengthOfByteString(bs)
        def loop(idx: BigInt, acc: ByteString): ByteString =
            if idx == len then acc
            else loop(idx + 1, consByteString(bs.at(idx), acc))
        loop(0, ByteString.empty)
}

@Compile
object OldStrictLookups {

    extension [A](self: List[A]) {
        @tailrec
        def findOrFail(predicate: A => Boolean): A = self match
            case List.Nil => fail("element not found")
            case List.Cons(head, tail) =>
                if predicate(head) then head else tail.findOrFail(predicate)

        def oneOrFail(message: String): A = self match
            case List.Cons(head, List.Nil) => head
            case _                         => fail(message)

    }

    extension [V](self: Value) {
        def existingQuantityOf(policyId: PolicyId, tokenName: TokenName): BigInt = {
            self.toSortedMap.lookupOrFail(policyId).lookupOrFail(tokenName)
        }
    }
    extension [V](self: SortedMap[ByteString, V]) {
        def lookupOrFail(key: ByteString): V = {
            @tailrec
            def go(lst: PairList[ByteString, V]): V = lst match
                case PairNil => fail("key not found")
                case PairCons((k, v), tail) =>
                    if key == k then v
                    else if key < k then fail("key not found")
                    else go(tail)

            go(self.toPairList)
        }
    }
}
