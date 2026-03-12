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

case class BlockSummary2(
    hash: BlockHash, // Block hash
    timestamp: PosixTime, // Bitcoin block timestamp (for median-time-past)
    addedTimeSeconds: PosixTimeSeconds // Cardano time when this block was added to forksTree
) derives FromData,
      ToData

enum ForkTree derives FromData, ToData {
    case Blocks(blocks: List[BlockSummary2], chainwork: BigInt, next: ForkTree)
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
    timestamps: List[BigInt], // newest-first (prepended during accumulation)
    height: BigInt,
    currentBits: CompactBits,
    prevDiffAdjTimestamp: BigInt,
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

    /** Initialize traversal context from confirmed state. */
    def initCtx(state: ChainState2): TraversalCtx =
        TraversalCtx(
          timestamps = state.recentTimestamps,
          height = state.blockHeight,
          currentBits = state.currentTarget,
          prevDiffAdjTimestamp = state.previousDifficultyAdjustmentTimestamp,
          lastBlockHash = state.blockHash
        )

    /** Accumulate one existing block into the traversal context. */
    def accumulateBlock(ctx: TraversalCtx, block: BlockSummary2): TraversalCtx = {
        val newHeight = ctx.height + 1
        val newBits = getNextWorkRequired(
          ctx.height,
          ctx.currentBits,
          ctx.timestamps.head,
          ctx.prevDiffAdjTimestamp
        )
        val newTimestamps = Cons(block.timestamp, ctx.timestamps)
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

    /** Accumulate all blocks in the list. */
    def accumulateCtx(ctx: TraversalCtx, blocks: List[BlockSummary2]): TraversalCtx = {
        blocks.foldLeft(ctx)(accumulateBlock)
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
        currentTime: BigInt
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

    /** Compute segment chainwork for a list of blocks by replaying difficulty from TraversalCtx. */
    def computeChainwork(
        blocks: List[BlockSummary2],
        ctx: TraversalCtx,
        chainwork: BigInt
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
                // Parent is the confirmed tip. Validate and attach as a new branch.
                val (newBlocks, newCw) =
                    validateAndCollectBlocks(headers, ctx, currentTime)
                val newBranch = Blocks(newBlocks, newCw, End)
                tree match
                    case End      => newBranch
                    case existing => Fork(existing, newBranch)
            case Cons(pathHead, pathTail) =>
                tree match
                    case Blocks(blocks, originalCw, next) =>
                        def loop(
                            count: BigInt,
                            remaining: List[BlockSummary2],
                            prefix: List[BlockSummary2],
                            newCtx: TraversalCtx
                        ): ForkTree = {
                            remaining match
                                case Nil if count == pathHead =>
                                    // All blocks consumed, pass through to `next`.
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
                                    // Found insertion point: block at pathHead is the parent.
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
                                                    // Parent is the last block — just append.
                                                    Blocks(
                                                      fullPrefix ++ newBlocks,
                                                      originalCw + newCw,
                                                      End
                                                    )
                                                case _ =>
                                                    // Parent is the last block with subtree — fork.
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
                                            // Parent is in the middle — split.
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
                                    // Not at pathHead yet — accumulate and advance.
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

    /** Find the best (highest chainwork) chain path through the tree. Returns (chainwork, depth,
      * path-to-best). Single full traversal. Reads segment chainwork directly from Blocks nodes.
      */
    def bestChainPath(tree: ForkTree, height: BigInt, chainwork: BigInt): (BigInt, BigInt, Path) = {
        tree match
            case Blocks(blocks, cw, next) =>
                bestChainPath(next, height + blocks.length, chainwork + cw)

            case Fork(left, right) =>
                val (leftWork, leftDepth, leftPath) = bestChainPath(left, height, chainwork)
                val (rightWork, rightDepth, rightPath) = bestChainPath(right, height, chainwork)
                if leftWork >= rightWork then (leftWork, leftDepth, Cons(0, leftPath))
                else (rightWork, rightDepth, Cons(1, rightPath))

            case End =>
                (chainwork, height, Nil)
    }

    // ============================================================================
    // Promotion and GC
    // ============================================================================

    /** Split promotable blocks from a Blocks node. Walks oldest→newest. A block is eligible if:
      * bestDepth - blockHeight >= MaturationConfirmations AND currentTime - addedTimeSeconds >=
      * ChallengeAging
      *
      * Returns (promoted oldest-first, remaining oldest-first).
      */
    def splitPromotable(
        blocks: List[BlockSummary2],
        ctx: TraversalCtx,
        bestDepth: BigInt,
        currentTime: PosixTimeSeconds
    ): (List[BlockSummary2], List[BlockSummary2], TraversalCtx) = {
        blocks match
            case Nil => (Nil, Nil, ctx)
            case Cons(block, tail) =>
                val blockHeight = ctx.height + 1
                val depth = bestDepth - blockHeight
                val age = currentTime - block.addedTimeSeconds
                if depth >= MaturationConfirmations && age >= ChallengeAging then
                    val newCtx = accumulateBlock(ctx, block)
                    val (morePromoted, remaining, finalCtx) =
                        splitPromotable(tail, newCtx, bestDepth, currentTime)
                    (Cons(block, morePromoted), remaining, finalCtx)
                else
                    // This block not eligible, so neither are any following (they are newer/shallower)
                    (Nil, blocks, ctx)
    }

    /** Promote eligible blocks and GC dead forks along the best path. Returns (promoted blocks
      * oldest-first, cleaned tree).
      */
    def promoteAndGC(
        tree: ForkTree,
        ctx: TraversalCtx,
        bestPath: Path,
        bestDepth: BigInt,
        currentTime: BigInt
    ): (List[BlockSummary2], ForkTree) = {
        tree match
            case Blocks(blocks, cw, next) =>
                val (promoted, remaining, newCtx) =
                    splitPromotable(blocks, ctx, bestDepth, currentTime)
                if promoted.isEmpty then
                    // No promotion here — accumulate and recurse into next for GC
                    val fullCtx = accumulateCtx(ctx, blocks)
                    val (nextPromoted, cleanedNext) =
                        promoteAndGC(next, fullCtx, bestPath, bestDepth, currentTime)
                    (nextPromoted, Blocks(blocks, cw, cleanedNext))
                else if remaining.isEmpty then
                    // All blocks promoted — recurse into next for more promotion
                    val (nextPromoted, cleanedNext) =
                        promoteAndGC(next, newCtx, bestPath, bestDepth, currentTime)
                    (promoted ++ nextPromoted, cleanedNext)
                else
                    // Partial promotion — derive remaining chainwork by subtraction
                    val promotedCw = computeChainwork(promoted, ctx, BigInt(0))
                    (promoted, Blocks(remaining, cw - promotedCw, next))

            case Fork(left, right) =>
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

            case End =>
                (Nil, End)
    }

    // ============================================================================
    // Apply promotions to confirmed state
    // ============================================================================

    /** Apply promoted blocks to the confirmed state, updating MPF root. */
    def applyPromotions(
        state: ChainState2,
        promoted: List[BlockSummary2],
        mpfProofs: List[List[ProofStep]],
        ctx0: TraversalCtx
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
          forksTree = state.forksTree // placeholder, will be overwritten by caller
        )
    }

    // ============================================================================
    // Main compute function
    // ============================================================================

    /** Compute the new ChainState2 after applying an update. */
    def computeUpdate(
        state: ChainState2,
        update: UpdateOracle2,
        currentTime: BigInt
    ): ChainState2 = {
        val headers = update.blockHeaders
        val ctx0 = initCtx(state)

        // Step 1: Validate and insert new blocks into tree
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

        // Step 2: Find best chain by chainwork (single full traversal)
        val (_, bestDepth, bestPath) = bestChainPath(newTree, state.blockHeight, BigInt(0))
        log("bestChainPath")

        // Step 3: Promote eligible blocks + GC dead forks (single traversal along bestPath)
//        val cleanedTree = newTree
        val (promoted, cleanedTree) = promoteAndGC(newTree, ctx0, bestPath, bestDepth, currentTime)
        log("promoteAndGC")

        // Step 4: Apply promotions (no-op when promoted is empty) and set cleaned tree
        val updatedState = applyPromotions(state, promoted, update.mpfInsertProofs, ctx0)
        log("applyPromotions")
        updatedState.copy(forksTree = cleanedTree)
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

        require(
          computedState.toData == providedOutputDatum,
          "Computed state does not match provided output datum"
        )
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
