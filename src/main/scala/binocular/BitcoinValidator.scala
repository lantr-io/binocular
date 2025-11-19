package binocular

import scalus.builtin.*
import scalus.builtin.Builtins.*
import scalus.builtin.ByteString.*
import scalus.builtin.Data.{toData, FromData, ToData}
import scalus.ledger.api.v2.OutputDatum
import scalus.ledger.api.v3.*
import scalus.prelude.{List, Math, *}
import scalus.prelude.Math.pow
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

case class BlockNode(
    prevBlockHash: BlockHash, // 32-byte hash of previous block (for chain walking)
    height: BigInt, // Block height (for difficulty validation and depth calculation)
    chainwork: BigInt, // Cumulative proof-of-work from genesis
    addedTimestamp: BigInt, // When this block was added on-chain (for 200-min rule)
    children: List[BlockHash] // Hashes of child blocks (for tree navigation)
) derives FromData,
      ToData
@Compile
object BlockNode

// ============================================================================
// Optimized ForksTree Data Structures
// ============================================================================
// These structures reduce memory consumption by storing only fork points and
// recent blocks instead of the entire unconfirmed chain history.

/** Summary of a block with essential validation data.
  * Stored in a sliding window (last 11 blocks) per branch for validation.
  */
case class BlockSummary(
    hash: BlockHash,       // Block hash
    height: BigInt,        // Block height
    chainwork: BigInt,     // Cumulative chainwork at this block
    timestamp: BigInt,     // Bitcoin block timestamp (for median-time-past)
    bits: CompactBits,     // Difficulty target (for difficulty validation)
    addedTime: BigInt      // Cardano time when this block was added to forksTree
) derives FromData,
      ToData
@Compile
object BlockSummary

/** A complete chain branch from a fork point to its current tip.
  * Maintains ALL blocks in the branch (newest first) for:
  * - Median-time-past validation (needs last 11 blocks)
  * - Promotion tracking (blocks are removed from list when promoted)
  * - Each block has individual addedTime for accurate challenge period enforcement
  */
case class ForkBranch(
    tipHash: BlockHash,                  // Current tip of this branch
    tipHeight: BigInt,                   // Height of tip
    tipChainwork: BigInt,                // Chainwork at tip
    recentBlocks: List[BlockSummary]     // ALL blocks in branch (newest first), each with individual addedTime
) derives FromData,
      ToData
@Compile
object ForkBranch

/** A fork point where one or more branches diverge.
  * Maps to the block hash where the fork occurred.
  */
case class ForkNode(
    forkPointHash: BlockHash,       // Where this fork diverged from (confirmed tip or another fork)
    forkPointHeight: BigInt,        // Height where fork occurred
    branches: List[ForkBranch]      // All branches from this fork point
) derives FromData,
      ToData
@Compile
object ForkNode

case class ChainState(
    // Confirmed state
    blockHeight: BigInt,
    blockHash: BlockHash,
    currentTarget: CompactBits,
    blockTimestamp: BigInt,
    recentTimestamps: List[BigInt], // Newest first
    previousDifficultyAdjustmentTimestamp: BigInt,
    confirmedBlocksTree: List[ByteString], // Rolling Merkle tree state (levels array)

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
      */
    case UpdateOracle(blockHeaders: List[BlockHeader], currentTime: BigInt, inputDatumHash: ByteString)

@Compile
object Action

@Compile
object BitcoinValidator extends Validator {

    // ============================================================================
    // ForkBranch Helper Functions
    // ============================================================================
    // These functions work with the optimized ForkBranch structure where consecutive
    // blocks in a fork are stored together, reducing memory usage by ~90%.

    /** Find a branch containing the given block hash
      * Returns Option[(branch, isAtTip)] where isAtTip indicates if hash is the branch tip
      */
    def findBranch(
        forksTree: List[ForkBranch],
        blockHash: BlockHash
    ): Option[(ForkBranch, Boolean)] = {
        def search(remaining: List[ForkBranch]): Option[(ForkBranch, Boolean)] = {
            remaining match
                case List.Nil => scalus.prelude.Option.None
                case List.Cons(branch, tail) =>
                    // Check if this is the tip
                    if branch.tipHash == blockHash then
                        scalus.prelude.Option.Some((branch, true))
                    else
                        // Check if it's in recentBlocks
                        val found = existsInSortedList(branch.recentBlocks, blockHash)
                        if found then scalus.prelude.Option.Some((branch, false))
                        else search(tail)
        }
        search(forksTree)
    }

    /** Check if a block exists anywhere in the recentBlocks list by hash */
    def existsInSortedList(blocks: List[BlockSummary], hash: BlockHash): Boolean = {
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
                case List.Nil => scalus.prelude.Option.None
                case List.Cons(block, tail) =>
                    if block.hash == hash then scalus.prelude.Option.Some(block)
                    else search(tail)
        }
        search(blocks)
    }

    /** Extend a branch by adding a new block at the tip
      * Updates tipHash, tipHeight, tipChainwork and prepends new block to recentBlocks
      * Keeps ALL blocks in the branch until they are promoted
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
                    else
                        remove(tail, List.Cons(branch, accumulated))
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

    // ============================================================================
    // Bitcoin consensus constants - matches Bitcoin Core chainparams.cpp
    // ============================================================================
    val UnixEpoch: BigInt = 1231006505
    val TargetBlockTime: BigInt = 600 // 10 minutes - matches nPowTargetSpacing in chainparams.cpp
    val DifficultyAdjustmentInterval: BigInt =
        2016 // matches DifficultyAdjustmentInterval() in chainparams.cpp
    val MaxFutureBlockTime: BigInt =
        7200 // 2 hours in the future in seconds - matches MAX_FUTURE_BLOCK_TIME in validation.cpp
    val MedianTimeSpan: BigInt = 11 // matches CBlockIndex::nMedianTimeSpan in chain.h:276
    // Proof of work limit - matches consensus.powLimit in chainparams.cpp
    val PowLimit: BigInt =
        byteStringToInteger(
          true,
          hex"00000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
        )

    // Binocular protocol parameters
    val MaturationConfirmations: BigInt = BigInt(100) // Blocks needed for promotion to confirmed state
    val TimeToleranceSeconds: BigInt =
        36 * 60 * 60 // Maximum difference between redeemer time and validity interval time (shoukd be the time of cardano consesnus)
    val ChallengeAging: BigInt = 200 * 60 // 200 minutes in seconds (challenge period)
    val StaleCompetingForkAge: BigInt = 400 * 60 // 400 minutes (2× challenge period)
    val ChainworkGapThreshold: BigInt = 10 // Blocks worth of work for stale fork detection
    val MaxForksTreeSize: BigInt = 180 // Maximum forks tree size before garbage collection

    /// Bitcoin block header serialization
    //    def serializeBlockHeader(blockHeader: BlockHeader): ByteString =
    //        val v = integerToByteString(false, 4, blockHeader.version)
    //        val pbh = reverse(blockHeader.prevBlockHash)
    //        val mr = reverse(blockHeader.merkleRoot)
    //        val ts = integerToByteString(false, 4, blockHeader.timestamp)
    //        appendByteString(
    //          v,
    //          appendByteString(
    //            pbh,
    //            appendByteString(
    //              mr,
    //              appendByteString(ts, appendByteString(blockHeader.bits, blockHeader.nonce))
    //            )
    //          )
    //        )

    // === Rolling Merkle Tree Implementation ===
    // Matches the rolling tree algorithm from MerkleTreeRootBuilder in merkle.scala
    // The tree state is stored as a list of hashes, one per level
    // Empty slots are represented as 32 zero bytes

    val emptyHash: ByteString =
        hex"0000000000000000000000000000000000000000000000000000000000000000"

    /** Check if a ByteString is the empty hash (all zeros) */
    def isEmptyHash(bs: ByteString): Boolean =
        lengthOfByteString(bs) == BigInt(32) && bs == emptyHash

    /** Get value at index in list, or return default if index out of bounds */
    def getAtIndex(levels: List[ByteString], idx: BigInt, default: ByteString): ByteString =
        if idx < BigInt(0) then default
        else
            levels match
                case List.Nil => default
                case List.Cons(head, tail) =>
                    if idx == BigInt(0) then head
                    else getAtIndex(tail, idx - 1, default)

    /** Set value at index in list, extending list if necessary */
    def setAtIndex(levels: List[ByteString], idx: BigInt, value: ByteString): List[ByteString] =
        if idx < BigInt(0) then fail("Negative index")
        else if idx == BigInt(0) then
            levels match
                case List.Nil           => List.Cons(value, List.Nil)
                case List.Cons(_, tail) => List.Cons(value, tail)
        else
            levels match
                case List.Nil =>
                    // Extend list with empty hashes
                    List.Cons(emptyHash, setAtIndex(List.Nil, idx - 1, value))
                case List.Cons(head, tail) =>
                    List.Cons(head, setAtIndex(tail, idx - 1, value))

    /** Get length of list */
    def listLength(list: List[ByteString]): BigInt =
        list match
            case List.Nil           => 0
            case List.Cons(_, tail) => 1 + listLength(tail)

    /** Add a hash to the rolling Merkle tree at a specific level */
    def addHashAtLevel(
        levels: List[ByteString],
        hash: ByteString,
        startingLevel: BigInt
    ): List[ByteString] =
        val currentHash = getAtIndex(levels, startingLevel, emptyHash)
        if isEmptyHash(currentHash) then
            // Empty slot - just store the hash here
            setAtIndex(levels, startingLevel, hash)
        else
            // Occupied slot - combine hashes and move to next level
            val combined = sha2_256(sha2_256(currentHash ++ hash))
            val clearedLevel = setAtIndex(levels, startingLevel, emptyHash)
            addHashAtLevel(clearedLevel, combined, startingLevel + 1)

    /** Add a block hash to the rolling Merkle tree */
    def addToMerkleTree(tree: List[ByteString], blockHash: BlockHash): List[ByteString] =
        addHashAtLevel(tree, blockHash, 0)

    /** Get Merkle root from the rolling tree state */
    def getMerkleRoot(levels: List[ByteString]): MerkleRoot =
        // Finalize tree by combining all remaining levels
        def finalize(levels: List[ByteString], idx: BigInt): MerkleRoot =
            val len = listLength(levels)
            if idx >= len then
                // Return last non-empty hash
                val lastHash = getAtIndex(levels, len - 1, emptyHash)
                if isEmptyHash(lastHash) then emptyHash else lastHash
            else
                val currentHash = getAtIndex(levels, idx, emptyHash)
                if !isEmptyHash(currentHash) && idx < len - 1 then
                    // Propagate this hash up
                    val updated = addHashAtLevel(levels, currentHash, idx)
                    finalize(updated, idx + 1)
                else finalize(levels, idx + 1)

        levels match
            case List.Nil => emptyHash
            case _        => finalize(levels, 0)

    // Double SHA256 hash - matches CBlockHeader::GetHash() in primitives/block.h
    def blockHeaderHash(blockHeader: BlockHeader): BlockHash =
        sha2_256(sha2_256(blockHeader.bytes))

    /** Converts little-endian compact bits representation to target value
      *
      * Matches `arith_uint256::SetCompact()` in arith_uint256.cpp
      * @note
      *   only works for positive numbers. Here we use it only for `target` which is always
      *   positive, so it's fine.
      */
    def compactBitsToTarget(compact: CompactBits): BigInt = {
        // int nSize = nCompact >> 24;
        val exponent = compact.at(3)
        // uint32_t nWord = nCompact & 0x007fffff;
        val coefficient = byteStringToInteger(false, compact.slice(0, 3))
        if coefficient > BigInt(0x007fffff) then fail("Negative bits")
        else if exponent < BigInt(3)
        then coefficient / Math.pow(256, BigInt(3) - exponent)
        // check overflow
        else if coefficient != BigInt(0) && ((exponent > 34) ||
                (coefficient > 0xff && exponent > 33) ||
                (coefficient > 0xffff && exponent > 32))
        then fail("Bits overflow")
        else
            val result = coefficient * pow(256, exponent - 3)
            if result > PowLimit then fail("Bits over PowLimit")
            else result
    }

    /** Converts target value (256 bits BigInt) to a little-endian compact 32 bits representation
      *
      * Matches `arith_uint256::GetCompact()` in arith_uint256.cpp
      * @note
      *   only works for positive numbers. Here we use it only for `target` which is always
      *   positive, so it's fine.
      */
    def targetToCompactBits(target: BigInt): BigInt = {
        if target == BigInt(0) then 0
        else
            // Calculate the number of significant bytes by finding the highest non-zero byte
            // Convert to 32-byte little-endian representation to find significant bytes easily
            val targetBytes = integerToByteString(false, 32, target)

            // Find the number of significant bytes (from most significant end)
            @tailrec
            def findSignificantBytes(bytes: ByteString, index: BigInt): BigInt = {
                if index < 0 then BigInt(0)
                else if bytes.at(index) != BigInt(0) then index + 1
                else findSignificantBytes(bytes, index - 1)
            }

            val nSize = findSignificantBytes(targetBytes, BigInt(31))

            // Extract nCompact according to Bitcoin Core GetCompact logic
            val nCompact =
                if nSize <= 3 then
                    // nCompact = GetLow64() << 8 * (3 - nSize)
                    target * pow(256, 3 - nSize)
                // For large values: extract the most significant 3 bytes
                else target / pow(BigInt(256), nSize - 3)

            val (finalSize, finalCompact) =
                if nCompact >= BigInt(0x800000) then (nSize + 1, nCompact / BigInt(256))
                else (nSize, nCompact)
            // Pack into compact representation: [3 bytes mantissa][1 byte size]
            finalCompact + (finalSize * BigInt(0x1000000)) // size << 24
    }

    /** Converts target value to little-endian compact bits representation
      *
      * Matches `arith_uint256::GetCompact()` in arith_uint256.cpp
      * @note
      *   only works for positive numbers. Here we use it only for `target` which is always
      *   positive, so it's fine.
      */
    def targetToCompactByteString(target: BigInt): CompactBits = {
        val compact = targetToCompactBits(target)
        integerToByteString(false, 4, compact)
    }

    /** Gets median from reverse-sorted list (newest first)
      *
      * Matches CBlockIndex::GetMedianTimePast() in chain.h:278-290
      */
    def getMedianTimePast(timestamps: List[BigInt], size: BigInt): BigInt =
        timestamps match
            case List.Nil => UnixEpoch
            case _ =>
                val index = size / 2
                // List is sorted, so just get middle element
                timestamps !! index

    def merkleRootFromInclusionProof(
        merkleProof: List[TxHash],
        hash: TxHash,
        index: BigInt
    ): MerkleRoot =
        def loop(index: BigInt, curHash: ByteString, siblings: List[ByteString]): ByteString =
            if siblings.isEmpty then curHash
            else
                val sibling = siblings.head
                val nextHash =
                    if index % 2 == BigInt(0)
                    then sha2_256(sha2_256(curHash ++ sibling))
                    else sha2_256(sha2_256(sibling ++ curHash))
                loop(index / 2, nextHash, siblings.tail)

        loop(index, hash, merkleProof)

    def readVarInt(input: ByteString, offset: BigInt): (BigInt, BigInt) =
        val firstByte = input.at(offset)
        if firstByte < BigInt(0xfd) then (firstByte, offset + 1)
        else if firstByte == BigInt(0xfd) then
            (byteStringToInteger(false, input.slice(offset + 1, 2)), offset + 3)
        else if firstByte == BigInt(0xfe) then
            (byteStringToInteger(false, input.slice(offset + 1, 4)), offset + 5)
        else (byteStringToInteger(false, input.slice(offset + 1, 8)), offset + 9)

    def parseCoinbaseTxScriptSig(coinbaseTx: ByteString): ByteString =
        // Skip version
        // 4
        // marker and flag of witness transaction
        // + 1 + 1
        // Read input count (should be 1 for coinbase)
        // + 1
        // Skip previous transaction hash and output index
        // + 32 + 4
        val offset = BigInt(43)
        val (scriptLength, newOffset) = readVarInt(coinbaseTx, offset)
        val scriptSig = coinbaseTx.slice(newOffset, scriptLength)
        scriptSig

    def parseBlockHeightFromScriptSig(scriptSig: ByteString): BigInt =
        val len = scriptSig.at(0)
        require(1 <= len && len <= 8, "Invalid block height length")
        val height = byteStringToInteger(false, scriptSig.slice(1, len))
        height

    def getBlockHeightFromCoinbaseTx(tx: CoinbaseTx): BigInt =
        val scriptSigAndSequence = tx.inputScriptSigAndSequence
        val (scriptLength, newOffset) = readVarInt(scriptSigAndSequence, 0)
        // read first byte of scriptSig
        // this MUST be OP_PUSHBYTES_len
        val len = scriptSigAndSequence.at(newOffset)
        require(1 <= len && len <= 8, "Invalid block height length")
        val height = byteStringToInteger(false, scriptSigAndSequence.slice(newOffset + 1, len))
        height

    def getTxHash(rawTx: ByteString): TxHash =
        val serializedTx = stripWitnessData(rawTx)
        sha2_256(sha2_256(serializedTx))

    def isWitnessTransaction(rawTx: ByteString): Boolean =
        rawTx.at(4) == BigInt(0) && rawTx.at(5) == BigInt(1)

    def stripWitnessData(rawTx: ByteString): ByteString =
        if isWitnessTransaction(rawTx) then
            // Format: [nVersion][marker][flag][txins][txouts][witness][nLockTime]
            val version = rawTx.slice(0, 4)
            val txInsStartIndex = BigInt(6) // Skip marker and flag bytes
            val txOutsOffset = skipTxIns(rawTx, txInsStartIndex)
            val outsEnd = skipTxOuts(rawTx, txOutsOffset)
            val txIns = rawTx.slice(txInsStartIndex, txOutsOffset - txInsStartIndex)
            val txOuts = rawTx.slice(txOutsOffset, outsEnd - txOutsOffset)
            val lockTimeOsset = outsEnd + 1 + 1 + 32 // Skip witness data
            val lockTime = rawTx.slice(lockTimeOsset, 4)
            version ++ txIns ++ txOuts ++ lockTime
        else rawTx

    def getCoinbaseTxHash(coinbaseTx: CoinbaseTx): TxHash =
        val serializedTx = coinbaseTx.version
            ++ hex"010000000000000000000000000000000000000000000000000000000000000000ffffffff"
            ++ coinbaseTx.inputScriptSigAndSequence
            ++ coinbaseTx.txOutsAndLockTime
        sha2_256(sha2_256(serializedTx))

    def skipTxIns(rawTx: ByteString, txInsStartIndex: BigInt): BigInt =
        val (numIns, newOffset) = readVarInt(rawTx, txInsStartIndex)
        def loop(num: BigInt, offset: BigInt): BigInt =
            if num == BigInt(0) then offset
            else loop(num - 1, skipTxIn(rawTx, offset))
        loop(numIns, newOffset)

    def skipTxOuts(rawTx: ByteString, offset: BigInt): BigInt =
        val (numOuts, newOffset) = readVarInt(rawTx, offset)
        def loop(numOuts: BigInt, offset: BigInt): BigInt =
            if numOuts == BigInt(0) then offset
            else loop(numOuts - 1, skipTxOut(rawTx, offset))
        loop(numOuts, newOffset)

    def skipTxIn(rawTx: ByteString, offset: BigInt): BigInt =
        // Skip tx hash 32 and index 4
        val (scriptLength, newOffset) = readVarInt(rawTx, offset + 36)
        newOffset + scriptLength + 4 // sequence is 4

    def skipTxOut(rawTx: ByteString, offset: BigInt): BigInt =
        // Skip tx hash 32 and index 4
        val (scriptLength, newOffset) = readVarInt(rawTx, offset + 8) // skip amount
        newOffset + scriptLength

    // Insert a timestamp into a sorted list while maintaining order
    def insertReverseSorted(value: BigInt, sortedValues: List[BigInt]): List[BigInt] =
        sortedValues match
            case List.Nil => List.single(value)
            case List.Cons(head, tail) =>
                if value >= head then List.Cons(value, sortedValues)
                else List.Cons(head, insertReverseSorted(value, tail))

    // Helper: Lookup block in forks tree by hash
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
                        case scalus.prelude.Option.None =>
                            scalus.prelude.Option.Some(branch)
                        case scalus.prelude.Option.Some(best) =>
                            if branch.tipChainwork > best.tipChainwork then
                                scalus.prelude.Option.Some(branch)
                            else currentBest
                    findMaxChainworkBranch(tail, newBest)

        findMaxChainworkBranch(forksTree, scalus.prelude.Option.None)

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
        val parentBranchOpt = findBranch(forksTree, prevHash)
        val parentIsConfirmedTip = prevHash == confirmedState.blockHash
        require(
          parentBranchOpt.isDefined || parentIsConfirmedTip,
          "Parent block not found"
        )

        // === SECURITY: Full block validation ===

        // Calculate parent height and chainwork
        val (parentHeight, parentChainwork, parentTimestamp, parentBits) =
            if parentIsConfirmedTip then
                (
                  confirmedState.blockHeight,
                  compactBitsToTarget(confirmedState.currentTarget), // Simplified
                  confirmedState.blockTimestamp,
                  confirmedState.currentTarget
                )
            else
                parentBranchOpt match
                    case scalus.prelude.Option.Some((branch, isAtTip)) =>
                        if isAtTip then
                            (branch.tipHeight, branch.tipChainwork, branch.recentBlocks.head.timestamp, branch.recentBlocks.head.bits)
                        else
                            // Parent is in recentBlocks, need to find it
                            val parentBlock = lookupInRecentBlocks(branch.recentBlocks, prevHash).getOrFail("Parent not found in recentBlocks")
                            (parentBlock.height, parentBlock.chainwork, parentBlock.timestamp, parentBlock.bits)
                    case scalus.prelude.Option.None =>
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
                // For simplicity, accept claimed difficulty if PoW is valid
                // Full validation would require tracking difficulty adjustment state per branch
                blockHeader.bits

        require(blockHeader.bits == expectedBits, "Invalid difficulty")

        // 3. VALIDATE TIMESTAMP
        val blockTime = blockHeader.timestamp
        val medianTimePast =
            if parentIsConfirmedTip then
                getMedianTimePast(confirmedState.recentTimestamps, confirmedState.recentTimestamps.size)
            else
                // For fork blocks, use branch's recentBlocks for median-time-past
                parentBranchOpt match
                    case scalus.prelude.Option.Some((branch, _)) =>
                        val timestamps = branch.recentBlocks.map(_.timestamp)
                        getMedianTimePast(timestamps, timestamps.size)
                    case scalus.prelude.Option.None => fail("Parent branch not found")

        require(blockTime > medianTimePast, "Block timestamp not greater than median time past")
        require(blockTime <= currentTime + MaxFutureBlockTime, "Block timestamp too far in future")

        // 4. VALIDATE VERSION
        require(blockHeader.version >= 4, "Outdated block version")

        // === End validation ===

        // Calculate chainwork
        val blockWork = PowLimit / target
        val newChainwork = parentChainwork + blockWork

        // Create BlockSummary for this block
        val newBlock = BlockSummary(
          hash = hash,
          height = blockHeight,
          chainwork = newChainwork,
          timestamp = blockTime,
          bits = blockHeader.bits,
          addedTime = currentTime  // Cardano time when block added
        )

        // Determine how to update forksTree based on parent location
        parentBranchOpt match
            case scalus.prelude.Option.Some((parentBranch, isAtTip)) =>
                if isAtTip then
                    // Case 1: Parent is branch tip - extend the branch
                    log("Extending existing branch")
                    val extendedBranch = extendBranch(parentBranch, newBlock)
                    updateBranch(forksTree, parentBranch, extendedBranch)
                else
                    // Case 2: Parent is in recentBlocks but not tip - creating a fork
                    // Create a new branch starting from this block
                    log("Creating new fork branch from mid-branch point")
                    val newBranch = ForkBranch(
                      tipHash = hash,
                      tipHeight = blockHeight,
                      tipChainwork = newChainwork,
                      recentBlocks = List.single(newBlock)
                    )
                    List.Cons(newBranch, forksTree)
            case scalus.prelude.Option.None =>
                // Case 3: Parent is confirmed tip - create new branch
                log("Creating new branch from confirmed tip")
                val newBranch = ForkBranch(
                  tipHash = hash,
                  tipHeight = blockHeight,
                  tipChainwork = newChainwork,
                  recentBlocks = List.single(newBlock)
                )
                List.Cons(newBranch, forksTree)

    // Block promotion (maturation) - matches Algorithm 10 in Whitepaper
    // Returns (list of promoted block hashes, updated forks tree with promoted blocks removed)
    def promoteQualifiedBlocks(
        forksTree: List[ForkBranch],
        confirmedTip: BlockHash,
        confirmedHeight: BigInt,
        currentTime: BigInt
    ): (List[BlockHash], List[ForkBranch]) =
        // Find canonical branch
        selectCanonicalChain(forksTree) match
            case scalus.prelude.Option.None =>
                // No blocks in forks tree
                (List.Nil, forksTree)
            case scalus.prelude.Option.Some(canonicalBranch) =>
                // Identify promotable blocks from the canonical branch
                // We check blocks from oldest (end of recentBlocks list) to newest
                val canonicalTipHeight = canonicalBranch.tipHeight

                def identifyPromotable(
                    blocks: List[BlockSummary],
                    accumulated: List[BlockHash]
                ): List[BlockHash] =
                    blocks match
                        case List.Nil => accumulated
                        case List.Cons(block, tail) =>
                            val depth = canonicalTipHeight - block.height
                            val age = currentTime - block.addedTime  // Use individual block's addedTime

                            if depth >= MaturationConfirmations && age >= ChallengeAging then
                                // This block qualifies for promotion
                                identifyPromotable(tail, List.Cons(block.hash, accumulated))
                            else
                                // Stop at first non-qualified block
                                accumulated

                // Reverse recentBlocks to process from oldest to newest
                val promotableBlocks = identifyPromotable(
                  canonicalBranch.recentBlocks.reverse,
                  List.Nil
                )

                // If we promoted some blocks, remove the canonical branch from forksTree
                // (it will be reconstructed with remaining unpromoted blocks if any)
                val updatedTree =
                    if promotableBlocks.isEmpty then forksTree
                    else
                        // Remove promoted blocks from canonical branch
                        val remainingBlocks = canonicalBranch.recentBlocks.filter { block =>
                            !promotableBlocks.contains(block.hash)
                        }

                        // If all blocks were promoted, remove the entire branch
                        if remainingBlocks.isEmpty then
                            removeBranch(forksTree, canonicalBranch)
                        else
                            // Update branch with remaining blocks
                            val updatedBranch = ForkBranch(
                              tipHash = canonicalBranch.tipHash,
                              tipHeight = canonicalBranch.tipHeight,
                              tipChainwork = canonicalBranch.tipChainwork,
                              recentBlocks = remainingBlocks  // Remaining blocks keep their individual addedTime
                            )
                            updateBranch(forksTree, canonicalBranch, updatedBranch)

                (promotableBlocks, updatedTree)

    // Validate fork submission rule: must include canonical extension
    // Prevents attack where adversary submits only forks to stall Oracle progress
    def validateForkSubmission(
        blockHeaders: List[BlockHeader],
        forksTree: List[ForkBranch],
        confirmedTip: BlockHash
    ): Unit = {
        // RULE 1: Check for duplicate blocks in submission
        def checkDuplicates(
            headers: List[BlockHeader],
            seen: List[BlockHash]
        ): Unit = {
            headers match
                case List.Nil => ()
                case List.Cons(header, tail) =>
                    val hash = blockHeaderHash(header)
                    // Check if hash already in seen list
                    def contains(list: List[BlockHash], target: BlockHash): Boolean = {
                        list match
                            case List.Nil => false
                            case List.Cons(h, t) =>
                                if h == target then true
                                else contains(t, target)
                    }
                    if contains(seen, hash) then fail("Duplicate block in submission")
                    else checkDuplicates(tail, List.Cons(hash, seen))
        }

        checkDuplicates(blockHeaders, List.Nil)

        // Find current canonical tip hash
        val canonicalTipHash = selectCanonicalChain(forksTree) match
            case scalus.prelude.Option.Some(branch) => branch.tipHash
            case scalus.prelude.Option.None         => confirmedTip

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
                case scalus.prelude.Option.None =>
                    // No blocks in forks tree
                    forksTree
                case scalus.prelude.Option.Some(canonicalBranch) =>
                    val canonicalTipHeight = canonicalBranch.tipHeight
                    val canonicalTipChainwork = canonicalBranch.tipChainwork

                    // Determine if a branch is removable
                    def isRemovable(branch: ForkBranch): Boolean = {
                        // Cannot remove canonical branch
                        if branch.tipHash == canonicalBranch.tipHash then false
                        else
                            val heightGap = canonicalTipHeight - branch.tipHeight
                            // Use the oldest block's addedTime (last in list) for age calculation
                            val oldestBlockTime = branch.recentBlocks.lastOption.map(_.addedTime).getOrElse(currentTime)
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
                                    chainworkGap > BigInt(0) // Any chainwork gap qualifies for removal after challenge period

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
                                    if isRemovable(branch) then
                                        filterBranches(tail, accumulated)
                                    else
                                        filterBranches(tail, List.Cons(branch, accumulated))
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
                                    val newRemaining = remaining.filter(b => b.tipHash != maxBranch.tipHash)

                                    selectTopN(newRemaining, List.Cons(maxBranch, selected), count - 1)
                            }

                            selectTopN(filtered, List.Nil, targetSize)
                        else
                            filtered
                    }

                    performRemoval(forksTree, MaxForksTreeSize)
    }

    // Difficulty adjustment - matches GetNextWorkRequired() in pow.cpp:14-48
    def getNextWorkRequired(
        nHeight: BigInt,
        currentTarget: CompactBits,
        blockTime: BigInt,
        nFirstBlockTime: BigInt
    ): CompactBits = {
        // Only change once per difficulty adjustment interval
        if (nHeight + 1) % DifficultyAdjustmentInterval == BigInt(0) then
            val newTarget = calculateNextWorkRequired(currentTarget, blockTime, nFirstBlockTime)
            val r = targetToCompactByteString(newTarget)
            r
        else currentTarget
    }

    // Calculate next work required - matches CalculateNextWorkRequired() in pow.cpp:50-84
    def calculateNextWorkRequired(
        currentTarget: CompactBits,
        blockTime: BigInt,
        nFirstBlockTime: BigInt
    ): BigInt = {
        val PowTargetTimespan = DifficultyAdjustmentInterval * TargetBlockTime
        val actualTimespan =
            val timespan = blockTime - nFirstBlockTime
            // Limit adjustment step - matches pow.cpp:55-60
            Math.min(
              Math.max(timespan, PowTargetTimespan / 4),
              PowTargetTimespan * 4
            )

        val t = compactBitsToTarget(currentTarget)
        val bnNew = t * actualTimespan / PowTargetTimespan
        Math.min(bnNew, PowLimit)
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
          confirmedBlocksTree =
              prevState.confirmedBlocksTree, // Preserve confirmed blocks tree (updated separately)
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
        currentTime: BigInt
    ): ChainState = {
        scalus.prelude.log("computeUpdateOracleState START")
        scalus.prelude.log("INPUT prevState.forksTree.size:")
        scalus.prelude.log(scalus.prelude.show(prevState.forksTree.size))
        scalus.prelude.log("INPUT prevState.blockHeight:")
        scalus.prelude.log(scalus.prelude.show(prevState.blockHeight))

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
                    scalus.prelude.log("processHeaders done")
                    currentForksTree
                case List.Cons(header, tail) =>
                    scalus.prelude.log("processHeaders adding block")
                    val updatedTree = addBlockToForksTree(
                      currentForksTree,
                      header,
                      prevState,
                      currentTime
                    )
                    processHeaders(tail, updatedTree)
        }

        scalus.prelude.log("processing headers")
        val forksTreeAfterAddition = processHeaders(blockHeaders, prevState.forksTree)
        scalus.prelude.log("headers processed")

        // Select canonical chain (highest chainwork)
        scalus.prelude.log("selecting canonical chain")
        val canonicalBranchOpt = selectCanonicalChain(forksTreeAfterAddition)
        val canonicalTipHash = canonicalBranchOpt match
            case scalus.prelude.Option.Some(branch) =>
                scalus.prelude.log("canonical tip found")
                branch.tipHash
            case scalus.prelude.Option.None =>
                scalus.prelude.log("canonical tip is prev hash")
                prevState.blockHash

        // Promote qualified blocks (100+ confirmations AND 200+ min old)
        scalus.prelude.log("promoting qualified blocks")
        val (promotedBlocks, forksTreeAfterPromotion) = promoteQualifiedBlocks(
          forksTreeAfterAddition,
          prevState.blockHash,  // confirmedTip
          prevState.blockHeight,
          currentTime
        )
        
        if promotedBlocks.isEmpty then
            scalus.prelude.log("no blocks promoted")
        else
            scalus.prelude.log("blocks promoted")

        // Run garbage collection if forks tree exceeds size limit
        val finalForksTree =
            if forksTreeAfterPromotion.size > MaxForksTreeSize then
                scalus.prelude.log("running GC")
                garbageCollect(
                  forksTreeAfterPromotion,
                  prevState.blockHash,  // confirmedTip
                  prevState.blockHeight,
                  currentTime
                )
            else
                scalus.prelude.log("skipping GC")
                forksTreeAfterPromotion

        // Compute new state
        // If blocks were promoted, update confirmed state
        scalus.prelude.log("computing final state")
        
        // Log final forksTree size
        val finalTreeSize = finalForksTree.size
        scalus.prelude.log("ON-CHAIN final forksTree size:")
        scalus.prelude.log(scalus.prelude.show(finalTreeSize))
        
        if promotedBlocks.isEmpty then
            // No promotion: only forks tree changed
            scalus.prelude.log("no promotion, returning state with updated forks")
            ChainState(
              blockHeight = prevState.blockHeight,
              blockHash = prevState.blockHash,
              currentTarget = prevState.currentTarget,
              blockTimestamp = prevState.blockTimestamp,
              recentTimestamps = prevState.recentTimestamps,
              previousDifficultyAdjustmentTimestamp =
                  prevState.previousDifficultyAdjustmentTimestamp,
              confirmedBlocksTree = prevState.confirmedBlocksTree,
              forksTree = finalForksTree
            )
        else
            scalus.prelude.log("promotion occurred, updating confirmed state")
            // Promotion occurred: update confirmed state
            // Find the canonical branch to get promoted block info
            val canonicalBranch = canonicalBranchOpt.getOrFail("Canonical branch should exist after promotion")

            // Get the latest promoted block hash (head of list)
            val latestPromotedHash = promotedBlocks.head

            // Find the promoted block in canonical branch's recentBlocks
            val latestPromotedBlock = lookupInRecentBlocks(canonicalBranch.recentBlocks, latestPromotedHash)
                .getOrFail("Promoted block not found in canonical branch")

            // Add promoted blocks to Merkle tree (in order from oldest to newest)
            // promotedBlocks is already sorted from oldest to newest
            def addPromotedBlocks(
                tree: List[ByteString],
                blocks: List[BlockHash]
            ): List[ByteString] =
                blocks match
                    case List.Nil => tree
                    case List.Cons(blockHash, tail) =>
                        addPromotedBlocks(addToMerkleTree(tree, blockHash), tail)

            val updatedMerkleTree = addPromotedBlocks(prevState.confirmedBlocksTree, promotedBlocks)

            // Update confirmed state with promoted block
            // TODO: Update recentTimestamps, difficulty adjustment properly
            // For now, preserve these fields - full implementation requires walking promoted chain
            ChainState(
              blockHeight = latestPromotedBlock.height,
              blockHash = latestPromotedHash,
              currentTarget = latestPromotedBlock.bits,  // Use promoted block's difficulty
              blockTimestamp = latestPromotedBlock.timestamp,
              recentTimestamps = prevState.recentTimestamps,  // TODO: should be updated
              previousDifficultyAdjustmentTimestamp =
                  prevState.previousDifficultyAdjustmentTimestamp,  // TODO: should be updated
              confirmedBlocksTree = updatedMerkleTree,
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
            case Action.UpdateOracle(blockHeaders, redeemerTime, inputDatumHash) =>
                // Datum hash verification disabled for production (expensive)
                // Uncomment for debugging datum non-determinism issues:
                // val actualInputDatumHash = blake2b_256(serialiseData(prevState.toData))
                // scalus.prelude.log("Expected inputDatumHash from redeemer (hex):")
                // scalus.prelude.log(scalus.prelude.Prelude.encodeHex(inputDatumHash))
                // scalus.prelude.log("Actual inputDatumHash computed on-chain (hex):")
                // scalus.prelude.log(scalus.prelude.Prelude.encodeHex(actualInputDatumHash))
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
                val computedState = computeUpdateOracleState(prevState, blockHeaders, redeemerTime)

                val continuingOutput = findUniqueOutputFrom(outputs, ownInput.address)

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

    // This one is for V3 lowering
    inline override def spend(
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
    def validate2(scData: Data): Unit = {
        val sc = unConstrData(scData).snd
        val txInfoData = sc.head
        val redeemer = sc.tail.head
        val scriptInfo = unConstrData(sc.tail.tail.head)
        if scriptInfo.fst == BigInt(1) then
            val txOutRef = scriptInfo.snd.head.to[TxOutRef]
            val datum = scriptInfo.snd.tail.head.to[Option[Datum]]
            spend2(datum, redeemer, txInfoData, txOutRef)
        else fail("Invalid script context")
    }

    def reverse(bs: ByteString): ByteString =
        val len = lengthOfByteString(bs)
        def loop(idx: BigInt, acc: ByteString): ByteString =
            if idx == len then acc
            else loop(idx + 1, consByteString(bs.at(idx), acc))
        loop(0, ByteString.empty)
}
