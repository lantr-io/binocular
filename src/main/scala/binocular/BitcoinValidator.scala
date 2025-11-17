package binocular

import scalus.builtin.*
import scalus.builtin.Builtins.*
import scalus.builtin.ByteString.*
import scalus.builtin.Data.{toData, FromData, ToData}
import scalus.compiler.sir.TargetLoweringBackend
import scalus.ledger.api.v2.OutputDatum
import scalus.ledger.api.v3.*
import scalus.prelude.Show.given
import scalus.prelude.{List, Math, *}
import scalus.prelude.Math.pow
import scalus.uplc.Program
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

case class ChainState(
    // Confirmed state
    blockHeight: BigInt,
    blockHash: BlockHash,
    currentTarget: CompactBits,
    blockTimestamp: BigInt,
    recentTimestamps: List[BigInt], // Newest first
    previousDifficultyAdjustmentTimestamp: BigInt,
    confirmedBlocksTree: List[ByteString], // Rolling Merkle tree state (levels array)

    // Forks tree
    forksTree: scalus.prelude.SortedMap[BlockHash, BlockNode] // Block hash → BlockNode mapping
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
      */
    case UpdateOracle(blockHeaders: List[BlockHeader], currentTime: BigInt)

@Compile
object Action

@Compile
object BitcoinValidator extends Validator {

    // Bitcoin consensus constants - matches Bitcoin Core chainparams.cpp
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
    val MaturationConfirmations: BigInt = 100 // Blocks needed for promotion to confirmed state
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
    def lookupBlock(
        forksTree: scalus.prelude.SortedMap[BlockHash, BlockNode],
        key: BlockHash
    ): Option[BlockNode] =
        forksTree.find((k, _) => k == key) match
            case scalus.prelude.Option.Some((_, value)) => scalus.prelude.Option.Some(value)
            case scalus.prelude.Option.None             => scalus.prelude.Option.None

    // Canonical chain selection - matches Algorithm 9 in Whitepaper
    // Selects the fork with highest cumulative chainwork
    def selectCanonicalChain(
        forksTree: scalus.prelude.SortedMap[BlockHash, BlockNode]
    ): Option[BlockHash] =
        // Find all tips (blocks with no children) and select one with max chainwork
        def findMaxChainworkTip(
            entries: List[(BlockHash, BlockNode)],
            currentBest: Option[(BlockHash, BigInt)]
        ): Option[BlockHash] =
            entries match
                case List.Nil =>
                    currentBest.map(_._1)
                case List.Cons((hash, node), tail) =>
                    // A tip has no children
                    if node.children.isEmpty then
                        val newBest = currentBest match
                            case scalus.prelude.Option.None =>
                                scalus.prelude.Option.Some((hash, node.chainwork))
                            case scalus.prelude.Option.Some((_, maxChainwork)) =>
                                if node.chainwork > maxChainwork then
                                    scalus.prelude.Option.Some((hash, node.chainwork))
                                else currentBest
                        findMaxChainworkTip(tail, newBest)
                    else findMaxChainworkTip(tail, currentBest)

        findMaxChainworkTip(forksTree.toList, scalus.prelude.Option.None)

    // Add block to forks tree - matches Transition 1 in Whitepaper
    def addBlockToForksTree(
        forksTree: scalus.prelude.SortedMap[BlockHash, BlockNode],
        blockHeader: BlockHeader,
        confirmedState: ChainState,
        currentTime: BigInt
    ): scalus.prelude.SortedMap[BlockHash, BlockNode] =
        val hash = blockHeaderHash(blockHeader)
        val hashInt = byteStringToInteger(false, hash)
        val prevHash = blockHeader.prevBlockHash

        // Check parent exists (in forksTree OR is confirmed tip)
        val parentExists =
            lookupBlock(forksTree, prevHash).isDefined || prevHash == confirmedState.blockHash
        require(parentExists, "Parent block not found")

        // === SECURITY: Full block validation ===

        // Calculate parent height
        val parentHeight =
            if prevHash == confirmedState.blockHash then confirmedState.blockHeight
            else lookupBlock(forksTree, prevHash).getOrFail("Parent not in forks tree").height

        val blockHeight = parentHeight + 1

        // 1. VALIDATE PROOF-OF-WORK
        val target = compactBitsToTarget(blockHeader.bits)
        require(hashInt <= target, "Invalid proof-of-work")
        require(target <= PowLimit, "Target exceeds PowLimit")

        // 2. VALIDATE DIFFICULTY
        // For fork blocks, we need parent's difficulty adjustment info
        // This is simplified - full validation would need to track difficulty state in BlockNode
        val expectedBits =
            if prevHash == confirmedState.blockHash then
                getNextWorkRequired(
                  parentHeight,
                  confirmedState.currentTarget,
                  confirmedState.blockTimestamp,
                  confirmedState.previousDifficultyAdjustmentTimestamp
                )
            else
                // For deep forks, difficulty validation is more complex
                // We'd need to track full difficulty state in BlockNode
                // For now, accept the claimed difficulty if PoW is valid
                blockHeader.bits

        require(blockHeader.bits == expectedBits, "Invalid difficulty")

        // 3. VALIDATE TIMESTAMP
        val blockTime = blockHeader.timestamp
        val medianTimePast =
            getMedianTimePast(confirmedState.recentTimestamps, confirmedState.recentTimestamps.size)
        require(blockTime > medianTimePast, "Block timestamp not greater than median time past")
        require(blockTime <= currentTime + MaxFutureBlockTime, "Block timestamp too far in future")

        // 4. VALIDATE VERSION
        require(blockHeader.version >= 4, "Outdated block version")

        // === End validation ===

        // Calculate chainwork: parent chainwork + work from this block
        val parentChainwork =
            if prevHash == confirmedState.blockHash then
                // Calculate confirmed chainwork (simplified - should be stored)
                compactBitsToTarget(confirmedState.currentTarget)
            else lookupBlock(forksTree, prevHash).getOrFail("Parent not in forks tree").chainwork

        val blockWork = PowLimit / target
        val newChainwork = parentChainwork + blockWork

        // Create BlockNode
        val node = BlockNode(
          prevBlockHash = prevHash,
          height = blockHeight,
          chainwork = newChainwork,
          addedTimestamp = currentTime,
          children = List.Nil
        )

        // Insert into forksTree
        val treeWithNewNode = forksTree.insert(hash, node)

        // Update parent's children list (if parent is in forks tree)
        lookupBlock(forksTree, prevHash) match
            case scalus.prelude.Option.Some(parentNode) =>
                val updatedParent = BlockNode(
                  prevBlockHash = parentNode.prevBlockHash,
                  height = parentNode.height,
                  chainwork = parentNode.chainwork,
                  addedTimestamp = parentNode.addedTimestamp,
                  children = List.Cons(hash, parentNode.children)
                )
                treeWithNewNode.insert(prevHash, updatedParent)
            case scalus.prelude.Option.None =>
                // Parent is confirmed tip, no update needed
                treeWithNewNode

    // Block promotion (maturation) - matches Algorithm 10 in Whitepaper
    // Returns (list of promoted block hashes, updated forks tree with promoted blocks removed)
    def promoteQualifiedBlocks(
        forksTree: scalus.prelude.SortedMap[BlockHash, BlockNode],
        confirmedTip: BlockHash,
        confirmedHeight: BigInt,
        currentTime: BigInt
    ): (List[BlockHash], scalus.prelude.SortedMap[BlockHash, BlockNode]) =
        // Find canonical chain
        selectCanonicalChain(forksTree) match
            case scalus.prelude.Option.None =>
                // No blocks in forks tree
                (List.Nil, forksTree)
            case scalus.prelude.Option.Some(canonicalTipHash) =>
                val canonicalTipNode =
                    lookupBlock(forksTree, canonicalTipHash).getOrFail("Canonical tip not found")
                val canonicalTipHeight = canonicalTipNode.height

                // Walk back from canonical tip to confirmed tip
                def walkChain(
                    currentHash: BlockHash,
                    acc: List[(BlockHash, BlockNode)]
                ): List[(BlockHash, BlockNode)] =
                    if currentHash == confirmedTip then acc
                    else
                        lookupBlock(forksTree, currentHash) match
                            case scalus.prelude.Option.Some(blockNode) =>
                                walkChain(
                                  blockNode.prevBlockHash,
                                  List.Cons((currentHash, blockNode), acc)
                                )
                            case scalus.prelude.Option.None =>
                                fail("Canonical chain broken")

                val chain = walkChain(canonicalTipHash, List.Nil)

                // Identify promotable blocks (from oldest to newest)
                // Now using height for accurate depth calculation
                def identifyPromotable(
                    blocks: List[(BlockHash, BlockNode)],
                    accumulated: List[BlockHash]
                ): List[BlockHash] =
                    blocks match
                        case List.Nil => accumulated
                        case List.Cons((hash, node), tail) =>
                            val depth = canonicalTipHeight - node.height
                            val age = currentTime - node.addedTimestamp

                            if depth >= MaturationConfirmations && age >= ChallengeAging then
                                // This block qualifies for promotion
                                identifyPromotable(tail, List.Cons(hash, accumulated))
                            else
                                // Stop at first non-qualified block
                                accumulated

                val promotableBlocks = identifyPromotable(chain, List.Nil)

                // Remove promoted blocks from forks tree
                def removeBlocks(
                    tree: scalus.prelude.SortedMap[BlockHash, BlockNode],
                    blocksToRemove: List[BlockHash]
                ): scalus.prelude.SortedMap[BlockHash, BlockNode] =
                    blocksToRemove match
                        case List.Nil => tree
                        case List.Cons(hash, tail) =>
                            removeBlocks(tree.delete(hash), tail)

                val updatedTree = removeBlocks(forksTree, promotableBlocks)

                (promotableBlocks, updatedTree)

    // Validate fork submission rule: must include canonical extension
    // Prevents attack where adversary submits only forks to stall Oracle progress
    def validateForkSubmission(
        blockHeaders: List[BlockHeader],
        forksTree: scalus.prelude.SortedMap[BlockHash, BlockNode],
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

        // Find current canonical tip
        val canonicalTip = selectCanonicalChain(forksTree) match
            case scalus.prelude.Option.Some(tip) => tip
            case scalus.prelude.Option.None      => confirmedTip

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
                    if prevHash == canonicalTip then
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

    // Build set of blocks on canonical chain (helper for garbage collection)
    def buildCanonicalChainSet(
        forksTree: scalus.prelude.SortedMap[BlockHash, BlockNode],
        canonicalTip: BlockHash,
        confirmedTip: BlockHash
    ): scalus.prelude.SortedMap[BlockHash, Boolean] = {
        // Walk backwards from canonical tip, building set of hashes
        def walk(
            currentHash: BlockHash,
            acc: scalus.prelude.SortedMap[BlockHash, Boolean]
        ): scalus.prelude.SortedMap[BlockHash, Boolean] = {
            if currentHash == confirmedTip then acc
            else
                lookupBlock(forksTree, currentHash) match
                    case scalus.prelude.Option.Some(node) =>
                        walk(node.prevBlockHash, acc.insert(currentHash, true))
                    case scalus.prelude.Option.None =>
                        fail("Canonical chain broken")
        }

        walk(canonicalTip, scalus.prelude.SortedMap.empty)
    }

    // Garbage collection - removes old dead forks to maintain bounded datum size
    def garbageCollect(
        forksTree: scalus.prelude.SortedMap[BlockHash, BlockNode],
        confirmedTip: BlockHash,
        confirmedHeight: BigInt,
        currentTime: BigInt
    ): scalus.prelude.SortedMap[BlockHash, BlockNode] = {
        val currentSize = forksTree.toList.size

        if currentSize <= MaxForksTreeSize then
            // No garbage collection needed
            forksTree
        else
            // Find canonical chain
            selectCanonicalChain(forksTree) match
                case scalus.prelude.Option.None =>
                    // No blocks in forks tree
                    forksTree
                case scalus.prelude.Option.Some(canonicalTipHash) =>
                    val canonicalTipNode = lookupBlock(forksTree, canonicalTipHash)
                        .getOrFail("Canonical tip not found")
                    val canonicalTipHeight = canonicalTipNode.height
                    val canonicalTipChainwork = canonicalTipNode.chainwork

                    // Build set of canonical chain blocks (cannot be removed)
                    val canonicalChain =
                        buildCanonicalChainSet(forksTree, canonicalTipHash, confirmedTip)

                    // Find removable blocks (dead fork tips)
                    def isRemovable(hash: BlockHash, node: BlockNode): Boolean = {
                        // Check if on canonical chain
                        val onCanonical = canonicalChain.find((k, _) => k == hash).isDefined

                        if onCanonical then false // Cannot remove canonical chain blocks
                        else
                            val heightGap = canonicalTipHeight - node.height
                            val age = currentTime - node.addedTimestamp
                            val isTip = node.children.isEmpty
                            val chainworkGap = canonicalTipChainwork - node.chainwork

                            // Criteria Set A: Old dead forks
                            val isOldDeadFork =
                                heightGap >= MaturationConfirmations &&
                                    age >= ChallengeAging &&
                                    isTip

                            // Criteria Set B: Stale competing forks
                            val isStaleCompetingFork =
                                age >= StaleCompetingForkAge &&
                                    chainworkGap >= (ChainworkGapThreshold * (PowLimit / compactBitsToTarget(
                                      hex"1d00ffff"
                                    ))) &&
                                    isTip

                            isOldDeadFork || isStaleCompetingFork
                    }

                    // Remove blocks that meet criteria (simplified - remove all removable)
                    def performRemoval(
                        tree: scalus.prelude.SortedMap[BlockHash, BlockNode],
                        allBlocks: List[(BlockHash, BlockNode)],
                        targetSize: BigInt,
                        currentSz: BigInt
                    ): scalus.prelude.SortedMap[BlockHash, BlockNode] = {
                        if currentSz <= targetSize then tree
                        else
                            allBlocks match
                                case List.Nil => tree
                                case List.Cons((hash, node), tail) =>
                                    if isRemovable(hash, node) && currentSz > targetSize then
                                        // Remove this block
                                        performRemoval(
                                          tree.delete(hash),
                                          tail,
                                          targetSize,
                                          currentSz - 1
                                        )
                                    else
                                        // Keep this block, continue with next
                                        performRemoval(tree, tail, targetSize, currentSz)
                    }

                    performRemoval(forksTree, forksTree.toList, MaxForksTreeSize, currentSize)
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
        // Validate fork submission rule (prevents stalling attack and duplicates)
        validateForkSubmission(blockHeaders, prevState.forksTree, prevState.blockHash)

        // Validate non-empty block headers
        require(!blockHeaders.isEmpty, "Empty block headers list")

        // Process all block headers: add each to forks tree
        def processHeaders(
            headers: List[BlockHeader],
            currentForksTree: scalus.prelude.SortedMap[BlockHash, BlockNode]
        ): scalus.prelude.SortedMap[BlockHash, BlockNode] = {
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

        // Select canonical chain (highest chainwork)
        val canonicalTip = selectCanonicalChain(forksTreeAfterAddition) match
            case scalus.prelude.Option.Some(tip) => tip
            case scalus.prelude.Option.None      => prevState.blockHash

        // Promote qualified blocks (100+ confirmations AND 200+ min old)
        val (promotedBlocks, forksTreeAfterPromotion) = promoteQualifiedBlocks(
          forksTreeAfterAddition,
          canonicalTip,
          prevState.blockHeight,
          currentTime
        )

        // Run garbage collection if forks tree exceeds size limit
        val finalForksTree =
            if forksTreeAfterPromotion.toList.size > MaxForksTreeSize then
                garbageCollect(
                  forksTreeAfterPromotion,
                  canonicalTip,
                  prevState.blockHeight,
                  currentTime
                )
            else forksTreeAfterPromotion

        // Compute new state
        // If blocks were promoted, update confirmed state
        if promotedBlocks.isEmpty then
            // No promotion: only forks tree changed
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
            // Promotion occurred: update confirmed state
            // Get the highest promoted block info
            val latestPromotedHash = promotedBlocks.head // List is sorted, first is highest
            val latestPromotedNode = lookupBlock(forksTreeAfterAddition, latestPromotedHash)
                .getOrFail("Promoted block not found in tree")

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
            // TODO: Update recentTimestamps, difficulty adjustment
            // For now, preserve these fields - full implementation requires walking promoted chain
            ChainState(
              blockHeight = latestPromotedNode.height,
              blockHash = latestPromotedHash,
              currentTarget = prevState.currentTarget,
              blockTimestamp = latestPromotedNode.addedTimestamp,
              recentTimestamps = prevState.recentTimestamps,
              previousDifficultyAdjustmentTimestamp =
                  prevState.previousDifficultyAdjustmentTimestamp,
              confirmedBlocksTree = updatedMerkleTree,
              forksTree = finalForksTree
            )
    }

    def findUniqueOutputFrom(outputs: List[TxOut], scriptAddress: Address): TxOut = {
        val matchingOutputs = outputs.filter(out => out.address === scriptAddress)
        require(matchingOutputs.size == BigInt(1), "There must be exactly one continuing output")
        matchingOutputs.head
    }

    inline override def spend(
        datum: Option[Datum],
        redeemer: Datum,
        tx: TxInfo,
        outRef: TxOutRef
    ): Unit = {
        val action = redeemer.to[Action]

        val intervalStartInSeconds = tx.validRange.from.boundType match
            case IntervalBoundType.Finite(time) => time / 1000
            case _                              => fail("Must have finite interval start")

        // Find the continuing output (output to the same script address)
        val ownInput = tx.inputs.find { _.outRef === outRef }.getOrFail("Input not found").resolved
        val prevState =
            ownInput.datum match
                case OutputDatum.OutputDatum(datum) =>
                    datum.to[ChainState]
                case _ => fail("No datum")
        action match
            case Action.UpdateOracle(blockHeaders, redeemerTime) =>
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

                val continuingOutput = findUniqueOutputFrom(tx.outputs, ownInput.address)

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

    def reverse(bs: ByteString): ByteString =
        val len = lengthOfByteString(bs)
        def loop(idx: BigInt, acc: ByteString): ByteString =
            if idx == len then acc
            else loop(idx + 1, consByteString(bs.at(idx), acc))
        loop(0, ByteString.empty)
}

object BitcoinContract {
    given Compiler.Options = Compiler.Options(
      optimizeUplc = true,
      generateErrorTraces = true,
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering
    )
    def compileBitcoinProgram(): Program =
        val sir = Compiler.compileWithOptions(summon[Compiler.Options], BitcoinValidator.validate)
        //    println(sir.showHighlighted)
        //    sir.toUplcOptimized(generateErrorTraces = false).plutusV3
        sir.toUplcOptimized().plutusV3
    //    println(uplc.showHighlighted)

    lazy val bitcoinProgram: Program = compileBitcoinProgram()
}
