package binocular

import scalus.uplc.builtin.*
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString.*
import scalus.cardano.onchain.plutus.prelude.{List, Math, *}
import scalus.cardano.onchain.plutus.prelude.Math.pow
import scalus.*

import scala.annotation.tailrec

@Compile
object BitcoinHelpers {

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

    // 2^256 constant for chainwork calculation
    // Used in GetBlockProof() formula: work = 2^256 / (target + 1)
    // Precomputed to avoid on-chain exponentiation
    val TwoTo256: BigInt = BigInt(
      "115792089237316195423570985008687907853269984665640564039457584007913129639936"
    )

    /** Calculate proof-of-work for a block given its target Matches GetBlockProof() in Bitcoin
      * Core's chain.cpp
      *
      * Formula: 2^256^ / (target + 1)
      *
      * This represents the expected number of hashes needed to find a valid block at this
      * difficulty
      */
    def calculateBlockProof(target: BigInt): BigInt = {
        TwoTo256 / (target + 1)
    }

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

    /** Gets the median time of past blocks from a reverse-sorted timestamp list.
      *
      * Matches CBlockIndex::GetMedianTimePast() in chain.h:278-290:
      * {{{
      * int64_t GetMedianTimePast() const {
      *     int64_t pmedian[nMedianTimeSpan];
      *     int64_t* pbegin = &pmedian[nMedianTimeSpan];
      *     int64_t* pend = &pmedian[nMedianTimeSpan];
      *     const CBlockIndex* pindex = this;
      *     for (int i = 0; i < nMedianTimeSpan && pindex; i++, pindex = pindex->pprev)
      *         *(--pbegin) = pindex->GetBlockTime();
      *     std::sort(pbegin, pend);
      *     return pbegin[(pend - pbegin) / 2];
      * }
      * }}}
      *
      * Bitcoin Core collects up to 11 timestamps, sorts ascending, and returns the element at index
      * `count / 2`. Here, `timestamps` is maintained in descending (reverse-sorted) order. For an
      * odd-sized list (always 11 in steady state), the element at `size / 2` is the same regardless
      * of sort direction — both yield the true median.
      *
      * @param timestamps
      *   reverse-sorted list of block timestamps (newest/largest first)
      * @param size
      *   number of elements in the list
      * @return
      *   the median timestamp, or [[UnixEpoch]] if the list is empty
      */
    def getMedianTimePast(timestamps: List[BigInt], size: BigInt): BigInt =
        timestamps match
            case List.Nil => UnixEpoch
            case _ =>
                val index = size / 2
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
}
