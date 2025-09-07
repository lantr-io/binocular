package binocular

import scalus.*
import scalus.builtin.Builtins.*
import scalus.builtin.ByteString.*
import scalus.builtin.Data.{FromData, ToData, toData}
import scalus.builtin.{Builtins, ByteString, Data, FromData, ToData}
import scalus.ledger.api.v2.OutputDatum
import scalus.ledger.api.v3.*
import scalus.prelude.*
import scalus.uplc.Program

import scala.annotation.tailrec

extension (a: ByteString) def reverse: ByteString = ByteString.fromArray(a.bytes.reverse)

@Compile
object BitcoinValidator extends Validator {

    // Bitcoin consensus constants - matches Bitcoin Core chainparams.cpp
    val UnixEpoch: BigInt = 1231006505
    val TargetBlockTime: BigInt = 600 // 10 minutes - matches nPowTargetSpacing in chainparams.cpp
    val DifficultyAdjustmentInterval: BigInt = 2016 // matches DifficultyAdjustmentInterval() in chainparams.cpp
    val MaxFutureBlockTime: BigInt =
        7200 // 2 hours in the future in seconds - matches MAX_FUTURE_BLOCK_TIME in validation.cpp
    val MedianTimeSpan: BigInt = 11 // matches CBlockIndex::nMedianTimeSpan in chain.h:276
    // Proof of work limit - matches consensus.powLimit in chainparams.cpp
    val PowLimit: BigInt =
        byteStringToInteger(true, hex"00000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffff")

    case class CoinbaseTx(
        version: ByteString,
        inputScriptSigAndSequence: ByteString,
        txOutsAndLockTime: ByteString
    )

    case class BlockHeader(bytes: ByteString)

    extension (bh: BlockHeader)
        inline def version: BigInt =
            byteStringToInteger(false, bh.bytes.slice(0, 4))

        inline def prevBlockHash: ByteString = bh.bytes.slice(4, 32)

        inline def bits: ByteString = bh.bytes.slice(72, 4)

        inline def merkleRoot: ByteString = bh.bytes.slice(36, 32)

        inline def timestamp: BigInt = byteStringToInteger(false, bh.bytes.slice(68, 4))

    case class ChainState(
        blockHeight: BigInt,
        blockHash: ByteString,
        currentTarget: ByteString,
        blockTimestamp: BigInt,
        recentTimestamps: List[BigInt], // Newest first
        previousDifficultyAdjustmentTimestamp: BigInt
    )

    enum Action:
        case NewTip(
            blockHeader: BlockHeader,
            ownerPubKey: ByteString,
            signature: ByteString
        )
        case FraudProof(
            blockHeader: BlockHeader
        )

    given FromData[CoinbaseTx] = FromData.derived
    given ToData[CoinbaseTx] = ToData.derived
    given FromData[BlockHeader] = FromData.derived
    given ToData[BlockHeader] = ToData.derived
    given FromData[ChainState] = FromData.derived
    given ToData[ChainState] = ToData.derived
    given FromData[Action] = FromData.derived
    given ToData[Action] = ToData.derived

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

    // Double SHA256 hash - matches CBlockHeader::GetHash() in primitives/block.h
    def blockHeaderHash(blockHeader: BlockHeader): ByteString =
        sha2_256(sha2_256(blockHeader.bytes))

    def checkSignature(
        blockHeader: BlockHeader,
        ownerPubKey: ByteString,
        signature: ByteString
    ): Boolean =
        val hash = blockHeaderHash(blockHeader)
        verifyEd25519Signature(ownerPubKey, hash, signature)

    def min(a: BigInt, b: BigInt): BigInt =
        if a < b then a else b

    def max(a: BigInt, b: BigInt): BigInt =
        if a > b then a else b

    def pow(n: BigInt, e: BigInt): BigInt =
        def pow(n: BigInt, e: BigInt): BigInt =
            if e == BigInt(0) then 1
            else if e % 2 == BigInt(0) then pow(n * n, e / 2)
            else n * pow(n, e - 1)
        if e < BigInt(0) then fail("Negative exponent")
        else pow(n, e)

    /** Converts little-endian compact bits representation to target value
      *
      * Matches `arith_uint256::SetCompact()` in arith_uint256.cpp
      * @note
      *   only works for positive numbers. Here we use it only for `target` which is always positive, so it's fine.
      */
    def compactBitsToTarget(compact: ByteString): BigInt = {
        // int nSize = nCompact >> 24;
        val exponent = compact.at(3)
        // uint32_t nWord = nCompact & 0x007fffff;
        val coefficient = byteStringToInteger(false, compact.slice(0, 3))
        if coefficient > BigInt(0x007fffff) then fail("Negative bits")
        else if exponent < BigInt(3)
        then coefficient / pow(256, BigInt(3) - exponent)
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
      *   only works for positive numbers. Here we use it only for `target` which is always positive, so it's fine.
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
      *   only works for positive numbers. Here we use it only for `target` which is always positive, so it's fine.
      */
    def targetToCompactByteString(target: BigInt): ByteString = {
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
        merkleProof: builtin.List[Data],
        hash: ByteString,
        index: BigInt
    ): ByteString =
        def loop(index: BigInt, curHash: ByteString, siblings: builtin.List[Data]): ByteString =
            if siblings.isEmpty then curHash
            else
                val sibling = siblings.head.toByteString
                val nextHash =
                    if index % 2 == BigInt(0)
                    then sha2_256(sha2_256(curHash ++ sibling))
                    else sha2_256(sha2_256(sibling ++ curHash))
                loop(index / 2, nextHash, siblings.tail)

        loop(index, hash, merkleProof)

    def readVarInt(input: ByteString, offset: BigInt): (BigInt, BigInt) =
        val firstByte = input.at(offset)
        if firstByte < BigInt(0xfd) then (firstByte, offset + 1)
        else if firstByte == BigInt(0xfd) then (byteStringToInteger(false, input.slice(offset + 1, 2)), offset + 3)
        else if firstByte == BigInt(0xfe) then (byteStringToInteger(false, input.slice(offset + 1, 4)), offset + 5)
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

    def getTxHash(rawTx: ByteString): ByteString =
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

    def getCoinbaseTxHash(coinbaseTx: CoinbaseTx): ByteString =
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
        currentTarget: ByteString,
        blockTime: BigInt,
        nFirstBlockTime: BigInt
    ): ByteString = {
        // Only change once per difficulty adjustment interval
        if (nHeight + 1) % DifficultyAdjustmentInterval == BigInt(0) then
            val newTarget = calculateNextWorkRequired(currentTarget, blockTime, nFirstBlockTime)
            val r = targetToCompactByteString(newTarget)
            r
        else currentTarget
    }

    // Calculate next work required - matches CalculateNextWorkRequired() in pow.cpp:50-84
    def calculateNextWorkRequired(currentTarget: ByteString, blockTime: BigInt, nFirstBlockTime: BigInt): BigInt = {
        val PowTargetTimespan = DifficultyAdjustmentInterval * TargetBlockTime
        val actualTimespan =
            val timespan = blockTime - nFirstBlockTime
            // Limit adjustment step - matches pow.cpp:55-60
            min(
              max(timespan, PowTargetTimespan / 4),
              PowTargetTimespan * 4
            )

        val t = compactBitsToTarget(currentTarget)
        val bnNew = t * actualTimespan / PowTargetTimespan
        min(bnNew, PowLimit)
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
        require(blockTime <= currentTime + MaxFutureBlockTime, "Block timestamp too far in the future")
        require(blockTime > medianTimePast, "Block timestamp must be greater than median time of past 11 blocks")

        val newDifficultyAdjustmentTimestamp =
            if (prevState.blockHeight + 1) % DifficultyAdjustmentInterval == BigInt(0) then blockTime
            else prevState.previousDifficultyAdjustmentTimestamp

        // Insert new blockTime maintaining reverse sort order
        val withNewTimestamp = insertReverseSorted(blockTime, prevState.recentTimestamps)
        val newTimestamps = withNewTimestamp.take(MedianTimeSpan)

        // Reject blocks with outdated version
        // Matches ContextualCheckBlockHeader() in validation.cpp:4201-4206
        val validVersion = blockHeader.version >= 4

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
          previousDifficultyAdjustmentTimestamp = newDifficultyAdjustmentTimestamp
        )

    def verifyNewTip(
        intervalStartInSeconds: BigInt,
        prevState: ChainState,
        newState: Data,
        blockHeader: BlockHeader,
        ownerPubKey: ByteString,
        signature: ByteString
    ): Unit =
        val expectedNewState = updateTip(prevState, blockHeader, intervalStartInSeconds)
        val validNewState = newState == expectedNewState.toData

        require(validNewState, "New state does not match expected state")

        val validSignature =
            verifyEd25519Signature(ownerPubKey, expectedNewState.blockHash, signature)

        require(validSignature, "Signature is not valid")

    def verifyFraudProof(state: ChainState, blockHeader: BlockHeader): Unit =
        ()

    override def spend(datum: Option[Datum], redeemer: Datum, tx: TxInfo, outRef: TxOutRef): Unit = {
        val action = redeemer.to[Action]
        val intervalStartInSeconds = tx.validRange.from.boundType match
            case IntervalBoundType.Finite(time) => time / 1000
            case _                              => fail("Must have finite interval start")
        val prevState =
            val input = tx.inputs.find { _.outRef === outRef }.getOrFail("Input not found")

            input.resolved.datum match
                case OutputDatum.OutputDatum(datum) => datum.to[ChainState]
                case _                              => fail("No datum")
        action match
            case Action.NewTip(blockHeader, ownerPubKey, signature) =>
                val state = datum.getOrFail("No datum")
                verifyNewTip(
                  intervalStartInSeconds,
                  prevState,
                  state,
                  blockHeader,
                  ownerPubKey,
                  signature
                )
            case Action.FraudProof(blockHeader) => verifyFraudProof(prevState, blockHeader)
    }

    def reverse(bs: ByteString): ByteString =
        val len = lengthOfByteString(bs)
        def loop(idx: BigInt, acc: ByteString): ByteString =
            if idx == len then acc
            else loop(idx + 1, consByteString(bs.at(idx), acc))
        loop(0, ByteString.empty)
}

val bitcoinProgram: Program =
    val sir = Compiler.compile(BitcoinValidator.validate)
    //    println(sir.showHighlighted)
    sir.toUplcOptimized(generateErrorTraces = false).plutusV3
//    sir.toUplcOptimized(using
//      Compiler.defaultOptions.copy(targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering)
//    )(generateErrorTraces = false)
//        .plutusV3
//    println(uplc.showHighlighted)
