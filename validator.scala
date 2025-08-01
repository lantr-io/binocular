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

extension (a: ByteString) def reverse: ByteString = ByteString.fromArray(a.bytes.reverse)

@Compile
object BitcoinValidator extends Validator {

    val UnixEpoch: BigInt = 1231006505
    val TargetBlockTime: BigInt = 600
    val DifficultyAdjustmentInterval: BigInt = 2016
    val MaxFutureBlockTime: BigInt = 7200 // 2 hours in the future in seconds
    val MedianTimeSpan: BigInt = 11
    val PowLimit = byteStringToInteger(true, hex"00000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffff")

    case class CoinbaseTx(
        version: ByteString,
        inputScriptSigAndSequence: ByteString,
        txOutsAndLockTime: ByteString
    )

    case class BlockHeader(bytes: ByteString)

    extension (bh: BlockHeader)
        inline def version: BigInt =
            byteStringToInteger(false, bh.bytes.slice(0, 4))

        inline def prevBlockHash: ByteString =
            sliceByteString(4, 32, bh.bytes)

        inline def bits: ByteString =
            sliceByteString(72, 4, bh.bytes)

        inline def merkleRoot: ByteString =
            sliceByteString(36, 32, bh.bytes)

        inline def timestamp: BigInt =
            byteStringToInteger(false, sliceByteString(68, 4, bh.bytes))

    case class ChainState(
        blockHeight: BigInt,
        blockHash: ByteString,
        currentDifficulty: BigInt,
        cumulativeDifficulty: BigInt,
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
        if e < BigInt(0) then throw new RuntimeException("Negative exponent")
        else pow(n, e)

    def bitsToBigInt(bits: ByteString): BigInt =
        val exponent = bits.at(3)
        val coefficient = byteStringToInteger(false, bits.slice(0, 3))
        if coefficient > BigInt(0x007fffff) then throw new RuntimeException("Negative bits")
        else if exponent < BigInt(3) then coefficient / pow(256, BigInt(3) - exponent)
        // check overflow
        else if coefficient != BigInt(0) && ((exponent > 34) ||
                (coefficient > 0xff && exponent > 33) ||
                (coefficient > 0xffff && exponent > 32))
        then throw new RuntimeException("Bits overflow")
        else
            val result = coefficient * pow(256, exponent - 3)
            if result > PowLimit then throw new RuntimeException("Bits over PowLimit")
            else result

    // Get median from reverse-sorted list (newest first)
    def getMedianTimePast(timestamps: List[BigInt], size: BigInt): BigInt =
        timestamps match
            case List.Nil => UnixEpoch
            case _ =>
                val index = size / 2
                // List is sorted, so just get middle element
                timestamps !! index

    def verifyTimestamp(timestamp: BigInt, medianTimePast: BigInt, currentTime: BigInt): Unit =
        require(timestamp <= currentTime + MaxFutureBlockTime, "Block timestamp too far in the future")
        require(timestamp > medianTimePast, "Block timestamp must be greater than median time of past 11 blocks")

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
                    then sha2_256(sha2_256(appendByteString(curHash, sibling)))
                    else sha2_256(sha2_256(appendByteString(sibling, curHash)))
                loop(index / 2, nextHash, siblings.tail)

        loop(index, hash, merkleProof)

    def readVarInt(input: ByteString, offset: BigInt): (BigInt, BigInt) =
        val firstByte = input.at(offset)
        if firstByte < BigInt(0xfd) then (firstByte, offset + 1)
        else if firstByte == BigInt(0xfd) then
            (byteStringToInteger(false, sliceByteString(offset + 1, 2, input)), offset + 3)
        else if firstByte == BigInt(0xfe) then
            (byteStringToInteger(false, sliceByteString(offset + 1, 4, input)), offset + 5)
        else (byteStringToInteger(false, sliceByteString(offset + 1, 8, input)), offset + 9)

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
        val scriptSig = sliceByteString(newOffset, scriptLength, coinbaseTx)
        scriptSig

    def parseBlockHeightFromScriptSig(scriptSig: ByteString): BigInt =
        val len = scriptSig.at(0)
        require(1 <= len && len <= 8, "Invalid block height length")
        val height = byteStringToInteger(false, scriptSig.slice(1, len))
        height

    def getBlockHeightFromCoinbaseTx(tx: CoinbaseTx): BigInt =
        val (scriptLength, newOffset) = readVarInt(tx.inputScriptSigAndSequence, 0)
        // read first byte of scriptSig
        // this MUST be OP_PUSHBYTES_len
        val len = tx.inputScriptSigAndSequence.at(newOffset)
        require(1 <= len && len <= 8, "Invalid block height length")
        val height = byteStringToInteger(false, tx.inputScriptSigAndSequence.slice(newOffset + 1, len))
        height

    def getTxHash(rawTx: ByteString): ByteString =
        val serializedTx = stripWitnessData(rawTx)
        sha2_256(sha2_256(serializedTx))

    def isWitnessTransaction(rawTx: ByteString): Boolean =
        rawTx.at(4) == BigInt(0) && rawTx.at(5) == BigInt(1)

    def stripWitnessData(rawTx: ByteString): ByteString =
        if isWitnessTransaction(rawTx) then
            // Format: [nVersion][marker][flag][txins][txouts][witness][nLockTime]
            val version = sliceByteString(0, 4, rawTx)
            val txInsStartIndex = BigInt(6) // Skip marker and flag bytes
            val txOutsOffset = skipTxIns(rawTx, txInsStartIndex)
            val outsEnd = skipTxOuts(rawTx, txOutsOffset)
            val txIns = sliceByteString(txInsStartIndex, txOutsOffset - txInsStartIndex, rawTx)
            val txOuts = sliceByteString(txOutsOffset, outsEnd - txOutsOffset, rawTx)
            val lockTimeOsset = outsEnd + 1 + 1 + 32 // Skip witness data
            val lockTime = sliceByteString(lockTimeOsset, 4, rawTx)
            appendByteString(version, appendByteString(txIns, appendByteString(txOuts, lockTime)))
        else rawTx

    def getCoinbaseTxHash(coinbaseTx: CoinbaseTx): ByteString =
        val serializedTx = appendByteString(
          coinbaseTx.version,
          appendByteString(
            hex"010000000000000000000000000000000000000000000000000000000000000000ffffffff",
            appendByteString(coinbaseTx.inputScriptSigAndSequence, coinbaseTx.txOutsAndLockTime)
          )
        )
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
            case List.Nil =>
                List.Cons(value, List.Nil)
            case List.Cons(head, tail) =>
                if value >= head then List.Cons(value, sortedValues)
                else List.Cons(head, insertReverseSorted(value, tail))

    def getNextWorkRequired(nHeight: BigInt, bits: BigInt, blockTime: BigInt, nFirstBlockTime: BigInt) = {
        // Only change once per difficulty adjustment interval
        if nHeight + 1 % DifficultyAdjustmentInterval == BigInt(0) then
            calculateNextWorkRequired(bits, blockTime, nFirstBlockTime)
        else bits
    }

    def calculateNextWorkRequired(bits: BigInt, blockTime: BigInt, nFirstBlockTime: BigInt): BigInt = {
        val PowTargetTimespan = DifficultyAdjustmentInterval * TargetBlockTime
        val actualTimespan =
            val timespan = blockTime - nFirstBlockTime
            min(
              max(timespan, PowTargetTimespan / 4),
              PowTargetTimespan * 4
            )

        val bnNew = bits * actualTimespan / PowTargetTimespan
        max(bnNew, PowLimit)
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

        // check proof of work
        val target = bitsToBigInt(blockHeader.bits)
        val validProofOfWork = hashBigInt <= target

        val nextDifficulty = getNextWorkRequired(
          prevState.blockHeight,
          target,
          blockTime,
          prevState.previousDifficultyAdjustmentTimestamp
        )
        val validDifficulty = target == nextDifficulty

        // Check blockTime against median of last 11 blocks
        val numTimestamps = prevState.recentTimestamps.size
        val medianTimePast = getMedianTimePast(prevState.recentTimestamps, numTimestamps)
        verifyTimestamp(blockTime, medianTimePast, currentTime)

        val newCumulativeDifficulty = prevState.cumulativeDifficulty + target
        val newDifficultyAdjustmentTimestamp =
            if prevState.blockHeight + 1 % DifficultyAdjustmentInterval == BigInt(0) then blockTime
            else prevState.previousDifficultyAdjustmentTimestamp

        // Insert new blockTime maintaining reverse sort order
        val withNewTimestamp = insertReverseSorted(blockTime, prevState.recentTimestamps)
        val newTimestamps = withNewTimestamp.take(MedianTimeSpan)

        // Reject blocks with outdated version

        val validVersion = blockHeader.version >= 4

        val validBlockHeader = validPrevBlockHash.?
            && validProofOfWork.?
            && validDifficulty.?
            && validVersion.?

        require(validBlockHeader, "Block header is not valid")
        ChainState(
          blockHeight = prevState.blockHeight + 1,
          blockHash = hash,
          currentDifficulty = nextDifficulty,
          cumulativeDifficulty = newCumulativeDifficulty,
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
    //    println(uplc.showHighlighted)
