package binocular

import scalus.*
import scalus.builtin
import scalus.builtin.Builtins
import scalus.builtin.Builtins.*
import scalus.builtin.ByteString
import scalus.builtin.Data
import scalus.builtin.Data.FromData
import scalus.builtin.Data.ToData
import scalus.builtin.FromData
import scalus.builtin.ByteString.given
import scalus.builtin.FromDataInstances.given
import scalus.builtin.ToData
import scalus.builtin.ToDataInstances.given
import scalus.builtin.given
import scalus.ledger.api.v3.*
import scalus.ledger.api.v3.FromDataInstances.given
import scalus.prelude.?
import scalus.prelude.Maybe
import scalus.sir.RemoveRecursivity
import scalus.uplc.Program
import scalus.uplc.Term
import scalus.utils.Utils

extension (a: Array[Byte]) def toHex: String = Utils.bytesToHex(a)
extension (a: ByteString) def reverse: ByteString = ByteString.fromArray(a.bytes.reverse)

@Compile
object BitcoinValidator {

    case class CoinbaseTx(
        version: ByteString,
        inputScriptSigAndSequence: ByteString,
        txOutsAndLockTime: ByteString
    )

    case class BlockHeader(
        blockHeight: BigInt,
        hash: ByteString,
        version: BigInt,
        prevBlockHash: ByteString,
        merkleRoot: ByteString,
        timestamp: BigInt,
        bits: ByteString,
        nonce: ByteString
    )

    case class State(a: BigInt)

    enum Action:
        case NewTip(
            blockHeader: BlockHeader,
            ownerPubKey: ByteString,
            signature: ByteString,
            coinbaseTx: CoinbaseTx,
            coinbaseInclusionProof: Data
        )
        case FraudProof(
            blockHeader: BlockHeader
        )

    given FromData[CoinbaseTx] = FromData.deriveCaseClass[CoinbaseTx]
    given ToData[CoinbaseTx] = ToData.deriveCaseClass[CoinbaseTx](0)
    given FromData[BlockHeader] = FromData.deriveCaseClass[BlockHeader]
    given ToData[BlockHeader] = ToData.deriveCaseClass[BlockHeader](0)
    given FromData[State] = FromData.deriveCaseClass[State]
    given ToData[State] = ToData.deriveCaseClass[State](0)
    given FromData[Data] = (data: Data) => data
    given FromData[Action] = FromData.deriveEnum[Action]
    given ToData[Action] = ToData.deriveEnum[Action]

    /// Bitcoin block header serialization
    def serializeBlockHeader(blockHeader: BlockHeader): ByteString =
        val v = integerToByteString(false, 4, blockHeader.version)
        val pbh = reverse(blockHeader.prevBlockHash)
        val mr = reverse(blockHeader.merkleRoot)
        val ts = integerToByteString(false, 4, blockHeader.timestamp)
        appendByteString(
          v,
          appendByteString(
            pbh,
            appendByteString(
              mr,
              appendByteString(ts, appendByteString(blockHeader.bits, blockHeader.nonce))
            )
          )
        )

    def blockHeaderHash(blockHeader: BlockHeader): ByteString =
        reverse(sha2_256(sha2_256(serializeBlockHeader(blockHeader))))

    def checkSignature(
        blockHeader: BlockHeader,
        ownerPubKey: ByteString,
        signature: ByteString
    ): Boolean =
        val hash = blockHeaderHash(blockHeader)
        verifyEd25519Signature(ownerPubKey, hash, signature)

    def bitsToTarget(bits: ByteString): ByteString =
        val exponent = indexByteString(bits, 3)
        val coefficient = sliceByteString(0, 3, bits)
        // produce a 32 byte ByteString target, where the `coefficient` is 3 bytes and it is shifted left by `exponent` bytes and left padded with zeros
        def loop(i: BigInt, acc: ByteString): ByteString =
            if i < exponent then loop(i + 1, consByteString(0, acc))
            else if i == exponent then loop(i + 3, appendByteString(reverse(coefficient), acc))
            else if i < 32 then loop(i + 1, consByteString(0, acc))
            else acc
        val target = loop(0, ByteString.empty)
        val maxTarget = hex"00000000ffff0000000000000000000000000000000000000000000000000000"
        if lessThanByteString(maxTarget, target) then maxTarget else target

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
        val firstByte = indexByteString(input, offset)
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
        val len = indexByteString(scriptSig, 0)
        if len < 1 || len > 8 then throw new Exception("Invalid block height length")
        val height = byteStringToInteger(false, sliceByteString(1, len, scriptSig))
        height

    def getBlockHeightFromCoinbaseTx(tx: CoinbaseTx): BigInt =
        val (scriptLength, newOffset) = readVarInt(tx.inputScriptSigAndSequence, 0)
        // read first byte of scriptSig
        // this MUST be OP_PUSHBYTES_len
        val len = indexByteString(tx.inputScriptSigAndSequence, newOffset)
        if len < 1 || len > 8 then throw new Exception("Invalid block height length")
        val height = byteStringToInteger(false, sliceByteString(newOffset + 1, len, tx.inputScriptSigAndSequence))
        height

    def getTxHash(rawTx: ByteString): ByteString =
        val serializedTx = stripWitnessData(rawTx)
        sha2_256(sha2_256(serializedTx))

    def isWitnessTransaction(rawTx: ByteString): Boolean =
        indexByteString(rawTx, 4) == BigInt(0) && indexByteString(rawTx, 5) == BigInt(1)

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

    def verifyNewTip(
        state: State,
        blockHeader: BlockHeader,
        ownerPubKey: ByteString,
        signature: ByteString,
        coinbaseTx: CoinbaseTx,
        inclusionProof: builtin.List[Data]
    ): Unit =
        val hash = blockHeaderHash(blockHeader)
        val validHash = hash == blockHeader.hash
        val target = bitsToTarget(blockHeader.bits)
        val blockHashSatisfiesTarget = lessThanByteString(hash, target)
        val height = getBlockHeightFromCoinbaseTx(coinbaseTx)
        val validBlockHeight = height == blockHeader.blockHeight

        val validSignature = verifyEd25519Signature(ownerPubKey, hash, signature)
        val coinbaseTxHash = getCoinbaseTxHash(coinbaseTx)
        val merkleRoot = merkleRootFromInclusionProof(inclusionProof, coinbaseTxHash, 0)
        val validCoinbaseInclusionProof = merkleRoot == reverse(blockHeader.merkleRoot)

        val isValid = validHash.?
            && validSignature.?
            && validCoinbaseInclusionProof.?
            && blockHashSatisfiesTarget.?
            && validBlockHeight.?
        if isValid then () else throw new Exception("Block is not valid")

    def verifyFraudProof(state: State, blockHeader: BlockHeader): Unit =
        ()

    def validator(ctx: Data): Unit = {
        val action = ctx.field[ScriptContext](_.redeemer).to[Action]
        val scriptInfo = ctx.field[ScriptContext](_.scriptInfo).to[SpendingScriptInfo]
        val state = scriptInfo.datum match
            case Maybe.Just(state) => state.to[State]
            case _                 => throw new Exception("No datum")
        action match
            case Action.NewTip(blockHeader, ownerPubKey, signature, coinbaseTx, inclusionProof) =>
                verifyNewTip(
                  state,
                  blockHeader,
                  ownerPubKey,
                  signature,
                  coinbaseTx,
                  inclusionProof.toList
                )
            case Action.FraudProof(blockHeader) => verifyFraudProof(state, blockHeader)
    }

    def reverse(bs: ByteString): ByteString =
        val len = lengthOfByteString(bs)
        def loop(idx: BigInt, acc: ByteString): ByteString =
            if idx == len then acc
            else loop(idx + 1, consByteString(indexByteString(bs, idx), acc))
        loop(0, ByteString.empty)
}

val compiledBitcoinValidator =
    Compiler.compile(BitcoinValidator.validator) |> RemoveRecursivity.apply
val bitcoinValidator: Term = compiledBitcoinValidator.toUplc(generateErrorTraces = false)
val bitcoinProgram: Program = Program((1, 1, 0), bitcoinValidator)
