package binocular

import scalus.*
import scalus.builtin.Builtins
import scalus.builtin.Builtins.*
import scalus.builtin.ByteString
import scalus.builtin.Data
import scalus.builtin.Data.FromData
import scalus.builtin.Data.ToData
import scalus.builtin.FromData
import scalus.builtin.FromDataInstances.given
import scalus.builtin.ToData
import scalus.builtin.ToDataInstances.given
import scalus.builtin.given
import scalus.ledger.api.v1.FromDataInstances.given
import scalus.ledger.api.v3.*
import scalus.ledger.api.v3.FromDataInstances.given
import scalus.prelude.?
import scalus.prelude.List
import scalus.prelude.Maybe
import scalus.prelude.Prelude.log
import scalus.sir.RemoveRecursivity
import scalus.uplc.Program
import scalus.uplc.Term
import scalus.utils.Utils

extension (a: Array[Byte]) def toHex: String = Utils.bytesToHex(a)

@Compile
object BitcoinValidator {

    case class BlockHeader(
        blockHeight: BigInt,
        hash: ByteString,
        version: BigInt,
        prevBlockHash: ByteString,
        merkleRoot: ByteString,
        timestamp: BigInt,
        bits: BigInt,
        nonce: BigInt
    )

    case class State(blockHeader: BlockHeader, ownerPubKey: ByteString, signature: ByteString)

    enum Action:
        case NewTip(
            blockHeader: BlockHeader,
            ownerPubKey: ByteString,
            signature: ByteString,
            coinbaseTx: ByteString,
            coinbaseInclusionProof: List[ByteString]
        )
        case FraudProof(
            blockHeader: BlockHeader
        )

    given FromData[BlockHeader] = FromData.deriveCaseClass[BlockHeader]
//    given ToData[BlockHeader] = ToData.deriveCaseClass[BlockHeader](0)
    given FromData[State] = FromData.deriveCaseClass[State]
//    given ToData[State] = ToData.deriveEnum[State]
    given FromData[Action] = FromData.deriveEnum[Action]
//    given ToData[Action] = ToData.deriveEnum[Action]

    /// Bitcoin block header serialization
    def serializeBlockHeader(blockHeader: BlockHeader): ByteString =
        val v = integerToByteString(false, 4, blockHeader.version)
        val pbh = reverse32(blockHeader.prevBlockHash)
        val mr = reverse32(blockHeader.merkleRoot)
        val ts = integerToByteString(false, 4, blockHeader.timestamp)
        val bits = integerToByteString(false, 4, blockHeader.bits)
        val nonce = integerToByteString(false, 4, blockHeader.nonce)
        appendByteString(
          v,
          appendByteString(
            pbh,
            appendByteString(mr, appendByteString(ts, appendByteString(bits, nonce)))
          )
        )

    def blockHeaderHash(blockHeader: BlockHeader): ByteString =
        reverse32(sha2_256(sha2_256(serializeBlockHeader(blockHeader))))

    def checkSignature(
        blockHeader: BlockHeader,
        ownerPubKey: ByteString,
        signature: ByteString
    ): Boolean =
        val hash = blockHeaderHash(blockHeader)
        verifyEd25519Signature(ownerPubKey, hash, signature)

    def verifyNewTip(
        state: State,
        blockHeader: BlockHeader,
        ownerPubKey: ByteString,
        signature: ByteString,
        coinbaseTx: ByteString,
        inclusionProof: List[ByteString]
    ): Unit =
        val hash = blockHeaderHash(blockHeader)
        val validHash = hash == blockHeader.hash
        val validSignature = verifyEd25519Signature(ownerPubKey, hash, signature)
        val isValid = validHash.? && validSignature.?
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
                verifyNewTip(state, blockHeader, ownerPubKey, signature, coinbaseTx, inclusionProof)
            case Action.FraudProof(blockHeader) => verifyFraudProof(state, blockHeader)
    }

    def reverse32(bs: ByteString): ByteString =
        def loop(idx: BigInt, acc: ByteString): ByteString =
            if idx > 31 then acc
            else loop(idx + 1, consByteString(indexByteString(bs, idx), acc))
        loop(0, ByteString.empty)
}

val compiledBitcoinValidator =
    Compiler.compile(BitcoinValidator.validator) |> RemoveRecursivity.apply
val bitcoinValidator: Term = compiledBitcoinValidator.toUplc(generateErrorTraces = false)
val bitcoinProgram: Program = Program((1, 1, 0), bitcoinValidator)
