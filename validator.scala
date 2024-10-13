package binocular

import scalus.*
import scalus.builtin.Builtins
import scalus.builtin.Builtins.*
import scalus.builtin.ByteString
import scalus.builtin.Data
import scalus.builtin.Data.FromData
import scalus.builtin.Data.ToData
import scalus.builtin.FromData
import scalus.builtin.List
import scalus.builtin.ToData
import scalus.builtin.ToDataInstances.given
import scalus.builtin.FromDataInstances.given
import scalus.builtin.given
import scalus.ledger.api.v1.FromDataInstances.given
import scalus.ledger.api.v3.FromDataInstances.given
import scalus.ledger.api.v3.*
import scalus.prelude.?
import scalus.prelude.Maybe
import scalus.prelude.Prelude.log
import scalus.utils.Utils
import scalus.uplc.Term
import scalus.uplc.Program
import scalus.sir.RemoveRecursivity

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
    enum State:
        case Initial
        case Open(blockHeader: BlockHeader)

    enum Action:
        case NewTip(blockHeader: BlockHeader)

    given FromData[BlockHeader] = FromData.deriveCaseClass[BlockHeader]
//    given ToData[BlockHeader] = ToData.deriveCaseClass[BlockHeader](0)
//    given FromData[State] = FromData.deriveEnum[State]
//    given ToData[State] = ToData.deriveEnum[State]
    given FromData[Action] = FromData.deriveEnum[Action]
//    given ToData[Action] = ToData.deriveEnum[Action]

    def reverse32(bs: ByteString): ByteString =
        import consByteString as cons
        val index = indexByteString(bs, _)
        cons(
          index(31),
          cons(
            index(30),
            cons(
              index(29),
              cons(
                index(28),
                cons(
                  index(27),
                  cons(
                    index(26),
                    cons(
                      index(25),
                      cons(
                        index(24),
                        cons(
                          index(23),
                          cons(
                            index(22),
                            cons(
                              index(21),
                              cons(
                                index(20),
                                cons(
                                  index(19),
                                  cons(
                                    index(18),
                                    cons(
                                      index(17),
                                      cons(
                                        index(16),
                                        cons(
                                          index(15),
                                          cons(
                                            index(14),
                                            cons(
                                              index(13),
                                              cons(
                                                index(12),
                                                cons(
                                                  index(11),
                                                  cons(
                                                    index(10),
                                                    cons(
                                                      index(9),
                                                      cons(
                                                        index(8),
                                                        cons(
                                                          index(7),
                                                          cons(
                                                            index(6),
                                                            cons(
                                                              index(5),
                                                              cons(
                                                                index(4),
                                                                cons(
                                                                  index(3),
                                                                  cons(
                                                                    index(2),
                                                                    cons(
                                                                      index(1),
                                                                      cons(
                                                                        index(0),
                                                                        ByteString.empty
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )

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
        val serialized = serializeBlockHeader(blockHeader)
        reverse32(sha2_256(sha2_256(serialized)))

    def validator(ctx: Data): Unit = {
        val action = ctx.field[ScriptContext](_.redeemer).to[Action]
        val scriptInfo = ctx.field[ScriptContext](_.scriptInfo).to[SpendingScriptInfo]
//        val state = scriptInfo.datum match
//            case Maybe.Just(state) => state.to[State]
//            case _                 => throw new Exception("No datum")
        action match
            case Action.NewTip(blockHeader) =>
                val hash = serializeBlockHeader(blockHeader)
                ()
        ()
    }
}

val compiledBitcoinValidator =
    Compiler.compile(BitcoinValidator.validator) |> RemoveRecursivity.apply
val bitcoinValidator: Term = compiledBitcoinValidator.toUplc(generateErrorTraces = false)
val bitcoinProgram: Program = Program((1, 1, 0), bitcoinValidator)
