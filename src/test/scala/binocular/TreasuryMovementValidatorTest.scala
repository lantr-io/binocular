package binocular

import binocular.bitcoin.*
import binocular.oracle.*
import binocular.watchtower.*

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry.ProofStep
import scalus.cardano.onchain.plutus.prelude.{List as PList, *}
import scalus.cardano.onchain.plutus.v1.{Address, Credential, PubKeyHash, Value}
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.v3.ScriptInfo.SpendingScript
import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF
import scalus.uplc.builtin.Builtins.integerToByteString
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData
import scalus.uplc.eval.PlutusVM

/** CEK-evaluation tests for [[TreasuryMovementValidator]] — the real (non-scaffold) treasury
  * movement Confirm validator.
  *
  * Builds a fully synthetic happy path: a small segwit TM tx, a single-tx Bitcoin block whose
  * merkle-root is that tx's txid, an oracle [[ChainState]] whose `confirmedBlocksRoot` is an
  * off-chain MPF holding the block hash, and the `Unconfirmed -> Confirmed` spend. Asserts the
  * contract accepts a proven confirmation and rejects tampering with the proof / parsed datum.
  */
class TreasuryMovementValidatorTest extends AnyFunSuite {

    private given PlutusVM = PlutusVM.makePlutusV3VM()

    // --- fixtures ---

    private val oracleHash = filled(0xcd, 28)
    private val tmScriptHash = filled(0xab, 28)
    private val authorityPkh = filled(0x7a, 28)
    private val tmValue = Value.lovelace(2_000_000)

    private def filled(v: Int, n: Int): ByteString =
        ByteString.fromArray(Array.fill[Byte](n)(v.toByte))

    /** A 2-in / 2-out segwit tx (empty witnesses) with known outpoints and outputs. */
    private val rawTm: ByteString = ByteString.fromHex(
      "02000000" + "0001" + // version, marker+flag
          "02" + // 2 inputs
          ("aa" * 32) + "00000000" + "00" + "ffffffff" + // in0
          ("bb" * 32) + "01000000" + "00" + "ffffffff" + // in1
          "02" + // 2 outputs
          "e803000000000000" + "16" + "0014" + ("11" * 20) + // out0: 1000 sats, P2WPKH
          "d007000000000000" + "16" + "0014" + ("22" * 20) + // out1: 2000 sats, P2WPKH
          "00" + "00" + // witnesses: 0 stack items per input
          "00000000" // locktime
    )

    private val txid = BitcoinHelpers.getTxHash(rawTm)

    // single-tx block: merkle-root == txid, so an empty merkle proof at index 0 verifies.
    private val blockHeader: ByteString =
        ByteString.fromHex("01000000") ++ filled(0x00, 32) ++ txid ++
            ByteString.fromHex("00000000") ++ ByteString.fromHex("ffff7f20") ++
            ByteString.fromHex("00000000")
    private val blockHash = BitcoinHelpers.blockHeaderHash(BlockHeader(blockHeader))

    private val mpf = OffChainMPF.empty.insert(blockHash, blockHash)
    private val mpfProof: PList[ProofStep] = mpf.proveMembership(blockHash)

    private val chainState = ChainState(
      confirmedBlocksRoot = mpf.rootHash,
      ctx = TraversalCtx(
        timestamps = PList.from(List(BigInt(1700000000))),
        height = BigInt(100),
        currentBits = integerToByteString(false, 4, BigInt(0x1d00ffff)),
        prevDiffAdjTimestamp = BigInt(1699990000),
        lastBlockHash = filled(0x00, 32)
      ),
      forkTree = ForkTree.End
    )

    private val expectedSwept: PList[ByteString] = PList.from(
      List(("aa" * 32) + "00000000", ("bb" * 32) + "01000000").map(ByteString.fromHex)
    )
    private val expectedFulfilled: PList[PegOutEntry] = PList.from(
      List(
        PegOutEntry(ByteString.fromHex("0014" + ("11" * 20)), BigInt(1000)),
        PegOutEntry(ByteString.fromHex("0014" + ("22" * 20)), BigInt(2000))
      )
    )

    private val ownRef = TxOutRef(TxId(filled(0x01, 32)), BigInt(0))
    private val unconfirmedDatum: Data = (TmDatum.Unconfirmed(rawTm): TmDatum).toData

    private def tmInput(value: Value, datum: Data) = TxInInfo(
      outRef = ownRef,
      resolved = TxOut(
        address = Address(Credential.ScriptCredential(tmScriptHash), Option.None),
        value = value,
        datum = OutputDatum.OutputDatum(datum),
        referenceScript = Option.None
      )
    )

    // The oracle NFT: policy = oracle script hash, empty asset name, qty 1.
    private val oracleNft: Value =
        Value.unsafeFromList(PList((oracleHash, PList((ByteString.empty, BigInt(1))))))

    private def oracleRefInput(value: Value = oracleNft) = TxInInfo(
      outRef = TxOutRef(TxId(filled(0x02, 32)), BigInt(0)),
      resolved = TxOut(
        address = Address(Credential.ScriptCredential(oracleHash), Option.None),
        value = value,
        datum = OutputDatum.OutputDatum(chainState.toData),
        referenceScript = Option.None
      )
    )

    private def confirmedOutput(value: Value, datum: Data) = TxOut(
      address = Address(Credential.ScriptCredential(tmScriptHash), Option.None),
      value = value,
      datum = OutputDatum.OutputDatum(datum),
      referenceScript = Option.None
    )

    private def redeemer(proof: PList[ProofStep], txIndex: BigInt = 0): Data =
        TmConfirmRedeemer(
          txIndex = txIndex,
          txMerkleProof = PList.Nil,
          blockMpfProof = proof,
          blockHeader = BlockHeader(blockHeader)
        ).toData

    private def scriptContext(
        outValue: Value,
        outDatum: Data,
        rdmr: Data,
        oracleRef: TxInInfo = oracleRefInput(),
        extraOutputs: PList[TxOut] = PList.Nil
    ): ScriptContext =
        ScriptContext(
          txInfo = TxInfo(
            inputs = PList.from(List(tmInput(tmValue, unconfirmedDatum))),
            referenceInputs = PList.from(List(oracleRef)),
            outputs = PList.Cons(confirmedOutput(outValue, outDatum), extraOutputs),
            id = TxId(filled(0x00, 32))
          ),
          redeemer = rdmr,
          scriptInfo = SpendingScript(ownRef, Option.Some(unconfirmedDatum))
        )

    private lazy val compiled = TreasuryMovementContract.contract(oracleHash, authorityPkh)
    private lazy val program = compiled.program.deBruijnedProgram
    // The TM NFT policy id == this script's own hash.
    private lazy val tmPolicy: ByteString = ByteString.fromArray(compiled.script.scriptHash.bytes)

    /** A minting ScriptContext: mint `nftQty` of the TM NFT, signed by `sigs`. */
    private def mintContext(nftQty: BigInt, sigs: PList[PubKeyHash]): ScriptContext =
        ScriptContext(
          txInfo = TxInfo(
            inputs = PList.from(List(tmInput(tmValue, unconfirmedDatum))),
            outputs = PList.Nil,
            mint = Value.unsafeFromList(PList((tmPolicy, PList((ByteString.empty, nftQty))))),
            signatories = sigs,
            id = TxId(filled(0x00, 32))
          ),
          redeemer = Data.unit,
          scriptInfo = ScriptInfo.MintingScript(tmPolicy)
        )

    private def confirmedDatum(
        swept: PList[ByteString] = expectedSwept,
        fulfilled: PList[PegOutEntry] = expectedFulfilled
    ): Data = (TmDatum.Confirmed(txid, swept, fulfilled): TmDatum).toData

    // --- tests ---

    test("contract compiles to UPLC and has a stable script hash") {
        val hash = compiled.script.scriptHash.toHex
        println(s"\n=== TreasuryMovementValidator script hash: $hash ===\n")
        assert(hash.length == 56)
    }

    test("TM NFT mint: +1 signed by the authority succeeds") {
        val sc = mintContext(BigInt(1), PList.from(List(PubKeyHash(authorityPkh))))
        assert(program.applyArg(sc.toData).evaluateDebug.isSuccess)
    }

    test("TM NFT mint: +1 without the authority signature fails") {
        val sc = mintContext(BigInt(1), PList.from(List(PubKeyHash(filled(0x11, 28)))))
        assert(!program.applyArg(sc.toData).evaluateDebug.isSuccess)
    }

    test("TM NFT mint: minting more than one fails") {
        val sc = mintContext(BigInt(2), PList.from(List(PubKeyHash(authorityPkh))))
        assert(!program.applyArg(sc.toData).evaluateDebug.isSuccess)
    }

    test("TM NFT burn: −1 is permissionless (no authority needed)") {
        val sc = mintContext(BigInt(-1), PList.Nil)
        assert(program.applyArg(sc.toData).evaluateDebug.isSuccess)
    }

    test("TmDatum / redeemer Data round-trip") {
        assert(unconfirmedDatum.to[TmDatum] == TmDatum.Unconfirmed(rawTm))
        val conf: TmDatum = TmDatum.Confirmed(txid, expectedSwept, expectedFulfilled)
        assert(conf.toData.to[TmDatum] == conf)
    }

    test("parses all input outpoints and all outputs from a raw TM") {
        assert(TreasuryMovementValidator.allInputOutpoints(rawTm) == expectedSwept)
        assert(TreasuryMovementValidator.allOutputs(rawTm) == expectedFulfilled)
    }

    test("proven confirmation with matching Confirmed datum succeeds") {
        val sc = scriptContext(tmValue, confirmedDatum(), redeemer(mpfProof))
        val result = program.applyArg(sc.toData).evaluateDebug
        assert(result.isSuccess, s"Expected success, got: $result")
    }

    test("tampered Confirmed datum (wrong peg-out amount) fails") {
        val wrongFulfilled = PList.from(
          List(
            PegOutEntry(ByteString.fromHex("0014" + ("11" * 20)), BigInt(999)), // tampered
            PegOutEntry(ByteString.fromHex("0014" + ("22" * 20)), BigInt(2000))
          )
        )
        val sc =
            scriptContext(tmValue, confirmedDatum(fulfilled = wrongFulfilled), redeemer(mpfProof))
        assert(!program.applyArg(sc.toData).evaluateDebug.isSuccess)
    }

    test("value not preserved (TM token dropped) fails") {
        val sc = scriptContext(Value.lovelace(1_000_000), confirmedDatum(), redeemer(mpfProof))
        assert(!program.applyArg(sc.toData).evaluateDebug.isSuccess)
    }

    test("oracle reference input without the oracle NFT fails") {
        val sc = scriptContext(
          tmValue,
          confirmedDatum(),
          redeemer(mpfProof),
          oracleRef = oracleRefInput(Value.lovelace(5_000_000)) // no NFT
        )
        assert(!program.applyArg(sc.toData).evaluateDebug.isSuccess)
    }

    test("a second output at the TM address fails") {
        val sc = scriptContext(
          tmValue,
          confirmedDatum(),
          redeemer(mpfProof),
          extraOutputs = PList.from(List(confirmedOutput(tmValue, confirmedDatum())))
        )
        assert(!program.applyArg(sc.toData).evaluateDebug.isSuccess)
    }

    test("block header not in oracle's confirmed-blocks root fails") {
        // Same merkle-root (bytes 36..68 = txid), but a different nonce → a different block hash
        // that the oracle's MPF does not contain. Isolates the MPF membership check.
        val tamperedHeader = blockHeader.slice(0, 76) ++ ByteString.fromHex("deadbeef")
        val rdmr = TmConfirmRedeemer(
          txIndex = 0,
          txMerkleProof = PList.Nil,
          blockMpfProof = mpfProof,
          blockHeader = BlockHeader(tamperedHeader)
        ).toData
        val sc = scriptContext(tmValue, confirmedDatum(), rdmr)
        assert(!program.applyArg(sc.toData).evaluateDebug.isSuccess)
    }
}
