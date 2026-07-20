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
    private val configNftPolicy = filled(0xc0, 28)
    private val configNftName = ByteString.fromHex("434f4e464947") // "CONFIG"
    // The TM UTxO carries the TM NFT (policy = the TM script's own hash — here the stand-in
    // `tmScriptHash` the input/output sit at — empty asset name, qty 1) plus some ADA. The spend
    // validator derives the NFT policy from the UTxO's own address and requires it on the continuing
    // output; the ADA need not be preserved.
    private val tmValue: Value =
        Value.unsafeFromList(
          PList(
            (ByteString.empty, PList((ByteString.empty, BigInt(2_000_000)))),
            (tmScriptHash, PList((ByteString.empty, BigInt(1))))
          )
        )

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

    private lazy val compiled =
        TreasuryMovementContract.contract(oracleHash, configNftPolicy, configNftName)
    private lazy val program = compiled.program.deBruijnedProgram
    // The TM NFT policy id == this script's own hash.
    private lazy val tmPolicy: ByteString = ByteString.fromArray(compiled.script.scriptHash.bytes)

    // The anchor outpoint = in0 of rawTm: aa*32 ++ 00000000 (txid internal order ++ vout LE).
    private val anchorOutpoint = ByteString.fromHex(("aa" * 32) + "00000000")

    /** A minimal 12-field config datum: only field 11 (initial_btc_treasury_utxo) matters here. */
    private def configDatum(anchor: ByteString): Data =
        Data.Constr(
          0,
          PList(
            Data.B(ByteString.empty),
            Data.B(ByteString.empty),
            Data.B(ByteString.empty),
            Data.B(ByteString.empty),
            Data.B(ByteString.empty),
            Data.B(ByteString.empty),
            Data.B(ByteString.empty),
            Data.B(ByteString.empty),
            Data.B(ByteString.empty),
            Data.I(0),
            Data.Constr(1, PList.empty),
            Data.B(anchor)
          )
        )

    /** The Config reference UTxO carrying the config NFT + a config datum with the anchor at field
      *   11. `withNft=false` simulates a forged config UTxO (right datum, no genuine NFT).
      */
    private def configRefInput(
        anchor: ByteString = anchorOutpoint,
        withNft: Boolean = true
    ): TxInInfo = TxInInfo(
      outRef = TxOutRef(TxId(filled(0x03, 32)), BigInt(0)),
      resolved = TxOut(
        address = Address(Credential.ScriptCredential(filled(0xc1, 28)), Option.None),
        value =
            if withNft then
                Value.unsafeFromList(PList((configNftPolicy, PList((configNftName, BigInt(1))))))
            else Value.lovelace(2_000_000),
        datum = OutputDatum.OutputDatum(configDatum(anchor)),
        referenceScript = Option.None
      )
    )

    /** A predecessor TM record UTxO with `Confirmed(prevTxid, [], [])` (or Unconfirmed when
      * `confirmed=false`), carrying the TM NFT unless `withNft=false`.
      */
    private def predecessorRefInput(
        prevTxid: ByteString,
        withNft: Boolean = true,
        confirmed: Boolean = true
    ): TxInInfo = TxInInfo(
      outRef = TxOutRef(TxId(filled(0x04, 32)), BigInt(0)),
      resolved = TxOut(
        address = Address(Credential.ScriptCredential(tmPolicy), Option.None),
        value =
            if withNft then
                Value.unsafeFromList(
                  PList(
                    (ByteString.empty, PList((ByteString.empty, BigInt(2_000_000)))),
                    (tmPolicy, PList((ByteString.empty, BigInt(1))))
                  )
                )
            else Value.lovelace(2_000_000),
        datum = OutputDatum.OutputDatum(
          if confirmed then (TmDatum.Confirmed(prevTxid, PList.Nil, PList.Nil): TmDatum).toData
          else unconfirmedDatum
        ),
        referenceScript = Option.None
      )
    )

    /** The freshly-posted Unconfirmed TM output the mint must bind the NFT to. */
    private def mintedTmOutput(
        datum: Data = unconfirmedDatum,
        credential: Credential = Credential.ScriptCredential(tmPolicy)
    ): TxOut = TxOut(
      address = Address(credential, Option.None),
      value = Value.unsafeFromList(
        PList(
          (ByteString.empty, PList((ByteString.empty, BigInt(2_000_000)))),
          (tmPolicy, PList((ByteString.empty, BigInt(1))))
        )
      ),
      datum = OutputDatum.OutputDatum(datum),
      referenceScript = Option.None
    )

    /** A minting ScriptContext: mint `nftQty` of the TM NFT with the given redeemer, reference
      * inputs, and outputs.
      */
    private def mintContext(
        nftQty: BigInt,
        rdmr: Data,
        refInputs: PList[TxInInfo],
        outputs: PList[TxOut]
    ): ScriptContext =
        ScriptContext(
          txInfo = TxInfo(
            inputs = PList.Nil,
            referenceInputs = refInputs,
            outputs = outputs,
            mint = Value.unsafeFromList(PList((tmPolicy, PList((ByteString.empty, nftQty))))),
            id = TxId(filled(0x00, 32))
          ),
          redeemer = rdmr,
          scriptInfo = ScriptInfo.MintingScript(tmPolicy)
        )

    private val genesisRdmr: Data = (TmMintRedeemer.Genesis: TmMintRedeemer).toData
    private def chainRdmr(i: BigInt): Data = (TmMintRedeemer.Chain(i): TmMintRedeemer).toData

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

    test("TM mint Genesis: +1 bound to Unconfirmed output, tx spends config anchor - succeeds") {
        val sc = mintContext(
          BigInt(1),
          genesisRdmr,
          PList.from(List(configRefInput())),
          PList.from(List(mintedTmOutput()))
        )
        val result = program.applyArg(sc.toData).evaluateDebug
        assert(result.isSuccess, s"Expected success, got: $result")
    }

    test("TM mint Genesis: wrong anchor outpoint fails") {
        val sc = mintContext(
          BigInt(1),
          genesisRdmr,
          PList.from(List(configRefInput(anchor = filled(0xee, 36)))),
          PList.from(List(mintedTmOutput()))
        )
        assert(!program.applyArg(sc.toData).evaluateDebug.isSuccess)
    }

    test("TM mint Genesis: config ref input without the config NFT fails") {
        val sc = mintContext(
          BigInt(1),
          genesisRdmr,
          PList.from(List(configRefInput(withNft = false))),
          PList.from(List(mintedTmOutput()))
        )
        assert(!program.applyArg(sc.toData).evaluateDebug.isSuccess)
    }

    test("TM mint Chain: predecessor Confirmed(txid=aa*32), tx spends (aa*32, 0) - succeeds") {
        val sc = mintContext(
          BigInt(1),
          chainRdmr(0),
          PList.from(List(predecessorRefInput(prevTxid = filled(0xaa, 32)))),
          PList.from(List(mintedTmOutput()))
        )
        val result = program.applyArg(sc.toData).evaluateDebug
        assert(result.isSuccess, s"Expected success, got: $result")
    }

    test("TM mint Chain: wrong predecessor txid fails") {
        val sc = mintContext(
          BigInt(1),
          chainRdmr(0),
          PList.from(List(predecessorRefInput(prevTxid = filled(0xbb, 32)))),
          PList.from(List(mintedTmOutput()))
        )
        assert(!program.applyArg(sc.toData).evaluateDebug.isSuccess)
    }

    test("TM mint Chain: predecessor without the TM NFT fails") {
        val sc = mintContext(
          BigInt(1),
          chainRdmr(0),
          PList.from(List(predecessorRefInput(prevTxid = filled(0xaa, 32), withNft = false))),
          PList.from(List(mintedTmOutput()))
        )
        assert(!program.applyArg(sc.toData).evaluateDebug.isSuccess)
    }

    test("TM mint Chain: Unconfirmed predecessor fails") {
        val sc = mintContext(
          BigInt(1),
          chainRdmr(0),
          PList.from(List(predecessorRefInput(prevTxid = filled(0xaa, 32), confirmed = false))),
          PList.from(List(mintedTmOutput()))
        )
        assert(!program.applyArg(sc.toData).evaluateDebug.isSuccess)
    }

    test("TM mint: NFT output at a foreign credential fails") {
        val sc = mintContext(
          BigInt(1),
          genesisRdmr,
          PList.from(List(configRefInput())),
          PList.from(
            List(mintedTmOutput(credential = Credential.ScriptCredential(filled(0x99, 28))))
          )
        )
        assert(!program.applyArg(sc.toData).evaluateDebug.isSuccess)
    }

    test("TM mint: NFT output with a Confirmed datum fails") {
        val sc = mintContext(
          BigInt(1),
          genesisRdmr,
          PList.from(List(configRefInput())),
          PList.from(List(mintedTmOutput(datum = confirmedDatum())))
        )
        assert(!program.applyArg(sc.toData).evaluateDebug.isSuccess)
    }

    test("TM mint: minting more than one fails") {
        val sc = mintContext(
          BigInt(2),
          genesisRdmr,
          PList.from(List(configRefInput())),
          PList.from(List(mintedTmOutput()))
        )
        assert(!program.applyArg(sc.toData).evaluateDebug.isSuccess)
    }

    test("TM NFT burn: -1 is permissionless (no authority needed)") {
        val sc = mintContext(BigInt(-1), Data.unit, PList.Nil, PList.Nil)
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

    test("TM NFT dropped from the continuing output fails") {
        // Continuing output keeps only ADA, no TM NFT — must be rejected (the NFT authenticates the
        // Confirmed UTxO downstream).
        val sc = scriptContext(Value.lovelace(1_000_000), confirmedDatum(), redeemer(mpfProof))
        assert(!program.applyArg(sc.toData).evaluateDebug.isSuccess)
    }

    test("lovelace reduced but TM NFT preserved succeeds") {
        // The exact Value need NOT be preserved — only the TM NFT. A smaller Confirmed datum means a
        // smaller min-UTxO; the lovelace difference (fees / watchtower reward) is allowed.
        val reduced =
            Value.unsafeFromList(
              PList(
                (ByteString.empty, PList((ByteString.empty, BigInt(1_000_000)))),
                (tmScriptHash, PList((ByteString.empty, BigInt(1))))
              )
            )
        val sc = scriptContext(reduced, confirmedDatum(), redeemer(mpfProof))
        val result = program.applyArg(sc.toData).evaluateDebug
        assert(result.isSuccess, s"Expected success, got: $result")
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
