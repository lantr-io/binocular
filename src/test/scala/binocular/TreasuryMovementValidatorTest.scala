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
import scalus.cardano.ledger.CardanoInfo
import scalus.uplc.eval.{PlutusVM, Result}

/** CEK-evaluation tests for [[TreasuryMovementValidator]] — the rev-5.4 treasury movement
  * validator: Confirm retires the TM record and advances the bridge-state singleton.
  *
  * Builds a fully synthetic happy path: a small segwit TM tx carrying one `"BTMR1"` two-root
  * commitment, a single-tx Bitcoin block whose merkle-root is that tx's txid, an oracle
  * [[ChainState]] whose `confirmedBlocksRoot` is an off-chain MPF holding the block hash, and the
  * Confirm spend that burns the TM NFT and rewrites the singleton's [[BridgeState]]. Asserts the
  * contract accepts a proven confirmation and rejects tampering with the proof, the linkage, and
  * the singleton datum ([CTM-17] through [CTM-30]).
  */
class TreasuryMovementValidatorTest extends AnyFunSuite {

    private given PlutusVM = PlutusVM.makePlutusV3VM()

    // --- fixtures ---

    private val oracleHash = filled(0xcd, 28)
    private val tmScriptHash = filled(0xab, 28)
    private val configNftPolicy = filled(0xc0, 28)
    private val configNftName = ByteString.fromHex("434f4e464947") // "CONFIG"
    // The bridge-state singleton: policy id published in Config field 3 (`bridge_state_policy`),
    // asset name "BSS", and the UTxO sits at the bridge_state script's own address.
    private val bssPolicy = filled(0xb5, 28)
    private val bssName = ByteString.fromString("BSS")
    // The TM UTxO carries the TM NFT (policy = the TM script's own hash — here the stand-in
    // `tmScriptHash` the input sits at — empty asset name, qty 1) plus some ADA.
    private val tmValue: Value =
        Value.unsafeFromList(
          PList(
            (ByteString.empty, PList((ByteString.empty, BigInt(2_000_000)))),
            (tmScriptHash, PList((ByteString.empty, BigInt(1))))
          )
        )

    private def filled(v: Int, n: Int): ByteString =
        ByteString.fromArray(Array.fill[Byte](n)(v.toByte))

    // --- raw TM builder -------------------------------------------------------------------------
    // TM output layout: [0] = treasury change, [1..m] = peg-out payments, [m+1] = the ONE "BTMR1"
    // two-root commitment. The commitment holds the swept-peg-ins and completed-peg-outs roots that
    // must hold after this TM; the FROST quorum signed them, and Confirm copies them into the
    // singleton ([CTM-20]/[CTM-30]).

    private val changeSpk = ByteString.fromHex("0014" + ("11" * 20))
    private val paySpk1 = ByteString.fromHex("0014" + ("22" * 20))
    private val paySpk2 = ByteString.fromHex("0014" + ("33" * 20))
    private val paySpk3 = ByteString.fromHex("0014" + ("44" * 20))

    // The two attested roots. ATTESTED, not verified ([CTM-20]/[CTM-30] only COPY them), so any
    // 32-byte constants serve.
    private val spiRootA = filled(0x5a, 32)
    private val cpoRootA = filled(0x6b, 32)

    /** `OP_RETURN OP_PUSHBYTES_69 <tag ++ spi ++ cpo>` — 71 script bytes. `tag` is "BTMR1"
      * (42544d5231) for a genuine commitment; the wrong-prefix test passes "BTMR2".
      */
    private def btmr1Spk(
        spi: ByteString = spiRootA,
        cpo: ByteString = cpoRootA,
        tag: String = "42544d5231"
    ): ByteString =
        ByteString.fromHex("6a45" + tag) ++ spi ++ cpo

    /** A 2-in segwit tx (empty witnesses, fixed outpoints) with the given `(scriptPubKey, sats)`
      * outputs. Input 0 spends `(aa*32, vout 0)` — the head the singleton fixture carries.
      */
    private def rawTxWith(outs: List[(ByteString, BigInt)]): ByteString = {
        val outsHex = outs.map { case (spk, amt) =>
            integerToByteString(false, 8, amt).toHex + f"${spk.size}%02x" + spk.toHex
        }.mkString
        ByteString.fromHex(
          "02000000" + "0001" + // version, marker+flag
              "02" + // 2 inputs
              ("aa" * 32) + "00000000" + "00" + "ffffffff" + // in0
              ("bb" * 32) + "01000000" + "00" + "ffffffff" + // in1
              f"${outs.size}%02x" + outsHex +
              "00" + "00" + // witnesses: 0 stack items per input
              "00000000" // locktime
        )
    }

    /** The default TM: treasury change + ONE fulfilled peg-out payment + the two-root commitment.
      */
    private val rawTm: ByteString = rawTxWith(
      List(
        (changeSpk, BigInt(1000)),
        (paySpk1, BigInt(2000)),
        (btmr1Spk(), BigInt(0))
      )
    )

    private val txid = BitcoinHelpers.getTxHash(rawTm)

    /** Input 0 of every `rawTxWith` tx: the head the spent singleton must carry ([CTM-18]). */
    private val treasuryHead: ByteString = ByteString.fromHex(("aa" * 32) + "00000000")
    private val zeroVout: ByteString = ByteString.fromHex("00000000")

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
        PegOutEntry(changeSpk, BigInt(1000)),
        PegOutEntry(paySpk1, BigInt(2000)),
        PegOutEntry(btmr1Spk(), BigInt(0))
      )
    )

    // --- bridge-state singleton fixtures ---------------------------------------------------------

    private val bssAddress = Address(Credential.ScriptCredential(bssPolicy), Option.None)

    private def bssValue(policy: ByteString = bssPolicy): Value =
        Value.unsafeFromList(
          PList(
            (ByteString.empty, PList((ByteString.empty, BigInt(2_000_000)))),
            (policy, PList((bssName, BigInt(1))))
          )
        )

    /** The state the singleton holds BEFORE the Confirm: stale-by-construction roots and amount the
      * Confirm must overwrite, and the head the TM's input 0 spends ([CTM-18]).
      */
    private def prevState(head: ByteString = treasuryHead): BridgeState =
        BridgeState(
          spiRoot = filled(0x01, 32),
          cpoRoot = filled(0x02, 32),
          treasuryUtxoId = head,
          treasuryAmount = BigInt(5000)
        )

    /** The state the Confirm must write for the default `rawTm` ([CTM-27]): both attested roots,
      * head = `txid ‖ 00000000` ([CTM-19]), amount = output 0's satoshis ([CTM-21]).
      */
    private val defaultNewState: BridgeState =
        BridgeState(
          spiRoot = spiRootA,
          cpoRoot = cpoRootA,
          treasuryUtxoId = txid ++ zeroVout,
          treasuryAmount = BigInt(1000)
        )

    private def bssInput(
        state: BridgeState = prevState(),
        value: Value = bssValue()
    ): TxInInfo = TxInInfo(
      outRef = TxOutRef(TxId(filled(0x06, 32)), BigInt(0)),
      resolved = TxOut(
        address = bssAddress,
        value = value,
        datum = OutputDatum.OutputDatum(state.toData),
        referenceScript = Option.None
      )
    )

    private def bssOutput(
        state: BridgeState = defaultNewState,
        value: Value = bssValue(),
        address: Address = bssAddress
    ): TxOut = bssOutputWithDatum(state.toData, value, address)

    /** A continuing singleton output carrying an ARBITRARY datum — for the malformed-datum tests.
      */
    private def bssOutputWithDatum(
        datum: Data,
        value: Value = bssValue(),
        address: Address = bssAddress
    ): TxOut = TxOut(
      address = address,
      value = value,
      datum = OutputDatum.OutputDatum(datum),
      referenceScript = Option.None
    )

    private val ownRef = TxOutRef(TxId(filled(0x01, 32)), BigInt(0))
    private val creatorPkh = PubKeyHash(filled(0x7a, 28))
    private val createdAt: BigInt = BigInt("1700000000000")
    // Rev-5.1 DA hint: 36-byte Cardano outpoints of the PegOutRequests this TM fulfills. NOTHING
    // on-chain reads it, so the default fixture carries a non-empty one — every happy-path test then
    // doubles as evidence that mint and confirm ignore its content.
    private val porOutpointHint: PList[ByteString] =
        PList.from(List(filled(0x31, 32) ++ zeroVout))
    private def unconfirmedDatumWith(hint: PList[ByteString], rawTx: ByteString = rawTm): Data =
        UnconfirmedTm(rawTx, creatorPkh, createdAt, hint).toData
    private val unconfirmedDatum: Data = unconfirmedDatumWith(porOutpointHint)

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

    private def confirmProof(
        proof: PList[ProofStep] = mpfProof,
        header: ByteString = blockHeader,
        txIndex: BigInt = 0
    ): TmConfirmProof =
        TmConfirmProof(
          txIndex = txIndex,
          txMerkleProof = PList.Nil,
          blockMpfProof = proof,
          blockHeader = BlockHeader(header)
        )

    private def confirmRdmr(proof: TmConfirmProof = confirmProof()): Data =
        (TmSpendRedeemer.Confirm(proof): TmSpendRedeemer).toData

    private val gcRdmr: Data = (TmSpendRedeemer.Gc: TmSpendRedeemer).toData

    /** A Confirm ScriptContext over the default `rawTm` (one fulfilled peg-out). The singleton is
      * spent (stale state, matching head) and recreated with the state that TM attests; the Config
      * reference input publishes `bssPolicy` at field 3; the TM NFT is burned.
      *
      * `outputs` is the WHOLE output list — the default has ONLY the continuing singleton
      * ([CTM-25]: nothing at the TM address).
      */
    private def scriptContext(
        rdmr: Data,
        oracleRef: TxInInfo = oracleRefInput(),
        extraInputs: List[TxInInfo] = List(bssInput()),
        outputs: List[TxOut] = List(bssOutput()),
        cfgRefs: List[TxInInfo] = List(configRefInput()),
        tmDatum: Data = unconfirmedDatum,
        burnQty: BigInt = BigInt(-1)
    ): ScriptContext =
        ScriptContext(
          txInfo = TxInfo(
            inputs = PList.from(tmInput(tmValue, tmDatum) :: extraInputs),
            referenceInputs = PList.from(oracleRef :: cfgRefs),
            outputs = PList.from(outputs),
            mint =
                if burnQty == BigInt(0) then Value.zero
                else
                    Value.unsafeFromList(
                      PList((tmScriptHash, PList((ByteString.empty, burnQty))))
                    )
            ,
            id = TxId(filled(0x00, 32))
          ),
          redeemer = rdmr,
          scriptInfo = SpendingScript(ownRef, Option.Some(tmDatum))
        )

    /** A Confirm ScriptContext for an ARBITRARY raw TM: derives its txid, wraps it in a single-tx
      * block, builds the oracle state proving that block, and wires the singleton in/out pair plus
      * the Config reference input. `newState` is what the continuing singleton claims — for a
      * well-formed TM the caller passes the state its commitment attests.
      */
    private def confirmContextFor(
        rawTx: ByteString,
        newState: BridgeState
    ): ScriptContext = {
        val id = BitcoinHelpers.getTxHash(rawTx)
        val hdr = ByteString.fromHex("01000000") ++ filled(0x00, 32) ++ id ++
            ByteString.fromHex("00000000") ++ ByteString.fromHex("ffff7f20") ++
            ByteString.fromHex("00000000")
        val bh = BitcoinHelpers.blockHeaderHash(BlockHeader(hdr))
        val obMpf = OffChainMPF.empty.insert(bh, bh)
        val oracleRef = TxInInfo(
          outRef = TxOutRef(TxId(filled(0x02, 32)), BigInt(0)),
          resolved = TxOut(
            address = Address(Credential.ScriptCredential(oracleHash), Option.None),
            value = oracleNft,
            datum = OutputDatum.OutputDatum(
              chainState.copy(confirmedBlocksRoot = obMpf.rootHash).toData
            ),
            referenceScript = Option.None
          )
        )
        val unconf: Data = unconfirmedDatumWith(porOutpointHint, rawTx)
        val rdmr: Data = (TmSpendRedeemer.Confirm(
          TmConfirmProof(
            txIndex = 0,
            txMerkleProof = PList.Nil,
            blockMpfProof = obMpf.proveMembership(bh),
            blockHeader = BlockHeader(hdr)
          )
        ): TmSpendRedeemer).toData
        ScriptContext(
          txInfo = TxInfo(
            inputs = PList.from(List(tmInput(tmValue, unconf), bssInput())),
            referenceInputs = PList.from(List(oracleRef, configRefInput())),
            outputs = PList.from(List(bssOutput(newState))),
            mint = Value.unsafeFromList(
              PList((tmScriptHash, PList((ByteString.empty, BigInt(-1)))))
            ),
            id = TxId(filled(0x00, 32))
          ),
          redeemer = rdmr,
          scriptInfo = SpendingScript(ownRef, Option.Some(unconf))
        )
    }

    /** The state a well-formed `rawTx` attests: its committed roots, its txid as the head, and its
      * output 0's satoshi amount — the off-chain construction of what [CTM-27] pins.
      */
    private def attestedState(rawTx: ByteString): BridgeState = {
        val (spi, cpo) = SweptPegInsTrie
            .committedRoots(
              TreasuryMovementValidator.allOutputs(rawTx).asScala.toSeq
            )
            .toOption
            .get
        BridgeState(
          spiRoot = spi,
          cpoRoot = cpo,
          treasuryUtxoId = BitcoinHelpers.getTxHash(rawTx) ++ zeroVout,
          treasuryAmount = TreasuryMovementValidator.allOutputs(rawTx).head.amount
        )
    }

    private lazy val compiled =
        TreasuryMovementContract.contract(oracleHash, configNftPolicy, configNftName)
    private lazy val program = compiled.program.deBruijnedProgram
    // The TM NFT policy id == this script's own hash.
    private lazy val tmPolicy: ByteString = ByteString.fromArray(compiled.script.scriptHash.bytes)

    // The trace-instrumented twin: IDENTICAL validator source compiled with
    // generateErrorTraces = true. The release compile strips trace strings, so a rejection there
    // reports only "Error evaluated" with no clue WHICH check failed. Rejection tests re-run the
    // same script context through this twin to pin the reason — see [[assertRejects]].
    private lazy val debugProgram =
        TreasuryMovementDebugContract.parameterized
            .apply(oracleHash)
            .apply(configNftPolicy)
            .apply(configNftName)
            .program
            .deBruijnedProgram

    /** Assert the DEPLOYED script rejects `sc`, and that it rejects it for `reason` (matched
      * against the trace log of the debug twin).
      *
      * Why the second half matters: several rejection fixtures are multi-cause. Pinning the message
      * makes each test fail for its own reason.
      */
    private def assertRejects(sc: ScriptContext, reason: String): Unit = {
        val ctx = sc.toData
        assert(!program.applyArg(ctx).evaluateDebug.isSuccess, "expected the script to reject")
        debugProgram.applyArg(ctx).evaluateDebug match
            case _: Result.Success =>
                fail("the trace twin ACCEPTED a context the release script rejected")
            case r: Result.Failure =>
                assert(
                  r.logs.exists(_.contains(reason)),
                  s"expected failure reason '$reason', got: ${r.logs.mkString(" | ")}"
                )
    }

    /** The real eight-field rev-5.4 [[ConfigDatum]] mirror. The Confirm path and the mint path read
      * field 3 (`bridge_state_policy`). The rest are inert here.
      */
    private def configDatum(
        bridgeStatePolicyArg: ByteString = bssPolicy
    ): Data = ConfigDatum(
      updateAuth = Option.None,
      bridgedTokenPolicy = ByteString.empty,
      completedPegInsPolicy = ByteString.empty,
      bridgeStatePolicy = bridgeStatePolicyArg,
      tmScriptHash = ByteString.empty,
      pegInScriptHash = ByteString.empty,
      pegOutScriptHash = ByteString.empty,
      params = ConfigParams(
        feeRateSatPerVb = BigInt(1),
        perPegoutFee = BigInt(0),
        minPegOutFbtc = BigInt(0),
        schedule = ScheduleParams(
          dkgR1Deadline = BigInt(0),
          dkgR2Deadline = BigInt(0),
          updateYDeadline = BigInt(0),
          tmBatchInterval = BigInt(0),
          signR1Window = BigInt(0),
          signR2Window = BigInt(0),
          leaderSlotT = BigInt(0),
          tmRecoveryWindow = BigInt(0),
          finalTmCutoff = BigInt(0),
          stabilityWindow = BigInt(0)
        )
      )
    ).toData

    /** The Config reference UTxO carrying the config NFT + a config datum with the bridge-state
      * policy at field 3. `withNft=false` simulates a forged config UTxO (right datum, no genuine
      * NFT).
      */
    private def configRefInput(
        withNft: Boolean = true,
        bridgeStatePolicyArg: ByteString = bssPolicy
    ): TxInInfo = TxInInfo(
      outRef = TxOutRef(TxId(filled(0x03, 32)), BigInt(0)),
      resolved = TxOut(
        address = Address(Credential.ScriptCredential(filled(0xc1, 28)), Option.None),
        value =
            if withNft then
                Value.unsafeFromList(PList((configNftPolicy, PList((configNftName, BigInt(1))))))
            else Value.lovelace(2_000_000),
        datum = OutputDatum.OutputDatum(configDatum(bridgeStatePolicyArg)),
        referenceScript = Option.None
      )
    )

    /** The singleton as a REFERENCE input for the mint path ([PTM-6]/[PTM-7]): carries the BSS NFT
      * (unless `withNft=false`) and a [[BridgeState]] whose head the posted TM must spend.
      */
    private def bssRefInput(
        head: ByteString = treasuryHead,
        withNft: Boolean = true
    ): TxInInfo = TxInInfo(
      outRef = TxOutRef(TxId(filled(0x04, 32)), BigInt(0)),
      resolved = TxOut(
        address = bssAddress,
        value = if withNft then bssValue() else Value.lovelace(2_000_000),
        datum = OutputDatum.OutputDatum(prevState(head).toData),
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
      * inputs, and outputs. The default reference inputs are `[config, singleton]`, so the default
      * redeemer index is 1.
      */
    private def mintContext(
        nftQty: BigInt,
        rdmr: Data,
        refInputs: PList[TxInInfo],
        outputs: PList[TxOut],
        // The mint requires `created == validRange.to` (a finite upper bound), so the default
        // window ends exactly at createdAt; Interval.always must fail.
        validRange: Interval = Interval.between(createdAt - 600_000, createdAt)
    ): ScriptContext =
        ScriptContext(
          txInfo = TxInfo(
            inputs = PList.Nil,
            referenceInputs = refInputs,
            outputs = outputs,
            mint = Value.unsafeFromList(PList((tmPolicy, PList((ByteString.empty, nftQty))))),
            validRange = validRange,
            id = TxId(filled(0x00, 32))
          ),
          redeemer = rdmr,
          scriptInfo = ScriptInfo.MintingScript(tmPolicy)
        )

    private val postRdmr: Data = TmMintRedeemer(bridgeStateRefInputIndex = BigInt(1)).toData
    private val defaultMintRefs: PList[TxInInfo] =
        PList.from(List(configRefInput(), bssRefInput()))

    /** A GC ScriptContext: spend an Unconfirmed TM UTxO with the Gc redeemer, optionally burning
      * the NFT, signed by `signer`, within `validRange`.
      */
    private def gcContext(
        burnQty: BigInt,
        signer: ByteString,
        validRange: Interval,
        extraInputs: List[TxInInfo] = List.empty,
        outputs: List[TxOut] = List.empty
    ): ScriptContext =
        ScriptContext(
          txInfo = TxInfo(
            inputs = PList.from(tmInput(tmValue, unconfirmedDatum) :: extraInputs),
            outputs = PList.from(outputs),
            mint =
                if burnQty == BigInt(0) then Value.zero
                else
                    Value.unsafeFromList(PList((tmScriptHash, PList((ByteString.empty, burnQty)))))
            ,
            signatories = PList.from(List(PubKeyHash(signer))),
            validRange = validRange,
            id = TxId(filled(0x00, 32))
          ),
          redeemer = gcRdmr,
          scriptInfo = SpendingScript(ownRef, Option.Some(unconfirmedDatum))
        )

    private val afterGrace: Interval =
        Interval.after(createdAt + TreasuryMovementValidator.GcGraceMs + 1)

    // --- tests ---

    test("contract compiles to UPLC and has a stable script hash") {
        val hash = compiled.script.scriptHash.toHex
        println(s"\n=== TreasuryMovementValidator script hash: $hash ===\n")
        assert(hash.length == 56)
    }

    // --- mint ([PTM-6]/[PTM-7]) ---

    test("TM mint: singleton head (aa*32, 0) matches the posted TM's input 0 - succeeds") {
        val sc = mintContext(
          BigInt(1),
          postRdmr,
          defaultMintRefs,
          PList.from(List(mintedTmOutput()))
        )
        val result = program.applyArg(sc.toData).evaluateDebug
        assert(result.isSuccess, s"Expected success, got: $result")
    }

    test("TM mint: a TM chaining from a stale head fails ([PTM-6])") {
        val sc = mintContext(
          BigInt(1),
          postRdmr,
          PList.from(List(configRefInput(), bssRefInput(head = filled(0xbb, 36)))),
          PList.from(List(mintedTmOutput()))
        )
        assert(!program.applyArg(sc.toData).evaluateDebug.isSuccess)
    }

    test("TM mint: a singleton reference without the BSS NFT fails ([PTM-7])") {
        val sc = mintContext(
          BigInt(1),
          postRdmr,
          PList.from(List(configRefInput(), bssRefInput(withNft = false))),
          PList.from(List(mintedTmOutput()))
        )
        assert(!program.applyArg(sc.toData).evaluateDebug.isSuccess)
    }

    test("TM mint: a missing config reference input fails") {
        // Without the config there is no authority on the singleton policy, so [PTM-7] cannot
        // authenticate the reference input.
        val sc = mintContext(
          BigInt(1),
          TmMintRedeemer(bridgeStateRefInputIndex = BigInt(0)).toData,
          PList.from(List(bssRefInput())),
          PList.from(List(mintedTmOutput()))
        )
        assert(!program.applyArg(sc.toData).evaluateDebug.isSuccess)
    }

    test("TM mint: the redeemer index must point AT the singleton, not near it") {
        // Index 0 is the config UTxO — it does not carry the BSS NFT, so [PTM-7] rejects it even
        // though a genuine singleton sits one slot over.
        val sc = mintContext(
          BigInt(1),
          TmMintRedeemer(bridgeStateRefInputIndex = BigInt(0)).toData,
          defaultMintRefs,
          PList.from(List(mintedTmOutput()))
        )
        assert(!program.applyArg(sc.toData).evaluateDebug.isSuccess)
    }

    test("TM mint: NFT output at a foreign credential fails") {
        val sc = mintContext(
          BigInt(1),
          postRdmr,
          defaultMintRefs,
          PList.from(
            List(mintedTmOutput(credential = Credential.ScriptCredential(filled(0x99, 28))))
          )
        )
        assert(!program.applyArg(sc.toData).evaluateDebug.isSuccess)
    }

    test("TM mint: minting more than one fails") {
        val sc = mintContext(
          BigInt(2),
          postRdmr,
          defaultMintRefs,
          PList.from(List(mintedTmOutput()))
        )
        assert(!program.applyArg(sc.toData).evaluateDebug.isSuccess)
    }

    test("TM NFT burn: -1 passes the mint policy (checks live in the spend path)") {
        val sc = mintContext(BigInt(-1), Data.unit, PList.Nil, PList.Nil)
        assert(program.applyArg(sc.toData).evaluateDebug.isSuccess)
    }

    test("TM mint: created != validity upper bound fails (backdating impossible)") {
        // A backdated `created` (upper bound after created) and any other mismatch both fail:
        // created must EQUAL validRange.to, making it an upper bound on the real posting time.
        val sc = mintContext(
          BigInt(1),
          postRdmr,
          defaultMintRefs,
          PList.from(List(mintedTmOutput())),
          validRange = Interval.between(createdAt + 7_200_000, createdAt + 7_800_000)
        )
        assert(!program.applyArg(sc.toData).evaluateDebug.isSuccess)
        val off = mintContext(
          BigInt(1),
          postRdmr,
          defaultMintRefs,
          PList.from(List(mintedTmOutput())),
          validRange = Interval.between(createdAt - 600_000, createdAt + 1)
        )
        assert(!program.applyArg(off.toData).evaluateDebug.isSuccess)
    }

    test("TM mint: unbounded validity range fails (created cannot be anchored)") {
        val sc = mintContext(
          BigInt(1),
          postRdmr,
          defaultMintRefs,
          PList.from(List(mintedTmOutput())),
          validRange = Interval.always
        )
        assert(!program.applyArg(sc.toData).evaluateDebug.isSuccess)
    }

    test("DA hint: mint accepts a garbled hint (nothing on-chain validates it)") {
        // A permissionless poster controls the hint. Junk in it must not block the mint — the
        // committed roots, not the hint, are what reconstruction verifies against.
        val garbled = PList.from(List(ByteString.fromHex("00"), filled(0x99, 200)))
        val sc = mintContext(
          BigInt(1),
          postRdmr,
          defaultMintRefs,
          PList.from(List(mintedTmOutput(datum = unconfirmedDatumWith(garbled))))
        )
        val result = program.applyArg(sc.toData).evaluateDebug
        assert(result.isSuccess, s"Expected success, got: $result")
    }

    test("TM mint: an Unconfirmed-shaped datum with a wrong Constr tag fails") {
        // A case-class decode is an erased retag, so without the explicit tag pin a poster could
        // mint a Constr-7 record that confirms on-chain yet is invisible to every harvester
        // (reconstruction, the SPI proof walk, confirm's poll filter all key on Constr 0).
        val wrongTag = unconfirmedDatum match
            case Data.Constr(0, fields) => Data.Constr(7, fields)
            case other                  => fail(s"expected Constr 0, got: $other")
        val sc = mintContext(
          BigInt(1),
          postRdmr,
          defaultMintRefs,
          PList.from(List(mintedTmOutput(datum = wrongTag)))
        )
        assert(!program.applyArg(sc.toData).evaluateDebug.isSuccess)
    }

    // --- GC ([CTM-16], [CTM-6..8], [CTM-17]) ---

    test("TM GC: creator burns an Unconfirmed record after the grace period") {
        val sc = gcContext(BigInt(-1), creatorPkh.hash, afterGrace)
        val result = program.applyArg(sc.toData).evaluateDebug
        assert(result.isSuccess, s"Expected success, got: $result")
    }

    test("TM GC: before the grace period elapses fails") {
        val sc = gcContext(BigInt(-1), creatorPkh.hash, Interval.after(createdAt + 1000))
        assertRejects(sc, "TM GC: grace period has not elapsed")
    }

    test("TM GC: non-creator signer fails") {
        val sc = gcContext(BigInt(-1), filled(0x11, 28), afterGrace)
        assertRejects(sc, "TM GC: not signed by the record's creator")
    }

    test("TM GC: spending without burning the TM NFT fails") {
        val sc = gcContext(BigInt(0), creatorPkh.hash, afterGrace)
        assertRejects(sc, "TM spend: must burn the TM NFT")
    }

    test("NFT containment (GC): spending two records while burning one is rejected") {
        // Two grace-expired Unconfirmed records (same creator) are spent; the tx burns ONE TM NFT
        // (mint == -1) and is signed by the creator after the grace period — so pre-[CTM-17] each
        // per-input GC invocation passes (its burn/grace/creator checks are all tx-wide and
        // satisfied). Ledger value-conservation then forces the un-burned SECOND NFT to escape to
        // the attacker output below with a fabricated Unconfirmed datum — a forged post that
        // skipped the mint checks. The single-TM-input rule rejects it.
        val secondRecord = TxInInfo(
          outRef = TxOutRef(TxId(filled(0x05, 32)), BigInt(1)),
          resolved = TxOut(
            address = Address(Credential.ScriptCredential(tmScriptHash), Option.None),
            value = tmValue,
            datum = OutputDatum.OutputDatum(unconfirmedDatum),
            referenceScript = Option.None
          )
        )
        val escapedOutput = TxOut(
          address = Address(Credential.ScriptCredential(filled(0x99, 28)), Option.None),
          value = tmValue, // the un-burned second NFT escapes
          datum = OutputDatum.OutputDatum(unconfirmedDatum),
          referenceScript = Option.None
        )
        val sc = gcContext(
          BigInt(-1),
          creatorPkh.hash,
          afterGrace,
          extraInputs = List(secondRecord),
          outputs = List(escapedOutput)
        )
        assertRejects(sc, "TM spend: exactly one TM-script input per tx")
    }

    // --- data round-trips ---

    test("UnconfirmedTm / redeemer Data round-trips pin the wire format") {
        // The 4-field Unconfirmed: the DA hint is the appended field 3 and survives the round trip
        // even though no validator reads it. epoch/leader_reward are GONE (spec §Leader reward:
        // DEFERRED).
        assert(
          unconfirmedDatum.to[UnconfirmedTm] ==
              UnconfirmedTm(rawTm, creatorPkh, createdAt, porOutpointHint)
        )
        unconfirmedDatum match
            case Data.Constr(0, fields) =>
                val positional = fields.asScala.toList
                assert(positional.size == 4, "Unconfirmed must encode as 4 positional fields")
                positional(3) match
                    case Data.List(items) =>
                        assert(items.asScala.toList == List(Data.B(porOutpointHint.head)))
                    case other => fail(s"field 3 must be a Data list, got: $other")
            case other => fail(s"Unconfirmed must encode as Constr 0, got: $other")
        // LOCKSTEP with bridge-state.ak [BSS-2]: Confirm MUST be Constr tag 0, Gc tag 1 — the
        // singleton validator discriminates a spend on exactly this tag.
        confirmRdmr() match
            case Data.Constr(0, _) => ()
            case other             => fail(s"Confirm must encode as Constr 0, got: $other")
        gcRdmr match
            case Data.Constr(1, fields) => assert(fields.asScala.isEmpty)
            case other                  => fail(s"Gc must encode as Constr 1 [], got: $other")
        val rt: TmSpendRedeemer = TmSpendRedeemer.Confirm(confirmProof())
        assert(rt.toData.to[TmSpendRedeemer] == rt)
        val mint = TmMintRedeemer(BigInt(7))
        assert(mint.toData.to[TmMintRedeemer] == mint)
        val state = defaultNewState
        assert(state.toData.to[BridgeState] == state)
    }

    test("parses all input outpoints and all outputs from a raw TM") {
        assert(TreasuryMovementValidator.allInputOutpoints(rawTm) == expectedSwept)
        assert(TreasuryMovementValidator.allOutputs(rawTm) == expectedFulfilled)
    }

    // --- Confirm: proof and linkage ---

    test("proven confirmation advancing the singleton succeeds") {
        // Assert the roots actually MOVE (a fixture whose previous state already matched would
        // pass vacuously).
        assert(prevState().spiRoot != spiRootA && prevState().cpoRoot != cpoRootA)
        val sc = scriptContext(confirmRdmr())
        val result = program.applyArg(sc.toData).evaluateDebug
        assert(result.isSuccess, s"Expected success, got: $result")
    }

    test("deployed blueprint .script matches .contract on a valid confirm (tagged param apply)") {
        // Regression for the `_scalusTag` bug: the DEPLOYED script is TreasuryMovementContract.script
        // — the blueprint compiledCode with the 3 ByteString params applied at the UPLC level
        // (BinocularBlueprint.bytesParam), NOT the typed `.contract` form that the other tests eval.
        // On a pre-1.0 Scalus those two DIVERGED under Options.release: `.contract` accepted a
        // valid confirm while the deployed `.script` ERRORED ("Error evaluated") on the spend
        // branch — so no deployed TM could ever be confirmed. Scalus 1.0.0 evaluates the tagged
        // shape correctly, so the contract compiles TAGGED again; this test evaluating the
        // DEPLOYED form is the guard that keeps that decision honest.
        val ctx = scriptContext(confirmRdmr()).toData

        // The typed `.contract` form (what other tests exercise) — must accept.
        assert(program.applyArg(ctx).evaluateDebug.isSuccess)

        // The deployed blueprint `.script` form — the exact UPLC that gets locked/spent on-chain.
        val bpProgram = binocular.blueprint.BinocularBlueprint
            .program("TreasuryMovementContract")
            .$(binocular.blueprint.BinocularBlueprint.bytesParam(oracleHash))
            .$(binocular.blueprint.BinocularBlueprint.bytesParam(configNftPolicy))
            .$(binocular.blueprint.BinocularBlueprint.bytesParam(configNftName))
            .deBruijnedProgram
        val deployedResult = bpProgram.applyArg(ctx).evaluateDebug
        assert(
          deployedResult.isSuccess,
          s"deployed blueprint .script must accept a valid confirm, got: $deployedResult"
        )
        // NB: `.contract` (typed .apply) and `.script` (UPLC bytesParam) produce different-but-
        // equivalent UPLC, so their hashes differ — that is fine: only `.script` is ever deployed
        // (address, NFT policy, spend all use it).
    }

    test("block header not in oracle's confirmed-blocks root fails") {
        // Same merkle-root (bytes 36..68 = txid), but a different nonce → a different block hash
        // that the oracle's MPF does not contain. Isolates the MPF membership check.
        val tamperedHeader = blockHeader.slice(0, 76) ++ ByteString.fromHex("deadbeef")
        val sc = scriptContext(confirmRdmr(confirmProof(header = tamperedHeader)))
        assert(!program.applyArg(sc.toData).evaluateDebug.isSuccess)
    }

    test("oracle reference input without the oracle NFT fails") {
        val sc = scriptContext(
          confirmRdmr(),
          oracleRef = oracleRefInput(Value.lovelace(5_000_000)) // no NFT
        )
        assert(!program.applyArg(sc.toData).evaluateDebug.isSuccess)
    }

    test("[CTM-24] a Confirm that does not burn the TM NFT fails") {
        val sc = scriptContext(confirmRdmr(), burnQty = BigInt(0))
        assertRejects(sc, "TM spend: must burn the TM NFT")
    }

    test("[CTM-25] any output at the TM script address fails") {
        // There is no Confirmed record: the record is retired, not recreated. Even a junk output
        // parked at the TM address by the confirmer is rejected.
        val junkAtTm = TxOut(
          address = Address(Credential.ScriptCredential(tmScriptHash), Option.None),
          value = Value.lovelace(2_000_000),
          datum = OutputDatum.OutputDatum(Data.unit),
          referenceScript = Option.None
        )
        val sc = scriptContext(confirmRdmr(), outputs = List(bssOutput(), junkAtTm))
        assertRejects(sc, "TM confirm: no output may sit at the TM address")
    }

    test("[CTM-17] NFT containment (Confirm): two duplicate Unconfirmed records are rejected") {
        // The fund-theft repro, rev-5.4 shape. Two TM-script inputs embed the SAME signedBtcTx
        // (permissionless duplicate posts, both bearing the fungible empty-name TM NFT). The tx
        // burns ONE NFT; pre-[CTM-17] each per-input invocation would accept, and ledger value
        // conservation forces the SECOND NFT to escape to an attacker output with a fabricated
        // Unconfirmed datum — a forged post that skipped the mint checks.
        val secondInput = TxInInfo(
          outRef = TxOutRef(TxId(filled(0x05, 32)), BigInt(1)),
          resolved = TxOut(
            address = Address(Credential.ScriptCredential(tmScriptHash), Option.None),
            value = tmValue,
            datum = OutputDatum.OutputDatum(unconfirmedDatum),
            referenceScript = Option.None
          )
        )
        val escapedOutput = TxOut(
          address = Address(Credential.ScriptCredential(filled(0x99, 28)), Option.None),
          value = tmValue, // carries the TM NFT (policy = tmScriptHash, empty name, qty 1)
          datum = OutputDatum.OutputDatum(unconfirmedDatum),
          referenceScript = Option.None
        )
        val sc = scriptContext(
          confirmRdmr(),
          extraInputs = List(secondInput, bssInput()),
          outputs = List(bssOutput(), escapedOutput)
        )
        assertRejects(sc, "TM spend: exactly one TM-script input per tx")
    }

    test("[CTM-18] a TM that does not spend the singleton's head fails") {
        // The singleton's head is some OTHER outpoint — the posted TM (input 0 = aa*32, 0) chains
        // from a head that no longer exists. This is the replay defense: an old TM's head is spent,
        // so a stale root can never be written back.
        val sc = scriptContext(
          confirmRdmr(),
          extraInputs = List(bssInput(state = prevState(head = filled(0xcc, 36))))
        )
        assertRejects(sc, "TM confirm: BTC tx does not spend the confirmed head")
    }

    // --- Confirm: the two-root commitment ([CTM-26], [CTM-20], [CTM-30]) ---

    test("three peg-out payments and one commitment succeed") {
        // Confirm is O(1) in the batch size: the payments are inert to this validator, only the
        // single commitment output matters.
        val raw = rawTxWith(
          List(
            (changeSpk, BigInt(1000)),
            (paySpk1, BigInt(2000)),
            (paySpk2, BigInt(3000)),
            (paySpk3, BigInt(4000)),
            (btmr1Spk(), BigInt(0))
          )
        )
        val sc = confirmContextFor(raw, attestedState(raw))
        val result = program.applyArg(sc.toData).evaluateDebug
        assert(result.isSuccess, s"Expected success, got: $result")
    }

    test("the commitment may sit anywhere in the output list") {
        // heimdall emits it last, but nothing on-chain depends on the position: the scan covers
        // every output, output 0 included.
        val raw = rawTxWith(
          List(
            (changeSpk, BigInt(1000)),
            (btmr1Spk(), BigInt(0)),
            (paySpk1, BigInt(2000))
          )
        )
        val sc = confirmContextFor(raw, attestedState(raw))
        val result = program.applyArg(sc.toData).evaluateDebug
        assert(result.isSuccess, s"Expected success, got: $result")
    }

    test("a zero-peg-out TM re-commits both roots") {
        // Change output + commitment only. The singleton still advances: same roots, new head.
        val raw = rawTxWith(List((changeSpk, BigInt(1000)), (btmr1Spk(), BigInt(0))))
        val sc = confirmContextFor(raw, attestedState(raw))
        val result = program.applyArg(sc.toData).evaluateDebug
        assert(result.isSuccess, s"Expected success, got: $result")
    }

    test("[CTM-26] a TM with no commitment output cannot be confirmed") {
        // Not a tolerated case: without a commitment the roots would be whatever the confirmer
        // chose. Every TM must state the roots that hold after it, peg-outs or none.
        val raw = rawTxWith(List((changeSpk, BigInt(1000)), (paySpk1, BigInt(2000))))
        val sc = confirmContextFor(raw, defaultNewState)
        assertRejects(sc, "TM confirm: missing two-root commitment")
    }

    test("[CTM-26] two commitment outputs fail") {
        // The validator must never have to choose which roots to copy — a permissionless confirmer
        // would make that choice by ordering the outputs.
        val raw = rawTxWith(
          List(
            (changeSpk, BigInt(1000)),
            (paySpk1, BigInt(2000)),
            (btmr1Spk(), BigInt(0)),
            (btmr1Spk(spi = filled(0x11, 32)), BigInt(0))
          )
        )
        val sc = confirmContextFor(raw, defaultNewState)
        assertRejects(sc, "TM confirm: multiple two-root commitments")
    }

    test("[CTM-26] two commitments of the SAME roots still fail") {
        // No "harmless duplicate" tolerance: one output, always. Duplicates mean the signer built
        // something the protocol does not describe.
        val raw = rawTxWith(
          List(
            (changeSpk, BigInt(1000)),
            (btmr1Spk(), BigInt(0)),
            (btmr1Spk(), BigInt(0))
          )
        )
        val sc = confirmContextFor(raw, defaultNewState)
        assertRejects(sc, "TM confirm: multiple two-root commitments")
    }

    test("a wrong prefix (\"BTMR2\") is not a commitment") {
        // Right length, right OP_RETURN push, wrong tag. Only "BTMR1" commits the roots, so the TM
        // reads as having none.
        val raw = rawTxWith(
          List(
            (changeSpk, BigInt(1000)),
            (paySpk1, BigInt(2000)),
            (btmr1Spk(tag = "42544d5232"), BigInt(0))
          )
        )
        val sc = confirmContextFor(raw, defaultNewState)
        assertRejects(sc, "TM confirm: missing two-root commitment")
    }

    test("a right-prefix output of the wrong length is not a commitment") {
        // 70 bytes instead of 71. Without the length check the slice would read past the payload
        // and a truncated push would attest roots nobody signed.
        val short = btmr1Spk().slice(0, 70)
        val raw = rawTxWith(
          List(
            (changeSpk, BigInt(1000)),
            (paySpk1, BigInt(2000)),
            (short, BigInt(0))
          )
        )
        val sc = confirmContextFor(raw, defaultNewState)
        assertRejects(sc, "TM confirm: missing two-root commitment")
    }

    test("commitment recognition: length and prefix are both required") {
        val spk = btmr1Spk()
        assert(spk.size == 71)
        assert(TreasuryMovementValidator.isTwoRootCommitment(spk))
        assert(
          TreasuryMovementValidator.committedRoots(
            PList.from(List(PegOutEntry(changeSpk, BigInt(1)), PegOutEntry(spk, BigInt(0))))
          ) == (spiRootA, cpoRootA)
        )
        // Wrong tag.
        assert(!TreasuryMovementValidator.isTwoRootCommitment(btmr1Spk(tag = "42544d5232")))
        // Right prefix, wrong length (truncated payload).
        assert(!TreasuryMovementValidator.isTwoRootCommitment(spk.slice(0, 70)))
        // A 71-byte payment script must not be mistaken for a commitment.
        assert(!TreasuryMovementValidator.isTwoRootCommitment(filled(0x51, 71)))
        // The old 39-byte "CPOR1" output is NOT a two-root commitment.
        assert(
          !TreasuryMovementValidator.isTwoRootCommitment(
            ByteString.fromHex("6a2543504f5231") ++ spiRootA
          )
        )
    }

    // --- Confirm: the singleton datum ([CTM-27], [CTM-19], [CTM-21], [CTM-28], [CTM-29]) ---

    test("[CTM-27] a continuing singleton with roots the TM did not commit fails") {
        for bad <- List(
              defaultNewState.copy(spiRoot = filled(0x77, 32)),
              defaultNewState.copy(cpoRoot = filled(0x77, 32))
            )
        do {
            val sc = scriptContext(confirmRdmr(), outputs = List(bssOutput(bad)))
            assertRejects(sc, "TM confirm: singleton datum is not the attested state")
        }
    }

    test("[CTM-19] a continuing singleton with a wrong head fails") {
        val bad = defaultNewState.copy(treasuryUtxoId = filled(0x77, 36))
        val sc = scriptContext(confirmRdmr(), outputs = List(bssOutput(bad)))
        assertRejects(sc, "TM confirm: singleton datum is not the attested state")
    }

    test("[CTM-21] a continuing singleton with a wrong treasury amount fails") {
        val bad = defaultNewState.copy(treasuryAmount = BigInt(999_999))
        val sc = scriptContext(confirmRdmr(), outputs = List(bssOutput(bad)))
        assertRejects(sc, "TM confirm: singleton datum is not the attested state")
    }

    test("[CTM-27] a singleton datum with the right fields but a wrong SHAPE fails") {
        // Confirming is permissionless, so the singleton datum's shape is attacker-chosen. On-chain
        // `FromData` is an erased retag — field access is a lazy projection with no tag or arity
        // check — so field-wise comparison would accept both of these and leave a datum the
        // protocol never describes at the singleton address for every downstream reader ([LIB-1]).
        // The whole-`OutputDatum` comparison rejects them.
        val fields = List(
          Data.B(spiRootA),
          Data.B(cpoRootA),
          Data.B(txid ++ zeroVout),
          Data.I(BigInt(1000))
        )
        val extraField = Data.Constr(0, PList.from(fields :+ Data.I(BigInt(7))))
        val wrongTag = Data.Constr(5, PList.from(fields))
        for bad <- List(extraField, wrongTag) do {
            val sc = scriptContext(confirmRdmr(), outputs = List(bssOutputWithDatum(bad)))
            assertRejects(sc, "TM confirm: singleton datum is not the attested state")
        }
    }

    test("[CTM-28] not spending the singleton fails") {
        val sc = scriptContext(confirmRdmr(), extraInputs = List.empty)
        assertRejects(sc, "TM confirm: bridge state singleton not spent")
    }

    test("[CTM-28] a forged singleton NFT policy is not accepted") {
        // A BSS-named token under a policy the Config does not publish is invisible to the
        // validator, so the genuine singleton is simply missing.
        val forged = filled(0xc9, 28)
        val sc = scriptContext(
          confirmRdmr(),
          extraInputs = List(bssInput(value = bssValue(forged))),
          outputs = List(bssOutput(value = bssValue(forged)))
        )
        assertRejects(sc, "TM confirm: bridge state singleton not spent")
    }

    test("[CTM-28] a missing config reference input fails") {
        val sc = scriptContext(confirmRdmr(), cfgRefs = List.empty)
        assertRejects(sc, "TM confirm: no config reference input")
    }

    test("[CTM-28] a config UTxO without the config NFT is ignored") {
        val sc = scriptContext(confirmRdmr(), cfgRefs = List(configRefInput(withNft = false)))
        assertRejects(sc, "TM confirm: no config reference input")
    }

    test("[CTM-28] a config publishing a different singleton policy fails") {
        // Config field 3 is the ONLY authority on which UTxO is the singleton. Point it elsewhere
        // and the singleton the tx actually spends is no longer found.
        val sc = scriptContext(
          confirmRdmr(),
          cfgRefs = List(configRefInput(bridgeStatePolicyArg = filled(0xc8, 28)))
        )
        assertRejects(sc, "TM confirm: bridge state singleton not spent")
    }

    test("[CTM-29] no continuing singleton output fails") {
        val sc = scriptContext(confirmRdmr(), outputs = List.empty)
        assertRejects(sc, "TM confirm: no continuing singleton output")
    }

    test("[CTM-29] moving the singleton NFT to a different address fails") {
        val sc = scriptContext(
          confirmRdmr(),
          outputs = List(
            bssOutput(address = Address(Credential.ScriptCredential(filled(0x99, 28)), Option.None))
          )
        )
        assertRejects(sc, "TM confirm: singleton address changed")
    }

    // --- the DA hint is decoded and ignored ---

    test("DA hint: confirm accepts any hint content") {
        // The hint is UNVERIFIED. Three Unconfirmed records differing only in the hint must all
        // confirm to the SAME singleton state (which has no hint field at all).
        val hints = List(
          PList.Nil,
          porOutpointHint,
          PList.from(List(filled(0xff, 36), filled(0x00, 36), filled(0xab, 36)))
        )
        for hint <- hints do {
            val datum = unconfirmedDatumWith(hint)
            val sc = scriptContext(confirmRdmr(), tmDatum = datum)
            val result = program.applyArg(sc.toData).evaluateDebug
            assert(result.isSuccess, s"Expected success for hint $hint, got: $result")
        }
    }

    test("DA hint: a 3-field Unconfirmed datum still confirms (the field is never projected)") {
        // Observed behaviour, pinned deliberately. Scalus lowers the pattern match to per-index
        // field projections and nothing in `spend` or `mint` touches index 3, so a datum that stops
        // at `created` — a truncated post — is never rejected for its arity. That follows from "the
        // hint is unverified": the singleton state the validator reconstructs does not contain it,
        // so there is nothing to be wrong about.
        //
        // If a future check ever reads `fulfilledPorOutpoints`, this test flips, and that is the
        // point: the change must be a decision, not a side effect.
        val short = Data.Constr(
          0,
          PList.from(List(Data.B(rawTm), Data.B(creatorPkh.hash), Data.I(createdAt)))
        )
        val sc = scriptContext(confirmRdmr(), tmDatum = short)
        val result = program.applyArg(sc.toData).evaluateDebug
        assert(result.isSuccess, s"Expected success, got: $result")
    }

    // --- budget measurements ---

    // Execution budgets for the treasury-movement happy paths, printed so they land in the CI log
    // (Milestone 4 performance evidence). Synthetic ScriptContexts, so only the script execution
    // budget and ex-unit fee are meaningful here (no full tx fee).
    test("Treasury movement budgets - mint Post, Confirm spend") {
        val pp = CardanoInfo.mainnet.protocolParams
        val maxCpu = pp.maxTxExecutionUnits.steps
        val maxMem = pp.maxTxExecutionUnits.memory

        def measure(name: String, sc: ScriptContext): Unit =
            program.applyArg(sc.toData).evaluateDebug match
                case r: Result.Success =>
                    val exFeeAda = r.budget.fee(pp.executionUnitPrices).value / 1_000_000.0
                    info(
                      f"$name%-13s | ${r.budget.steps}%,13d (${r.budget.steps * 100.0 / maxCpu}%5.2f%%) | ${r.budget.memory}%,10d (${r.budget.memory * 100.0 / maxMem}%5.2f%%) | $exFeeAda%.6f ADA"
                    )
                    assert(r.budget.steps <= maxCpu && r.budget.memory <= maxMem)
                case r: Result.Failure =>
                    fail(s"$name failed: ${r.exception.getMessage}\n${r.logs.mkString("\n")}")

        info("TREASURY MOVEMENT BUDGETS | CPU Steps (% limit) | Memory (% limit) | Ex Fee")
        measure(
          "Mint Post",
          mintContext(
            BigInt(1),
            postRdmr,
            defaultMintRefs,
            PList.from(List(mintedTmOutput()))
          )
        )
        measure("Confirm spend", scriptContext(confirmRdmr()))
        // Batch sizing evidence. Confirm folds nothing: both roots are copied from ONE commitment
        // output whatever the batch size, so the only cost that grows with the batch is parsing the
        // extra outputs. Measure a 0-peg-out and an 8-peg-out TM to show the gap.
        val zeroPegOut = rawTxWith(List((changeSpk, BigInt(1000)), (btmr1Spk(), BigInt(0))))
        measure("Confirm 0 pegout", confirmContextFor(zeroPegOut, attestedState(zeroPegOut)))
        val batch = List.range(0, 8).map { i =>
            val spk = ByteString.fromHex("0014" + (f"$i%02x" * 20))
            (spk, BigInt(1000 + i))
        }
        val batchRaw = rawTxWith(
          ((changeSpk, BigInt(1000)) :: batch) :+ (btmr1Spk(), BigInt(0))
        )
        measure("Confirm 8 pegout", confirmContextFor(batchRaw, attestedState(batchRaw)))
    }
}
