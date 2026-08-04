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
    // The completed-peg-outs trie: policy id published in Config field 3, asset name "CPO", and the
    // UTxO sits at the trie script's own address (policy id == script hash).
    private val triePolicy = filled(0xc2, 28)
    private val cpoName = ByteString.fromString("CPO")
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

    // --- raw TM builder -------------------------------------------------------------------------
    // TM output layout: [0] = treasury change, then one (payment, "POR" marker) PAIR per fulfilled
    // peg-out. The marker commits the POR id of the PegOutRequest the payment settles.

    private val changeSpk = ByteString.fromHex("0014" + ("11" * 20))
    private val paySpk1 = ByteString.fromHex("0014" + ("22" * 20))
    private val paySpk2 = ByteString.fromHex("0014" + ("33" * 20))
    private val paySpk3 = ByteString.fromHex("0014" + ("44" * 20))
    private val porId1 = filled(0xd1, 32)
    private val porId2 = filled(0xd2, 32)
    private val porId3 = filled(0xd3, 32)

    /** `OP_RETURN OP_PUSHBYTES_35 <tag ++ por_id>` — 37 script bytes. `tag` is "POR" (504f52) for a
      * genuine marker; the wrong-prefix test passes the peg-in tag "BFR" (424652).
      */
    private def markerSpk(porId: ByteString, tag: String = "504f52"): ByteString =
        ByteString.fromHex("6a23" + tag) ++ porId

    /** A 2-in segwit tx (empty witnesses, fixed outpoints) with the given `(scriptPubKey, sats)`
      * outputs.
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

    /** The value a fulfilled peg-out records in the trie: `dest_spk ++ amount_le8`. Must match
      * `peg-out.ak`'s Complete branch byte for byte, or completion breaks.
      */
    private def trieValue(spk: ByteString, amount: BigInt): ByteString =
        spk ++ integerToByteString(false, 8, amount)

    /** The default TM: treasury change + ONE fulfilled peg-out (payment + its POR marker). */
    private val rawTm: ByteString = rawTxWith(
      List(
        (changeSpk, BigInt(1000)),
        (paySpk1, BigInt(2000)),
        (markerSpk(porId1), BigInt(0))
      )
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
        PegOutEntry(changeSpk, BigInt(1000)),
        PegOutEntry(paySpk1, BigInt(2000)),
        PegOutEntry(markerSpk(porId1), BigInt(0))
      )
    )

    // --- completed-peg-outs trie fixtures --------------------------------------------------------

    private val emptyTrie = OffChainMPF.empty
    private val emptyRoot = emptyTrie.rootHash

    /** Insert-fold `entries` into `start`: returns the matching redeemer steps and the new trie.
      * Each `Insert` step carries the NON-membership proof taken against the trie state that step
      * sees, exactly as an honest confirmer would build it.
      */
    private def insertSteps(
        start: OffChainMPF,
        entries: List[(ByteString, ByteString)]
    ): (PList[PegOutTrieStep], OffChainMPF) = {
        var trie = start
        val steps = entries.map { case (k, v) =>
            val step: PegOutTrieStep = PegOutTrieStep.Insert(trie.proveNonMembership(k))
            trie = trie.insert(k, v)
            step
        }
        (PList.from(steps), trie)
    }

    private val (defaultSteps, defaultTrie) =
        insertSteps(emptyTrie, List((porId1, trieValue(paySpk1, BigInt(2000)))))
    private val defaultEndRoot = defaultTrie.rootHash

    private val trieAddress = Address(Credential.ScriptCredential(triePolicy), Option.None)

    private def trieNftValue(policy: ByteString = triePolicy): Value =
        Value.unsafeFromList(
          PList(
            (ByteString.empty, PList((ByteString.empty, BigInt(2_000_000)))),
            (policy, PList((cpoName, BigInt(1))))
          )
        )

    private def trieInput(
        root: ByteString = emptyRoot,
        value: Value = trieNftValue()
    ): TxInInfo = TxInInfo(
      outRef = TxOutRef(TxId(filled(0x06, 32)), BigInt(0)),
      resolved = TxOut(
        address = trieAddress,
        value = value,
        datum = OutputDatum.OutputDatum(CompletedPegOutsTrieDatum(root).toData),
        referenceScript = Option.None
      )
    )

    private def trieOutput(
        root: ByteString,
        value: Value = trieNftValue(),
        address: Address = trieAddress
    ): TxOut = TxOut(
      address = address,
      value = value,
      datum = OutputDatum.OutputDatum(CompletedPegOutsTrieDatum(root).toData),
      referenceScript = Option.None
    )

    private val ownRef = TxOutRef(TxId(filled(0x01, 32)), BigInt(0))
    private val creatorPkh = PubKeyHash(filled(0x7a, 28))
    private val createdAt: BigInt = BigInt("1700000000000")
    // N7 datum fields — carried through Confirm, not yet enforced on-chain (pin lands with N9).
    private val tmEpoch: BigInt = BigInt(42)
    private val tmLeaderReward: BigInt = BigInt(2_000_000)
    private val unconfirmedDatum: Data =
        (TmDatum.Unconfirmed(rawTm, creatorPkh, createdAt, tmEpoch, tmLeaderReward): TmDatum).toData

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

    private def redeemer(
        proof: PList[ProofStep],
        txIndex: BigInt = 0,
        pegOutSteps: PList[PegOutTrieStep] = defaultSteps
    ): Data =
        TmConfirmRedeemer(
          txIndex = txIndex,
          txMerkleProof = PList.Nil,
          blockMpfProof = proof,
          blockHeader = BlockHeader(blockHeader),
          pegOutSteps = pegOutSteps
        ).toData

    /** A Confirm ScriptContext over the default `rawTm` (one fulfilled peg-out). The trie UTxO is
      * spent (empty root) and recreated with the root after inserting that peg-out, and the Config
      * reference input publishes `triePolicy` at field 3.
      */
    private def scriptContext(
        outValue: Value,
        outDatum: Data,
        rdmr: Data,
        oracleRef: TxInInfo = oracleRefInput(),
        extraOutputs: List[TxOut] = List.empty,
        extraInputs: List[TxInInfo] = List(trieInput()),
        trieOutputs: List[TxOut] = List(trieOutput(defaultEndRoot)),
        cfgRefs: List[TxInInfo] = List(configRefInput())
    ): ScriptContext =
        ScriptContext(
          txInfo = TxInfo(
            inputs = PList.from(tmInput(tmValue, unconfirmedDatum) :: extraInputs),
            referenceInputs = PList.from(oracleRef :: cfgRefs),
            outputs = PList.from(
              (confirmedOutput(outValue, outDatum) :: extraOutputs) ++ trieOutputs
            ),
            id = TxId(filled(0x00, 32))
          ),
          redeemer = rdmr,
          scriptInfo = SpendingScript(ownRef, Option.Some(unconfirmedDatum))
        )

    /** A Confirm ScriptContext for an ARBITRARY raw TM: derives its txid, wraps it in a single-tx
      * block, builds the oracle state proving that block, reconstructs the expected `Confirmed`
      * datum, and wires the trie in/out pair plus the Config reference input.
      */
    private def confirmContextFor(
        rawTx: ByteString,
        trieInRoot: ByteString,
        trieOutRoot: ByteString,
        steps: PList[PegOutTrieStep]
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
        val unconf: Data =
            (TmDatum.Unconfirmed(
              rawTx,
              creatorPkh,
              createdAt,
              tmEpoch,
              tmLeaderReward
            ): TmDatum).toData
        val conf: Data = (TmDatum.Confirmed(
          id,
          TreasuryMovementValidator.allInputOutpoints(rawTx),
          TreasuryMovementValidator.allOutputs(rawTx),
          false,
          creatorPkh,
          createdAt,
          tmEpoch,
          tmLeaderReward
        ): TmDatum).toData
        val rdmr: Data = TmConfirmRedeemer(
          txIndex = 0,
          txMerkleProof = PList.Nil,
          blockMpfProof = obMpf.proveMembership(bh),
          blockHeader = BlockHeader(hdr),
          pegOutSteps = steps
        ).toData
        ScriptContext(
          txInfo = TxInfo(
            inputs = PList.from(List(tmInput(tmValue, unconf), trieInput(trieInRoot))),
            referenceInputs = PList.from(List(oracleRef, configRefInput())),
            outputs = PList.from(List(confirmedOutput(tmValue, conf), trieOutput(trieOutRoot))),
            id = TxId(filled(0x00, 32))
          ),
          redeemer = rdmr,
          scriptInfo = SpendingScript(ownRef, Option.Some(unconf))
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
      * Why the second half matters: several rejection fixtures are multi-cause. A malformed marker
      * pair, for example, also leaves the trie root unequal to the continuing output's, so a plain
      * `!isSuccess` would still pass if the marker check were deleted outright. Pinning the message
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

    // The anchor outpoint = in0 of rawTm: aa*32 ++ 00000000 (txid internal order ++ vout LE).
    private val anchorOutpoint = ByteString.fromHex(("aa" * 32) + "00000000")

    /** The real 17-field [[ConfigDatum]] mirror. The Confirm path reads field 3
      * (`completed_peg_outs_merkle_tree_policy_id`); the mint path reads field 11
      * (`initial_btc_treasury_utxo`). The rest are inert here.
      */
    private def configDatum(
        anchor: ByteString,
        cpoPolicy: ByteString = triePolicy
    ): Data = ConfigDatum(
      bridgedTokenPolicyId = ByteString.empty,
      bridgedTokenAssetName = ByteString.empty,
      completedPegInsMerkleTreePolicyId = ByteString.empty,
      completedPegOutsMerkleTreePolicyId = cpoPolicy,
      pegInWithdrawScriptHash = ByteString.empty,
      pegOutWithdrawScriptHash = ByteString.empty,
      pegInCloseVerifierScriptHash = ByteString.empty,
      legitTmAndPegOutProducedVerifierScriptHash = ByteString.empty,
      legitTmAndPegOutNotProducedVerifierScriptHash = ByteString.empty,
      minStake = BigInt(0),
      updateAuth = Option.None,
      initialBtcTreasuryUtxo = anchor,
      feeRateSatPerVb = BigInt(1),
      perPegoutFee = BigInt(0),
      minPegOutFbtc = BigInt(0),
      leaderReward = BigInt(0),
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
    ).toData

    /** The Config reference UTxO carrying the config NFT + a config datum with the anchor at field
      * 11 and the completed-peg-outs trie policy at field 3. `withNft=false` simulates a forged
      * config UTxO (right datum, no genuine NFT).
      */
    private def configRefInput(
        anchor: ByteString = anchorOutpoint,
        withNft: Boolean = true,
        cpoPolicy: ByteString = triePolicy
    ): TxInInfo = TxInInfo(
      outRef = TxOutRef(TxId(filled(0x03, 32)), BigInt(0)),
      resolved = TxOut(
        address = Address(Credential.ScriptCredential(filled(0xc1, 28)), Option.None),
        value =
            if withNft then
                Value.unsafeFromList(PList((configNftPolicy, PList((configNftName, BigInt(1))))))
            else Value.lovelace(2_000_000),
        datum = OutputDatum.OutputDatum(configDatum(anchor, cpoPolicy)),
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
          if confirmed then
              (TmDatum
                  .Confirmed(
                    prevTxid,
                    PList.Nil,
                    PList.Nil,
                    false,
                    creatorPkh,
                    createdAt,
                    tmEpoch,
                    tmLeaderReward
                  ): TmDatum).toData
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

    /** A GC (Confirmed-spend) ScriptContext: spend a Confirmed TM UTxO, optionally burning the NFT,
      * signed by `signer`, within `validRange`.
      */
    private def gcContext(
        burnQty: BigInt,
        signer: ByteString,
        validRange: Interval,
        datum: Data = confirmedDatum()
    ): ScriptContext =
        ScriptContext(
          txInfo = TxInfo(
            inputs = PList.from(List(tmInput(tmValue, datum))),
            outputs = PList.Nil,
            mint =
                if burnQty == BigInt(0) then Value.zero
                else
                    Value.unsafeFromList(PList((tmScriptHash, PList((ByteString.empty, burnQty)))))
            ,
            signatories = PList.from(List(PubKeyHash(signer))),
            validRange = validRange,
            id = TxId(filled(0x00, 32))
          ),
          redeemer = Data.unit,
          scriptInfo = SpendingScript(ownRef, Option.Some(datum))
        )

    private val afterGrace: Interval =
        Interval.after(createdAt + TreasuryMovementValidator.GcGraceMs + 1)

    private val genesisRdmr: Data = (TmMintRedeemer.Genesis(0): TmMintRedeemer).toData
    private def genesisRdmrAt(i: BigInt): Data = (TmMintRedeemer.Genesis(i): TmMintRedeemer).toData
    private def chainRdmr(i: BigInt): Data = (TmMintRedeemer.Chain(i): TmMintRedeemer).toData

    private def confirmedDatum(
        swept: PList[ByteString] = expectedSwept,
        fulfilled: PList[PegOutEntry] = expectedFulfilled,
        // N10b flag. Default false: `rawTm`'s input 0 has 0 witness items (not a 3-item script-path),
        // so the validator computes false — a continuing datum must carry the same value to match.
        spentViaFederationLeaf: Boolean = false
    ): Data =
        (TmDatum.Confirmed(
          txid,
          swept,
          fulfilled,
          spentViaFederationLeaf,
          creatorPkh,
          createdAt,
          tmEpoch,
          tmLeaderReward
        ): TmDatum).toData

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

    test("TM mint Genesis: reference-input index out of range fails") {
        val sc = mintContext(
          BigInt(1),
          genesisRdmrAt(1), // only one reference input exists
          PList.from(List(configRefInput())),
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

    test("TM NFT burn: -1 passes the mint policy (checks live in the spend path)") {
        val sc = mintContext(BigInt(-1), Data.unit, PList.Nil, PList.Nil)
        assert(program.applyArg(sc.toData).evaluateDebug.isSuccess)
    }

    test("TM mint: created != validity upper bound fails (backdating impossible)") {
        // A backdated `created` (upper bound after created) and any other mismatch both fail:
        // created must EQUAL validRange.to, making it an upper bound on the real posting time.
        val sc = mintContext(
          BigInt(1),
          genesisRdmr,
          PList.from(List(configRefInput())),
          PList.from(List(mintedTmOutput())),
          validRange = Interval.between(createdAt + 7_200_000, createdAt + 7_800_000)
        )
        assert(!program.applyArg(sc.toData).evaluateDebug.isSuccess)
        val off = mintContext(
          BigInt(1),
          genesisRdmr,
          PList.from(List(configRefInput())),
          PList.from(List(mintedTmOutput())),
          validRange = Interval.between(createdAt - 600_000, createdAt + 1)
        )
        assert(!program.applyArg(off.toData).evaluateDebug.isSuccess)
    }

    test("TM mint: unbounded validity range fails (created cannot be anchored)") {
        val sc = mintContext(
          BigInt(1),
          genesisRdmr,
          PList.from(List(configRefInput())),
          PList.from(List(mintedTmOutput())),
          validRange = Interval.always
        )
        assert(!program.applyArg(sc.toData).evaluateDebug.isSuccess)
    }

    test("TM GC: creator burns a Confirmed record after the grace period") {
        val sc = gcContext(BigInt(-1), creatorPkh.hash, afterGrace)
        val result = program.applyArg(sc.toData).evaluateDebug
        assert(result.isSuccess, s"Expected success, got: $result")
    }

    test("TM GC: before the grace period elapses fails") {
        val sc = gcContext(BigInt(-1), creatorPkh.hash, Interval.after(createdAt + 1000))
        assert(!program.applyArg(sc.toData).evaluateDebug.isSuccess)
    }

    test("TM GC: non-creator signer fails") {
        val sc = gcContext(BigInt(-1), filled(0x11, 28), afterGrace)
        assert(!program.applyArg(sc.toData).evaluateDebug.isSuccess)
    }

    test("TM GC: spending without burning the TM NFT fails") {
        val sc = gcContext(BigInt(0), creatorPkh.hash, afterGrace)
        assert(!program.applyArg(sc.toData).evaluateDebug.isSuccess)
    }

    test("TmDatum / redeemer Data round-trip") {
        assert(
          unconfirmedDatum.to[TmDatum] == TmDatum
              .Unconfirmed(rawTm, creatorPkh, createdAt, tmEpoch, tmLeaderReward)
        )
        val conf: TmDatum =
            TmDatum.Confirmed(
              txid,
              expectedSwept,
              expectedFulfilled,
              false,
              creatorPkh,
              createdAt,
              tmEpoch,
              tmLeaderReward
            )
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

    test("deployed blueprint .script matches .contract on a valid confirm (untagged param apply)") {
        // Regression for the `_scalusTag` bug: the DEPLOYED script is TreasuryMovementContract.script
        // — the blueprint compiledCode with the 3 ByteString params applied at the UPLC level
        // (BinocularBlueprint.bytesParam), NOT the typed `.contract` form that the other tests eval.
        // With plain Options.release those two DIVERGED: `.contract` accepted a valid confirm while
        // the deployed `.script` ERRORED ("Error evaluated") on the spend branch — so no deployed TM
        // could ever be confirmed. Options.releaseUntagged makes UPLC-level application land the
        // params correctly, so the two agree. Assert that invariant here.
        val ctx = scriptContext(tmValue, confirmedDatum(), redeemer(mpfProof)).toData

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
        // (address, NFT policy, spend all use it). The invariant that matters is that the DEPLOYED
        // form accepts a valid confirm (asserted above); with plain Options.release it did NOT.
    }

    test("tampered Confirmed datum (wrong peg-out amount) fails") {
        val wrongFulfilled = PList.from(
          List(
            PegOutEntry(changeSpk, BigInt(999)), // tampered
            PegOutEntry(paySpk1, BigInt(2000)),
            PegOutEntry(markerSpk(porId1), BigInt(0))
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

    test("forged spent_via_federation_leaf=true fails (N10b — flag is unforgeable)") {
        // The confirmer cannot set the federation-leaf flag freely: the validator recomputes it from
        // the mint-committed signedBtcTx (here `rawTm`, whose input 0 has 0 witness items → false)
        // and bakes it into the datum the continuing output must match. A continuing datum claiming
        // true is therefore rejected — without this, a permissionless confirmer could fabricate the
        // dead-roster evidence treasury.ak::FederationReset consumes.
        val sc =
            scriptContext(
              tmValue,
              confirmedDatum(spentViaFederationLeaf = true),
              redeemer(mpfProof)
            )
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

    test("extra TM-address outputs are tolerated (first output is the checked one)") {
        // The confirm check takes the FIRST output at the TM address; it must carry the NFT and
        // the exact Confirmed datum. Later duplicates are unauthenticated junk (downstream readers
        // filter by the TM NFT, which exists in only one output).
        val sc = scriptContext(
          tmValue,
          confirmedDatum(),
          redeemer(mpfProof),
          extraOutputs = List(confirmedOutput(Value.lovelace(2_000_000), confirmedDatum()))
        )
        val result = program.applyArg(sc.toData).evaluateDebug
        assert(result.isSuccess, s"Expected success, got: $result")
    }

    test("a first TM-address output without the NFT fails (decoy ordering)") {
        // If a decoy without the NFT comes FIRST, the NFT-preservation check fails - the prover
        // cannot demote the genuine continuing output to a later position.
        val decoyFirst = scriptContext(
          Value.lovelace(2_000_000),
          confirmedDatum(),
          redeemer(mpfProof),
          extraOutputs = List(confirmedOutput(tmValue, confirmedDatum()))
        )
        assert(!program.applyArg(decoyFirst.toData).evaluateDebug.isSuccess)
    }

    test("block header not in oracle's confirmed-blocks root fails") {
        // Same merkle-root (bytes 36..68 = txid), but a different nonce → a different block hash
        // that the oracle's MPF does not contain. Isolates the MPF membership check.
        val tamperedHeader = blockHeader.slice(0, 76) ++ ByteString.fromHex("deadbeef")
        val rdmr = TmConfirmRedeemer(
          txIndex = 0,
          txMerkleProof = PList.Nil,
          blockMpfProof = mpfProof,
          blockHeader = BlockHeader(tamperedHeader),
          pegOutSteps = defaultSteps
        ).toData
        val sc = scriptContext(tmValue, confirmedDatum(), rdmr)
        assert(!program.applyArg(sc.toData).evaluateDebug.isSuccess)
    }

    test("NFT containment (Confirm): spending two duplicate Unconfirmed records is rejected") {
        // The fund-theft repro. Two TM-script inputs embed the SAME signedBtcTx (permissionless
        // duplicate posts, both bearing the fungible empty-name TM NFT). Otherwise this is a valid
        // Confirm: the first output at the TM address carries the correct Confirmed datum + the NFT,
        // and the oracle proof holds. Pre-fix, both per-input spend invocations accept that single
        // continuing output, and ledger value-conservation forces the SECOND NFT to escape to the
        // attacker output below (foreign address) bearing a fabricated Confirmed datum — which
        // peg_in.ak trusts by NFT alone → unbacked fBTC. The single-TM-input rule rejects the tx.
        val secondInput = TxInInfo(
          outRef = TxOutRef(TxId(filled(0x05, 32)), BigInt(1)),
          resolved = TxOut(
            address = Address(Credential.ScriptCredential(tmScriptHash), Option.None),
            value = tmValue,
            datum = OutputDatum.OutputDatum(unconfirmedDatum),
            referenceScript = Option.None
          )
        )
        // The escaped NFT lands at an attacker address with a fabricated Confirmed datum (claims an
        // arbitrary outpoint was swept). Ignored by the continuing-output check (foreign address).
        val fabricated: Data =
            (TmDatum.Confirmed(
              filled(0xde, 32),
              PList.from(List(filled(0xfe, 36))),
              PList.Nil,
              false,
              creatorPkh,
              createdAt,
              tmEpoch,
              tmLeaderReward
            ): TmDatum).toData
        val escapedOutput = TxOut(
          address = Address(Credential.ScriptCredential(filled(0x99, 28)), Option.None),
          value = tmValue, // carries the TM NFT (policy = tmScriptHash, empty name, qty 1)
          datum = OutputDatum.OutputDatum(fabricated),
          referenceScript = Option.None
        )
        val sc = scriptContext(
          tmValue,
          confirmedDatum(),
          redeemer(mpfProof),
          extraOutputs = List(escapedOutput),
          extraInputs = List(secondInput, trieInput())
        )
        assert(!program.applyArg(sc.toData).evaluateDebug.isSuccess)
    }

    test("NFT containment (GC): spending two Confirmed records while burning one is rejected") {
        // The GC-path variant. Two grace-expired Confirmed records (same creator) are spent; the tx
        // burns ONE TM NFT (mint == -1) and is signed by the creator after the grace period — so
        // pre-fix each per-input GC invocation passes (its burn/grace/creator checks are all tx-wide
        // and satisfied). Ledger value-conservation then forces the un-burned SECOND NFT to escape
        // to the attacker output below with a fabricated Confirmed datum. The single-TM-input rule
        // on the GC branch rejects it.
        val secondConfirmed = TxInInfo(
          outRef = TxOutRef(TxId(filled(0x05, 32)), BigInt(1)),
          resolved = TxOut(
            address = Address(Credential.ScriptCredential(tmScriptHash), Option.None),
            value = tmValue,
            datum = OutputDatum.OutputDatum(confirmedDatum()),
            referenceScript = Option.None
          )
        )
        val escapedOutput = TxOut(
          address = Address(Credential.ScriptCredential(filled(0x99, 28)), Option.None),
          value = tmValue, // the un-burned second NFT escapes
          datum = OutputDatum.OutputDatum(confirmedDatum()),
          referenceScript = Option.None
        )
        val sc = ScriptContext(
          txInfo = TxInfo(
            inputs = PList.from(List(tmInput(tmValue, confirmedDatum()), secondConfirmed)),
            outputs = PList.from(List(escapedOutput)),
            mint = Value.unsafeFromList(
              PList((tmScriptHash, PList((ByteString.empty, BigInt(-1)))))
            ),
            signatories = PList.from(List(creatorPkh)),
            validRange = afterGrace,
            id = TxId(filled(0x00, 32))
          ),
          redeemer = Data.unit,
          scriptInfo = SpendingScript(ownRef, Option.Some(confirmedDatum()))
        )
        assert(!program.applyArg(sc.toData).evaluateDebug.isSuccess)
    }

    // --- completed-peg-outs trie fold ---

    test("trie fold: one fulfilled peg-out inserts (por_id -> dest_spk ++ amount_le8)") {
        // The default fixture IS the 1-peg-out case. Assert the fold moved the root (a stale root
        // would make this test pass vacuously) and that the value is the exact bytes peg-out.ak
        // rebuilds on Complete.
        assert(defaultEndRoot != emptyRoot)
        assert(
          trieValue(paySpk1, BigInt(2000)) ==
              (paySpk1 ++ ByteString.fromHex("d007000000000000"))
        )
        val sc = scriptContext(tmValue, confirmedDatum(), redeemer(mpfProof))
        val result = program.applyArg(sc.toData).evaluateDebug
        assert(result.isSuccess, s"Expected success, got: $result")
    }

    test("trie fold: three fulfilled peg-outs fold in output order") {
        val raw = rawTxWith(
          List(
            (changeSpk, BigInt(1000)),
            (paySpk1, BigInt(2000)),
            (markerSpk(porId1), BigInt(0)),
            (paySpk2, BigInt(3000)),
            (markerSpk(porId2), BigInt(0)),
            (paySpk3, BigInt(4000)),
            (markerSpk(porId3), BigInt(0))
          )
        )
        val (steps, endTrie) = insertSteps(
          emptyTrie,
          List(
            (porId1, trieValue(paySpk1, BigInt(2000))),
            (porId2, trieValue(paySpk2, BigInt(3000))),
            (porId3, trieValue(paySpk3, BigInt(4000)))
          )
        )
        val sc = confirmContextFor(raw, emptyRoot, endTrie.rootHash, steps)
        val result = program.applyArg(sc.toData).evaluateDebug
        assert(result.isSuccess, s"Expected success, got: $result")
    }

    test("trie fold: a TM with no peg-outs leaves the root unchanged") {
        // Change output only. No pairs, so no steps, and the trie UTxO round-trips untouched. A
        // non-empty starting root proves the root is carried through, not reset.
        val raw = rawTxWith(List((changeSpk, BigInt(1000))))
        val sc = confirmContextFor(raw, defaultEndRoot, defaultEndRoot, PList.Nil)
        val result = program.applyArg(sc.toData).evaluateDebug
        assert(result.isSuccess, s"Expected success, got: $result")
    }

    test("trie fold: a zero-peg-out TM may not change the root") {
        val raw = rawTxWith(List((changeSpk, BigInt(1000))))
        val sc = confirmContextFor(raw, emptyRoot, defaultEndRoot, PList.Nil)
        assertRejects(sc, "TM confirm: trie root does not match the folded steps")
    }

    test("trie fold: AlreadyPresent accepts a duplicate marker with the SAME value") {
        // SPO double-fulfillment tolerance: a second TM re-paying the same PegOutRequest must still
        // confirm, or every peg-in that TM swept is stranded. The root is unchanged.
        val value1 = trieValue(paySpk1, BigInt(2000))
        val preTrie = emptyTrie.insert(porId1, value1)
        val steps: PList[PegOutTrieStep] =
            PList.from(List(PegOutTrieStep.AlreadyPresent(preTrie.proveMembership(porId1))))
        val sc = confirmContextFor(rawTm, preTrie.rootHash, preTrie.rootHash, steps)
        val result = program.applyArg(sc.toData).evaluateDebug
        assert(result.isSuccess, s"Expected success, got: $result")
    }

    test("trie fold: AlreadyPresent with a DIFFERENT stored value fails") {
        // The tolerance is exact-value only. If the trie already maps this POR id to some other
        // payment, accepting would silently rewrite history — membership verification rejects it.
        val preTrie = emptyTrie.insert(porId1, trieValue(paySpk1, BigInt(999)))
        val steps: PList[PegOutTrieStep] =
            PList.from(List(PegOutTrieStep.AlreadyPresent(preTrie.proveMembership(porId1))))
        val sc = confirmContextFor(rawTm, preTrie.rootHash, preTrie.rootHash, steps)
        assertRejects(sc, "Membership verification failed")
    }

    test("trie fold: Insert over an already-present key fails") {
        // `insert` proves ABSENCE; a duplicate must use AlreadyPresent, never Insert.
        val preTrie = emptyTrie.insert(porId1, trieValue(paySpk1, BigInt(2000)))
        val steps: PList[PegOutTrieStep] =
            PList.from(List(PegOutTrieStep.Insert(preTrie.proveMembership(porId1))))
        val sc = confirmContextFor(rawTm, preTrie.rootHash, preTrie.rootHash, steps)
        assertRejects(sc, "Invalid proof or element exists")
    }

    test("trie fold: a continuing output with the wrong final root fails") {
        val sc = scriptContext(
          tmValue,
          confirmedDatum(),
          redeemer(mpfProof),
          trieOutputs = List(trieOutput(filled(0x5a, 32)))
        )
        assertRejects(sc, "TM confirm: trie root does not match the folded steps")
    }

    test("trie fold: an unchanged root on a 1-peg-out TM fails") {
        val sc = scriptContext(
          tmValue,
          confirmedDatum(),
          redeemer(mpfProof),
          trieOutputs = List(trieOutput(emptyRoot))
        )
        assertRejects(sc, "TM confirm: trie root does not match the folded steps")
    }

    test("trie fold: not spending the trie UTxO fails") {
        val sc =
            scriptContext(tmValue, confirmedDatum(), redeemer(mpfProof), extraInputs = List.empty)
        assertRejects(sc, "TM confirm: completed-peg-outs trie not spent")
    }

    test("trie fold: no continuing trie output fails") {
        val sc =
            scriptContext(tmValue, confirmedDatum(), redeemer(mpfProof), trieOutputs = List.empty)
        assertRejects(sc, "TM confirm: no continuing completed-peg-outs output")
    }

    test("trie fold: a forged trie NFT policy is not accepted") {
        // A CPO-named token under a policy the Config does not publish is invisible to the
        // validator, so the genuine trie UTxO is simply missing.
        val forged = filled(0xc9, 28)
        val sc = scriptContext(
          tmValue,
          confirmedDatum(),
          redeemer(mpfProof),
          extraInputs = List(trieInput(value = trieNftValue(forged))),
          trieOutputs = List(trieOutput(defaultEndRoot, value = trieNftValue(forged)))
        )
        assertRejects(sc, "TM confirm: completed-peg-outs trie not spent")
    }

    test("trie fold: moving the trie NFT to a different address fails") {
        val sc = scriptContext(
          tmValue,
          confirmedDatum(),
          redeemer(mpfProof),
          trieOutputs = List(
            trieOutput(
              defaultEndRoot,
              address = Address(Credential.ScriptCredential(filled(0x99, 28)), Option.None)
            )
          )
        )
        assertRejects(sc, "TM confirm: trie address changed")
    }

    test("trie fold: a missing config reference input fails") {
        val sc =
            scriptContext(tmValue, confirmedDatum(), redeemer(mpfProof), cfgRefs = List.empty)
        assertRejects(sc, "TM confirm: no config reference input")
    }

    test("trie fold: a config UTxO without the config NFT is ignored") {
        val sc = scriptContext(
          tmValue,
          confirmedDatum(),
          redeemer(mpfProof),
          cfgRefs = List(configRefInput(withNft = false))
        )
        assertRejects(sc, "TM confirm: no config reference input")
    }

    test("trie fold: a config publishing a different trie policy fails") {
        // Config field 3 is the ONLY authority on which UTxO is the trie. Point it elsewhere and
        // the trie input the tx actually spends is no longer found.
        val sc = scriptContext(
          tmValue,
          confirmedDatum(),
          redeemer(mpfProof),
          cfgRefs = List(configRefInput(cpoPolicy = filled(0xc8, 28)))
        )
        assertRejects(sc, "TM confirm: completed-peg-outs trie not spent")
    }

    test("trie fold: an odd output count after the change output fails") {
        // A payment with no marker: the protocol would have no way to know which request it settles.
        val raw = rawTxWith(List((changeSpk, BigInt(1000)), (paySpk1, BigInt(2000))))
        val (steps, endTrie) =
            insertSteps(emptyTrie, List((porId1, trieValue(paySpk1, BigInt(2000)))))
        val sc = confirmContextFor(raw, emptyRoot, endTrie.rootHash, steps)
        assertRejects(sc, "TM confirm: odd output count after the change output")
    }

    test("trie fold: a marker with the peg-in \"BFR\" prefix fails") {
        // Right length, right OP_RETURN push, wrong tag. Only "POR" commits a peg-out id.
        val raw = rawTxWith(
          List(
            (changeSpk, BigInt(1000)),
            (paySpk1, BigInt(2000)),
            (markerSpk(porId1, tag = "424652"), BigInt(0)) // "BFR"
          )
        )
        val (steps, endTrie) =
            insertSteps(emptyTrie, List((porId1, trieValue(paySpk1, BigInt(2000)))))
        val sc = confirmContextFor(raw, emptyRoot, endTrie.rootHash, steps)
        assertRejects(sc, "TM confirm: payment output without a POR marker")
    }

    test("trie fold: a marker sitting in the payment position fails") {
        // Swapped pair. Without the payment-position check a marker's own script+0 sats could be
        // recorded as a fulfilled payment.
        val raw = rawTxWith(
          List(
            (changeSpk, BigInt(1000)),
            (markerSpk(porId1), BigInt(0)),
            (paySpk1, BigInt(2000))
          )
        )
        val (steps, endTrie) =
            insertSteps(emptyTrie, List((porId1, trieValue(paySpk1, BigInt(2000)))))
        val sc = confirmContextFor(raw, emptyRoot, endTrie.rootHash, steps)
        assertRejects(sc, "TM confirm: POR marker in a payment position")
    }

    test("trie fold: fewer steps than marker pairs fails") {
        val sc = scriptContext(
          tmValue,
          confirmedDatum(),
          redeemer(mpfProof, pegOutSteps = PList.Nil)
        )
        assertRejects(sc, "TM confirm: missing trie step for a pair")
    }

    test("trie fold: more steps than marker pairs fails") {
        val raw = rawTxWith(List((changeSpk, BigInt(1000))))
        val (steps, endTrie) =
            insertSteps(emptyTrie, List((porId1, trieValue(paySpk1, BigInt(2000)))))
        val sc = confirmContextFor(raw, emptyRoot, endTrie.rootHash, steps)
        assertRejects(sc, "TM confirm: more trie steps than marker pairs")
    }

    test("trie fold: a wrong trie value (tampered amount) cannot be inserted") {
        // The confirmer builds the steps, but not the value: the validator derives it from the TM's
        // own payment output, so a step proving a different value yields a different root.
        val (steps, endTrie) =
            insertSteps(emptyTrie, List((porId1, trieValue(paySpk1, BigInt(1999)))))
        val sc = confirmContextFor(rawTm, emptyRoot, endTrie.rootHash, steps)
        assertRejects(sc, "TM confirm: trie root does not match the folded steps")
    }

    test("trie fold: a wrong POR id key cannot be inserted") {
        val (steps, endTrie) =
            insertSteps(emptyTrie, List((porId2, trieValue(paySpk1, BigInt(2000)))))
        val sc = confirmContextFor(rawTm, emptyRoot, endTrie.rootHash, steps)
        assertRejects(sc, "TM confirm: trie root does not match the folded steps")
    }

    test("marker recognition: length and prefix are both required") {
        assert(TreasuryMovementValidator.isPorMarker(markerSpk(porId1)))
        assert(TreasuryMovementValidator.porMarkerId(markerSpk(porId1)) == porId1)
        // Wrong tag.
        assert(!TreasuryMovementValidator.isPorMarker(markerSpk(porId1, tag = "424652")))
        // Right prefix, wrong length (truncated payload).
        assert(!TreasuryMovementValidator.isPorMarker(markerSpk(porId1).slice(0, 36)))
        // A 37-byte payment script must not be mistaken for a marker.
        assert(!TreasuryMovementValidator.isPorMarker(filled(0x51, 37)))
    }

    // --- budget measurements ---

    // Execution budgets for the three treasury-movement happy paths, printed so they land in the
    // CI log (Milestone 4 performance evidence). Synthetic ScriptContexts, so only the script
    // execution budget and ex-unit fee are meaningful here (no full tx fee).
    test("Treasury movement budgets - mint Genesis, mint Chain, Confirm spend") {
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
          "Mint Genesis",
          mintContext(
            BigInt(1),
            genesisRdmr,
            PList.from(List(configRefInput())),
            PList.from(List(mintedTmOutput()))
          )
        )
        measure(
          "Mint Chain",
          mintContext(
            BigInt(1),
            chainRdmr(0),
            PList.from(List(predecessorRefInput(prevTxid = filled(0xaa, 32)))),
            PList.from(List(mintedTmOutput()))
          )
        )
        measure("Confirm spend", scriptContext(tmValue, confirmedDatum(), redeemer(mpfProof)))
        // Batch sizing evidence: the trie fold adds one MPF insert (proof walk + two blake2b_256)
        // per fulfilled peg-out, so the Confirm cost grows with the batch. Measure a 0-peg-out and
        // an 8-peg-out TM to bracket it.
        measure(
          "Confirm 0 pegout",
          confirmContextFor(
            rawTxWith(List((changeSpk, BigInt(1000)))),
            defaultEndRoot,
            defaultEndRoot,
            PList.Nil
          )
        )
        val batch = List.range(0, 8).map { i =>
            val spk = ByteString.fromHex("0014" + (f"$i%02x" * 20))
            val id = filled(0xe0 + i, 32)
            (id, spk, BigInt(1000 + i))
        }
        val batchRaw = rawTxWith(
          (changeSpk, BigInt(1000)) ::
              batch.flatMap((id, spk, amt) => List((spk, amt), (markerSpk(id), BigInt(0))))
        )
        val (batchSteps, batchTrie) =
            insertSteps(emptyTrie, batch.map((id, spk, amt) => (id, trieValue(spk, amt))))
        measure(
          "Confirm 8 pegout",
          confirmContextFor(batchRaw, emptyRoot, batchTrie.rootHash, batchSteps)
        )
    }

}
