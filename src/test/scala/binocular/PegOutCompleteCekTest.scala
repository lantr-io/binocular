package binocular

import binocular.watchtower.*

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry.ProofStep
import scalus.cardano.onchain.plutus.prelude.{List as PList, *}
import scalus.cardano.onchain.plutus.v1.{Address, Credential, Interval, Value}
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.v3.ScriptInfo.RewardingScript
import scalus.uplc.Program
import scalus.uplc.builtin.Builtins.{integerToByteString, serialiseData, sha2_256}
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData
import scalus.uplc.eval.PlutusVM

/** CEK-evaluation tests for the peg-out completion path against the REAL Aiken `peg_out_validator`
  * from ft-bifrost-bridge's blueprint.
  *
  * This is the test that matters for the POR sweeper. Everything the sweeper computes off-chain is
  * only useful if `peg-out.ak` accepts it, and four separate encodings have to line up for that:
  *
  *   - the POR id ([[CpoTrieMirror.porId]] vs Aiken `utils.hash_output_ref` on the input's own
  *     outpoint),
  *   - the trie value ([[CompletedPegOutsTrie.trieValue]] vs
  *     `dest ++ integer_to_bytearray(False, 8, locked − fee)`),
  *   - the MPF membership proof (Scalus's off-chain `proveMembership` vs Aiken's `mpf.has`),
  *   - the [[PegOutWithdrawRedeemer]] Constr shape, including the fact that BOTH indices address
  *     `reference_inputs`.
  *
  * Every one of them is exercised here by running the deployed script bytes, so a drift in any of
  * them fails at build time rather than as a rejected transaction on preprod.
  *
  * The blueprint is [[BifrostBlueprint.packaged]] — the same bytes the runtime falls back to — so
  * these tests also pin the packaged resource against the sweeper's expectations.
  */
class PegOutCompleteCekTest extends AnyFunSuite {

    private given PlutusVM = PlutusVM.makePlutusV3VM()

    private def filled(v: Int, n: Int): ByteString =
        ByteString.fromArray(Array.fill[Byte](n)(v.toByte))

    // --- protocol identifiers ---
    private val configNftPolicy = filled(0xc0, 28)
    private val configNftAsset = ByteString.fromString("BIFCFG")
    private val bridgedTokenPolicy = filled(0xa1, 28)
    // spec [CFG-1]: the asset name is a protocol constant, not a Config field.
    private val bridgedTokenAsset = ConfigDatum.BridgedTokenAssetName
    private val bridgeStatePolicy = filled(0xb2, 28)
    private val bssAsset = ByteString.fromString("BSS")
    private val ownerPkh = filled(0xd3, 28)
    private val destSpk = ByteString.fromHex("0014" + ("ab" * 20))

    private val locked = 100_000L
    private val fee = 1_000L
    private val created = 1_700_000_000_000L

    /** `peg_out.ak::peg_out_cancel_timeout_ms` — 30 days in milliseconds. */
    private val cancelTimeoutMs = 2_592_000_000L

    private val blueprint = BifrostBlueprint.packaged
    private val pegOutHash =
        ByteString.fromArray(
          PegOutContract(blueprint, configNftPolicy).policyId.bytes
        )

    /** The deployed script bytes, with the two CIP-57 parameters applied — exactly what
      * [[PegOutContract]] hashes.
      */
    private val program: Program =
        Program
            .fromCborHex(blueprint.compiledCode(PegOutContract.ValidatorTitle))
            .$(Data.B(configNftPolicy))

    // --- fixtures -------------------------------------------------------------------------------

    private def ada(n: Long): (ByteString, PList[(ByteString, BigInt)]) =
        (ByteString.empty, PList((ByteString.empty, BigInt(n))))

    private def porRef(seed: Int = 0x33, index: Int = 0) =
        TxOutRef(TxId(filled(seed, 32)), BigInt(index))

    private def porId(ref: TxOutRef): ByteString =
        CpoTrieMirror.porId(ref.id.hash, ref.idx.toLong)

    /** The rev-5.5 twelve-field ConfigDatum. The withdraw reads fields 2 (bridged token policy) and
      * 4 (bridge_state_policy) — both shifted up by one when `params` moved to index 1 — and the
      * asset name is the [CFG-1] constant.
      */
    private def configDatum: Data = ConfigDatum(
      updateAuth = Option.None,
      bridgedTokenPolicy = bridgedTokenPolicy,
      completedPegInsPolicy = ByteString.empty,
      bridgeStatePolicy = bridgeStatePolicy,
      tmScriptHash = ByteString.empty,
      pegInScriptHash = ByteString.empty,
      pegOutScriptHash = pegOutHash,
      spoBansPolicyId = ByteString.empty,
      sposRegistryPolicyId = ByteString.empty,
      treasuryInfoPolicyId = ByteString.empty,
      yFederation = ByteString.fromHex("f9" * 32),
      federationOneShot = TxOutRef(TxId(ByteString.fromHex("c3" * 32)), BigInt(0)),
      params = ConfigParams(
        baseBanDurationMs = BigInt(0),
        maxFaultsBeforePermanent = BigInt(0),
        maxValidityWindowMs = BigInt(0),
        federationCsvBlocks = BigInt(144),
        peginRefundTimeoutBlocks = BigInt(720),
        feeRateSatPerVb = BigInt(1),
        perPegoutFee = BigInt(fee),
        minPegOutFbtc = BigInt(0),
        schedule = ScheduleParams(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
      )
    ).toData

    private def configRefInput = TxInInfo(
      outRef = TxOutRef(TxId(filled(0x11, 32)), BigInt(0)),
      resolved = TxOut(
        address = Address(Credential.ScriptCredential(configNftPolicy), Option.None),
        value = Value.unsafeFromList(
          PList(ada(2_000_000), (configNftPolicy, PList((configNftAsset, BigInt(1)))))
        ),
        datum = OutputDatum.OutputDatum(configDatum),
        referenceScript = Option.None
      )
    )

    /** The bridge-state singleton as a REFERENCE input (spec [CPO-13]), authenticated by the NFT
      * `(bridge_state_policy, "BSS")` and carrying the full 4-field [[BridgeState]] datum. The
      * `spi_root` is a DECOY (a different root): a [LIB-2]-style blind field-0 read would pick it
      * up and fail every membership/exclusion proof, so only reading `cpo_root` by name passes.
      */
    private def trieRefInput(root: ByteString, withNft: Boolean = true) = TxInInfo(
      outRef = TxOutRef(TxId(filled(0x22, 32)), BigInt(0)),
      resolved = TxOut(
        address = Address(Credential.ScriptCredential(bridgeStatePolicy), Option.None),
        value =
            if withNft then
                Value.unsafeFromList(
                  PList(ada(2_000_000), (bridgeStatePolicy, PList((bssAsset, BigInt(1)))))
                )
            else Value.unsafeFromList(PList(ada(2_000_000))),
        datum = OutputDatum.OutputDatum(
          BridgeState(
            spiRoot = filled(0x5a, 32),
            cpoRoot = root,
            treasuryUtxoId = filled(0x33, 32) ++ ByteString.fromHex("00000000"),
            treasuryAmount = BigInt(5_000_000)
          ).toData
        ),
        referenceScript = Option.None
      )
    )

    private def porDatum(perFee: Long): Data = PegOutDatum(
      ownerAuth = AuthorizationMethod.CardanoSignature(ownerPkh),
      sourceChainDestinationAddress = destSpk,
      perPegoutFee = BigInt(perFee),
      created = BigInt(created)
    ).toData

    private def porInput(ref: TxOutRef, lockedSat: Long, perFee: Long) = TxInInfo(
      outRef = ref,
      resolved = TxOut(
        address = Address(Credential.ScriptCredential(pegOutHash), Option.None),
        value = Value.unsafeFromList(
          PList(ada(2_000_000), (bridgedTokenPolicy, PList((bridgedTokenAsset, BigInt(lockedSat)))))
        ),
        datum = OutputDatum.OutputDatum(porDatum(perFee)),
        referenceScript = Option.None
      )
    )

    private def burn(qty: Long): Value =
        Value.unsafeFromList(PList((bridgedTokenPolicy, PList((bridgedTokenAsset, BigInt(qty))))))

    /** The trie the watchtower's mirror would hold, and the proof it would build. */
    private def mirrorWith(entries: Seq[(ByteString, ByteString)]): CpoTrieMirror =
        CpoTrieMirror.fromEntries(entries).fold(e => fail(e), identity)

    private def trieValue(spk: ByteString, netSat: Long): ByteString =
        CompletedPegOutsTrie.trieValue(PegOutEntry(spk, BigInt(netSat)))

    private def withdrawContext(
        por: TxInInfo,
        trie: TxInInfo,
        action: PegOutActionType,
        mint: Value,
        signatories: PList[PubKeyHash] = PList.Nil,
        validRange: Interval = Interval.always
    ): Data = ScriptContext(
      txInfo = TxInfo(
        inputs = PList(por),
        referenceInputs = PList(configRefInput, trie),
        mint = mint,
        validRange = validRange,
        signatories = signatories,
        id = TxId(filled(0x00, 32))
      ),
      redeemer = PegOutWithdrawRedeemer(
        // BOTH indices address `reference_inputs`: 0 = Config, 1 = the bridge-state singleton.
        configRefInputIndex = 0,
        completedPegOutsRefInputIndex = 1,
        actionType = action
      ).toData,
      scriptInfo = RewardingScript(Credential.ScriptCredential(pegOutHash))
    ).toData

    private def accepts(ctx: Data): Boolean = program.$(ctx).evaluateDebug.isSuccess

    // --- Complete -------------------------------------------------------------------------------

    /** The single-entry trie a TM that fulfilled exactly this request would attest. */
    private def happyMirror(ref: TxOutRef) =
        mirrorWith(Seq(porId(ref) -> trieValue(destSpk, locked - fee)))

    private def proofFor(m: CpoTrieMirror, key: ByteString): PList[ProofStep] =
        m.proveMembership(key).fold(e => fail(e), identity)

    test("the sweeper's proof + redeemer complete a paid request") {
        val ref = porRef()
        val m = happyMirror(ref)
        assert(
          accepts(
            withdrawContext(
              porInput(ref, locked, fee),
              trieRefInput(m.root),
              PegOutActionType.CompletePegOut(proofFor(m, porId(ref))),
              burn(-locked)
            )
          )
        )
    }

    test("completion needs no signature from the request's owner") {
        // Permissionless cleanup (spec rev 5.1): the sweeper never has the owner's key. A stranger's
        // signature is present here purely to show it is neither required nor sufficient-by-identity.
        val ref = porRef()
        val m = happyMirror(ref)
        assert(
          accepts(
            withdrawContext(
              porInput(ref, locked, fee),
              trieRefInput(m.root),
              PegOutActionType.CompletePegOut(proofFor(m, porId(ref))),
              burn(-locked),
              signatories = PList(PubKeyHash(filled(0xee, 28)))
            )
          )
        )
    }

    test("a proof from a multi-entry trie completes the right request") {
        // The single-entry case only exercises a Leaf proof step. A populated trie forces real
        // Branch/Fork steps, which is where an off-chain/on-chain encoding drift would show up.
        val refs: Vector[TxOutRef] = Vector.tabulate(6)(i => porRef(0x40 + i, i))
        val entries: Vector[(ByteString, ByteString)] = refs.zipWithIndex.map { case (r, i) =>
            porId(r) -> trieValue(destSpk, locked - fee + i)
        }
        val m = mirrorWith(entries)
        assert(m.size == 6)
        val target = refs(3)
        assert(
          accepts(
            withdrawContext(
              porInput(target, locked + 3, fee),
              trieRefInput(m.root),
              PegOutActionType.CompletePegOut(proofFor(m, porId(target))),
              burn(-(locked + 3))
            )
          )
        )
    }

    test("a request whose pinned fee disagrees with the trie value cannot complete") {
        val ref = porRef()
        val m = happyMirror(ref)
        assert(
          !accepts(
            withdrawContext(
              // fee + 1 shifts net_amount by one, so the value the validator rebuilds no longer
              // matches what the trie committed.
              porInput(ref, locked, fee + 1),
              trieRefInput(m.root),
              PegOutActionType.CompletePegOut(proofFor(m, porId(ref))),
              burn(-locked)
            )
          )
        )
    }

    test("a proof for a different request's POR id is rejected") {
        val ref = porRef()
        val foreign = porRef(0x77, 0)
        val m = mirrorWith(
          Seq(
            porId(ref) -> trieValue(destSpk, locked - fee),
            porId(foreign) -> trieValue(destSpk, locked - fee)
          )
        )
        assert(
          !accepts(
            withdrawContext(
              porInput(ref, locked, fee),
              trieRefInput(m.root),
              PegOutActionType.CompletePegOut(proofFor(m, porId(foreign))),
              burn(-locked)
            )
          )
        )
    }

    test("a partial burn is rejected") {
        val ref = porRef()
        val m = happyMirror(ref)
        assert(
          !accepts(
            withdrawContext(
              porInput(ref, locked, fee),
              trieRefInput(m.root),
              PegOutActionType.CompletePegOut(proofFor(m, porId(ref))),
              burn(-(locked - 1))
            )
          )
        )
    }

    test("a singleton reference input without the BSS NFT is rejected") {
        val ref = porRef()
        val m = happyMirror(ref)
        assert(
          !accepts(
            withdrawContext(
              porInput(ref, locked, fee),
              trieRefInput(m.root, withNft = false),
              PegOutActionType.CompletePegOut(proofFor(m, porId(ref))),
              burn(-locked)
            )
          )
        )
    }

    test("a stale mirror root is rejected") {
        // The mirror holds an entry the on-chain singleton does not yet commit to. This is the
        // failure mode the sweeper's hard root check exists to prevent it from ever submitting.
        val ref = porRef()
        val m = happyMirror(ref)
        val stale = CpoTrieMirror.empty
        assert(
          !accepts(
            withdrawContext(
              porInput(ref, locked, fee),
              trieRefInput(stale.root),
              PegOutActionType.CompletePegOut(proofFor(m, porId(ref))),
              burn(-locked)
            )
          )
        )
    }

    // --- Cancel ---------------------------------------------------------------------------------

    test("the mirror also serves the Cancel exclusion proof") {
        // Not a sweeper path, but the same mirror is what a request's owner uses to cancel an
        // UNPAID request, so the non-membership encoding is pinned here too.
        val ref = porRef()
        val other = porRef(0x88, 0)
        val m = mirrorWith(Seq(porId(other) -> trieValue(destSpk, locked - fee)))
        val proof = m.trie.proveNonMembership(porId(ref))
        assert(
          accepts(
            withdrawContext(
              porInput(ref, locked, fee),
              trieRefInput(m.root),
              PegOutActionType.Cancel(proof),
              Value.zero,
              signatories = PList(PubKeyHash(ownerPkh)),
              validRange = Interval.after(BigInt(created + cancelTimeoutMs + 1))
            )
          )
        )
    }

    test("a paid request cannot be cancelled") {
        val ref = porRef()
        val m = happyMirror(ref)
        // `proveNonMembership` refuses a key that IS present, so the owner has to fabricate a proof.
        // The empty proof is the natural attempt; it reconstructs the empty root, not this one.
        assert(
          !accepts(
            withdrawContext(
              porInput(ref, locked, fee),
              trieRefInput(m.root),
              PegOutActionType.Cancel(PList.Nil),
              Value.zero,
              signatories = PList(PubKeyHash(ownerPkh)),
              validRange = Interval.after(BigInt(created + cancelTimeoutMs + 1))
            )
          )
        )
    }

    // --- encodings ------------------------------------------------------------------------------

    test("the trie value is dest_spk ++ 8-byte little-endian net amount") {
        val v = trieValue(destSpk, locked - fee)
        assert(v.size == destSpk.size + 8)
        assert(v.slice(0, destSpk.size) == destSpk)
        assert(
          v.slice(destSpk.size, 8) == integerToByteString(false, 8, BigInt(locked - fee))
        )
    }

    test("the POR id is sha2_256 of the serialised OutputReference") {
        val ref = porRef()
        val expected = sha2_256(serialiseData(ref.toData))
        assert(porId(ref) == expected)
        assert(porId(ref) != porId(porRef(0x33, 1)))
    }
}
