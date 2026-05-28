package binocular.watchtower

import binocular.*
import binocular.bitcoin.*
import binocular.oracle.{BlockHeader, ChainState}

import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry as MPF
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry.ProofStep
import scalus.cardano.onchain.plutus.prelude.{List as ScalusList, *}
import scalus.cardano.onchain.plutus.v1.{Credential, PubKeyHash}
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.*
import scalus.compiler.{Compile, Options}
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.*
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.Data.{toData, FromData, ToData}

/** A single fulfilled peg-out parsed from a Treasury Movement output: the raw Bitcoin
  * `scriptPubKey` the TM pays to, and the satoshi `amount`. Mirrors the doc's `fulfilled_peg_outs`
  * entries (technical_documentation.md §"Confirm TM tx").
  */
case class PegOutEntry(scriptPubKey: ByteString, amount: BigInt) derives FromData, ToData

@Compile
object PegOutEntry

/** Datum of the treasury-movement (TM) UTxO.
  *
  *   - [[Unconfirmed]] — created when the signed Bitcoin TM is posted to Cardano. Carries the full
  *     segwit-serialized `signedBtcTx` (the bytes watchtowers relay to Bitcoin). Constr tag 0. This
  *     is the single-field shape heimdall's `publish.rs` (`Constr(0, [signed_btc_tx])`) and
  *     binocular's `create-tmtx` scaffold already post — do not add fields.
  *   - [[Confirmed]] — produced by the Confirm transition once the TM is Binocular-confirmed. Holds
  *     the `btcTxid`, the list of swept peg-in outpoints (`sweptPegInUtxoIds`, 36-byte
  *     prev_txid++vout each), and the `fulfilledPegOuts`. Constr tag 1.
  *
  * Variant order is positional in the Plutus Constr — do not reorder.
  */
enum TmDatum derives FromData, ToData {
    case Unconfirmed(signedBtcTx: ByteString)
    case Confirmed(
        btcTxid: ByteString,
        sweptPegInUtxoIds: ScalusList[ByteString],
        fulfilledPegOuts: ScalusList[PegOutEntry]
    )
}

@Compile
object TmDatum

/** Redeemer for the Confirm transition.
  *
  * @param txIndex
  *   0-based index of the TM tx within its Bitcoin block.
  * @param txMerkleProof
  *   sibling hashes from the TM txid up to the block header's tx-merkle-root.
  * @param blockMpfProof
  *   MPF membership proof that the block hash is in the oracle's `confirmedBlocksRoot`.
  * @param blockHeader
  *   the 80-byte Bitcoin block header (its merkle-root is checked, and it must hash to the
  *   oracle-confirmed block hash).
  */
case class TmConfirmRedeemer(
    txIndex: BigInt,
    txMerkleProof: ScalusList[ByteString],
    blockMpfProof: ScalusList[ProofStep],
    blockHeader: BlockHeader
) derives FromData,
      ToData

@Compile
object TmConfirmRedeemer

/** Datum of the TM-control state UTxO — holds the key authorized to mint TM NFTs. Lives in a UTxO
  * authenticated by a one-shot control NFT (see [[TreasuryMovementValidator.findControlInput]]);
  * the NFT, not this datum's location, is the trust anchor (a forged UTxO can carry any datum but
  * not the one-shot NFT). `authorizedMinter` is a 28-byte pubkey-hash; level C will generalize this
  * to a roster + leader-election rule, with no change to the TM script hash / address.
  */
case class TmControlDatum(authorizedMinter: ByteString) derives FromData, ToData

@Compile
object TmControlDatum

/** Treasury-movement validator: enforces the `Unconfirmed -> Confirmed` transition on-chain.
  *
  * This replaces the always-ok scaffold (`TmtxScript`). The only legal spend of an
  * [[TmDatum.Unconfirmed]] UTxO recreates it as [[TmDatum.Confirmed]], and only if the spender
  * *proves* the TM is confirmed on Bitcoin against the Binocular oracle:
  *
  *   1. `txid = sha256d(strip_witness(signedBtcTx))` — recomputed on-chain, never trusted.
  *   2. the block header is in the oracle's `confirmedBlocksRoot` (MPF membership; oracle UTxO is a
  *      reference input, identified by the script hash applied as a compile parameter).
  *   3. the header hashes to the MPF-proven block hash.
  *   4. `txid` is merkle-included in the header's tx-merkle-root at `txIndex`.
  *   5. the continuing output sits at the same TM script address, preserves the UTxO value (so the
  *      TM identity token rides along), and carries a `Confirmed` datum whose `btcTxid` /
  *      `sweptPegInUtxoIds` / `fulfilledPegOuts` are exactly what the contract parsed out of the
  *      raw TM transaction.
  *   6. **NOT YET ENFORCED** (open TODO at the spend handler): check that `signedBtcTx` is in fact
  *      the protocol's Treasury Movement transaction by matching its input/output script-pubkey
  *      against the address recorded in a referenced Treasury-State UTxO (NFT-authenticated). The
  *      current implementation validates only the TM NFT identity + the parsed-vs-datum agreement
  *      below, so on-chain does NOT yet rule out an attacker producing a syntactically valid but
  *      semantically-unrelated raw Bitcoin tx whose merkle inclusion proof happens to validate
  *      against the same block header. Wiring this is gated on the Treasury-State UTxO shape being
  *      finalised; until then operators must rely on heimdall posting only legitimate TM bytes.
  *
  * Parameterized by the Binocular oracle script hash (applied via
  * [[TreasuryMovementContract.contract]]).
  */
@Compile
object TreasuryMovementValidator {

    /** All input outpoints (prev_txid(32) ++ prev_vout(4), 36 bytes each) of a raw Bitcoin tx, in
      * input order. These are the `sweptPegInUtxoIds` of a TM (the old treasury input is included —
      * inert, as no PegInRequest can match it).
      */
    def allInputOutpoints(rawTx: ByteString): ScalusList[ByteString] = {
        val txInsStart = if BitcoinHelpers.isWitnessTransaction(rawTx) then BigInt(6) else BigInt(4)
        val numAndOffset = BitcoinHelpers.readVarInt(rawTx, txInsStart)
        def loop(remaining: BigInt, offset: BigInt): ScalusList[ByteString] =
            if remaining == BigInt(0) then ScalusList.Nil
            else
                val outpoint = rawTx.slice(offset, 36)
                val lenAndAfter = BitcoinHelpers.readVarInt(rawTx, offset + 36)
                val scriptLen = lenAndAfter._1
                val afterVarInt = lenAndAfter._2
                val nextOffset = afterVarInt + scriptLen + 4 // + 4-byte sequence
                ScalusList.Cons(outpoint, loop(remaining - 1, nextOffset))
        loop(numAndOffset._1, numAndOffset._2)
    }

    /** All outputs of a raw Bitcoin tx as `(scriptPubKey, amount)`, in output order. These are the
      * `fulfilledPegOuts` of a TM (the new treasury output is included — inert).
      */
    def allOutputs(rawTx: ByteString): ScalusList[PegOutEntry] = {
        val txInsStart = if BitcoinHelpers.isWitnessTransaction(rawTx) then BigInt(6) else BigInt(4)
        val afterIns = BitcoinHelpers.skipTxIns(rawTx, txInsStart)
        val numAndOffset = BitcoinHelpers.readVarInt(rawTx, afterIns)
        def loop(remaining: BigInt, offset: BigInt): ScalusList[PegOutEntry] =
            if remaining == BigInt(0) then ScalusList.Nil
            else
                val amount = byteStringToInteger(false, rawTx.slice(offset, 8))
                val lenAndAfter = BitcoinHelpers.readVarInt(rawTx, offset + 8)
                val scriptLen = lenAndAfter._1
                val afterVarInt = lenAndAfter._2
                val script = rawTx.slice(afterVarInt, scriptLen)
                val nextOffset = afterVarInt + scriptLen
                ScalusList.Cons(PegOutEntry(script, amount), loop(remaining - 1, nextOffset))
        loop(numAndOffset._1, numAndOffset._2)
    }

    /** Find the Binocular oracle UTxO among reference inputs. Matches both the oracle script hash
      * AND the oracle NFT (policy = oracle script hash, empty asset name, qty 1) so a stray/junk
      * UTxO sitting at the oracle address cannot feed a stale or forged ChainState.
      */
    def findOracleInput(
        refInputs: ScalusList[TxInInfo],
        oracleScriptHash: ByteString
    ): TxOut = {
        def search(remaining: ScalusList[TxInInfo]): TxOut =
            remaining match
                case ScalusList.Nil => fail("Oracle reference input not found")
                case ScalusList.Cons(input, tail) =>
                    val resolved = input.resolved
                    resolved.address.credential match
                        case Credential.ScriptCredential(hash) =>
                            if hash == oracleScriptHash
                                && resolved.value.quantityOf(
                                  oracleScriptHash,
                                  ByteString.empty
                                ) == BigInt(1)
                            then resolved
                            else search(tail)
                        case _ => search(tail)
        search(refInputs)
    }

    /** Find the TM-control state UTxO among reference inputs by its one-shot control NFT
      * `(controlNftPolicy, controlNftName)`. The NFT — not the UTxO's address — is the trust
      * anchor: it is minted exactly once, so a forged UTxO with an attacker-chosen
      * [[TmControlDatum]] cannot carry it and is never read.
      */
    def findControlInput(
        refInputs: ScalusList[TxInInfo],
        controlNftPolicy: ByteString,
        controlNftName: ByteString
    ): TxOut = {
        def search(remaining: ScalusList[TxInInfo]): TxOut =
            remaining match
                case ScalusList.Nil => fail("TM-control reference input (NFT) not found")
                case ScalusList.Cons(input, tail) =>
                    val resolved = input.resolved
                    if resolved.value.quantityOf(controlNftPolicy, controlNftName) == BigInt(1) then
                        resolved
                    else search(tail)
        search(refInputs)
    }

    /** Resolve the TM script address from the input being spent (identified by `ownRef`). */
    def ownResolved(inputs: ScalusList[TxInInfo], ownRef: TxOutRef): TxOut = {
        def search(remaining: ScalusList[TxInInfo]): TxOut =
            remaining match
                case ScalusList.Nil => fail("Own input not found")
                case ScalusList.Cons(input, tail) =>
                    if equalsData(input.outRef.toData, ownRef.toData) then input.resolved
                    else search(tail)
        search(inputs)
    }

    /** The UNIQUE transaction output paid to `address` (the continuing TM UTxO). Fails if there are
      * zero or more than one — otherwise a spend could add a second TM-address output to split the
      * marker token or seed a parallel forged UTxO while only the first is value/datum-checked.
      */
    def continuingOutput(outputs: ScalusList[TxOut], address: Address): TxOut = {
        def loop(remaining: ScalusList[TxOut], acc: Option[TxOut]): TxOut =
            remaining match
                case ScalusList.Nil =>
                    acc match
                        case Option.Some(o) => o
                        case Option.None    => fail("No continuing TM output")
                case ScalusList.Cons(out, tail) =>
                    if equalsData(out.address.toData, address.toData) then
                        acc match
                            case Option.Some(_) => fail("Multiple TM-address outputs")
                            case Option.None    => loop(tail, Option.Some(out))
                    else loop(tail, acc)
        loop(outputs, Option.None)
    }

    def spend(
        oracleScriptHash: ByteString,
        datumOpt: Option[Datum],
        tx: TxInfo,
        ownRef: TxOutRef,
        redeemer: Datum
    ): Unit = {
        val datum = datumOpt match
            case Option.Some(d) => d.to[TmDatum]
            case Option.None    => fail("Missing TM datum")

        // Only the Unconfirmed -> Confirmed transition is a legal spend.
        val signedBtcTx = datum match
            case TmDatum.Unconfirmed(rawTx) => rawTx
            case TmDatum.Confirmed(_, _, _) =>
                // TODO: implement burning the TM NFT to clean up Confirmed UTxOs after some time
                // (or via a separate cleanup command) so we don't need to keep them around forever.
                // For now, just fail if anyone tries to spend a Confirmed UTxO.
                fail("TM UTxO is not Unconfirmed")

        val proof = redeemer.to[TmConfirmRedeemer]

        // 1. Recompute the txid from the witness-stripped serialization — never trust the caller.
        val txid = BitcoinHelpers.getTxHash(signedBtcTx)

        // 2. The block is in the oracle's confirmed-blocks trie.
        val oracleState = findOracleInput(tx.referenceInputs, oracleScriptHash).datum match
            case OutputDatum.OutputDatum(d) => d.to[ChainState]
            case _                          => fail("Oracle must have an inline datum")
        val blockHash = BitcoinHelpers.blockHeaderHash(proof.blockHeader)
        MPF(oracleState.confirmedBlocksRoot).verifyMembership(
          blockHash,
          blockHash,
          proof.blockMpfProof
        )

        // 3+4. The header hashes to that block hash and commits to txid at txIndex.
        val computedRoot = BitcoinHelpers.merkleRootFromInclusionProof(
          proof.txMerkleProof,
          txid,
          proof.txIndex
        )
        require(computedRoot == proof.blockHeader.merkleRoot, "TM tx not in block merkle root")

        // 5. The continuing output carries the TM NFT and the parsed Confirmed datum.
        val ownOut = ownResolved(tx.inputs, ownRef)
        val contOut = continuingOutput(tx.outputs, ownOut.address)
        // Preserve the TM NFT (the minted part), NOT the exact Value. The lovelace need not match:
        // the Confirmed datum is a different size (so a different min-UTxO), and any lovelace
        // difference (tx fees / a watchtower reward) is allowed. The TM NFT (policy = this script's
        // own hash, since spend + mint share the script; empty asset name) is what authenticates the
        // Confirmed UTxO downstream, so it MUST ride along.
        val tmNftPolicy = ownOut.address.credential match
            case Credential.ScriptCredential(h) => h
            case _                              => fail("TM input is not at a script address")
        require(
          contOut.value.quantityOf(tmNftPolicy, ByteString.empty) == BigInt(1),
          "TM NFT not preserved on the continuing output"
        )

        // 6. Ensure it's the real TM transaction by checking the presence of the TM input and output using address from Treasury State UTxO (reference input by NFT)
        // TODO:

        val swept = allInputOutpoints(signedBtcTx)
        val fulfilled = allOutputs(signedBtcTx)
        val exp = OutputDatum.OutputDatum(TmDatum.Confirmed(txid, swept, fulfilled).toData)
        require(
          equalsData(exp.toData, contOut.datum.toData),
          "Continuing output datum does not match parsed TM Confirmed"
        )
    }

    /** Minting policy for the TM NFT — the policy id IS this script's hash, so the NFT and the
      * spend logic share one script. Gated by an authority signature (the SPO/poster key): minting
      * a new TM NFT requires exactly +1 of the NFT (empty asset name) AND the authority's
      * signature; burning (draining a Confirmed TM) is permissionless cleanup.
      *
      * This is the single-key "level B" gate — the precursor to the protocol's roster +
      * leader-election (technical_documentation.md §"Post signed TM"). Only the authority key can
      * create a legitimate TM NFT, so a Confirmed TM UTxO bearing this policy's token is authentic.
      */
    def mint(
        controlNftPolicy: ByteString,
        controlNftName: ByteString,
        ownPolicyId: ByteString,
        tx: TxInfo
    ): Unit = {
        val minted = tx.mint.quantityOf(ownPolicyId, ByteString.empty)
        if minted > BigInt(0) then {
            require(minted == BigInt(1), "TM mint: must mint exactly one TM NFT")
            // Read the authorized minter from the NFT-authenticated control UTxO (reference input),
            // then require that key to have signed. The NFT proves WHICH datum is authoritative; the
            // signature proves the authorized key acted. Neither alone suffices.
            val authorizedMinter =
                findControlInput(tx.referenceInputs, controlNftPolicy, controlNftName).datum match
                    case OutputDatum.OutputDatum(d) => d.to[TmControlDatum].authorizedMinter
                    case _                          => fail("TM-control UTxO needs an inline datum")
            require(
              signedByAuthority(tx.signatories, authorizedMinter),
              "TM mint: not signed by the authorized minter"
            )
        } else
            // minted < 0 => burning a TM NFT (drain). Permissionless cleanup. minted == 0 (only other
            // asset names under this policy) is rejected.
            // TODO: allow burning when spending a Confirmed TM UTxO, so cleanup can be done in one step.
            // For now, just fail if anyone tries to burn without minting in the same tx.
            require(minted < BigInt(0), "TM mint: nothing minted/burned under the TM policy")
    }

    /** True iff `pkh` is among the transaction's signatories. */
    def signedByAuthority(sigs: ScalusList[PubKeyHash], pkh: ByteString): Boolean = {
        def loop(remaining: ScalusList[PubKeyHash]): Boolean =
            remaining match
                case ScalusList.Nil           => false
                case ScalusList.Cons(s, tail) => if s.hash == pkh then true else loop(tail)
        loop(sigs)
    }

    /** Entry point: dispatch on script purpose — minting (the TM NFT) or spending (the Confirm
      * transition).
      *
      * Decodes the script context with the typed [[ScriptContext]] / [[ScriptInfo]] and pattern
      * matches on the purpose. (This used to hand-decode via `unConstrData`/`unBData` — a
      * workaround from before Scalus V3 lowering made `to`/`toData` no-ops on the structural
      * script-context types; the straightforward form now compiles to the same field projections.)
      */
    def validate(
        oracleScriptHash: ByteString,
        controlNftPolicy: ByteString,
        controlNftName: ByteString,
        scData: Data
    ): Unit = {
        val ctx = scData.to[ScriptContext]
        ctx.scriptInfo match
            // MintingScript(policyId): policyId is this script's own hash.
            case ScriptInfo.MintingScript(ownPolicyId) =>
                mint(controlNftPolicy, controlNftName, ownPolicyId, ctx.txInfo)
            case ScriptInfo.SpendingScript(ownRef, datumOpt) =>
                spend(oracleScriptHash, datumOpt, ctx.txInfo, ownRef, ctx.redeemer)
            case _ => fail("TM validator: unsupported script purpose")
    }
}

/** The TM validator parameterized with the Binocular oracle script hash and the TM-control NFT
  * `(policy, name)`. The compiled script's hash is BOTH the TM UTxO address (spend) and the TM NFT
  * policy id (mint). All three parameters are STABLE — the address does NOT depend on any
  * participant key, so it never moves when the authorized minter / roster rotates (that lives in
  * the NFT-authenticated [[TmControlDatum]], read at runtime). `Unconfirmed` UTxOs are locked here
  * and spent into `Confirmed` ones; the TM NFT can only be minted by the key the control datum
  * names.
  */
object TreasuryMovementContract {
    given opts: Options = Options.release

    /** Curried form: `oracleScriptHash -> controlNftPolicy -> controlNftName -> (scriptContext ->
      * ())`. Applied via `.apply`, like the always-ok scaffold bakes in its salt.
      */
    lazy val parameterized: PlutusV3[ByteString => (ByteString => (ByteString => (Data => Unit)))] =
        PlutusV3.compile((oracleScriptHash: ByteString) =>
            (controlNftPolicy: ByteString) =>
                (controlNftName: ByteString) =>
                    (scData: Data) =>
                        TreasuryMovementValidator.validate(
                          oracleScriptHash,
                          controlNftPolicy,
                          controlNftName,
                          scData
                        )
        )

    def contract(
        oracleScriptHash: ByteString,
        controlNftPolicy: ByteString,
        controlNftName: ByteString
    ): PlutusV3[Data => Unit] =
        parameterized.apply(oracleScriptHash).apply(controlNftPolicy).apply(controlNftName)
}
