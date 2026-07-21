package binocular.watchtower

import binocular.*
import binocular.bitcoin.*
import binocular.blueprint.BinocularBlueprint
import binocular.oracle.BlockHeader
import binocular.oracle.ChainState
import scalus.cardano.blueprint.Blueprint
import scalus.cardano.blueprint.Contract
import scalus.cardano.ledger.Script
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry as MPF
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry.ProofStep
import scalus.cardano.onchain.plutus.prelude.{List as ScalusList, *}
import scalus.cardano.onchain.plutus.v1.Credential
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.*
import scalus.compiler.Compile
import scalus.compiler.Options
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.*
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString.hex
import scalus.uplc.builtin.Data.FromData
import scalus.uplc.builtin.Data.ToData
import scalus.uplc.builtin.Data.toData

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
  *     segwit-serialized `signedBtcTx` (the bytes watchtowers relay to Bitcoin), the poster's
  *     `creator` key hash, and `created` (POSIX ms, must equal the posting tx's validity upper
  *     bound — see the mint branch). Constr tag 0, `[signed_btc_tx, creator, created]` — the shape
  *     heimdall's `publish.rs` and binocular's `create-tmtx` post.
  *   - [[Confirmed]] — produced by the Confirm transition once the TM is Binocular-confirmed. Holds
  *     the `btcTxid`, the list of swept peg-in outpoints (`sweptPegInUtxoIds`, 36-byte
  *     prev_txid++vout each), the `fulfilledPegOuts`, and `creator`/`created` carried verbatim from
  *     the Unconfirmed input (they drive the GC path). Constr tag 1.
  *
  * Variant order and field order are positional in the Plutus Constr — do not reorder.
  */
enum TmDatum derives FromData, ToData {
    case Unconfirmed(signedBtcTx: ByteString, creator: PubKeyHash, created: PosixTime)
    case Confirmed(
        btcTxid: ByteString,
        sweptPegInUtxoIds: ScalusList[ByteString],
        fulfilledPegOuts: ScalusList[PegOutEntry],
        creator: PubKeyHash,
        created: PosixTime
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

/** Mint redeemer: which anchor the posted TM chains from. Both variants carry the 0-based
  * reference-input index of their anchor UTxO; the anchor is authenticated by its NFT at that index
  * (config NFT / TM NFT), never by position alone.
  *
  *   - [[Genesis]] — the FIRST Treasury Movement: the reference input at `configRefInputIndex` must
  *     be the Config UTxO (config NFT), and the embedded BTC tx's input 0 must spend the initial
  *     treasury outpoint stored in its field 11 (`initial_btc_treasury_utxo`).
  *   - [[Chain]] — every subsequent TM: the reference input at `prevTmRefInputIndex` must be a
  *     `Confirmed` TM record (TM NFT), and the embedded BTC tx's input 0 must spend that record's
  *     treasury output `(btcTxid, vout 0)`.
  *
  * Minting is PERMISSIONLESS: anyone may post a TM chaining from any anchor, but a Bitcoin outpoint
  * spends exactly once, so at most one such TM can ever confirm — the Confirmed chain cannot fork.
  * Uniqueness is inherited from Bitcoin, not enforced here.
  */
enum TmMintRedeemer derives FromData, ToData {
    case Genesis(configRefInputIndex: BigInt)
    case Chain(prevTmRefInputIndex: BigInt)
}

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
  *
  * That `signedBtcTx` is the protocol's real Treasury Movement transaction is enforced at MINT time
  * (see [[TmMintRedeemer]]): the minted TM NFT is bound to an `Unconfirmed` output whose embedded
  * BTC tx spends the protocol treasury outpoint — the config anchor (first TM) or the referenced
  * predecessor `Confirmed` record's output 0 (every subsequent TM). The Confirm spend needs no
  * linkage re-check: the bytes were committed at mint.
  *
  * A `Confirmed` record is additionally spendable by its `creator` once the [[GcGraceMs]] grace
  * period after `created` elapses: the spend burns the TM NFT and reclaims the min-ADA (garbage
  * collection — see the Confirmed branch of `spend`). Operational rule: never GC the chain TIP.
  *
  * Parameterized by the Binocular oracle script hash and the config NFT `(policy, name)` (applied
  * via [[TreasuryMovementContract.contract]]).
  */
@Compile
object TreasuryMovementValidator {

    /** Decode an inline datum as `A`, failing on a missing/hashed datum. Every datum this validator
      * reads (oracle ChainState, TM records, the Config) is required to be inline. `inline` so the
      * `FromData[A]` derivation expands at the call site — a non-inline generic would reference the
      * companion's `derived$FromData` module, which is not `@Compile`d for externally-defined types
      * like [[ConfigDatum]] and `ChainState`.
      */
    extension (d: OutputDatum) {
        inline def of[A: FromData]: A = d match
            case OutputDatum.OutputDatum(datum) => datum.to[A]
            case _                              => fail("Expected inline datum")
    }

    /** Grace period before a Confirmed record's creator may GC it (burn NFT + reclaim min-ADA).
      * BigInt arithmetic — the equivalent Int literal product (30*24*3600*1000) overflows Int32.
      */
    val GcGraceMs: BigInt = BigInt(30) * 24 * 3600 * 1000 // 30 days

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
        refInputs
            .find { input =>
                val resolved = input.resolved
                resolved.address.credential match
                    case Credential.ScriptCredential(hash) =>
                        hash == oracleScriptHash && resolved.value.quantityOf(
                          oracleScriptHash,
                          ByteString.empty
                        ) == BigInt(1)
                    case _ => false
            }
            .get
            .resolved
    }

    /** Count the transaction inputs sitting at the TM script address (Script credential == the TM
      * script hash). A legal TM spend — Confirm (`Unconfirmed -> Confirmed`) or GC (creator burns a
      * grace-expired `Confirmed`) — spends EXACTLY ONE TM record; both branches of [[spend]]
      * require this.
      *
      * Why: the TM NFT has an empty asset name and no one-shot seed, so `(policy, "")` is fungible
      * across posts — permissionless posting lets the SAME confirmed `signedBtcTx` be posted as two
      * `Unconfirmed` records, each bearing the token. Spending two TM records in one tx runs this
      * validator once per input; every invocation accepts the single continuing output (Confirm) or
      * the single NFT burn (GC), and ledger value-conservation forces the second token to escape to
      * an attacker output carrying a fabricated `Confirmed` datum. `peg_in.ak` authenticates the
      * Confirmed record by the NFT, not the address, so that fabricated record would be trusted —
      * minting fBTC with no treasury backing. Requiring one TM input per spend closes the escape on
      * both the Confirm and GC paths.
      */
    def tmInputCount(inputs: ScalusList[TxInInfo], tmScriptHash: ByteString): BigInt = {
        def loop(remaining: ScalusList[TxInInfo]): BigInt =
            remaining match
                case ScalusList.Nil => BigInt(0)
                case ScalusList.Cons(inp, tail) =>
                    val here = inp.resolved.address.credential match
                        case Credential.ScriptCredential(h) =>
                            if h == tmScriptHash then BigInt(1) else BigInt(0)
                        case _ => BigInt(0)
                    here + loop(tail)
        loop(inputs)
    }

    def spend(
        oracleScriptHash: ByteString,
        datumOpt: Option[Datum],
        tx: TxInfo,
        ownRef: TxOutRef,
        redeemer: Datum
    ): Unit = {
        val datum = datumOpt.getOrFail("Missing TM datum").to[TmDatum]
        // Only the Unconfirmed -> Confirmed transition is a legal spend.
        datum match
            case TmDatum.Unconfirmed(signedBtcTx, creator, created) =>
                val proof = redeemer.to[TmConfirmRedeemer]

                // 1. Recompute the txid from the witness-stripped serialization — never trust the caller.
                val txid = BitcoinHelpers.getTxHash(signedBtcTx)

                // 2. The block is in the oracle's confirmed-blocks trie.
                val oracleState =
                    findOracleInput(tx.referenceInputs, oracleScriptHash).datum.of[ChainState]
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
                require(
                  computedRoot == proof.blockHeader.merkleRoot,
                  "TM tx not in block merkle root"
                )

                // 5. The continuing output carries the TM NFT and the parsed Confirmed datum.
                val ownOut = tx.findOwnInput(ownRef).get.resolved
                val contOut = tx.outputs.find(out => out.address === ownOut.address).get
                // Preserve the TM NFT (the minted part), NOT the exact Value. The lovelace need not match:
                // the Confirmed datum is a different size (so a different min-UTxO), and any lovelace
                // difference (tx fees / a watchtower reward) is allowed. The TM NFT (policy = this script's
                // own hash, since spend + mint share the script; empty asset name) is what authenticates the
                // Confirmed UTxO downstream, so it MUST ride along.
                val tmNftPolicy = ownOut.address.credential match
                    case Credential.ScriptCredential(h) => h
                    case _ => fail("TM input is not at a script address")
                // NFT containment: exactly one TM record may be spent per Confirm tx. Without this,
                // spending two duplicate Unconfirmed records lets the second (fungible, empty-name)
                // TM NFT escape to an attacker output with a fabricated Confirmed datum — see
                // [[tmInputCount]].
                require(
                  tmInputCount(tx.inputs, tmNftPolicy) == BigInt(1),
                  "TM confirm: exactly one TM-script input per tx"
                )
                require(
                  contOut.value.quantityOf(tmNftPolicy, ByteString.empty) == BigInt(1),
                  "TM NFT not preserved on the continuing output"
                )

                // 6. Ensure it's the real TM transaction by checking the presence of the TM input and output using address from Treasury State UTxO (reference input by NFT)
                // TODO:

                val swept = allInputOutpoints(signedBtcTx)
                val fulfilled = allOutputs(signedBtcTx)
                val exp = OutputDatum.OutputDatum(
                  TmDatum.Confirmed(txid, swept, fulfilled, creator, created).toData
                )
                require(
                  exp === contOut.datum,
                  "Continuing output datum does not match parsed TM Confirmed"
                )
            case TmDatum.Confirmed(_, _, _, creator, created) =>
                // Garbage collection: after the grace period the CREATOR may reclaim the record's
                // min-ADA, burning the TM NFT. By then all peg-ins/peg-outs swept by this TM are
                // expected to be completed (the record is no longer needed as proof material).
                // `created` is anchored to the mint tx's validity interval (see `mint`), so the
                // grace period cannot be shortcut by backdating.
                //
                // OPERATIONAL RULE (accepted residual risk): burning the chain-TIP record leaves
                // the next TM with no predecessor to reference (and Genesis no longer applies), so
                // the creator must not burn the tip. While the bridge is active a successor lands
                // well within the grace period; after a >30-day quiet spell, recovery is a config
                // Update re-anchoring `initial_btc_treasury_utxo` to the current outpoint.
                val ownOut = tx.findOwnInput(ownRef).get.resolved
                ownOut.address.credential match
                    case Credential.ScriptCredential(ownScriptHash) =>
                        require(
                          tx.mint.quantityOf(ownScriptHash, ByteString.empty) == BigInt(-1),
                          "Must burn TM NFT"
                        )
                        // NFT containment on the GC path too: burning ONE NFT while spending two
                        // grace-expired Confirmed records (same creator) would let the un-burned
                        // second NFT escape — see [[tmInputCount]].
                        require(
                          tmInputCount(tx.inputs, ownScriptHash) == BigInt(1),
                          "TM GC: exactly one TM-script input per tx"
                        )
                    case Credential.PubKeyCredential(_) => impossible()
                val timeout = created + GcGraceMs
                require(
                  tx.validRange.isEntirelyAfter(timeout),
                  "TM GC: grace period has not elapsed"
                )
                require(tx.isSignedBy(creator), "TM GC: not signed by the record's creator")
    }

    inline def validateMinting(
        configNftPolicy: PolicyId,
        configNftName: ByteString,
        redeemer: TmMintRedeemer,
        ownPolicyId: PolicyId,
        tx: TxInfo
    ) = {
        // Bind the NFT to a TM-address output whose Unconfirmed datum embeds the BTC tx being
        // verified — without this binding the linkage check below would gate nothing.
        val tmOut = tx.outputs
            .find(txout => txout.value.quantityOf(ownPolicyId, ByteString.empty) == BigInt(1))
            .get
        tmOut.address.credential match
            case Credential.ScriptCredential(h) if h == ownPolicyId => ()
            case _ => fail("TM mint: NFT output not at own script address")
        val signedBtcTx = tmOut.datum.of[TmDatum] match
            case TmDatum.Unconfirmed(rawTx, _, created) =>
                val txHappenedBefore = tx.validRange.to.finiteOrFail(
                  "TM mint: validity range upper bound must be finite"
                )
                // The tx cannot be included after `txHappenedBefore`, so requiring
                // `created == txHappenedBefore` makes `created` a guaranteed upper bound on the
                // real posting time: the GC grace period (see the Confirmed spend branch) can
                // start late but never early, and cannot be backdated. Future-dating only delays
                // the poster's own reclaim.
                require(
                  created == txHappenedBefore,
                  "TM mint: created field must be equal to `tx.validRange.to`"
                )
                rawTx
            case _ => fail("TM mint: NFT output datum is not Unconfirmed")
        // The outpoint the embedded BTC tx spends first: input 0 is the treasury by the
        // deterministic TM layout (input[0] = treasury, output[0] = treasury change).
        val spent = allInputOutpoints(signedBtcTx).head
        val expected = redeemer match
            case TmMintRedeemer.Genesis(i) =>
                val cfg = tx.referenceInputs.at(i).resolved
                // The config NFT — not the index or address — is the trust anchor: it is
                // minted exactly once, so a forged UTxO cannot carry it.
                require(
                  cfg.value.quantityOf(configNftPolicy, configNftName) == BigInt(1),
                  "TM mint: reference input lacks the config NFT"
                )
                cfg.datum.of[ConfigDatum].initialBtcTreasuryUtxo
            case TmMintRedeemer.Chain(i) =>
                val prev = tx.referenceInputs.at(i).resolved
                require(
                  prev.value.quantityOf(ownPolicyId, ByteString.empty) == BigInt(1),
                  "TM mint: predecessor lacks the TM NFT"
                )
                prev.datum.of[TmDatum] match
                    case TmDatum.Confirmed(btcTxid, _, _, _, _) =>
                        // Predecessor treasury output = (btcTxid, vout 0).
                        btcTxid ++ hex"00000000"
                    case _ => fail("TM mint: predecessor is not Confirmed")

        require(
          spent == expected,
          "TM mint: BTC tx does not spend the treasury outpoint"
        )
    }

    /** Minting policy for the TM NFT — the policy id IS this script's hash, so the NFT and the
      * spend logic share one script. PERMISSIONLESS, gated by chain linkage: the freshly posted
      * `Unconfirmed` TM must embed a BTC tx whose input 0 spends the protocol treasury outpoint —
      * the config anchor ([[TmMintRedeemer.Genesis]]) or the referenced predecessor `Confirmed`
      * record's output 0 ([[TmMintRedeemer.Chain]]). See [[TmMintRedeemer]] for why permissionless
      * minting is safe (Bitcoin's spend-once semantics — the Confirmed chain cannot fork). Burning
      * (draining a Confirmed TM) is permissionless cleanup.
      */
    def mint(
        configNftPolicy: ByteString,
        configNftName: ByteString,
        ownPolicyId: ByteString,
        tx: TxInfo,
        redeemer: Datum
    ): Unit = {
        tx.mint.tokens(ownPolicyId).toList match
            case ScalusList.Cons((nft, amount), ScalusList.Nil) if nft == ByteString.empty =>
                if amount == BigInt(1) then
                    validateMinting(
                      configNftPolicy,
                      configNftName,
                      redeemer.to[TmMintRedeemer],
                      ownPolicyId,
                      tx
                    )
                else if amount == BigInt(-1) then
                    // burning is allowed, all the check are in `spend` validator
                    ()
                else fail("Only singe TM NFT is allowed")
            case _ => fail("Only singe TM NFT is allowed")
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
        configNftPolicy: ByteString,
        configNftName: ByteString,
        scData: Data
    ): Unit = {
        val ctx = scData.to[ScriptContext]
        ctx.scriptInfo match
            // MintingScript(policyId): policyId is this script's own hash.
            case ScriptInfo.MintingScript(ownPolicyId) =>
                mint(configNftPolicy, configNftName, ownPolicyId, ctx.txInfo, ctx.redeemer)
            case ScriptInfo.SpendingScript(ownRef, datumOpt) =>
                spend(oracleScriptHash, datumOpt, ctx.txInfo, ownRef, ctx.redeemer)
            case _ => fail("TM validator: unsupported script purpose")
    }
}

/** The TM validator parameterized with the Binocular oracle script hash and the config NFT
  * `(policy, name)`. The compiled script's hash is BOTH the TM UTxO address (spend) and the TM NFT
  * policy id (mint). All three parameters are STABLE — the address does NOT depend on any
  * participant key. `Unconfirmed` UTxOs are locked here and spent into `Confirmed` ones; the TM NFT
  * can be minted by ANYONE whose posted TM chains from the current treasury outpoint (config anchor
  * or predecessor `Confirmed` record — see [[TmMintRedeemer]]).
  */
object TreasuryMovementContract extends Contract {
    given opts: Options = Options.release

    /** Curried form: `oracleScriptHash -> configNftPolicy -> configNftName -> (scriptContext ->
      * ())`. Applied via `.apply`, like the always-ok scaffold bakes in its salt.
      */
    lazy val parameterized: PlutusV3[ByteString => (ByteString => (ByteString => (Data => Unit)))] =
        PlutusV3.compile((oracleScriptHash: ByteString) =>
            (configNftPolicy: ByteString) =>
                (configNftName: ByteString) =>
                    (scData: Data) =>
                        TreasuryMovementValidator.validate(
                          oracleScriptHash,
                          configNftPolicy,
                          configNftName,
                          scData
                        )
        )

    def contract(
        oracleScriptHash: ByteString,
        configNftPolicy: ByteString,
        configNftName: ByteString
    ): PlutusV3[Data => Unit] =
        parameterized.apply(oracleScriptHash).apply(configNftPolicy).apply(configNftName)

    /** Treasury-movement script for the given params: the unapplied program from the generated
      * CIP-57 blueprint with the three `ByteString` params applied at the UPLC level as bare
      * bytestring constants (the validator is compiled from curried `ByteString` lambdas, not
      * `Data` — see [[parameterized]]).
      */
    def script(
        oracleScriptHash: ByteString,
        configNftPolicy: ByteString,
        configNftName: ByteString
    ): Script.PlutusV3 =
        BinocularBlueprint.script(
          "TreasuryMovementContract",
          BinocularBlueprint.bytesParam(oracleScriptHash),
          BinocularBlueprint.bytesParam(configNftPolicy),
          BinocularBlueprint.bytesParam(configNftName)
        )

    /** CIP-57 blueprint over the UNAPPLIED parameterized program: consumers (and [[script]]) apply
      * the three params UPLC-level, Aiken-style. Built manually because the `Blueprint.plutusV3`
      * helpers only model single-parameter validators.
      */
    lazy val blueprint: Blueprint = {
        // Validator would clash with plutus.v3.Validator (wildcard-imported above) — keep scoped
        import scalus.cardano.blueprint.{Preamble, Validator}
        import scalus.utils.Hex.toHex
        val title = "TreasuryMovementContract"
        val description =
            "Bifrost treasury-movement validator: holds Unconfirmed→Confirmed TM state, " +
                "parameterized by (oracleScriptHash, configNftPolicy, configNftName)."
        val bytes = BinocularBlueprint.bytesParamDescription
        Blueprint(
          preamble = Preamble(
            title,
            description,
            "1.0.0",
            plutusVersion = parameterized.language,
            license = Some("Apache-2.0")
          ),
          validators = Seq(
            Validator(
              title = title,
              description = Some(description),
              redeemer = Some(BinocularBlueprint.opaqueDataDescription),
              datum = None,
              parameters = Some(scala.List(bytes, bytes, bytes)),
              compiledCode = Some(parameterized.program.cborEncoded.toHex),
              hash = Some(parameterized.script.scriptHash.toHex)
            )
          )
        )
    }
}
