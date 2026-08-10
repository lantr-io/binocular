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

/** Datum of the treasury-movement (TM) UTxO — rev 5.4: a single-constructor record (Constr 0).
  *
  * The `Confirmed` variant is gone (spec §No Confirmed record): Confirm spends the record, burns
  * the TM NFT ([CTM-24]), produces no output at the TM address ([CTM-25]), and writes everything
  * downstream readers need into the bridge-state singleton ([[BridgeState]]). The SPENT record
  * stays in Cardano history forever and is the permanent source of the raw TM bytes for trie
  * reconstruction ([SPI-7], [OB-9]).
  *
  * Created when the signed Bitcoin TM is posted to Cardano. Carries the full segwit-serialized
  * `signedBtcTx` (the bytes watchtowers relay to Bitcoin), the poster's `creator` key hash,
  * `created` (POSIX ms, must equal the posting tx's validity upper bound — see the mint branch),
  * and the rev-5.1 data-availability hint `fulfilledPorOutpoints`. Constr tag 0,
  * `[signed_btc_tx, creator, created, fulfilled_por_outpoints]` — the shape heimdall's `publish.rs`
  * and binocular's `create-tmtx` post. The rev-5.3 `epoch` and `leader_reward` fields LEFT the
  * datum (spec §Leader reward: DEFERRED): no reward is paid anywhere on-chain, and carrying a
  * half-enforced fee field would hand a permissionless poster a toll on every swept depositor.
  * `tm_sequence` is likewise NOT a datum field (off-chain signing counter, spec §Cardano submission
  * and leader reward).
  *
  * Field order is positional in the Plutus Constr — do not reorder; new fields are APPENDED, never
  * inserted. The Constr TAG is a wire fact too: every harvester keys history by `Constr 0` records,
  * and a plain case-class decode does not check the tag, so the mint branch pins it explicitly.
  */
case class UnconfirmedTm(
    signedBtcTx: ByteString,
    creator: PubKeyHash,
    created: PosixTime,
    /** Rev-5.1 data-availability HINT: the Cardano outpoints of the PegOutRequests this TM
      * fulfills, 36 bytes each (Cardano tx hash (32) ++ output index as 4 little-endian bytes).
      *
      * UNVERIFIED. Neither `mint` nor `spend` reads a single byte of it: the FROST-signed `"BTMR1"`
      * root commitment inside `signedBtcTx` is the sole integrity anchor for the completed-peg-outs
      * trie. Posting a TM is permissionless, so a hostile poster can garble this list; that costs
      * the protocol nothing.
      *
      * What it is for: rebuilding the completed-peg-outs trie from chain data alone (cold start,
      * recovery, a new SPO). Reconstruction reads it from the SPENT record's inline datum, which
      * stays in Cardano history forever and is indexable by address alone (Kupo indexes datums of
      * spent outputs; it does not index tx metadata — which is why the hint is a datum field). With
      * the hint, reconstruction resolves each outpoint to its POR datum and inserts the entry
      * directly; without it, the fallback is to match the TM's payment outputs against the
      * PegOutRequests open at that time and search assignments until the running root equals that
      * TM's committed root. The committed root turns reconstruction from trust into
      * search-and-check either way.
      */
    fulfilledPorOutpoints: ScalusList[ByteString]
) derives FromData,
      ToData

@Compile
object UnconfirmedTm

/** Scalus mirror of the RETIRED `completed-peg-outs-merkle-tree.ak` datum (rev 5.4 deleted that
  * validator: the bridge-state singleton carries `cpo_root` now). The Confirm branch no longer
  * spends a trie UTxO; this type survives only for the legacy off-chain readers
  * (`BridgeSweepSetup`, `PorSweeper`) until task `bss-bootstrap-cleanup` retires them.
  */
case class CompletedPegOutsTrieDatum(root: ByteString) derives FromData, ToData

@Compile
object CompletedPegOutsTrieDatum

/** Datum of the rev-5.4 bridge-state singleton — the one UTxO every TM Confirm advances, and the
  * one datum `peg-in.ak`, `peg-out.ak` and `treasury.ak` read.
  *
  * Scalus side of a cross-language mirror: the Aiken `BridgeState` type MUST move in lockstep with
  * this one. The datum is a Plutus `Constr`, so the tag (0) and the FIELD ORDER are
  * consensus-visible. Spec §BridgeState, the singleton datum:
  *
  * | Index | Field            | Bytes | Written at Confirm from                 |
  * |:------|:-----------------|:------|:----------------------------------------|
  * | 0     | `spiRoot`        | 32    | the TM's commitment output, first root  |
  * | 1     | `cpoRoot`        | 32    | the TM's commitment output, second root |
  * | 2     | `treasuryUtxoId` | 36    | `btc_txid ++ 00000000`                  |
  * | 3     | `treasuryAmount` | int   | satoshi amount of the TM's output 0     |
  *
  *   - [LIB-1] every reader decodes this type and reads its fields BY NAME. A bare "field 0" read
  *     would return `spiRoot` where the caller wanted `cpoRoot`, and a wrong root makes `mpf.miss`
  *     SUCCEED — which cancels a paid PegOutRequest.
  *   - [LIB-3] a new field is APPENDED, never inserted. An insert shifts every later index and
  *     silently re-points the Aiken readers.
  *
  * @param spiRoot
  *   swept peg-ins MPF root, 32 bytes. Attested by the TM, not derived on-chain.
  * @param cpoRoot
  *   completed peg-outs MPF root, 32 bytes. Attested by the TM, not derived on-chain.
  * @param treasuryUtxoId
  *   the current treasury UTxO on Bitcoin, `btc_txid`(32) ++ `vout`(4, little-endian, always
  *   `00000000`). The next TM MUST spend it as its input 0.
  * @param treasuryAmount
  *   that UTxO's satoshi amount.
  */
case class BridgeState(
    spiRoot: ByteString,
    cpoRoot: ByteString,
    treasuryUtxoId: ByteString,
    treasuryAmount: BigInt
) derives FromData,
      ToData

@Compile
object BridgeState

/** The Confirm proof payload ([CTM-14]'s `Confirm(proof)`).
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
case class TmConfirmProof(
    txIndex: BigInt,
    txMerkleProof: ScalusList[ByteString],
    blockMpfProof: ScalusList[ProofStep],
    blockHeader: BlockHeader
) derives FromData,
      ToData

@Compile
object TmConfirmProof

/** Spend redeemer of a TM record — [CTM-14]: the validator decodes THIS before it looks at the
  * datum, because the redeemer is the transition selector:
  *
  *   - [[Confirm]] (Constr 0) — the validated transition: prove the TM on Bitcoin, burn the TM NFT,
  *     advance the bridge-state singleton.
  *   - [[Gc]] (Constr 1) — grace-period reclaim by the record's creator ([CTM-16]).
  *
  * LOCKSTEP: `bridge-state.ak` discriminates a singleton spend on this redeemer's Constr TAG
  * ([BSS-2]: tag 0 = Confirm) — not on any datum tag ([BSS-6]) and not on the NFT burn ([BSS-7]),
  * which a Gc spend also performs. Do not renumber the variants.
  */
enum TmSpendRedeemer derives FromData, ToData {
    case Confirm(proof: TmConfirmProof)
    case Gc
}

@Compile
object TmSpendRedeemer

/** Mint redeemer: the 0-based reference-input index of the bridge-state singleton.
  *
  * [PTM-5] is WITHDRAWN — the rev-5.1 `Genesis`/`Chain` anchor split is retired. Every posted TM
  * chains from the singleton's `treasury_utxo_id` ([PTM-6]); the reference input at
  * `bridgeStateRefInputIndex` is authenticated by the singleton NFT `(Config bridge_state_policy,
  * "BSS")`, never by position alone ([PTM-7]).
  *
  * Minting is PERMISSIONLESS: anyone may post a TM chaining from the current head, but a Bitcoin
  * outpoint spends exactly once, so at most one such TM can ever confirm — the confirmed chain
  * cannot fork. Uniqueness is inherited from Bitcoin, not enforced here. A TM posted from a STALE
  * head cannot be posted at all (the singleton's head has moved), which is what stops dead records
  * from accumulating (spec §Post signed TM, Why note).
  */
case class TmMintRedeemer(bridgeStateRefInputIndex: BigInt) derives FromData, ToData

@Compile
object TmMintRedeemer

/** Treasury-movement validator, rev 5.4: Confirm retires the TM record and advances the
  * bridge-state singleton.
  *
  * The only legal [[TmSpendRedeemer.Confirm]] spend of an [[UnconfirmedTm]] UTxO *proves* the TM is
  * confirmed on Bitcoin against the Binocular oracle:
  *
  *   1. `txid = sha256d(strip_witness(signedBtcTx))` — recomputed on-chain, never trusted
  *      ([CTM-1]).
  *   2. the block header is in the oracle's `confirmedBlocksRoot` (MPF membership; oracle UTxO is a
  *      reference input, identified by the script hash applied as a compile parameter) ([CTM-3]).
  *   3. the header hashes to the MPF-proven block hash.
  *   4. `txid` is merkle-included in the header's tx-merkle-root at `txIndex` ([CTM-2]).
  *
  * and then retires the record and advances the singleton:
  *
  *   - the TM NFT is burned ([CTM-24]) and NO output sits at the TM address ([CTM-25]) — there is
  *     no `Confirmed` record (spec §No Confirmed record);
  *   - the bridge-state singleton — the UTxO carrying `(Config bridge_state_policy, "BSS")`
  *     ([CTM-28]) — is spent, its head must be what the TM's input 0 spends ([CTM-18]), and the
  *     continuing singleton output ([CTM-29]) must carry EXACTLY the [[BridgeState]] this TM
  *     attests ([CTM-27]): both roots from its single `"BTMR1"` commitment output ([CTM-20],
  *     [CTM-26], [CTM-30]), the new head `txid ‖ 00000000` ([CTM-19]), and output 0's satoshi
  *     amount ([CTM-21]).
  *
  * That `signedBtcTx` is the protocol's real Treasury Movement transaction is enforced at MINT time
  * (see [[TmMintRedeemer]]): the minted TM NFT is bound to an `Unconfirmed` output whose embedded
  * BTC tx spends the singleton's `treasury_utxo_id` ([PTM-6]). The Confirm spend re-checks the
  * linkage against the SPENT singleton ([CTM-18]), which is what makes replaying an old TM
  * impossible: the head it chained from is gone.
  *
  * An `Unconfirmed` record is additionally spendable by its `creator` with [[TmSpendRedeemer.Gc]]
  * once the [[GcGraceMs]] grace period after `created` elapses: the spend burns the TM NFT and
  * reclaims the min-ADA ([CTM-16] — a record whose TM never mines is permanently unconfirmable
  * under [CTM-18], so it is dead weight).
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

    /** Grace period before a TM record's creator may GC it (burn NFT + reclaim min-ADA) ([CTM-8],
      * [CTM-16]). BigInt arithmetic — the equivalent Int literal product (30*24*3600*1000)
      * overflows Int32.
      */
    val GcGraceMs: BigInt = BigInt(30) * 24 * 3600 * 1000 // 30 days

    /** First 7 bytes of a completed-peg-outs root commitment `scriptPubKey`:
      * `OP_RETURN OP_PUSHBYTES_37 "CPOR1"`. The 37-byte push is `"CPOR1" ++ new_root`, so the whole
      * script is 39 bytes ([[RootCommitmentScriptLength]]).
      *
      * Why `"CPOR1"` and not the peg-in `"BFR"` prefix: watchtowers detect peg-in deposits by
      * scanning for `"BFR"`-prefixed OP_RETURN outputs, and a TM pays the treasury address, so a
      * `"BFR"`-tagged output inside a TM could be misdetected as a deposit. The trailing `1` is a
      * format version, so a future commitment layout gets its own tag.
      */
    val RootCommitmentPrefix: ByteString = hex"6a2543504f5231"

    /** Length of [[RootCommitmentPrefix]]: `OP_RETURN`(1) + `OP_PUSHBYTES_37`(1) + `"CPOR1"`(5).
      * Also the offset at which the committed root starts.
      */
    val RootCommitmentPrefixLength: BigInt = 7

    /** Length of the committed completed-peg-outs MPF root. */
    val RootLength: BigInt = 32

    /** Length in bytes of a well-formed root commitment `scriptPubKey` = prefix(7) + root(32) = 39.
      */
    val RootCommitmentScriptLength: BigInt = RootCommitmentPrefixLength + RootLength

    /** Asset name of the completed-peg-outs trie NFT. Mirrors
      * `bifrost/constants.ak::completed_peg_outs_root_asset_name`.
      */
    val CompletedPegOutsAssetName: ByteString = ByteString.fromString("CPO")

    /** First 7 bytes of a rev-5.4 TWO-ROOT commitment `scriptPubKey`:
      * `OP_RETURN OP_PUSHBYTES_69 "BTMR1"`. The 69-byte push is `"BTMR1" ++ spi_root ++ cpo_root`,
      * so the whole script is 71 bytes ([[TwoRootCommitmentScriptLength]]).
      *
      * `"BTMR1"` means Bifrost TM Roots, version 1. It is deliberately NOT `"BFR"`-prefixed:
      * watchtowers detect peg-in deposits by scanning for that prefix, and a TM pays the treasury
      * address, so a `"BFR"` tag here could be misread as a deposit. The trailing `1` is a format
      * version, so a future commitment layout gets its own tag.
      */
    val TwoRootCommitmentPrefix: ByteString = hex"6a4542544d5231"

    /** Length of [[TwoRootCommitmentPrefix]]: `OP_RETURN`(1) + `OP_PUSHBYTES_69`(1) + `"BTMR1"`(5).
      * Also the offset at which `spi_root` starts.
      */
    val TwoRootCommitmentPrefixLength: BigInt = 7

    /** Offset at which `cpo_root` starts: after the prefix and the 32-byte `spi_root`. */
    val CpoRootOffset: BigInt = TwoRootCommitmentPrefixLength + RootLength

    /** Length in bytes of a well-formed two-root commitment `scriptPubKey` = prefix(7) +
      * `spi_root`(32) + `cpo_root`(32) = 71. The single `OP_PUSHBYTES_69` pushes the last 69 of
      * those bytes, `"BTMR1" ++ spi_root ++ cpo_root`, well inside every datacarrier standardness
      * limit.
      */
    val TwoRootCommitmentScriptLength: BigInt = CpoRootOffset + RootLength

    /** Asset name of the rev-5.4 bridge-state singleton NFT. The Aiken side has no constant for it
      * yet; when `bifrost/constants.ak` gains one it MUST carry these same bytes, because the
      * singleton is identified by `(policy, name)` on both sides.
      */
    val BridgeStateAssetName: ByteString = ByteString.fromString("BSS")

    /** Is this `scriptPubKey` a two-root commitment? Length AND prefix, so a short script cannot
      * slice past its end and a 71-byte payment script cannot masquerade as one. The length check
      * also rejects the old 39-byte `"CPOR1"` output.
      */
    def isTwoRootCommitment(scriptPubKey: ByteString): Boolean =
        scriptPubKey.length == TwoRootCommitmentScriptLength &&
            scriptPubKey.slice(0, TwoRootCommitmentPrefixLength) == TwoRootCommitmentPrefix

    /** Both MPF roots the TM's outputs attest, as `(spi_root, cpo_root)`: bytes [7, 39) and [39,
      * 71) of its single [[isTwoRootCommitment]] output.
      *
      * `outs` is the TM's FULL parsed output list, treasury change included. The commitment may sit
      * at any position (heimdall emits it last).
      *
      * EXACTLY ONE commitment output is required ([CTM-26]). Zero fails: every TM must state both
      * roots that hold after it, including a TM that sweeps nothing and fulfills no peg-out (which
      * re-commits the unchanged roots), so each root is pinned by an unbroken chain of quorum
      * attestations. Two or more fail because the validator would otherwise have to choose, and a
      * permissionless confirmer would make that choice.
      *
      * The roots are ATTESTED, not verified: they are whatever the FROST quorum signed into the TM.
      * This validator only copies them into the bridge-state singleton. Correctness rests on the
      * same quorum honesty that already custodies the treasury — every co-signer recomputes both
      * expected roots from its own tries before signing ([SPI-2]), so a leader proposing a wrong
      * root fails quorum.
      */
    def committedRoots(outs: ScalusList[PegOutEntry]): (ByteString, ByteString) =
        // `filter` then match, NOT `find`: `find` stops at the first commitment and would silently
        // accept a TM carrying a second one.
        outs.filter(out => isTwoRootCommitment(out.scriptPubKey)) match
            case ScalusList.Cons(only, ScalusList.Nil) =>
                val spk = only.scriptPubKey
                (
                  spk.slice(TwoRootCommitmentPrefixLength, RootLength),
                  spk.slice(CpoRootOffset, RootLength)
                )
            case ScalusList.Nil => fail("TM confirm: missing two-root commitment")
            case _              => fail("TM confirm: multiple two-root commitments")

    /** Is this `scriptPubKey` a completed-peg-outs root commitment? Length AND prefix, so a short
      * script cannot slice past its end and a 39-byte payment script cannot masquerade as one.
      */
    def isRootCommitment(scriptPubKey: ByteString): Boolean =
        scriptPubKey.length == RootCommitmentScriptLength &&
            scriptPubKey.slice(0, RootCommitmentPrefixLength) == RootCommitmentPrefix

    /** The completed-peg-outs MPF root the TM's outputs attest: the 32 bytes after the prefix of
      * its single [[isRootCommitment]] output, i.e. bytes [7, 39) of that `scriptPubKey`.
      *
      * `outs` is the TM's FULL parsed output list, treasury change included. The commitment may sit
      * at any position (heimdall emits it last).
      *
      * EXACTLY ONE commitment output is required. Zero fails: every TM must state the trie root
      * that holds after it, including a TM that fulfills no peg-out (which re-commits the unchanged
      * root), so the trie root is pinned by an unbroken chain of quorum attestations. Two or more
      * fail because the validator would otherwise have to choose, and a permissionless confirmer
      * would make that choice.
      *
      * The root is ATTESTED, not verified: it is whatever the FROST quorum signed into the TM. This
      * validator only copies it into the trie UTxO. Root correctness rests on the same quorum
      * honesty that already custodies the treasury — every co-signer recomputes the expected root
      * from its own trie before signing, so a leader proposing a wrong root fails quorum.
      */
    def committedRoot(outs: ScalusList[PegOutEntry]): ByteString =
        // `filter` then match, NOT `find`: `find` stops at the first commitment and would silently
        // accept a TM carrying a second one.
        outs.filter(out => isRootCommitment(out.scriptPubKey)) match
            case ScalusList.Cons(only, ScalusList.Nil) =>
                only.scriptPubKey.slice(RootCommitmentPrefixLength, RootLength)
            case ScalusList.Nil => fail("TM confirm: missing root commitment")
            case _              => fail("TM confirm: multiple root commitments")

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
                resolved.address.credential === Credential.ScriptCredential(oracleScriptHash)
                && resolved.value.quantityOf(oracleScriptHash, ByteString.empty) == BigInt(1)
            }
            .get
            .resolved
    }

    /** Count the transaction inputs sitting at the TM script address (Script credential == the TM
      * script hash). A legal TM spend — Confirm or GC — spends EXACTLY ONE TM record ([CTM-17]);
      * both branches of [[spend]] require this.
      *
      * Why: the TM NFT has an empty asset name and no one-shot seed, so `(policy, "")` is fungible
      * across posts — permissionless posting lets the SAME `signedBtcTx` be posted as two
      * `Unconfirmed` records, each bearing the token. Spending two TM records in one tx runs this
      * validator once per input; every invocation sees the same transaction-wide −1 mint, so only
      * ONE token is burned, and ledger value-conservation forces the second token to escape to an
      * attacker output with a fabricated `Unconfirmed` datum — a forged post that skipped the mint
      * checks. Requiring one TM input per spend closes the escape on both paths.
      */
    def tmInputCount(inputs: ScalusList[TxInInfo], tmScriptHash: ByteString): BigInt =
        inputs.count(
          _.resolved.address.credential === Credential.ScriptCredential(tmScriptHash)
        )

    /** TM spend — dispatches on the [[TmSpendRedeemer]] FIRST ([CTM-14]), then on the (single)
      * datum variant. Shared by both transitions: exactly one TM-script input ([CTM-17]) and the
      * transaction-wide burn of the TM NFT ([CTM-24] / [CTM-6]).
      */
    def spend(
        oracleScriptHash: ByteString,
        configNftPolicy: ByteString,
        configNftName: ByteString,
        datumOpt: Option[Datum],
        tx: TxInfo,
        ownRef: TxOutRef,
        redeemer: Datum
    ): Unit = {
        // spec [CTM-14] decode the redeemer before the datum: it is the transition selector.
        val spendRedeemer = redeemer.to[TmSpendRedeemer]
        // `fulfilledPorOutpoints` is decoded positionally, never read here. The tag was pinned at
        // mint (the NFT only ever binds to a Constr-0 record). See the UnconfirmedTm scaladoc.
        val record = datumOpt.getOrFail("Missing TM datum").to[UnconfirmedTm]
        val signedBtcTx = record.signedBtcTx
        // Both transitions retire the record the same way. The TM input is authenticated by
        // the spend purpose itself (`findOwnInput`); the TM NFT policy IS this script's own
        // hash (spend and mint share the script).
        val ownOut = tx.findOwnInput(ownRef).get.resolved
        val tmScriptHash = ownOut.address.credential match
            case Credential.ScriptCredential(h) => h
            case _                              => fail("TM input is not at a script address")
        // spec [CTM-17] exactly one TM-script input — the NFT is fungible across posts, so
        // spending two records at once would let the second, un-burned token escape to an
        // attacker output (see [[tmInputCount]]).
        require(
          tmInputCount(tx.inputs, tmScriptHash) == BigInt(1),
          "TM spend: exactly one TM-script input per tx"
        )
        // spec [CTM-24] (Confirm) / [CTM-6] (Gc): the TM NFT is burned — there is no
        // Confirmed record for it to ride to.
        require(
          tx.mint.quantityOf(tmScriptHash, ByteString.empty) == BigInt(-1),
          "TM spend: must burn the TM NFT"
        )
        spendRedeemer match
            case TmSpendRedeemer.Confirm(proof) =>
                // spec [CTM-1] recompute the txid from the witness-stripped serialization —
                // never trust the caller.
                val txid = BitcoinHelpers.getTxHash(signedBtcTx)

                // spec [CTM-3] the block is in the oracle's confirmed-blocks trie.
                val oracleState =
                    findOracleInput(tx.referenceInputs, oracleScriptHash).datum
                        .of[ChainState]
                val blockHash = BitcoinHelpers.blockHeaderHash(proof.blockHeader)
                MPF(oracleState.confirmedBlocksRoot).verifyMembership(
                  blockHash,
                  blockHash,
                  proof.blockMpfProof
                )

                // spec [CTM-2] the header (which hashes to that block hash) commits to txid
                // at txIndex.
                val computedRoot = BitcoinHelpers.merkleRootFromInclusionProof(
                  proof.txMerkleProof,
                  txid,
                  proof.txIndex
                )
                require(
                  computedRoot == proof.blockHeader.merkleRoot,
                  "TM tx not in block merkle root"
                )

                // spec [CTM-25] no output at the TM address: the record is retired, not
                // recreated. Everything downstream reads the singleton instead.
                require(
                  !tx.outputs.exists(
                    _.address.credential === Credential.ScriptCredential(tmScriptHash)
                  ),
                  "TM confirm: no output may sit at the TM address"
                )

                // spec [CTM-28] the singleton policy comes from the Config reference input
                // at RUNTIME ([PAR-1]): the bridge_state script takes THIS script's hash as
                // its own parameter, so a compile-time link would be a cycle.
                val cfgOut = tx.referenceInputs
                    .find(refIn =>
                        refIn.resolved.value
                            .quantityOf(configNftPolicy, configNftName) == BigInt(1)
                    )
                    .getOrFail("TM confirm: no config reference input")
                    .resolved
                val bssPolicy = cfgOut.datum.of[ConfigDatum].bridgeStatePolicy

                // spec [CTM-28] the spent singleton, authenticated by its NFT. Its own
                // validator ([BSS-1]/[BSS-2]) gates the spend on this very transition and
                // delegates datum correctness here ([CTM-27]).
                val bssIn = tx.inputs
                    .find(inp =>
                        inp.resolved.value
                            .quantityOf(bssPolicy, BridgeStateAssetName) == BigInt(1)
                    )
                    .getOrFail("TM confirm: bridge state singleton not spent")
                    .resolved

                // spec [CTM-18] the TM spends the confirmed head. This is what makes
                // re-posting an OLD TM permanently unconfirmable: the head it chained from
                // is spent, so a stale root can never be written back (the rev-5.1 root
                // rollback this revision exists to prevent).
                require(
                  allInputOutpoints(signedBtcTx).head
                      == bssIn.datum.of[BridgeState].treasuryUtxoId,
                  "TM confirm: BTC tx does not spend the confirmed head"
                )

                // spec [CTM-26] exactly one "BTMR1" commitment output; [CTM-20]/[CTM-30]
                // both roots are read from it ([[committedRoots]] — attested, not derived).
                val outs = allOutputs(signedBtcTx)
                val roots = committedRoots(outs)

                // spec [CTM-29] the continuing singleton carries the NFT at the same
                // address — otherwise the next TM could never find it.
                val bssOut = tx.outputs
                    .find(out => out.value.quantityOf(bssPolicy, BridgeStateAssetName) == BigInt(1))
                    .getOrFail("TM confirm: no continuing singleton output")
                require(
                  bssOut.address === bssIn.address,
                  "TM confirm: singleton address changed"
                )

                // spec [CTM-27] rebuild the WHOLE expected datum and compare the whole
                // OutputDatum. On-chain FromData is an erased retag (no tag or arity
                // check), so field-wise reads would also accept `Constr 5 [root, junk]` at
                // the singleton address — attacker-chosen, since confirming is
                // permissionless. spec [CTM-19] head = txid ++ 00000000 (the TM chain
                // layout fixes the treasury change at vout 0); spec [CTM-21] amount =
                // output 0's satoshis.
                val exp = OutputDatum.OutputDatum(
                  BridgeState(
                    spiRoot = roots._1,
                    cpoRoot = roots._2,
                    treasuryUtxoId = txid ++ hex"00000000",
                    treasuryAmount = outs.head.amount
                  ).toData
                )
                require(
                  exp === bssOut.datum,
                  "TM confirm: singleton datum is not the attested state"
                )

            case TmSpendRedeemer.Gc =>
                // Garbage collection ([CTM-16]): after the grace period the CREATOR may
                // reclaim the record's min-ADA. A record whose TM never mines is
                // permanently unconfirmable under [CTM-18] (its head was spent by the TM
                // that did confirm), so it is dead weight nothing ever reads again.
                // `created` is anchored to the mint tx's validity interval (see `mint`), so
                // the grace period cannot be shortcut by backdating.
                // spec [CTM-8] the validity interval lies ENTIRELY after the boundary.
                val timeout = record.created + GcGraceMs
                require(
                  tx.validRange.isEntirelyAfter(timeout),
                  "TM GC: grace period has not elapsed"
                )
                // spec [CTM-7] only the record's creator.
                require(tx.isSignedBy(record.creator), "TM GC: not signed by the record's creator")
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
        require(
          tmOut.address.credential === Credential.ScriptCredential(ownPolicyId),
          "TM mint: NFT output not at own script address"
        )
        // Pin the datum's Constr TAG to 0. A case-class decode is an erased retag with no tag
        // check, and every harvester (reconstruction, the SPI proof walk, confirm's poll filter)
        // keys history by `Constr 0` records — a wrong-tag record would be mintable and
        // confirmable yet invisible to every reader. `fulfilledPorOutpoints` stays decoded
        // positionally and IGNORED — the mint validates nothing about the DA hint.
        val rawDatum = tmOut.datum match
            case OutputDatum.OutputDatum(d) => d
            case _                          => fail("TM mint: NFT output datum must be inline")
        require(
          unConstrData(rawDatum).fst == BigInt(0),
          "TM mint: NFT output datum is not an UnconfirmedTm record"
        )
        val record = rawDatum.to[UnconfirmedTm]
        val txHappenedBefore = tx.validRange.to.finiteOrFail(
          "TM mint: validity range upper bound must be finite"
        )
        // The tx cannot be included after `txHappenedBefore`, so requiring
        // `created == txHappenedBefore` makes `created` a guaranteed upper bound on the real
        // posting time: the GC grace period (the Gc spend branch) can start late but never
        // early, and cannot be backdated. Future-dating only delays the poster's own reclaim.
        require(
          record.created == txHappenedBefore,
          "TM mint: created field must be equal to `tx.validRange.to`"
        )
        val signedBtcTx = record.signedBtcTx
        // The outpoint the embedded BTC tx spends first: input 0 is the treasury by the
        // deterministic TM layout (input[0] = treasury, output[0] = treasury change).
        val spent = allInputOutpoints(signedBtcTx).head
        // spec [PTM-7] the singleton reference input is authenticated by its NFT
        // `(Config bridge_state_policy, "BSS")`, never by position alone. The policy comes from
        // the Config reference input at runtime ([PAR-1]).
        val cfgOut = tx.referenceInputs
            .find(refIn =>
                refIn.resolved.value.quantityOf(configNftPolicy, configNftName) == BigInt(1)
            )
            .getOrFail("TM mint: no config reference input")
            .resolved
        val bssPolicy = cfgOut.datum.of[ConfigDatum].bridgeStatePolicy
        val bssRef = tx.referenceInputs.at(redeemer.bridgeStateRefInputIndex).resolved
        require(
          bssRef.value.quantityOf(bssPolicy, BridgeStateAssetName) == BigInt(1),
          "TM mint: reference input lacks the singleton NFT"
        )
        // spec [PTM-6] the embedded BTC tx chains from the singleton's confirmed head. [CTM-18]
        // already makes the design safe; this mint-time copy of the check stops a TM chaining from
        // a stale head being POSTED at all, so dead records do not accumulate.
        require(
          spent == bssRef.datum.of[BridgeState].treasuryUtxoId,
          "TM mint: BTC tx does not spend the singleton's head"
        )
    }

    /** Minting policy for the TM NFT — the policy id IS this script's hash, so the NFT and the
      * spend logic share one script. PERMISSIONLESS, gated by chain linkage: the freshly posted
      * `Unconfirmed` TM must embed a BTC tx whose input 0 spends the bridge-state singleton's
      * `treasury_utxo_id` ([PTM-6]/[PTM-7] — the singleton is a reference input, authenticated by
      * its NFT). See [[TmMintRedeemer]] for why permissionless minting is safe (Bitcoin's
      * spend-once semantics — the confirmed chain cannot fork). Burning happens on every legal
      * spend (Confirm and Gc alike); all its checks live in `spend`.
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
                spend(
                  oracleScriptHash,
                  configNftPolicy,
                  configNftName,
                  datumOpt,
                  ctx.txInfo,
                  ownRef,
                  ctx.redeemer
                )
            case _ => fail("TM validator: unsupported script purpose")
    }
}

/** The TM validator parameterized with the Binocular oracle script hash and the config NFT
  * `(policy, name)`. The compiled script's hash is BOTH the TM UTxO address (spend) and the TM NFT
  * policy id (mint). All three parameters are STABLE — the address does NOT depend on any
  * participant key. `Unconfirmed` UTxOs are locked here until Confirm retires them; the TM NFT can
  * be minted by ANYONE whose posted TM chains from the bridge-state singleton's head (see
  * [[TmMintRedeemer]]).
  */
object TreasuryMovementContract extends Contract {
    // MUST be releaseUntagged (no `_scalusTag` wrapper), like TmtxScript. This validator is
    // parameterized by THREE curried ByteString params applied at the UPLC level (via
    // BinocularBlueprint.bytesParam — the deployable, blueprint-reproducible path, and how
    // `aiken blueprint apply`/Blaze apply params). With the `_scalusTag` wrapper that plain
    // Options.release adds, that UPLC-level application mis-lands the params relative to the tag and
    // the compiled [[script]] ERRORS on the spend/Confirm branch (while the typed `.apply` used by
    // [[contract]] compensates and works — so unit tests over `contract` passed but the DEPLOYED
    // `script` could never confirm a TM). See the "blueprint .script vs .contract" regression test.
    given opts: Options = Options.releaseUntagged

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

/** Trace-instrumented twin of [[TreasuryMovementContract]] for Scalus diagnostic replay. Compiled
  * from the IDENTICAL validator source but with `generateErrorTraces = true` — the release compile
  * strips trace strings, so a failing on-chain confirm reports only "Error evaluated" with no clue
  * which `require` failed. This twin's script hash DIFFERS from the deployed
  * [[TreasuryMovementContract.script]] (traces change the UPLC) — which is fine and intended: it is
  * registered UNDER the deployed hash via `TxBuilder.withDebugScript` and only re-evaluated against
  * the same script context to surface the failing check. Same validator logic ⇒ same failing
  * require, now with its trace string.
  *
  * Kept in a SEPARATE object so its traces-on `Options` given does not clash with
  * [[TreasuryMovementContract.opts]] during Scalus macro expansion.
  */
object TreasuryMovementDebugContract {
    // Untagged like TreasuryMovementContract (so the twin mirrors the deployed script's logic +
    // param application exactly), plus trace strings the release compile strips.
    given opts: Options = Options.releaseUntagged.copy(generateErrorTraces = true)

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

    /** Trace-compiled [[Script.PlutusV3]] twin for the given params (for `withDebugScript`). */
    def script(
        oracleScriptHash: ByteString,
        configNftPolicy: ByteString,
        configNftName: ByteString
    ): Script.PlutusV3 =
        parameterized
            .apply(oracleScriptHash)
            .apply(configNftPolicy)
            .apply(configNftName)
            .script
}
