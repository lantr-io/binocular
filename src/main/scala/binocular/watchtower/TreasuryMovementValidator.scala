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
  *     `creator` key hash, `created` (POSIX ms, must equal the posting tx's validity upper bound —
  *     see the mint branch), the N7 fields `epoch` (the Cardano epoch this TM belongs to) and
  *     `leaderReward` (the reward amount, a copy of the Config `leader_reward` tunable at post),
  *     and the rev-5.1 data-availability hint `fulfilledPorOutpoints`. Constr tag 0,
  *     `[signed_btc_tx, creator, created, epoch, leader_reward, fulfilled_por_outpoints]` — the
  *     shape heimdall's `publish.rs` and binocular's `create-tmtx` post. N7 note:
  *     `epoch`/`leaderReward` are carried here but NOT yet enforced on-chain — the leader_reward
  *     pin against the Config and the reward payout land with N9 (see
  *     [[2026-07-21-n9-leader-reward-attribution]] in internal-docs); `tm_sequence` is deliberately
  *     NOT a datum field (off-chain signing counter, spec §Cardano submission and leader reward).
  *   - [[Confirmed]] — produced by the Confirm transition once the TM is Binocular-confirmed. Holds
  *     the `btcTxid`, the list of swept peg-in outpoints (`sweptPegInUtxoIds`, 36-byte
  *     prev_txid++vout each), the `fulfilledPegOuts`, the N10b `spentViaFederationLeaf` flag, and
  *     `creator`/`created`/`epoch`/`leaderReward` carried verbatim from the Unconfirmed input
  *     (`creator`/`created` drive the GC path). Constr tag 1. `spentViaFederationLeaf` is the
  *     objective dead-roster evidence `treasury.ak::FederationReset` gates on: it is `true` iff
  *     this TM's treasury input (input 0, pinned to the treasury outpoint at mint) was spent via
  *     the federation CSV leaf. The treasury taproot tree has a SINGLE leaf (the federation
  *     `<csv> OP_CSV OP_DROP <y_fed> OP_CHECKSIG`; internal key `Y_51` for the key-path roster
  *     sweep), so a *confirmed* 3-item script-path spend of input 0 can only be that leaf — and it
  *     could confirm only after `OP_CSV` was satisfied, i.e. the tip aged unmoved past
  *     `federation_csv_blocks` (a live roster's coins never age that far). The flag is therefore
  *     computed COARSELY as [[BitcoinHelpers.isValidScriptPathWitness]]`(signedBtcTx, 0)` (3-item
  *     witness) — no `y_federation`/`csv` needed on-chain — and ENFORCED here: the Confirm branch
  *     bakes it into the reconstructed `exp` datum the continuing output must match, so a
  *     (permissionless) confirmer cannot forge it. [[binocular.cli.commands.ConfirmTmtxCommand]]
  *     computes the same value off-chain to build a matching output.
  *
  * Variant order and field order are positional in the Plutus Constr — do not reorder. New fields
  * are normally APPENDED (epoch/leaderReward after created), never inserted.
  * `spentViaFederationLeaf` is the one deliberate exception: the Aiken mirror
  * `treasury-movement.ak` is a decode-only PREFIX of this datum (it omits
  * `creator`/`created`/`epoch`/`leaderReward`, which no Aiken validator reads), and
  * `treasury.ak::FederationReset` binds the flag by its DECLARED position — index 3 — in that
  * 4-field mirror. So the flag is pinned at Constr index 3 HERE too (inserted after
  * `fulfilledPegOuts`, before `creator`); appending it after `leaderReward` would make Aiken read
  * `creator` as the Bool. Downstream prefix readers that stop at index 2 — Aiken `peg_in.ak`
  * (indices 0,1) and heimdall `parse_confirmed_tm_datum` (indices 0,1,2, `len>=3`) — are unaffected
  * by the insert.
  */
enum TmDatum derives FromData, ToData {
    case Unconfirmed(
        signedBtcTx: ByteString,
        creator: PubKeyHash,
        created: PosixTime,
        epoch: BigInt,
        leaderReward: BigInt,
        /** Rev-5.1 data-availability HINT: the Cardano outpoints of the PegOutRequests this TM
          * fulfills, 36 bytes each (Cardano tx hash (32) ++ output index as 4 little-endian bytes).
          *
          * UNVERIFIED. Neither `mint` nor `spend` reads a single byte of it: the FROST-signed
          * `"CPOR1"` root commitment inside `signedBtcTx` is the sole integrity anchor for the
          * completed-peg-outs trie. Posting a TM is permissionless, so a hostile poster can garble
          * this list; that costs the protocol nothing.
          *
          * What it is for: rebuilding the completed-peg-outs trie from chain data alone (cold
          * start, recovery, a new SPO). Reconstruction reads it from the SPENT `Unconfirmed`
          * output's inline datum, which stays in Cardano history forever and is indexable by
          * address alone (Kupo indexes datums of spent outputs; it does not index tx metadata —
          * which is why the hint is a datum field). With the hint, reconstruction resolves each
          * outpoint to its POR datum and inserts the entry directly; without it, the fallback is to
          * match the TM's payment outputs against the PegOutRequests open at that time and search
          * assignments until the running root equals that TM's committed root. The committed root
          * turns reconstruction from trust into search-and-check either way.
          *
          * [[Confirmed]] deliberately does NOT carry it: the Unconfirmed record is the permanent
          * source, and keeping `Confirmed` at 8 fields leaves `peg_in.ak`,
          * `treasury.ak::FederationReset`, and heimdall's Confirmed parser untouched.
          */
        fulfilledPorOutpoints: ScalusList[ByteString]
    )
    case Confirmed(
        btcTxid: ByteString,
        sweptPegInUtxoIds: ScalusList[ByteString],
        fulfilledPegOuts: ScalusList[PegOutEntry],
        // N10b: index 3, pinned here to match the Aiken FederationReset positional read — see the
        // datum scaladoc above. Inserted before the provenance fields, NOT appended.
        spentViaFederationLeaf: Boolean,
        creator: PubKeyHash,
        created: PosixTime,
        epoch: BigInt,
        leaderReward: BigInt
    )
}

@Compile
object TmDatum

/** Scalus mirror of `completed-peg-outs-merkle-tree.ak::CompletedPegOutsMerkleTreeDatum` — the MPF
  * root of the completed peg-outs. Bootstrapped with the empty root (32 zero bytes), then spent and
  * recreated by EVERY TM Confirm, which copies the root the FROST quorum attested in that TM's
  * `"CPOR1"` commitment output (a TM that fulfills no peg-out re-commits the unchanged root).
  *
  * Kept HERE, not in `ConfigTypes.scala`, because this validator is the only writer and the file's
  * `@Compile` companion is what lets the Confirm branch decode it on-chain.
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
  *   - [[Genesis]] — RETIRED (spec [PTM-5] WITHDRAWN): the rev-5.4 Config datum no longer carries
  *     `initial_btc_treasury_utxo`, so this variant now always fails. It stays in the enum so
  *     [[Chain]]'s constructor index does not move. TODO(bridge-state migration): [PTM-6]/[PTM-7]
  *     replace both variants with a check against the bridge-state singleton's head.
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
  *   6. the completed-peg-outs trie UTxO (policy from Config field 3, asset name `"CPO"`) is spent
  *      and recreated at the same address, carrying the root the TM's single `"CPOR1"` commitment
  *      output attests — see [[committedRoot]].
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
        configNftPolicy: ByteString,
        configNftName: ByteString,
        datumOpt: Option[Datum],
        tx: TxInfo,
        ownRef: TxOutRef,
        redeemer: Datum
    ): Unit = {
        val datum = datumOpt.getOrFail("Missing TM datum").to[TmDatum]
        // Only the Unconfirmed -> Confirmed transition is a legal spend.
        datum match
            // `fulfilledPorOutpoints` is the rev-5.1 DA hint: decoded positionally, never read. See
            // the TmDatum scaladoc for why nothing on-chain validates it.
            case TmDatum.Unconfirmed(signedBtcTx, creator, created, epoch, leaderReward, _) =>
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

                // 6. "Is this the real TM record?" is settled above, and no longer the way the old
                // checklist framed it (look the TM address up in a Treasury State reference UTxO).
                // There is no Treasury State UTxO: this script's address is fixed by its three
                // applied parameters, so the TM input is authenticated by the spend purpose itself
                // (`findOwnInput(ownRef)`), the TM NFT policy IS this script's own hash (spend and
                // mint share it), `tmInputCount == 1` bounds the tx to one TM record, and the
                // continuing output must carry that NFT back. The `signedBtcTx` those checks
                // authenticate was in turn bound to the NFT at mint time (`validateMinting`), which
                // also chained it to the previous Confirmed record or to the config's
                // `initial_btc_treasury_utxo`. Nothing further is needed here.

                val swept = allInputOutpoints(signedBtcTx)
                val fulfilled = allOutputs(signedBtcTx)
                // N10b: was the treasury (input 0) swept via the federation CSV leaf? The treasury
                // taproot tree is single-leaf (federation `<csv> OP_CSV OP_DROP <y_fed> OP_CHECKSIG`;
                // internal key Y_51 for the key-path roster sweep), so a 3-item script-path witness on
                // input 0 IS that leaf. Because this TM is being confirmed against the oracle (it is
                // mined on Bitcoin), that script-path spend satisfied OP_CSV — the tip had aged past
                // federation_csv_blocks, i.e. the roster is provably dead. Computing it HERE (from the
                // mint-committed signedBtcTx) and baking it into `exp` makes the flag unforgeable by a
                // permissionless confirmer. treasury.ak::FederationReset consumes it. See the datum doc.
                val spentViaFederationLeaf =
                    BitcoinHelpers.isValidScriptPathWitness(signedBtcTx, BigInt(0))
                // N7: epoch + leaderReward ride through the Confirm transition verbatim (like
                // creator/created), so the Confirmed record carries the poster-declared reward
                // amount for N9's later payout enforcement.
                val exp = OutputDatum.OutputDatum(
                  TmDatum
                      .Confirmed(
                        txid,
                        swept,
                        fulfilled,
                        spentViaFederationLeaf,
                        creator,
                        created,
                        epoch,
                        leaderReward
                      )
                      .toData
                )
                require(
                  exp === contOut.datum,
                  "Continuing output datum does not match parsed TM Confirmed"
                )

                // 7. Completed-peg-outs trie update. The TM carries exactly one "CPOR1" OP_RETURN
                // output committing the trie root that holds AFTER it. Copy that root into the
                // continuing trie output.
                //
                // Why here and not in peg-out.ak: the ONLY place the protocol learns which Bitcoin
                // payment settles which peg-out request is the FROST-signed TM itself. Recording it
                // at Confirm makes peg-out Complete a single MPF membership proof, with no need to
                // re-parse the raw TM.
                //
                // Why a copy and not an on-chain fold: the root is committed INSIDE the bytes the
                // quorum signed, so it is a quorum attestation exactly like the payments themselves.
                // Re-deriving it on-chain from per-peg-out markers (the previous design) cost ~46 vB
                // of Bitcoin per peg-out and an MPF insert per peg-out in the Confirm budget, and
                // bought no trust the quorum did not already hold: it never proved a payment settles
                // the request its marker names. See the design note "rev 5.1".
                //
                // The trie UTxO must be SPENT here (not referenced): its own Aiken validator
                // (`completed-peg-outs-merkle-tree.ak`) gates its spend on exactly this transition
                // — a TM-NFT input with a tag-0 datum plus a TM-NFT output with a tag-1 datum — and
                // delegates root correctness to this check.
                val cfgOut = tx.referenceInputs
                    .find(refIn =>
                        refIn.resolved.value
                            .quantityOf(configNftPolicy, configNftName) == BigInt(1)
                    )
                    .getOrFail("TM confirm: no config reference input")
                    .resolved
                // Config field 3 (rev 5.4: `bridge_state_policy`). Read at RUNTIME, not applied
                // as a parameter (spec [PAR-1]): the state script takes THIS script's hash as its
                // own parameter, so a compile-time link would be a parameterization cycle.
                //
                // INTERIM until the TM singleton migration ([CTM-18]..[CTM-30]) rewrites this
                // whole block: field 3 still carries the completed-peg-outs trie policy at deploy
                // time, and this branch still spends/recreates that trie. TODO(bridge-state
                // migration): spend the singleton (NFT asset "BSS") and write BridgeState here.
                val triePolicy = cfgOut.datum.of[ConfigDatum].bridgeStatePolicy
                val trieIn = tx.inputs
                    .find(inp =>
                        inp.resolved.value
                            .quantityOf(triePolicy, CompletedPegOutsAssetName) == BigInt(1)
                    )
                    .getOrFail("TM confirm: completed-peg-outs trie not spent")
                    .resolved
                val trieOut = tx.outputs
                    .find(out =>
                        out.value.quantityOf(triePolicy, CompletedPegOutsAssetName) == BigInt(1)
                    )
                    .getOrFail("TM confirm: no continuing completed-peg-outs output")
                // The NFT must come back to the same script address, or the next TM could not find
                // it and the trie would be permanently unspendable.
                require(trieOut.address === trieIn.address, "TM confirm: trie address changed")
                // The attested root, taken from the TM's single "CPOR1" output. A TM that fulfills
                // no peg-out commits the UNCHANGED root, so the trie UTxO round-trips with the same
                // datum — but the commitment output is still mandatory.
                //
                // EXACT datum equality, not `trieOut.datum.of[CompletedPegOutsTrieDatum].root ==
                // newRoot`. On-chain `FromData` is an erased retag: field access is a lazy
                // projection with no constructor-tag or arity check, so a root-only comparison would
                // also accept `Constr 5 [root, junk]` at the trie address. Confirming is
                // permissionless, so that shape is attacker-chosen, and every downstream reader
                // (the Aiken trie validator, peg-out Complete, reconstruction tooling) would then
                // have to cope with a datum the protocol never describes. Rebuilding the expected
                // datum and comparing the whole `OutputDatum` pins the tag, the arity, and
                // inline-ness in one check — the same discipline as the `exp === contOut.datum`
                // check on the TM record above.
                val newRoot = committedRoot(fulfilled)
                val expTrieDatum =
                    OutputDatum.OutputDatum(CompletedPegOutsTrieDatum(newRoot).toData)
                require(
                  expTrieDatum === trieOut.datum,
                  "TM confirm: trie datum is not the canonical committed root"
                )
            case TmDatum.Confirmed(_, _, _, _, creator, created, _, _) =>
                // Garbage collection: after the grace period the CREATOR may reclaim the record's
                // min-ADA, burning the TM NFT. By then all peg-ins/peg-outs swept by this TM are
                // expected to be completed (the record is no longer needed as proof material).
                // `created` is anchored to the mint tx's validity interval (see `mint`), so the
                // grace period cannot be shortcut by backdating.
                //
                // OPERATIONAL RULE (accepted residual risk): burning the chain-TIP record leaves
                // the next TM with no predecessor to reference (and Genesis is retired), so
                // the creator must not burn the tip. While the bridge is active a successor lands
                // well within the grace period; after a >30-day quiet spell, recovery arrives with
                // the bridge-state singleton migration (the head lives there, spec §Recovery).
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
            // The 6th field (`fulfilledPorOutpoints`) is decoded positionally and IGNORED — the mint
            // validates nothing about the DA hint. See the TmDatum scaladoc.
            case TmDatum.Unconfirmed(rawTx, _, created, _, _, _) =>
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
            case TmMintRedeemer.Genesis(_) =>
                // RETIRED (spec [PTM-5] WITHDRAWN with rev 5.4): the Config datum no longer
                // carries `initial_btc_treasury_utxo` — [BSS-4] anchors the chain in the
                // bridge-state singleton's bootstrap redeemer instead. The variant stays in the
                // enum so `Chain`'s constructor index is unchanged. TODO(bridge-state migration):
                // [PTM-6]/[PTM-7] replace both variants with a check against the singleton head.
                fail("TM mint: Genesis is retired ([PTM-5]); the chain anchors in the singleton")
            case TmMintRedeemer.Chain(i) =>
                val prev = tx.referenceInputs.at(i).resolved
                require(
                  prev.value.quantityOf(ownPolicyId, ByteString.empty) == BigInt(1),
                  "TM mint: predecessor lacks the TM NFT"
                )
                prev.datum.of[TmDatum] match
                    case TmDatum.Confirmed(btcTxid, _, _, _, _, _, _, _) =>
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
  * participant key. `Unconfirmed` UTxOs are locked here and spent into `Confirmed` ones; the TM NFT
  * can be minted by ANYONE whose posted TM chains from the current treasury outpoint (config anchor
  * or predecessor `Confirmed` record — see [[TmMintRedeemer]]).
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
