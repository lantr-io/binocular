# Bridge state singleton: binocular decisions

Implements the core types of `ft-bifrost-bridge/docs/superpowers/specs/2026-08-06-bridge-state-singleton-design.md`,
rev 5.4: sections **§BridgeState, the singleton datum**, **§The two deposit tries**,
**§Root commitment output**, the `[SPI-*]` checks, and the `[BSS-4]`/`[BSS-5]` parameterization.

This file records the decisions that the code cannot state by itself, and the alternative each one
rejected. It is not a description of the code. Read the doc comments for that.

Code: `src/main/scala/binocular/watchtower/TreasuryMovementValidator.scala` (`BridgeState`,
`committedRoots`, the `BTMR1` constants), `SweptPegInsTrie.scala`, `MpfSetBuilder.scala`.

## Scope

The migration is COMPLETE: TM Confirm burns the record and advances the singleton
([CTM-17..30]), the mint chains from the singleton head ([PTM-6]/[PTM-7]), the confirm/deploy/
bootstrap/sweep commands run on the singleton, and the `"CPOR1"` single-root constants and
`committedRoot` are deleted — a fresh deployment's history contains only `"BTMR1"` TMs, so no
reader of the old form remains.

## `BridgeState` has four fields, not five

The spec's §BridgeState table is normative and lists four fields. Its surrounding prose calls the
type "five flat primitives" and an aside elsewhere mentions a federation sweep txid.

Decision: implement the four-field table. The prose is stale drafting from an earlier revision.

Rejected: adding a fifth field to match the prose. Under `[LIB-3]` a field may only be appended, so
a wrong guess is cheap to add later and expensive to remove: removing one shifts nothing, but it
does force every Aiken reader and every already-written datum through a migration. Four fields that
are certainly right beat five where the fifth is a guess.

`BridgeState` lives in `TreasuryMovementValidator.scala`, beside `CompletedPegOutsTrieDatum` and the
other on-chain datums, rather than in a file of its own. It is `@Compile`d on-chain state, and the
file is where a reader looks for the datums this validator writes.

## `committedRoots` filters, it does not `find`

`[CTM-26]` requires **exactly one** commitment output per TM. The reader filters the output list and
matches on a one-element result.

Rejected: `outs.find(isTwoRootCommitment)`. `find` returns the first match and ignores the rest, so a
TM carrying two commitment outputs would confirm against whichever came first. A permissionless
confirmer picks the transaction, so that choice would be an attacker's. Zero and two are both
failures, and they get distinct messages because they are different bugs in the producing TM.

The `isTwoRootCommitment` guard checks length **and** prefix. Length alone would admit a 71-byte
payment script; prefix alone would let a truncated script be sliced past its end, and would also
admit the old 39-byte `"CPOR1"` output, whose roots mean something else.

## SPI values are the sweeping TM's input-0 outpoint

`[SPI-3]`, and the spec gives the reason: `spi_root` rides in the same transaction's commitment
output and a txid hashes every output, so a value of the sweeping TM's txid would need a hash fixed
point. That rationale is repeated in the `SweptPegInsTrie` doc comment because it is the first thing
a reader tries to "fix".

`entriesOf` parses the raw TM through `TreasuryMovementValidator.allInputOutpoints`, the function the
on-chain validator uses.

Rejected: a second, off-chain-only input parser. Two parsers of the same bytes can disagree about
what the inputs are, and the disagreement would surface as an SPI root that no TM ever committed,
detectable only after a peg-in proof fails.

## `MpfSetBuilder` is shared with the CPO trie

The de-duplicating set-to-root loop was moved out of `CompletedPegOutsTrie` into `MpfSetBuilder` and
is now used by both tries. `CompletedPegOutsTrie.trieFrom` keeps its signature and its behaviour; only
the key label in the duplicate-conflict message is now passed in.

Rejected: copying the loop into `SweptPegInsTrie`. Both mirrors resolve entries from many TMs in
arbitrary order, and both must treat a repeated key identically. Two copies could drift on the
duplicate rule, and a drift there produces a root that differs from the attested one with no other
symptom.

## `membershipProof` returns `Either`, and offers no non-membership proof

A watchtower serves these proofs to depositors, so "not swept yet" is an ordinary answer during the
window between the deposit and the sweeping TM. It is returned as `Left`, not raised.

Rejected: also exposing a non-membership proof. Absence from the SPI trie proves nothing: the trie
only grows, and a deposit missing from it may simply not be swept yet. `mpf.miss` on this trie must
never become the basis of an on-chain decision. (The CPI trie is different: there, absence is the
replay check, and it is the completion itself that records presence.)

## Vendoring the rev-5.4 blueprint (`[BSS-4]`, `[BSS-5]`)

Code: `src/main/scala/binocular/watchtower/BifrostContracts.scala` (`BridgeStateContract`,
`BifrostBlueprint.packaged`), `src/main/resources/bifrost-plutus-min.json`.

### The min-json no longer carries the deleted CPO validator

ft rev 5.4 removed `completed-peg-outs-merkle-tree.ak`. The vendored min-json carried its last
published `compiledCode` for as long as call sites still resolved that policy
(`BootstrapCompletedPegOutsCommand`, `BridgeSweepSetup`, `ConfirmTmtxCommand`, `PorSweeper`). The
`bss-bootstrap-cleanup` migration retired every one of those call sites, so the entry is gone and
the vendored resource matches ft's published titles again.

### `BridgeStateContract` landed before its callers

The wrapper landed one task before the commands that use it (`deploy-bridge`,
`bootstrap-bridge-state`, `confirm-tmtx`, `deploy-script-refs`), so its regression pin was
established once and a later change to a derived policy fails the pin rather than a bootstrap flow.

### Two freshness checks, not one

`BifrostContractsTest` asserts a hard-coded `bridge_state` policy id **and** compares the vendored
`compiledCode` against a sibling ft checkout.

Rejected: either one alone. The pin is computed from the vendored resource, so it locks that
resource against itself and can stay green through a full ft validator rewrite — this is exactly how
a stale `peg_out` survived in 2026-08. The ft comparison catches that, but it cancels wherever no
checkout exists, which is CI and every release build. Neither check covers the other's blind spot.

### The first parameter is derived, not configured

`[BSS-4]` names `tm_nft_policy_id`, and that value is the `TreasuryMovementValidator` script hash.
The test derives it from `TreasuryMovementContract.script(...)` rather than accepting a configured
constant, because a configured one could drift from the deployed TM validator and would only be
caught on-chain, when the singleton's spend handler failed to find its TM Confirm.

The known-answer pin uses a fixed 28-byte placeholder instead, so that it depends on the vendored
`bridge_state` code and the CIP-57 parameter encoding alone. A pin taken against the real TM hash
would also move whenever binocular's own TM validator changed, which is unrelated drift.

## Test notes

`SweptPegInsTrieTest` builds its raw TM in the test, from outpoints and scripts it names, rather than
using a mainnet fixture. The assertions are about which inputs become keys and which outpoint becomes
the value, so the test must be able to state the expected outpoints. A recorded fixture would move
those expectations into opaque hex.

`BridgeStateTest` asserts the `Constr` tag, the field order **and** the arity. The arity assertion is
the `[LIB-3]` guard: an inserted field shifts every later index and silently re-points the Aiken
readers, so it must break this test rather than a validator.

# Peg-in completion off the Confirmed record

Implements spec **§Complete peg-in** (`[CPI-3]`, `[CPI-6]`, `[CPI-8]`, `[CPI-9]`, `[CPI-10]`),
**§No Confirmed record** (`[OB-5]`), **§Off-chain: binocular** (`[OB-10]`, `[OB-11]`) and
`[CLR-7]`/`[CLR-9]` from **§Close PegInRequest**.

Code: `src/main/scala/binocular/watchtower/PegInTypes.scala` (`PegInDatum`, `BifrostMessages`,
`PegInActionType.CompletePegIn`), `PegInCompleteTx.scala`, `PegInRequestTx.scala`,
`src/main/scala/binocular/cli/commands/PegInCompleteCommand.scala`, `PegInRequestCommand.scala`,
`SignPeginMsgCommand.scala`.

## The `[CPI-3]` preimage helper lives on `BifrostMessages`

`completionDigest` and `completionSignText` sit beside `mintTag`, not on `PegInCompleteTx`.

Rejected: building the preimage inside the transaction builder. Two commands need the message and
neither builds a transaction to get it: `pegin-complete --dry-run` prints it before any signature
exists, and `sign-pegin-msg` never sees the deposit at all. A builder-owned preimage would have to
be duplicated in the CLI, and a duplicated hash preimage drifts silently – the depositor signs one
message and the validator rebuilds another, with a phase-2 failure as the only symptom.

`completionSignText` takes the digest rather than its inputs for the same reason: `sign-pegin-msg`
is handed the printed digest, so it must be able to produce the exact signed text from that alone.

## The completion command resolves the singleton itself, not through `ProofService`

`PegInCompleteCommand` repeats `ProofService.resolveBridge`'s shape: a RAW positional read of the
config datum for `bridge_state_policy` and `tm_script_hash` (`[PAR-1]`), then the UTxO carrying
`(bridge_state_policy, "BSS")`, then a `Try`-guarded `fromData[BridgeState]`.

Rejected: calling `ProofService`. It answers in JSON and keeps `resolveBridge` private, and the
command needs the singleton **UTxO** – it goes into the transaction as a reference input at
`bridge_state_ref_input_index` (`[CPI-10]`), not just its datum.

The raw positional read is deliberate in both places: a deployed Config datum with more fields than
the Scalus `ConfigDatum` mirror knows must still resolve. The two field positions are now named
constants on `ConfigDatum`, shared by both readers, so a spec renumbering is one edit and not a hunt
for integer literals.

## The swept-set proof is fetched before the `--dry-run` exit

`--dry-run` reconciles the swept set and builds the `[CPI-9]` proof, even though it prints a digest
and stops.

Rejected: deferring the proof to the real submit. The digest is what the depositor signs, so a
dry run that succeeds is an invitation to sign. Learning only afterwards that the deposit is not in
the confirmed swept set (`[SPI-6]`) wastes a signing round trip. The retired flow was equally strict
– it needed the Confirmed TM UTxO at the same point – so nothing became slower.

## Both tries take their value from one reconciled swept set

`completedPegInsUpdate` replays the CPI trie and inserts this deposit, taking **every** value from
the SPI trie that `SweptPegInsProofService.confirmedTrie` reconciled. The command therefore keeps
that trie instead of discarding it after one proof.

Rejected: `insert(peg_in_utxo_id, peg_in_utxo_id)`. `peg-in.ak` CompletePegIn checks
`mpf.insert(input_tree, peg_in_utxo_id, sweeping_tm_input_0, proof) == output_tree`, so a
key-as-value entry produces a root the validator rejects. Worse, once one such completion has
landed, a reconstruction that repeats the mistake never reproduces the on-chain root either, and the
error surfaces as an unexplained root mismatch far from its cause.

Rejected: deriving each prior completion's value from its own SPI proof, fetched one at a time. One
reconciliation already holds every value, and a per-entry fetch would let two entries be read from
two different swept-set snapshots.

`--prior-pegin` keeps its `<pegInUtxoId>` form for the same reason: the value is derived, never
typed. A `key=value` flag would let an operator assert a value the swept set contradicts.

## The PegInRequest NFT burn carries a rebuilt mint redeemer

`[CPI-8]` requires the request NFT burned in the completion. The `peg_in` mint handler's burn-only
branch returns true without reading its redeemer, but Aiken still **decodes** it as a
`PegInMintRedeemer`, and a malformed one traps before the branch is reached.

Rejected: an empty or placeholder redeemer. It must decode, so the redeemer is rebuilt from the
request being retired – its own outpoint and its own datum – with the Bitcoin proof fields empty.
Those fields are unread on this path, and filling them would mean re-fetching a block header for a
burn that ignores it.

`pirNftBurn` refuses a request that carries no token, or several, under the `peg_in` policy. Such a
UTxO is not an authentic request, and refusing here beats building a transaction that fails phase 2.

## `PegInDatum` was migrated here, not left to a later task

The Scalus mirror still had the rev-5.1 shape (`source_chain_treasury_utxo_id`, no `created`).
Nothing else in this pipeline edits `PegInTypes.scala`, so the mirror was orphaned.

Rejected: leaving it. Field positions are consensus-visible: against a rev-5.4 deployment
`peg_in_amount` sits at position 4 as a `Data.I` where the old shape expects a `Data.B`, so the old
mirror cannot decode **any** real PegInRequest. Peg-in completion is exactly the code that decodes
one, so the task could not be correct while the mirror was stale.

`created` and the mint transaction's validity upper bound are derived from **one** slot, through
`cardanoInfo.slotConfig`. `[CLR-7]` requires them equal and the bound finite. Rejected: taking
`Instant.now()` twice, or letting the builder pick the bound. Two clock reads can straddle a slot
boundary, and the datum and the transaction would then disagree by one slot with no local symptom.

## Left undone, deliberately

`PegInActionType.Cancel` still mirrors the retired constr-0 payload. Rev 5.4 renames the branch to
`Close`, gives it two fields and a `ClosePegInProof` sum. No binocular code builds it and no test
covers it, and the off-chain Close flow is `clr-close-pir`'s scope. Mirroring a branch nothing
constructs would add an untested shape that the next revision may change again.

`PegInRequestCommand` still sets `owner_auth` to an inert, never-satisfiable signature credential.
Under `[CLR-9]` that makes a request unclosable by anyone – only completion can retire it. Giving it
a real credential is part of the Close flow, so the comment now says so instead of claiming, as it
used to, that the field is dead.
