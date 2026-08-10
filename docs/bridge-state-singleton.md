# Bridge state singleton: binocular decisions

Implements the core types of `ft-bifrost-bridge/docs/superpowers/specs/2026-08-06-bridge-state-singleton-design.md`,
rev 5.4: sections **§BridgeState, the singleton datum**, **§The two deposit tries**,
**§Root commitment output**, and the `[SPI-*]` checks.

This file records the decisions that the code cannot state by itself, and the alternative each one
rejected. It is not a description of the code. Read the doc comments for that.

Code: `src/main/scala/binocular/watchtower/TreasuryMovementValidator.scala` (`BridgeState`,
`committedRoots`, the `BTMR1` constants), `SweptPegInsTrie.scala`, `MpfSetBuilder.scala`.

## Scope

This change is **additive only**. No existing code path reads `BridgeState`, calls `committedRoots`,
or builds an SPI trie yet. The old `"CPOR1"` single-root constants and `committedRoot` stay in
place, untouched, and the validator still uses them.

Rejected: migrating the reader and deleting `"CPOR1"` in the same change. The single-root commitment
is what every already-confirmed TM on chain carries. Reconstruction (`CpoReconstruction`,
`PorSweeper.recover`) replays those TMs, so the old reader must keep working until the migration
tasks land and decide how history is read.

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

## Test notes

`SweptPegInsTrieTest` builds its raw TM in the test, from outpoints and scripts it names, rather than
using a mainnet fixture. The assertions are about which inputs become keys and which outpoint becomes
the value, so the test must be able to state the expected outpoints. A recorded fixture would move
those expectations into opaque hex.

`BridgeStateTest` asserts the `Constr` tag, the field order **and** the arity. The arity assertion is
the `[LIB-3]` guard: an inserted field shifts every later index and silently re-points the Aiken
readers, so it must break this test rather than a validator.
