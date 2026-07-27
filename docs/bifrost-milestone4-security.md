# Bifrost Milestone 4 – Security Analysis and Applied Mitigations

**Date:** 2026-07-27
**Scope:** the Binocular oracle validator, the Bifrost treasury movement and peg-in/peg-out glue
contracts in this repository, and the watchtower/SPO off-chain programs. The SPO consensus layer
(DKG, banning, federation fallback) is covered in the Heimdall repository
(https://github.com/lantr-io/heimdall).

This iteration ran an internal adversarial review of the whole oracle + bridge stack against the
unhappy-path categories named in the milestone: Bitcoin reorg attacks, malicious players
(theft/forgery attempts), watchtower censorship, and Cardano network saturation. This document
lists the attack vectors identified and the mitigations applied or verified.

## 1. Issues found and fixed in this iteration

### 1.1 TM NFT escape via duplicate records (fund theft) – fixed

**Vector.** Treasury Movement records are authenticated by a fungible TM NFT (policy = the TM
validator's own hash). Because posting Unconfirmed TM records is permissionless, an attacker could
post two records embedding the same signed BTC transaction and spend both in one Confirm (or GC)
transaction: both per-input validator runs accepted the single continuing output, and ledger value
conservation forced the second NFT into an attacker output carrying a fabricated Confirmed datum -
which downstream peg-in validation trusts by NFT alone, enabling unbacked fBTC.

**Fix.** The validator now enforces exactly one TM-script input per spend, on both the Confirm and
GC paths (PR #14, commit `ce6a9e6`). Regression tests: the "NFT containment" cases in
`TreasuryMovementValidatorTest` reproduce the two-record theft for both paths and assert rejection.

### 1.2 fBTC recipient hijack in peg-in completion – fixed

**Vector.** Peg-in completion minted fBTC to a recipient chosen by the transaction builder; a
malicious watchtower completing someone else's peg-in could redirect the minted fBTC, and the
peg-in input was selected by caller-provided index rather than by script address.

**Fix.** The fBTC recipient is now bound on-chain in `PegInDepositorAuthValidator`, and the peg-in
input is selected by script address, not by caller index (PR #11, commits `d4e65fb`, `0662228`).

### 1.3 Deep Bitcoin reorgs beyond the confirmation depth – recovery path added

**Vector.** Reorgs within the maturation window are handled natively by the fork tree (highest
chainwork wins, nothing is confirmed before `maturationConfirmations` + the challenge aging
period). A reorg deeper than the confirmed history, however, previously had no recovery short of
closing and redeploying the oracle - which cascades into a full bridge redeploy because every
bridge contract is parameterized by the oracle script hash.

**Mitigations applied:**

- **`SetState` owner redeemer** (commit `4485ed9`, Whitepaper "Owner State Reset"): replaces the
  stale `ChainState` in a single transaction while keeping the oracle NFT and script hash, so all
  downstream bridge contracts remain valid. All consensus validation of subsequent updates stays
  in force; the reset itself is owner-signed and auditable on-chain.
- **Detection and fail-fast**: the watchtower detects a reorg into confirmed history, alerts
  (Discord) with the full reorg depth, and stops instead of looping (commits `75b0d38`,
  `45af99c`, `f0eb7e9`; alert delivery is flushed before exit, `b1fb1c4`).
- **Autonomous recovery**: with `oracle.auto-reset` enabled the watchtower waits out the staleness
  gate and issues the `SetState` itself, then resumes syncing (commit `a0b9171`).
- **Range-seeded re-initialization** (`init --confirmed-until`, commit `55d34a3`) as the fallback
  when the oracle UTxO itself is lost.

**Field evidence:** four real deep reorgs on Bitcoin testnet4 (2026-07-11 through 2026-07-27, up
to ~55 blocks) were recovered on preprod with this machinery, without redeploying the bridge.

### 1.4 Watchtower liveness hardening – fixed

- Transient Bitcoin RPC failures no longer permanently skip a TM confirmation (`f927616`); only
  operator-declared dead TMs are skipped (`5e7d118`), and a TM is declared dead only on on-chain
  evidence that its inputs were double-spent (`TmLiveness`).
- Daemon loops run on isolated virtual threads; a fatal loop death crashes the process (systemd
  restarts it) instead of leaving a half-alive watchtower (`c568a8e`).
- Fee estimation bugs that could stall oracle recovery and TM confirmation under CIP-33 reference
  scripts were fixed (`f120d34`).
- The deployed oracle script hash is pinned in config and verified against the derived hash at
  startup, preventing a mis-built binary from watching the wrong oracle (`31addc7`).
- Operational alerting: rate-limited Discord notifications for blocks and TM relay/confirm,
  immediate alerts for errors (`a835038`, `ad1cb07`).

### 1.5 Execution-budget exhaustion (oracle stall) – bounded and benchmarked

**Vector.** On-chain update cost grows with fork-tree size and shape; an adversary shaping the
tree with valid low-difficulty forks could try to push promotion transactions past Cardano's
execution-unit limits, wedging the oracle (a Cardano-side saturation/DoS analogue).

**Mitigations:**

- `maxBlocksInForkTree = 256` bounds the tree; the bound is validated on-chain and sized so the
  worst-case balanced-tree griefing shape still fits the 16 KB datum with headroom
  (`BitcoinContract.scala`).
- Empirical execution-unit benchmarks run in CI on every push: block-header throughput, promotion
  throughput at a 100-block tree, and fork-tree capacity tests (`BitcoinValidatorTest`).
- The off-chain builder sizes update batches against actual execution-unit and transaction-size
  limits, not byte heuristics (`buildOptimalUpdateTransaction`).
- The Milestone 4 optimization work (see `bifrost-milestone4-performance.md`) reduced validator
  CPU cost by 18-32%, adding budget headroom: max headers per update went from 55 to 71, and max
  promotions per transaction from 21 to 28.

## 2. Attack vectors covered by the existing design (verified this iteration)

- **Pre-computed fork attack** (offline-mined 100+ block fork published at once): defeated by the
  200-minute on-chain challenge aging before promotion plus highest-chainwork canonical selection;
  honest parties need only submit the real chain within the window (Whitepaper, "Attack
  Scenarios", Theorems 1-3).
- **51% Bitcoin hashrate attack**: economically self-defeating; the whitepaper's cost analysis was
  refreshed with July 2026 market data (`dd349d7`). The residual assumption - a reorg deeper than
  the confirmation depth - is now additionally covered by the 1.3 recovery path.
- **Fork-tree spam**: every submitted header must carry valid PoW, difficulty, and timestamps;
  bloating the tree requires real mining work, and capacity is bounded (1.5).
- **Time manipulation**: transaction validity intervals are constrained (`MaxValidityWindow`), and
  block timestamps are validated against median-time-past and the +2h future bound, so neither
  Cardano transaction timing nor Bitcoin timestamps can be used to age forks prematurely.
- **Watchtower censorship**: the oracle has no privileged submitter - anyone can submit headers,
  promotions, and TM confirmations without registration or bonds, and TM record minting is
  permissionless, gated only by on-chain linkage to the confirmed treasury chain (`4be9800`,
  `8b2b057`). A censoring watchtower can at most withhold its own transactions; a single honest
  party restores liveness (the protocol's 1-honest-party assumption). Value and staking-address
  preservation of the oracle UTxO are enforced on-chain and covered by tests.
- **Cardano network saturation**: all protocol deadlines are wall-clock based, sized in hours, and
  independent of Cardano throughput: the 200-minute challenge aging and the staleness gates use
  transaction validity bounds, so congestion delays but never corrupts state. Single-UTxO
  contention resolves by retry, and after an outage the oracle catches up in large batches (up to
  71 headers, roughly 12 hours of Bitcoin blocks, in a single transaction). Duplicate submissions
  race benignly: the loser's transaction is invalidated by the winner's UTxO spend, and the
  watchtower re-bases and retries.

## 3. Known limitations tracked for the next iterations

- The generic `TransactionVerifierValidator` (a standalone dApp-facing helper, not used by the
  bridge's own paths) does not yet bind Bitcoin merkle proofs to the block's transaction count;
  SPV-style hardening is planned before it is advertised for third-party use. Bridge peg-in and TM
  confirmation are not exposed: they fully parse the raw transaction, which a forged 64-byte node
  cannot satisfy.
- The peg-out cancel/refund path (`PegOutNotProducedVerifier`) is explicitly out of scope for this
  iteration and hard-fails; peg-outs are safe on the happy path only until the exclusion-proof
  design lands.
- `testingMode` (PoW bypass for regtest harnesses) remains a compile-time parameter; it is baked
  into the script hash and therefore externally auditable, but removal from mainnet-reachable code
  is planned.
- SPO fault-proof verification and banning economics are handled in Heimdall; see the Milestone 4
  ban and federation-reset experiment reports there.
