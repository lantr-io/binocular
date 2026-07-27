# Bifrost Milestone 4 – Smart Contract Performance Report

**Date:** 2026-07-27
**Scope:** CPU / memory / fee improvements in the Binocular oracle and Bifrost treasury movement
contracts after upgrading Scalus from `0.18.2` to `0.18.2+99-94cc447a-SNAPSHOT` and retargeting
code generation to the vanRossem hard fork (major protocol version 11).

## Methodology

All numbers are produced by the test suite and printed to the CI log on every push, so they are
independently reproducible from the GitHub Actions links below.

- **Oracle flows** (`BitcoinValidatorTest`): real transactions built with the Scalus tx builder and
  evaluated with the Plutus V3 CEK machine against Cardano mainnet protocol parameters. Reported:
  execution units (CPU steps, memory), execution-unit fee, full transaction fee, and per-transaction
  throughput (how many Bitcoin headers fit in one oracle update).
- **Treasury movement flows** (`TreasuryMovementValidatorTest`, test
  `Treasury movement budgets`): the three happy paths (mint Genesis, mint Chain, Confirm spend)
  evaluated on synthetic script contexts. Reported: execution units and the execution-unit fee
  (no full tx fee, since no complete transaction is built).

**Before:** Scalus `0.18.2`, oracle codegen targeting plominPV (protocol version 10).
**After:** Scalus `0.18.2+99-94cc447a-SNAPSHOT`, oracle codegen targeting vanRossemPV (protocol
version 11), which lets the compiler use the vanRossem builtin set and improved code generation.

## Results

### Oracle: Bifrost scenario (100 blocks in fork tree, add 1 header, promote 1 block)

The canonical block addition + confirmation flow a watchtower executes continuously.

| Metric | Before | After | Improvement |
|---|---:|---:|---:|
| CPU steps | 1,538,975,636 | 1,143,597,942 | **-25.7%** |
| Memory units | 5,123,053 | 4,220,634 | **-17.6%** |
| Execution fee | 0.4066 ADA | 0.3260 ADA | **-19.8%** |
| Full tx fee | 0.9581 ADA | 0.8575 ADA | **-10.5%** |
| Script size | 8,754 B | 7,387 B | **-15.6%** |

### Oracle: throughput per transaction

| Metric | Before | After | Improvement |
|---|---:|---:|---:|
| Max headers per update tx | 55 | 71 | **+29.1%** |
| CPU steps per header (at max batch) | 85.5M | 58.2M | **-31.9%** |
| Execution fee per header | 0.0234 ADA | 0.0175 ADA | **-25.3%** |
| Max headers + promotions per tx | 21 | 28 | **+33.3%** |

Batch size remains bound by the per-transaction memory limit (≥ 98% utilized at max batch), so
the memory savings translate directly into larger batches per transaction.

### Treasury movement flows

| Flow | CPU steps before → after | Memory before → after | Ex fee before → after |
|---|---:|---:|---:|
| Mint Genesis | 59,061,706 → 40,303,734 (**-31.8%**) | 205,178 → 153,807 (**-25.0%**) | 0.0161 → 0.0118 ADA (**-26.8%**) |
| Mint Chain | 61,016,507 → 41,599,719 (**-31.8%**) | 214,329 → 160,370 (**-25.2%**) | 0.0168 → 0.0123 ADA (**-26.9%**) |
| Confirm spend | 115,931,016 → 94,963,439 (**-18.1%**) | 373,789 → 312,861 (**-16.3%**) | 0.0299 → 0.0249 ADA (**-16.8%**) |

## Summary

The upgrade delivers the expected ~20% class of improvement across both contract families:
CPU -18% to -32%, memory -16% to -25%, execution fees -17% to -27% per flow, and oracle
throughput +29% to +33% headers per transaction. The gains come from Scalus compiler code
generation targeting the vanRossem protocol version; the on-chain cost model is unchanged between
the two measurements (mainnet parameters in both runs).

## Evidence (GitHub Actions)

CI runs `sbt "testOnly binocular.*"` on every push; the budget tables appear in the test step log
under `BitcoinValidatorTest` and `TreasuryMovementValidatorTest`.

- **Before** (Scalus 0.18.2, commit `4f0afbc`):
  https://github.com/lantr-io/binocular/actions/runs/30261189558
- **After** (Scalus 0.18.2+99-94cc447a-SNAPSHOT + vanRossemPV, commit on this branch):
  link added after the CI run of the upgrade commit completes.

## Deployment note

Retargeting codegen changes the compiled scripts: the oracle blueprint hash moved from
`86cc8af8f92e1fc9ff6ed60902d0215fb5ab633b41bb55e3a9f0e33a` to
`7c222f96ae82aef4a550783ec3d495b34dfc2b470121298826150ebe` (all six pinned CIP-57 blueprints were
re-pinned via `sbt blueprintPin`). Deployed instances pin `oracle.script-hash` in their config, so
rolling this out requires re-initializing the oracle UTxO with the new script and updating the
watchtower config; existing deployments keep working on the old script until then.
