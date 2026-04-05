# BitcoinValidator Security & Optimization Analysis

**Date:** 2026-04-04
**Scope:** `src/main/scala/binocular/BitcoinValidator.scala`, `src/main/scala/binocular/BitcoinHelpers.scala`
**Focus:** Security vulnerabilities + fee optimization for Bifrost scenario (100 blocks in tree, 1 header addition, 1 promotion)

---

## Security Review

### Methodology

Analyzed the `@Compile` annotated `BitcoinValidator` (extends `DataParameterizedValidator`) against the Scalus smart contract security vulnerability checklist. For each potential vulnerability, traced concrete attack transactions through the validator logic.

### Findings

#### No Critical or High Vulnerabilities Found

| ID | Check | Result | Evidence |
|----|-------|--------|----------|
| V001 | Redirect Attack | **SAFE** | `BitcoinValidator.scala:1089` -- `out.address.toData == ownInput.address.toData` checks full address including staking credential |
| V002 | Token/NFT Not Verified | **SAFE** | `BitcoinValidator.scala:1090` -- `quantityOf(policyId, ByteString.empty) == BigInt(1)` + line 1096 `withoutLovelace.toData ==` preserves all non-ADA tokens |
| V003 | Inexact Burn/Mint | **SAFE** | `BitcoinValidator.scala:1142` and `1163` -- exact equality against `SortedMap.singleton(...)`, no `>=` or `<=` |
| V004 | Integer Overflow | **N/A** | UPLC uses arbitrary-precision `BigInt` |
| V005 | Double Satisfaction | **SAFE** | One-shot minting policy (line 1144-1146) guarantees a single NFT. `outputs.find` with NFT+address check finds at most one matching output |
| V010 | Other Redeemer | **SAFE** | Scalus compiler adds default `fail` for unimplemented purposes. Both `spend` and `mint` are implemented with independent validation |
| V011 | Other Token Name | **SAFE** | `BitcoinValidator.scala:1142` -- checks entire `tx.mint.tokens(policyId).toData` against a singleton map, rejecting any extra token names |
| V012 | Missing UTxO Auth | **SAFE** | NFT at `policyId` (= script hash) authenticates the oracle UTxO; one-shot minting prevents forgery |
| V013 | Time Handling | **SAFE** | `BitcoinValidator.scala:1053-1056` -- both `validRange.from` and `.to` must be finite; window bounded by `MaxValidityWindow` (10 min) |
| V014 | Missing Signature | **SAFE** | `UpdateOracle` is permissionless by design; `CloseOracle` requires `isSignedBy(params.owner)` at line 1121 |
| V015 | Datum Mutation | **SAFE** | `BitcoinValidator.scala:1107-1112` -- output datum must match deterministically computed `expectedOutputDatum` |
| V016 | Staking Control | **SAFE** | Full address comparison at line 1089 includes staking credential; confirmed by test at line 1305 |
| V019 | Unbounded Datum | **SAFE** | `BitcoinValidator.scala:996-999` -- `forkTreeBlockCount(newTree) <= params.maxBlocksInForkTree` bounds tree growth |
| V024 | Parameterization | **SAFE** | Params affect script hash; NFT policyId = script hash acts as auth token for legitimate instances |

#### Informational Findings

**I-01: `testingMode` bypasses PoW validation**

**Location:** `BitcoinValidator.scala:352-354`

```scala
if !params.testingMode then
    require(hashInt <= target, "Invalid proof-of-work")
    require(target <= params.powLimit, "Target exceeds PowLimit")
```

Since `testingMode` is part of `BitcoinValidatorParams` which affects the script hash, a `testingMode=true` deployment produces a different script (different address, different NFT policy). Not exploitable, but worth a deployment checklist item to ensure production always uses `testingMode = false`.

**I-02 (V021): Single-UTxO contention**

The oracle is a single-UTxO design. Concurrent submissions compete for the same UTxO. This is a known design trade-off documented in the whitepaper. Mitigation is through the permissionless design (anyone can resubmit after a failed attempt).

### Security Grade: **A**

The contract demonstrates strong security hygiene: exact value checks, full address comparison, NFT authentication, bounded validity intervals, deterministic datum validation, and bounded datum growth.

---

## Optimization Review: Bifrost Scenario

**Scenario:** 100 blocks in fork tree, add 1 block header, promote 1 block.

### Execution Flow Analysis

Tracing `computeUpdate` for the Bifrost scenario:

| Phase | Function | Operations | Dominant Cost |
|-------|----------|------------|---------------|
| 1. Insert | `validateAndInsertInPath` loop | 100 `accumulateBlock` calls + list reverse + list append | **~45% of budget** |
| 1. Insert | `validateBlock` | 1 double-SHA256, 1 `getNextWorkRequired`, 1 `compactBitsToTarget`, `insertionSort(11)` | ~10% |
| 1. Insert | `forkTreeBlockCount` | O(101) list length traversal | ~5% |
| 2. Best chain | `bestChainPath` | O(101) `blocks.length` | ~5% |
| 3. Promote+GC | `splitPromotable` | 1-2 iterations | ~1% |
| 3. Promote+GC | `computeChainwork` (1 block) | 1 iteration | ~1% |
| 4. Apply | `applyPromotions` (1 MPF insert) | 1 `accumulateBlock` + MPF insert | ~15% |
| 4. Apply | Datum comparison | `equalsData` on output datum | ~5% |

**The 100-block ctx replay in `validateAndInsertInPath` is the dominant cost.** Each of the 100 iterations performs: BigInt addition, BigInt modulo (height % 2016), Cons construction, TraversalCtx case class copy, and prefix list building.

### Findings

#### [HIGH] O-01: Add `blockCount` field to `Blocks` node

**Location:** `BitcoinValidator.scala:73`
**Estimated savings:** ~10% steps (eliminates two O(n) list traversals)

`forkTreeBlockCount` (line 747) and `bestChainPath` (line 728) both call `blocks.length`, which is O(n) on a linked list. For 101 blocks, that's 202 redundant list traversals.

**Current code** (`BitcoinValidator.scala:73,728,747`):
```scala
case Blocks(blocks: NonEmptyList[BlockSummary], chainwork: Chainwork, next: ForkTree)
// ...
case Blocks(blocks, cw, next) => blocks.length + forkTreeBlockCount(next)  // O(n)
// ...
case Blocks(blocks, cw, next) => bestChainPath(next, height + blocks.length, ...)  // O(n)
```

**Optimized code:**
```scala
case Blocks(blocks: NonEmptyList[BlockSummary], chainwork: Chainwork, blockCount: BigInt, next: ForkTree)
// ...
case Blocks(_, _, count, next) => count + forkTreeBlockCount(next)  // O(1)
// ...
case Blocks(_, cw, count, next) => bestChainPath(next, height + count, ...)  // O(1)
```

**Trade-off:** +8 bytes per `Blocks` node in datum. For a single linear chain, negligible.

**Note:** Every place that creates a `Blocks` node must set `blockCount` correctly: `validateAndInsert` (insert), `promoteAndGC` (split/promote), `computeChainwork`, etc. The `blockCount` can be computed as `blocks.length` at creation time (paid once) and then propagated via arithmetic for splits (`prefixCount`, `originalCount - prefixCount`).

---

#### [HIGH] O-02: Store `tipCtx` in `Blocks` node to eliminate ctx replay

**Location:** `BitcoinValidator.scala:545-658`
**Estimated savings:** ~30-40% steps for Bifrost scenario

The `validateAndInsertInPath` loop walks ALL blocks to build the traversal context. For `path=[99]` (append at tip of 100-block chain), this replays 100 `accumulateBlock` calls -- each with BigInt modulo, BigInt addition, Cons, and case class construction.

**Current architecture:** Only `ChainState.ctx` (ctx at confirmed tip) is stored. The ctx at the fork tree tip must be recomputed every time.

**Proposed:** Add `tipCtx: TraversalCtx` to the `Blocks` node:
```scala
case Blocks(
    blocks: NonEmptyList[BlockSummary],
    chainwork: Chainwork,
    blockCount: BigInt,
    tipCtx: TraversalCtx,  // ctx after accumulating all blocks in this segment
    next: ForkTree
)
```

For **append at tip** (pathHead = blockCount - 1, tail = Nil, next = End):
- Use `tipCtx` directly as the parent ctx
- Skip the 100-iteration loop entirely
- Just: validate 1 header, create `Blocks(blocks ++ newBlocks, cw + newCw, count + 1, newTipCtx, End)`

For **mid-split** or **fork-at-end**: still replay from `ChainState.ctx` (fallback to current behavior).

**Trade-off:** +~140 bytes per `Blocks` node in datum (11 timestamps x ~8 bytes + height + bits + prevDiffAdj + lastBlockHash). For a single linear chain, this is one-time overhead.

**Impact in Bifrost:** Reduces the dominant cost phase from O(100) `accumulateBlock` calls to O(1) lookup. This is the single biggest optimization opportunity.

---

#### [MEDIUM] O-03: Skip prefix list construction for append-at-tip

**Location:** `BitcoinValidator.scala:545-658`
**Estimated savings:** ~5% steps

Even without O-02, the append-at-tip case reconstructs the block list unnecessarily. The loop builds a reversed `prefix`, then reverses it: `Cons(block, prefix).reverse`. Then appends: `fullPrefix ++ newBlocks`.

For the append case (last block, tail=Nil, next=End), the result `fullPrefix ++ newBlocks` equals `blocks ++ newBlocks`. The 100-element reverse + 100-element append could be replaced with a single 100-element append.

**Current code** (`BitcoinValidator.scala:581-593`):
```scala
val fullPrefix = Cons(block, prefix).reverse  // O(100) reverse
// ...
Blocks(fullPrefix ++ newBlocks, originalCw + newCw, End)  // O(100) append
```

If `blockCount` (from O-01) is available:
```scala
// Fast path: append at tip
if pathHead == blockCount - 1 && pathTail.isEmpty then
    val tipCtx = blocks.foldLeft(ctx)((c, b) => accumulateBlock(c, b, params.powLimit))
    val (newBlocks, newCw) = validateAndCollectBlocks(headers, tipCtx, currentTime, 0, Nil, params)
    Blocks(blocks ++ newBlocks, originalCw + newCw, blockCount + newBlocks.length, End)
```

Saves the prefix construction (100 Cons operations in the loop) and the reverse (100 operations). Still pays the foldLeft for ctx (unless combined with O-02).

---

#### [LOW] O-04: Use `targetToCompactBitsV2` in `targetToCompactByteString`

**Location:** `BitcoinHelpers.scala:167`
**Estimated savings:** Saves ~10-30 steps at each retarget boundary (every 2016 blocks)

**Current code** (`BitcoinHelpers.scala:166-169`):
```scala
def targetToCompactByteString(target: BigInt): CompactBits = {
    val compact = targetToCompactBits(target)  // uses recursive loop
    integerToByteString(false, 4, compact)
}
```

**Optimized code:**
```scala
def targetToCompactByteString(target: BigInt): CompactBits = {
    val compact = targetToCompactBitsV2(target)  // uses findFirstSetBit builtin
    integerToByteString(false, 4, compact)
}
```

`targetToCompactBitsV2` replaces the recursive `findSignificantBytes` loop (up to 32 iterations) with a single `findFirstSetBit` builtin. Trivial change, no risk.

---

#### [LOW] O-05: `insertionSort` on 11 timestamps

**Location:** `BitcoinValidator.scala:299-300,357`
**Estimated savings:** Negligible (~1-2%)

`insertionSort` is O(n^2) = 121 comparisons for 11 elements. Called once per `validateBlock`. For the Bifrost scenario (1 new header), this is 121 comparisons total. Not worth optimizing -- the constant is small and it's called only once.

---

### Summary

| ID | Impact | Finding | Est. Savings | Complexity |
|----|--------|---------|-------------|------------|
| O-01 | HIGH | Add `blockCount` to `Blocks` node | ~10% steps | Medium (data structure change) |
| O-02 | HIGH | Store `tipCtx` in `Blocks` for tip-append O(1) | ~30-40% steps | High (data structure + all insert/promote paths) |
| O-03 | MEDIUM | Skip prefix reconstruction for append-at-tip | ~5% steps | Low (conditional fast path) |
| O-04 | LOW | Use `targetToCompactBitsV2` | Saves at retarget | Trivial (1-line change) |
| O-05 | LOW | `insertionSort` on 11 elements | ~1-2% | Not worth changing |

**Recommended implementation order:**
1. O-04 (trivial, zero risk)
2. O-01 (moderate change, clear savings)
3. O-03 (requires O-01, moderate savings)
4. O-02 (highest savings but most invasive -- consider after benchmarking O-01+O-03)

### Fee Estimates

**Current Bifrost fee:** 1.053147 ADA (from test assertion at `BitcoinValidatorTest.scala:1228`)

| Optimization Set | Estimated Fee | Reduction |
|-----------------|---------------|-----------|
| Current | 1.053 ADA | -- |
| O-01 + O-03 | ~0.90-0.95 ADA | ~10-15% |
| O-01 + O-02 + O-03 | ~0.65-0.75 ADA | ~30-35% |

---

## O-02 Implementation Report: Store `tipCtx` in `Blocks` Node

**Date:** 2026-04-04
**Status:** REJECTED — unacceptable security regression

### Implementation Summary

Added `tipCtx: TraversalCtx` field to `ForkTree.Blocks` to cache the traversal context at the tip
of each segment. This eliminates the O(n) `accumulateBlock` replay in `validateAndInsertInPath`
(append-at-tip fast path) and `promoteAndGC` (no-promotion path), replacing them with O(1) lookups.

**Files modified:** 8 files, +271/-189 lines

| File | Changes |
|------|---------|
| `BitcoinValidator.scala` | Added `tipCtx` field, `trimTipCtx` helper, fast paths in `validateAndInsertInPath`, O(1) `promoteAndGC` |
| `ForkTreePretty.scala` | Updated 3 pattern match sites |
| `BitcoinContract.scala` | Reduced `DefaultMaxBlocksInForkTree` 256 → 64 |
| `OracleConfig.scala` | Updated default to match |
| `BitcoinValidatorTest.scala` | Updated constructors, assertions |
| `BitcoinValidatorGenerators.scala` | Added `dummyTipCtx`, updated generators |
| `ForkTreePropertyTest.scala` | Updated constructors, pattern matches |
| `BitcoinMainnetCekTest.scala` | Updated pattern matches |

### Performance Results

| Metric | Before | After | Change |
|--------|--------|-------|--------|
| **Bifrost fee** | 1.052 ADA | 0.871 ADA | **-17.3%** |
| Bifrost CPU steps | ~1,190,000,000 | 1,042,483,603 | -12.4% |
| Bifrost CPU % of limit | ~11.9% | 10.4% | |
| Bifrost memory | ~4,500,000 | 3,931,805 | -12.6% |
| Header throughput | 51 headers/tx | 51 headers/tx | unchanged |
| Promotion throughput | 23 headers+promotions/tx | 23 headers+promotions/tx | unchanged |
| Contract size | 7,539 bytes | 8,800 bytes | +16.7% |

### Capacity Impact

| Metric | Before | After | Change |
|--------|--------|-------|--------|
| **Single branch (linear)** | 368 blocks | 366 blocks | -0.5% |
| **Balanced tree (griefing)** | **256 blocks** | **64 blocks** | **-75%** |
| Left-leaning forks | 274 | 95 | -65% |
| Balanced tree depth | 8 | 6 | -25% |

### Security Analysis: Griefing Attack

The `tipCtx` overhead (~180 bytes per `Blocks` node) has a negligible effect on the normal case
(linear chain: 1 `Blocks` node, -2 blocks) but a catastrophic effect on the adversarial case
(balanced tree: 2^d `Blocks` nodes, each paying the full overhead).

**Griefing attack model:** An attacker fills the fork tree with single-block branches, all below
`maturationConfirmations` (100), so nothing qualifies for promotion. The oracle is halted until
blocks age out or are displaced.

- **Before:** Attacker must mine **256 valid Bitcoin blocks** to fill the tree
- **After:** Attacker must mine only **64 valid Bitcoin blocks** — a **4x cost reduction**

The `maxBlocksInForkTree` parameter must be ≤ balanced tree capacity (the worst-case shape an
attacker can construct). Reducing it from 256 to 64 fundamentally weakens the protocol's resistance
to griefing attacks.

### Why the Asymmetry?

In a linear chain (normal operation), there is **1 `Blocks` node** storing 1 `tipCtx` — the
overhead is constant (~180 bytes total). In a balanced griefing tree with depth d, there are
**2^d `Blocks` nodes**, each storing its own `tipCtx` — the overhead scales as O(2^d × 180 bytes),
consuming space that would otherwise hold blocks.

This is a fundamental trade-off: per-node caching helps the normal case but punishes the adversarial
case proportionally to the number of nodes.

### Conclusion

**The O-02 optimization is rejected.** While it achieves a meaningful 17% fee reduction for the
Bifrost scenario, it reduces griefing attack resistance by 75% (256 → 64 blocks). This is an
unacceptable security regression that violates the protocol's design assumptions.

The performance gain does not justify weakening the security model. Alternative approaches should
be explored:

- **O-01 + O-03** (add `blockCount` field + skip prefix reconstruction) offer ~10-15% savings
  with minimal capacity impact (~8 bytes per node vs ~180 bytes)
- A hybrid approach storing `tipCtx` only at the deepest tip node (not all nodes) could preserve
  security while capturing most of the benefit, but adds significant complexity
