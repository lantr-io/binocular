# Adversarial Attack Demo — Eve vs. Alice

Demonstrates that the oracle's canonical chain is chosen by **chainwork**, not
block count or first-seen. Eve mines valid-PoW blocks committing to fabricated
transactions; Alice relays the real chain; Alice's chainwork wins.

## Prerequisites
- A running oracle on the target network (`binocular init` already done).
- Two wallets with funds: Alice's mnemonic in her config, Eve's *different*
  mnemonic in hers (so they pay their own fees and race the same oracle UTxO).

## Regtest (fast, deterministic)
Regtest `powLimit` (0x7fff…) makes mining instant, so the race resolves in
seconds. Because regtest blocks are equal-difficulty, construct Alice's
advantage by having her submit more/heavier real blocks than Eve.

```bash
# Terminal 1 — Alice (honest)
binocular --config application-regtest.conf run

# Terminal 2 — Eve (adversary), fork from the tip
binocular --config eve-regtest.conf attack --parent 0 --rogue-sprint 6
```

Watch each loop's log line: Eve reports `branch chainwork=…`; Alice reports tip
height. Confirm the oracle's `bestChainTipHash` (shown in the fork-tree dump)
follows Alice once her chainwork exceeds Eve's.

Try deeper forks: `attack --parent 50` makes Eve fork 50 blocks back; the
honest branch should still win selection.

## Testnet4 (authentic, real time)
On testnet4 Eve's blocks are difficulty-1 (min-difficulty rule); the real chain
generally carries higher difficulty, so Alice's chainwork dominates over time.
Eve front-loads ~6 blocks (the +2h window), then ~1 block / 20 min.

```bash
# Terminal 1 — Alice
binocular --config application-testnet4.conf run

# Terminal 2 — Eve
binocular --config eve-testnet4.conf attack --parent 0
```

Observe: Eve briefly leads on block COUNT; Alice's branch chainwork overtakes;
the oracle best tip flips to Alice and stays there. Eve's valid-PoW rogue blocks
remain in the fork tree, unpromoted, and are eventually pruned.

## What this proves / does not prove
- PROVES: valid-PoW blocks with fabricated transactions are accepted into the
  fork tree (the validator only checks headers), yet lose the chainwork race.
- DOES NOT prove promotion: confirming a chain needs 100 confirmations + 200-min
  aging (hours). Success here is best-chain *selection*, not promotion.
