# Operating binocular for a Bifrost bridge

This guide is for the people who **run a bridge**: whoever deploys it, holds its Config's update
key, and runs its watchtower. It covers standing a bridge up, running binocular beside it, and
changing its Config afterwards.

It is not for stake pool operators. An SPO runs heimdall, and its guide is
[heimdall's operator guide](https://github.com/lantr-io/heimdall/blob/main/docs/operator-guide.md).
It is not for depositors either. The peg-in and peg-out commands binocular ships are there for
testing and demos. They are listed at the end.

---

## What binocular does for a bridge

| Role | Commands | Runs |
|---|---|---|
| **Oracle**: Bitcoin block headers on Cardano, which every peg-in proof and TM confirm reads | `init`, `run`, `update-oracle`, `verify-oracle` | continuously |
| **Watchtower**: relays signed treasury movements to Bitcoin, confirms them on Cardano, completes paid peg-outs, serves proofs | `watchtower` (or `relay`, `confirm-tmtx`, `serve-proofs` separately) | continuously |
| **Genesis**: mints the bridge's Config, the completion contracts, the treasury and the SPO registry and ban list | `deploy-bridge`, `deploy-script-refs`, `register-bridge-creds` | once |
| **Governance**: changes the Config in place | `update-config` | when you decide to |

heimdall covers the SPO side: the roster, distributed key generation, signing, and a registry
revision's registry and ban list. The two meet in the Config. binocular writes it at genesis and
updates it, and every heimdall reads it.

---

## Install and run

Build the jar and run it as described in the [README](../Readme.md#running-as-a-standalone-jar). The
NixOS service used for the preprod bridge, and how a new jar is deployed to it, are in
[deploy/README.md](../deploy/README.md).

Every command takes `--config <file>`. Settings are read in this order, highest first:
environment variables, the `--config` file, `application.conf`, `reference.conf`. The shipped
`reference.conf` documents every key and its environment variable.

Keep **one config file per bridge**. Everything in its `bridge` section describes one deployment,
and most of it is copied from what `deploy-bridge` printed for that deployment.

---

## The `bridge` section

The settings that identify the bridge:

| Key | What it is | Set from |
|---|---|---|
| `contracts` | the ft-bifrost-bridge contracts release the bridge was **deployed with**: `rev5.5` or `rev5.6` | your choice at genesis, then never changed |
| `config-nft-policy-id` | the Config NFT policy, which is the bridge's identity | `deploy-bridge` output |
| `completed-peg-ins-one-shot-ref`, `bridge-state-one-shot-ref` | the one-shot outpoint genesis spent (the same value under both keys) | `deploy-bridge` output |
| `bridged-token-policy-id` | the fBTC policy | `deploy-bridge` output |
| `plutus-json` | a blueprint file replacing the packaged one, for developing the validators | empty |

The genesis inputs (`y-federation-hex`, `federation-csv-blocks`, `pegin-refund-timeout-blocks`,
`ban-schedule`, `initial-btc-treasury-utxo`, `initial-btc-treasury-amount-sat`) are read by
`deploy-bridge` only. After genesis every node reads their published values from the Config, so
editing them in a file changes nothing.

### `contracts`

binocular carries two releases of the ft-bifrost-bridge contracts, and `contracts` says which one
**this bridge** was deployed with. It is a property of the bridge, not of the binocular version.

- **`rev5.5`** is what ft-bifrost-bridge `main` builds, and what the preprod bridge runs. It is
  the default, so a config file written before this key existed keeps working.
- **`rev5.6`** adds the single-use registration signatures and the `Migrate` branch that lets a
  running bridge revise its registry. Choose it for a new bridge that should run those contracts.

Of the scripts binocular derives, three differ between the releases:

- `config.ak`: same parameters, different code, so a different Config policy id.
- `spos_registry`: rev 5.6 adds the Config NFT policy as a parameter.
- `spo_bans`: its first parameter is the registry policy in rev 5.5 and the Config NFT policy in
  rev 5.6.

A rev 5.6 genesis also writes a Config with 14 fields rather than 13. Field #13 starts empty.

**It never changes on a running bridge, and a registry revision does not change it either.** A
revision replaces the registry and the ban list, which heimdall deploys. The Config, the treasury,
the peg scripts and the TM validator, the ones binocular spends, stay those of the release the
bridge was deployed with. A bridge deployed on `rev5.5` and revised afterwards keeps
`contracts = "rev5.5"`.

A wrong value does not go unnoticed. `update-config` derives the Config policy from the declared
release and refuses when it does not match `config-nft-policy-id`.

### `plutus-json`

Empty by default: binocular uses the blueprint packaged for `contracts`. Set it to a `plutus.json`
path only while developing the Aiken validators. A path that cannot be read is refused rather than
replaced by the packaged blueprint. Earlier versions defaulted to a sibling ft-bifrost-bridge
checkout and fell back silently, so the contracts a command used depended on which branch that
checkout was on.

---

## Standing a bridge up

Genesis is one binocular command, but it needs inputs from heimdall first, and heimdall's SPOs
need its outputs afterwards.

1. **Form the federation key.** The federation runs heimdall's ceremony, in the appendix
   "forming the initial federation" of heimdall's guide, or uses a single seed on a test bridge.
   `heimdall bootstrap-treasury` then prints the key and the genesis treasury address. Put the key
   in `bridge.y-federation-hex`.
2. **Fund the genesis treasury** on Bitcoin at that address, and put the outpoint and amount in
   `bridge.initial-btc-treasury-utxo` and `bridge.initial-btc-treasury-amount-sat`.
3. **Have a live oracle.** The TM validator and the peg-in validator are parameterized by the
   oracle's policy. For a new one, use `init`, then keep `run` (or `watchtower`) going.
4. **Choose `contracts`** and the genesis inputs: `federation-csv-blocks`,
   `pegin-refund-timeout-blocks` (it must exceed the CSV) and `ban-schedule`. These are published
   at genesis and cannot be changed later. The CSV and refund timeout are hashed into the Bitcoin
   addresses, and the ban schedule into the ban policy.
5. **Fund the sponsor wallet** with at least two clean, ADA-only UTxOs of 5 ADA or more. Genesis
   spends two one-shots in two transactions, because the registry, the ban list and the Config
   together exceed Cardano's 16 kB transaction limit.
6. **Deploy:**

   ```bash
   binocular --config bridge.conf deploy-bridge --dry-run
   binocular --config bridge.conf deploy-bridge
   ```

   The first transaction bootstraps the federation: the treasury state, the registry root and the
   ban root. The second mints the Config, the completed-peg-ins trie and the bridge-state
   singleton, and registers the peg-in and peg-out reward accounts. The command prints every value
   to copy into this config file, plus the Config NFT policy heimdall's SPOs configure.
7. **Publish the reference scripts:**

   ```bash
   binocular --config bridge.conf deploy-script-refs
   ```

   This publishes the heavy scripts once for everyone, so the completion transactions and the SPOs'
   registration transactions stay under the size limit. It skips whatever is already published.
   Run `register-bridge-creds` only if `deploy-bridge`'s second transaction stopped partway.
8. **Tell the SPOs the Config NFT policy.** From there they follow heimdall's guide.

---

## Running the watchtower

```bash
binocular --config bridge.conf watchtower
```

`watchtower` runs the oracle sync, the TM relay and the TM confirm in one process. It also serves
the proof API and, after each confirm, completes the peg-outs it paid. `bridge.por-sweeper` and
`bridge.proof-server` (port `proof-server-port`) turn those two off, and the `notifications`
section sends its events to a chat. Confirming needs `bridge-state-one-shot-ref`, and the watchtower refuses to start
without it.

Everything the watchtower does is permissionless. It needs a funded wallet for fees, not a key the
bridge trusts. Anyone may run one, and more than one is fine.

---

## Changing the Config

The Config is updated in place by `update-config`. The transaction must be signed by the Config's
`update_auth` key, which `deploy-bridge` set to `oracle.owner-pkh`, so run it with that wallet.

**Always dry-run first.** A dry run computes the new datum and prints every field that changes,
old and new, without submitting:

```bash
binocular --config bridge.conf update-config <options> --dry-run
```

Everything you name goes into **one** transaction. That matters when you swap a validator whose
dependents must flip at the same moment.

| Options | Fields | Notes |
|---|---|---|
| `--bridge-state-policy`, `--tm-script-hash`, `--peg-in-withdraw-hash`, `--peg-out-withdraw-hash` | the script hashes | a validator redeploy; swap its dependents in the same call |
| `--fee-rate`, `--per-pegout-fee`, `--min-peg-out` | operational parameters | every SPO's TM builder reads them from the next batch |
| `--schedule NAME=VALUE` (repeatable) | the epoch and TM schedule | checked against the spec's constraints; see below |
| `--spo-bans-policy` | #8, the ban list | moves when the registry does |
| `--migrate-registry-to`, `--end-registry-migration` | #9 and #13 | a registry revision; see below |

A schedule edit prints the batch grid before and after, and refuses a schedule that breaks a spec
constraint. Nothing on chain would refuse it, and the failure is silent: a bridge that idles for
days looks like one whose SPOs are down. `--allow-unsafe-schedule` publishes it anyway, for a test
bridge.

The ban schedule is not listed because it is not a setting you can move by itself. It is an input
to the ban policy id, so changing it means deploying a new ban list.

### A registry revision

A contracts release can replace the SPO registry, and with it the ban list. heimdall carries every
registration across with no cold key. The order is **upgrade first, switch later**:

1. Operators upgrade heimdall at their own pace. The new version runs the unrevised bridge exactly
   as the previous one does, so a roster that is half upgraded keeps holding ceremonies.
2. Once every node runs it, the federation deploys the new registry and ban list, and you make the
   governance Update below.
3. The nodes see the Update and carry themselves across, and the federation runs one command for
   any stragglers.
4. When everyone has crossed, you end the window.

binocular's part is the Update in the middle, and ending the window afterwards:

```bash
binocular --config bridge.conf update-config \
    --migrate-registry-to <new registry policy> --spo-bans-policy <new ban policy> --dry-run
# … every pool crosses …
binocular --config bridge.conf update-config --end-registry-migration --dry-run
```

`--migrate-registry-to` moves #9 and writes the registry #9 held into #13. #13 is taken from the
datum, not typed, so it cannot name the wrong list. The option requires `--spo-bans-policy` in the
same Update, and refuses while another migration is still recorded. `--end-registry-migration`
empties #13 and never removes the field.

The full runbook covers deploying the new registry and ban list, carrying the pools across, bans,
and checks. It is heimdall's appendix "carrying a bridge across a registry revision", because
heimdall does everything except the Update.

What changes for binocular: nothing in its config file. `contracts` stays. `deploy-script-refs`
and `register-bridge-creds` notice that the Config names a revised registry and skip the SPO half
with a message saying so, because heimdall publishes those reference scripts and registers that
credential. binocular reads a 14-field Config and writes it back intact.

---

## Upgrading binocular

For the NixOS service, `deploy/deploy.sh user@host --v2` builds, copies and restarts
([deploy/README.md](../deploy/README.md#routine-deploys-new-jar-only)). `--no-restart` stages the
jar first.

A binocular upgrade never changes `contracts`. A binocular that knows a newer contracts release
still runs a bridge deployed on an older one.

---

## When something is wrong

| You see | What it means |
|---|---|
| `Derived config policy … does not match bridge.config-nft-policy-id` | `contracts` is not the release the bridge was deployed with, or the one-shot ref is wrong. |
| `bridge.plutus-json = '…' is not readable` | The override points nowhere. Empty it to use the packaged blueprint. |
| `bridge.contracts = '…' is not a contracts release this binocular knows` | A typo, or a release newer than this binocular. |
| `federation half skipped — the Config names a revised registry …` from `deploy-script-refs` | Expected after a registry revision: heimdall publishes those scripts. |
| `derived treasury_info policy … does not match the Config's` | The one-shot this command derived from is not the bridge's, or `contracts` is wrong. |
| `a registry migration is already in progress` from `update-config` | End the current one with `--end-registry-migration` first. |
| `--migrate-registry-to needs --spo-bans-policy in the same Update` | The ban list moves with the registry. Pass both. |

---

## Testing and demo commands

These exist for testing a bridge end to end and for demos. None of them is needed to run one.

| Command | Does |
|---|---|
| `pegin-request`, `pegin-complete`, `sign-pegin-msg` | a peg-in, from the depositor's side |
| `peg-out-request`, `peg-out-complete` | a peg-out, and completing paid ones |
| `deposit-proof`, `spi-proof`, `serve-proofs` | the proofs a frontend fetches, one-off or as a server |
| `traffic-address` | the demo Bitcoin funding address |
| `tm-script` | the TM validator's policy id, address and CBOR |
| `bootstrap-bridge-state` | mints a replacement bridge-state singleton (recovery) |
| `migrate-script-refs` | moves reference scripts to the native holding address |
| `set-state`, `attack` | oracle recovery and adversarial testing |
