# Deploying Binocular to a NixOS box

One systemd service, `binocular-bridge-v2`, runs the `watchtower` command: oracle sync, TM relay,
TM confirm and the proof API as four supervised workers in a single JVM. Beside it run a local
testnet4 `bitcoind` and (optionally) Dolos. The fat jar, config, and secrets live in
`/var/lib/binocular` (out of the Nix store); only the service/bitcoind/JRE definitions are
declarative.

Two Nix modules, both imported:

| Module | Provides |
| --- | --- |
| `deploy/nixos/binocular-bridge-v2.nix` | the `binocular-bridge-v2` service |
| `deploy/nixos/binocular-watchtower.nix` | the `binocular` user, `/var/lib/binocular`, testnet4 `bitcoind`, and the retired v1 unit |

## One-time setup on the box

1. Add both modules to your host's NixOS configuration:

   ```nix
   imports = [
     /path/to/binocular/deploy/nixos/binocular-watchtower.nix
     /path/to/binocular/deploy/nixos/binocular-bridge-v2.nix
   ];
   # Keeps the user, /var/lib/binocular and bitcoind; does NOT define the v1 unit.
   services.binocular-watchtower.enable = true;
   services.binocular-watchtower.runWatchtower = false;
   services.binocular-bridge-v2.enable = true;
   ```

   Then `sudo nixos-rebuild switch`. The service will fail to start until the jar + config +
   secrets are present — that's expected.

2. Install the secrets file (never enters git or the Nix store):

   ```bash
   cp deploy/secrets.env.example secrets.env   # edit WALLET_MNEMONIC + BLOCKFROST_PROJECT_ID
   scp secrets.env user@host:/tmp/secrets.env
   ssh user@host 'sudo install -o binocular -g binocular -m 600 /tmp/secrets.env /var/lib/binocular/secrets.env && rm /tmp/secrets.env'
   ```

3. First deploy (jar + config):

   ```bash
   deploy/deploy.sh user@host --v2 --with-config
   ```

## Routine deploys (new jar only)

```bash
deploy/deploy.sh user@host --v2
```

Builds the jar (`sbt assembly`), copies it to `/var/lib/binocular/binocular-v2.jar`, and restarts
the service. No `nixos-rebuild` needed — that's only for changes to the service/bitcoind/JRE.

`--no-restart` stages the jar without bouncing the service, which is how you pre-flight a build
before committing to it.

## Watching logs

```bash
ssh user@host 'journalctl -fu binocular-bridge-v2 -o cat'
```

Output is plain and one-line-per-event: the Console auto-detects the non-TTY journal, drops ANSI
colors, tags each line with its worker (`[oracle]` / `[relay]` / `[confirm]` / `[proofs]`), and
emits the polling heartbeat only when it changes. `-o cat` strips journald's own metadata prefix.

All four tags appearing within a poll interval is the health check that the supervisor started
everything.

## Retiring and restoring v1

The 2026-07-17 demo (`binocular-watchtower`, `binocular.jar`, `application-preprod.conf`, built
from branch `deploy/preprod-tm-control-e15a472b`) is retired. It is off, not deleted: its jar and
config are untouched in `/var/lib/binocular`, and `runWatchtower = false` is the only thing keeping
its unit undefined.

To roll back to v1: set `runWatchtower = true`, `services.binocular-bridge-v2.enable = false`,
`sudo nixos-rebuild switch`, then `sudo systemctl start binocular-watchtower`. `bitcoind` is
unaffected in either direction — it lives outside both switches.

Never run both at once. Both write the same oracle UTxO with the same wallet, and only one process
may do that.

## Notes

- **bitcoind runs with `txindex=1`** (a full, non-pruned node) — required by `confirm-tmtx`'s
  `getrawtransaction(txid)`. RPC (48332) and P2P (48333) are bound to `127.0.0.1` only (defense in
  depth on top of the host firewall). `txindex` and `prune` are mutually exclusive; switching a
  previously pruned datadir to `txindex` needs a full reindex/re-sync (wipe the chain dir).
- **testnet4 module support.** Verify your `nixpkgs` `services.bitcoind` accepts the `testnet4`
  chain; the module passes it through `extraConfig`. Confirm the bundled Bitcoin Core version
  supports testnet4.
- **RPC credentials.** The configs use `bitcoin`/`bitcoin` for the local node. Set matching
  credentials in the bitcoind service (or switch both to a `rpcpasswordfile`).
- **The proof API is not firewalled — deliberately.** `ProofServer` binds `0.0.0.0` with no option
  to narrow it, so `bridge.proof-server-port` (9061) MUST stay out of
  `networking.firewall.allowedTCPPorts`. The firewall is the only thing keeping the API off the
  public internet; Caddy reaches it over loopback, which the firewall does not filter.
- **Oracle must already be initialized.** The watchtower syncs/relays/confirms against the oracle
  named by `oracle.tx-out-ref`; it does not `init`. `oracle.script-hash` is verified against the
  derived hash at startup, so a jar whose pinned blueprint moved fails loudly instead of watching
  an oracle that has no UTxO.
- **Contracts come from the pinned blueprints**, not from a fresh compile: the runtime always loads
  `META-INF/scalus/blueprints` out of the jar. Regenerate deliberately with `sbt blueprintPin` —
  that commit IS the decision to deploy a changed script.
- **Exit code 3 is deliberate.** It means the watchtower found unrecoverable state (a deep reorg
  orphaned the oracle's confirmed history). `RestartPreventExitStatus = 3` leaves the service
  stopped, because restarting would only re-detect it. Fix the state, then `systemctl start`.
- **No wallet coordination.** The four workers run independently and share the sponsor wallet; if
  the oracle-update and confirm loops briefly pick the same UTxO, the losing tx fails and is
  retried by that loop. Expect occasional `UtxoNotAvailable` / `CollateralContainsNonADA` alerts.
  This is expected and self-healing — do not page on isolated occurrences.
- **The POR sweeper spends, and earns.** After each TM Confirm the confirm loop completes every PAID
  PegOutRequest: it burns the locked fBTC against a membership proof and keeps the request's
  MIN_ADA. That is the protocol's cleanup incentive, and it is on by default
  (`bridge.por-sweeper = false` turns it off). Each completion is a separate transaction, submitted
  one at a time so they do not race each other for wallet UTxOs.
- **The sweeper keeps state.** `bridge.state-dir` (`/var/lib/binocular-v2`, created by the unit's
  `StateDirectory`) holds `cpo-trie.json`, the local mirror of the completed-peg-outs trie. Losing
  it is recoverable — the sweeper rebuilds it from chain history — but reconstruction reads the full
  transaction history of two addresses, so put it on durable storage. It must never be shared with
  another bridge's mirror. Grep the log for `sweeper: HALTING`: that means the mirror could not be
  reconciled with the on-chain root and no completion will be submitted until an operator looks.
  Confirming is unaffected.
