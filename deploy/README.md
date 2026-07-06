# Deploying the Binocular watchtower to a NixOS box

Runs the oracle sync, TM relay, and TM confirm daemons together as one systemd service
(`binocular-watchtower`), alongside a local testnet4 `bitcoind`. The fat jar, config, and secrets
live in `/var/lib/binocular` (out of the Nix store); only the service/bitcoind/JRE definitions are
declarative.

## One-time setup on the box

1. Add the module to your host's NixOS configuration:

   ```nix
   imports = [ /path/to/binocular/deploy/nixos/binocular-watchtower.nix ];
   services.binocular-watchtower.enable = true;
   ```

   Then `sudo nixos-rebuild switch`. This creates the `binocular` user, `/var/lib/binocular`,
   the `binocular-watchtower` service, and a testnet4 `bitcoind` (RPC `127.0.0.1:48332`,
   `txindex=1`). The service will fail to start until the jar + config + secrets are present —
   that's expected.

2. Install the secrets file (never enters git or the Nix store):

   ```bash
   cp deploy/secrets.env.example secrets.env   # edit WALLET_MNEMONIC + BLOCKFROST_PROJECT_ID
   scp secrets.env user@host:/tmp/secrets.env
   ssh user@host 'sudo install -o binocular -g binocular -m 600 /tmp/secrets.env /var/lib/binocular/secrets.env && rm /tmp/secrets.env'
   ```

3. First deploy (jar + config):

   ```bash
   deploy/deploy.sh user@host --with-config
   ```

## Routine deploys (new jar only)

```bash
deploy/deploy.sh user@host
```

Builds the jar (`sbt assembly`), copies it to `/var/lib/binocular/binocular.jar`, and restarts the
service. No `nixos-rebuild` needed — that's only for changes to the service/bitcoind/JRE.

## Watching logs

```bash
ssh user@host 'journalctl -fu binocular-watchtower -o cat'
```

Output is plain and one-line-per-event: the Console auto-detects the non-TTY journal, drops ANSI
colors, tags each line with its daemon (`[oracle]` / `[relay]` / `[confirm]`), and emits the
polling heartbeat only when it changes. `-o cat` strips journald's own metadata prefix.

## Notes

- **bitcoind runs with `txindex=1`** (a full, non-pruned node) — required by `confirm-tmtx`'s
  `getrawtransaction(txid)`. RPC (48332) and P2P (48333) are bound to `127.0.0.1` only (defense in
  depth on top of the host firewall). `txindex` and `prune` are mutually exclusive; switching a
  previously pruned datadir to `txindex` needs a full reindex/re-sync (wipe the chain dir).
- **testnet4 module support.** Verify your `nixpkgs` `services.bitcoind` accepts the `testnet4`
  chain; the module passes it through `extraConfig`. Confirm the bundled Bitcoin Core version
  supports testnet4.
- **RPC credentials.** `application-preprod.conf` uses `bitcoin`/`bitcoin` for the local node. Set
  matching credentials in the bitcoind service (or switch both to a `rpcpasswordfile`).
- **Oracle must already be initialized.** The watchtower syncs/relays/confirms against the oracle
  named by `oracle.tx-out-ref`; it does not `init`.
- **No wallet coordination.** The three loops run independently and share the sponsor wallet; if the
  oracle-update and confirm loops briefly pick the same UTxO, the losing tx fails and is retried by
  that loop. This is expected and self-healing.
