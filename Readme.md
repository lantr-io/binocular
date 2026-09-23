# Binocular -- a decentralized Bitcoin Oracle on Cardano

Binocular enables Cardano smart contracts to access and verify Bitcoin blockchain state. Anyone can
submit Bitcoin block headers to a single on-chain Oracle UTxO without registration or bonding.
All blocks are validated against Bitcoin consensus rules (proof-of-work, difficulty adjustment,
timestamp constraints) enforced by a Plutus smart contract written in [Scalus](https://scalus.org).

## How to build

Using Nix with flakes and/or direnv is recommended.

    nix develop

or

    direnv allow

Then build with sbt:

    sbt compile
    sbt test

## Running as a standalone JAR

Build a self-contained fat jar that bundles all dependencies, so you can run the oracle `run`
daemon, the Bifrost `relay` watchtower, and every other CLI command with plain `java` — no sbt
required:

    sbt assembly

This produces `target/out/jvm/scala-3.3.7/binocular/binocular.jar`.

The convenience launcher `./bin/binocular` finds the jar, picks up `$JAVA_HOME`, and forwards all
arguments:

    ./bin/binocular --config application-preprod.conf run     # oracle syncer daemon
    ./bin/binocular --config application-preprod.conf relay    # Bifrost watchtower
    ./bin/binocular --version

Or invoke `java` directly:

    java --sun-misc-unsafe-memory-access=allow \
      -jar target/out/jvm/scala-3.3.7/binocular/binocular.jar \
      --config application-preprod.conf run

The `--sun-misc-unsafe-memory-access=allow` flag is required on JDK 23+ because Scalus' on-chain
interpreter uses `sun.misc.Unsafe`, which newer JDKs deny by default. The `bin/binocular` launcher
adds it automatically when the JDK is new enough. Configuration is loaded exactly as for `sbt run`
(env vars > `--config` file > `application.conf` > `reference.conf`).

Launcher env overrides: `BINOCULAR_JAR` (jar path), `JAVA` (java launcher), `JAVA_OPTS` (extra JVM
options, e.g. `JAVA_OPTS=-Xmx4g`).

## Configuration

Binocular uses [HOCON](https://github.com/lightbend/config) configuration with PureConfig.
Configuration is loaded with the following priority (highest wins):

    environment variables > --config file > application.conf > reference.conf

Pass a config file with `--config`:

    binocular --config application-preprod.conf <command>

### Configuration sections

| Section        | Description                          | Key env vars                                      |
|----------------|--------------------------------------|---------------------------------------------------|
| `bitcoin-node` | Bitcoin RPC connection               | `BITCOIN_NODE_URL`, `BITCOIN_NODE_USER`, `BITCOIN_NODE_PASSWORD`, `BITCOIN_NETWORK` |
| `cardano`      | Cardano network and backend          | `CARDANO_NETWORK`, `CARDANO_BACKEND`, `BLOCKFROST_PROJECT_ID` |
| `wallet`       | Wallet mnemonic for signing          | `WALLET_MNEMONIC`                                 |
| `oracle`       | Oracle parameters (UTxO ref, owner)  | `ORACLE_TX_OUT_REF`, `ORACLE_OWNER_PKH`, `ORACLE_START_HEIGHT` |
| `relay`        | TMTx relay settings                  | `RELAY_TMTX_POLICY_ID`, `RELAY_TMTX_ASSET_NAME`  |
| `bridge`       | The Bifrost bridge this binocular serves: its contracts release and identity | `BIFROST_CONTRACTS`, `CONFIG_NFT_POLICY_ID` |

Running a Bifrost bridge — genesis, the watchtower, and changing the Config — is covered in
[docs/operator-guide.md](docs/operator-guide.md).

### Bitcoin networks

Set `bitcoin-node.network` to one of: `mainnet`, `testnet`, `testnet4`, `regtest`.

### Cardano backends

Set `cardano.backend` to one of:
- `blockfrost` -- requires `blockfrost-project-id`
- `yaci` -- local Yaci DevKit, uses `yaci-store-url` and `yaci-admin-url`

## CLI Commands

All commands accept `--config <path>` to specify a configuration file.

### Oracle commands

| Command             | Description                                              |
|---------------------|----------------------------------------------------------|
| `init`              | Initialize a new oracle. Options: `--start-block`, `--dry-run` |
| `run`               | Continuous daemon: poll Bitcoin and submit oracle updates. Option: `--dry-run` |
| `update-oracle`     | Submit a single oracle update. Options: `--from`, `--to` |
| `deploy-script`     | Deploy the oracle validator as a reference script on-chain |
| `close`             | Close the oracle and burn the NFT                        |
| `info`              | Display oracle configuration and validator parameters    |
| `list-oracles`      | List oracle UTxOs at the script address. Option: `--limit` |
| `verify-oracle`     | Verify the on-chain oracle state                         |
| `blueprint`         | Print the CIP-57 Blueprint JSON                          |
| `prove-transaction` | Prove a Bitcoin transaction's inclusion in a confirmed block |

### Bifrost bridge commands

These serve a [Bifrost](https://github.com/FluidTokens/ft-bifrost-bridge) bridge. What each is for,
and in what order, is in [docs/operator-guide.md](docs/operator-guide.md).

| Command | Description |
|---------|-------------|
| `watchtower` | Oracle sync, TM relay, TM confirm, peg-out completion and the proof API, in one process |
| `relay` / `confirm-tmtx` / `serve-proofs` | The same workers, one at a time |
| `deploy-bridge` | Genesis: the Config, the completion contracts, the treasury and the SPO registry and ban list |
| `deploy-script-refs` | Publish the bridge's heavy scripts as reference scripts |
| `register-bridge-creds` | Register the withdraw reward accounts, if genesis stopped partway |
| `update-config` | Change the Config in place (governance), including a registry migration |
| `tm-script` | Print the TM validator's policy id, address and CBOR |
| `pegin-request`, `pegin-complete`, `peg-out-request`, `peg-out-complete` | Peg-in and peg-out, for testing and demos |
| `deposit-proof`, `spi-proof` | One proof, for one deposit outpoint |

### Other

| Command     | Description          |
|-------------|----------------------|
| `-v, --version` | Print version and exit |

## Examples

### Run the oracle daemon on preprod/testnet4

```bash
binocular --config application-preprod.conf run
```

### Initialize a new oracle

```bash
binocular --config application-preprod.conf init --start-block 129400
```

### Prove a Bitcoin transaction inclusion

```bash
binocular --config application-preprod.conf prove-transaction <BTC_TX_ID>
```

### Relay TMTx transactions (Bifrost watchtower)

#### Optional demo traffic

Set `binocular.traffic.enabled = true` (or `TRAFFIC_ENABLED=true`) to add demo traffic to
`watchtower`. Every five minutes it reads both ledgers and does at most one thing: refund a
deposit the treasury never took, complete a swept peg-in, request a confirmed deposit, or start
a random deposit or peg-out. Bitcoin and Cardano mainnet are refused.

```bash
binocular --config application-preprod.conf traffic-address
# Fund the printed Bitcoin address. The sponsor also needs ADA and deployed bridge script refs.
binocular --config application-preprod.conf watchtower --dry-run
binocular --config application-preprod.conf watchtower
```

Keys: `virtual-epoch-slots` (match Heimdall), `max-amount-sat` (amounts are uniform between
the live `min_peg_out_fbtc` and this, default 50,000), `mean-interval` (mean time between
random deposits, and between random peg-outs, default `4h`). Peg-outs only spend fBTC that
earlier peg-ins minted, so the two sums converge. Fee estimates come from bitcoind, capped at
10,000 sat per transaction; Treasury Movement fees remain Heimdall's responsibility.

The one-day default epoch requires a shortened live bridge schedule; the deployment defaults
have a 36-hour stability window and require the normal 432,000-slot epoch. Startup rejects a
cycle with no eligible deposit window.

There is no state file: a restart re-reads the ledgers and carries on, and deposits are found
by walking the funding address's history. Every action, successful or failed, is logged and
posted to the configured notifier (Discord); after a failure the next tick tries again.
`--dry-run` observes both ledgers and logs what the first tick would do, without sending.

#### Relay only

```bash
# Start the relay daemon
binocular --config application-preprod.conf relay

# Dry-run: check for TMTx UTxOs without broadcasting
binocular --config application-preprod.conf relay --dry-run
```

### Test the relay with a dummy TMTx

```bash
# Create a test TMTx UTxO with some hex bytes as the Bitcoin transaction
binocular --config application-preprod.conf create-tmtx 0200000001abcdef...

# Clean up: destroy all TMTx UTxOs
binocular --config application-preprod.conf spend-tmtx
```

## Bitcoin Core Setup

To test with real Bitcoin data, you need Bitcoin Core running locally or access to a remote node.

**Quick setup:**
```bash
export BITCOIN_NODE_URL="http://localhost:8332"
export BITCOIN_NODE_USER="bitcoin"
export BITCOIN_NODE_PASSWORD="your_password"
```

For detailed setup instructions, see [docs/BITCOIN_SETUP.md](docs/BITCOIN_SETUP.md).

## Testing the relay end-to-end (regtest BTC + preprod Cardano)

The `relay` command broadcasts Bitcoin transactions it finds parked on Cardano
as TMTx UTxOs. To exercise that flow without waiting on real mempool activity,
use `regtest-create-tmtx.sh`: it mines regtest funds, crafts a BTC transaction,
and submits the raw hex as a TMTx on Cardano.

**Prerequisites:**

- `bitcoind` running in regtest mode with an RPC-enabled `bitcoin.conf`
- The Cardano wallet in `application-regtest.conf` has preprod tADA
  (override the mnemonic via `WALLET_MNEMONIC` if you'd rather not commit it)

**Run it:**

```bash
BITCOIN_CONF=/path/to/bitcoin.conf ./regtest-create-tmtx.sh
```

Optional env overrides: `BINOCULAR_CONFIG` (default `application-regtest.conf`),
`BTC_WALLET` (default `binocular-regtest`), `BTC_SEND_AMOUNT` (default `0.001`).

**Verify the relay picks it up:**

```bash
sbt "run --config application-regtest.conf relay --dry-run"
```

Drop `--dry-run` to actually broadcast to regtest bitcoind.

## Documentation

- [Whitepaper](Whitepaper.md) -- complete technical specification
- [Litepaper](Litepaper.md) -- concise overview

### Generating PDFs

```bash
./generate-pdfs.sh
```

Requirements:
- All dependencies are included in the Nix development environment
- On macOS: install Chrome or Chromium separately (`brew install --cask google-chrome`)
- On Linux: Chromium is provided through Nix

Output: `pdfs/Litepaper.pdf`, `pdfs/Whitepaper.pdf`
