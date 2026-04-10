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

### Bifrost relay commands

These commands support the [Bifrost](https://github.com/nicofunke/ft-bifrost-bridge) bridge
protocol by relaying signed Treasury Movement transactions from Cardano to Bitcoin.

| Command        | Description                                                      |
|----------------|------------------------------------------------------------------|
| `relay`        | Poll Cardano for TMTx UTxOs and broadcast signed Bitcoin transactions. Option: `--dry-run` |
| `create-tmtx`  | Create a test TMTx UTxO on Cardano. Argument: `BTC_TX_HEX`      |
| `spend-tmtx`   | Spend (destroy) all TMTx UTxOs at the script address             |

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
