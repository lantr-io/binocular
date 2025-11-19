# Binocular Demo Plan

This guide walks through a complete demonstration of the Binocular Bitcoin oracle on Cardano testnet, including transaction verification using a smart contract.

## Prerequisites

### 1. Bitcoin Node or RPC Service

You need access to a Bitcoin node with RPC enabled. Options:

**Option A: Local Bitcoin Core Node**
```bash
# Install Bitcoin Core and configure bitcoin.conf:
rpcuser=your_rpc_user
rpcpassword=your_rpc_password
server=1
txindex=1  # Required for transaction lookups
```

**Option B: Public RPC Service**
- [GetBlock.io](https://getblock.io/) - Free tier available
- [BlockCypher](https://www.blockcypher.com/) - Free tier available
- [QuickNode](https://www.quicknode.com/) - Free trial

### 2. Cardano Testnet Access

**Option A: Blockfrost (Recommended)**
1. Create account at [blockfrost.io](https://blockfrost.io/)
2. Create a project for "Cardano Testnet (Preview)" or "Cardano Testnet (Preprod)"
3. Copy your API key

**Option B: Local Cardano Node with Kupo**
- Requires running cardano-node and kupo indexer

### 3. Testnet ADA

Get testnet ADA from the faucet:
- Preview: https://docs.cardano.org/cardano-testnet/tools/faucet/
- Preprod: https://docs.cardano.org/cardano-testnet/tools/faucet/

### 4. Build Binocular CLI

```bash
# Clone and build
git clone https://github.com/lantr-io/binocular.git
cd binocular
sbt assembly

# The CLI jar will be at target/scala-3.3.7/binocular-assembly-*.jar
# Or run directly with sbt:
sbt "runMain binocular.cli.Main --help"
```

## Configuration

### Environment Variables

Set up your configuration via environment variables:

```bash
# Bitcoin Node Configuration
export BITCOIN_RPC_URL="http://localhost:8332"
export BITCOIN_RPC_USER="your_rpc_user"
export BITCOIN_RPC_PASSWORD="your_rpc_password"
export BITCOIN_NETWORK="mainnet"  # or "testnet"

# Cardano Configuration
export CARDANO_NETWORK="preview"  # or "preprod" or "mainnet"
export CARDANO_BACKEND="blockfrost"
export BLOCKFROST_API_KEY="your_blockfrost_api_key"

# Wallet Configuration (24-word mnemonic)
export WALLET_MNEMONIC="your twenty four word mnemonic phrase here ..."

# Oracle Configuration
export ORACLE_START_HEIGHT="866970"  # Bitcoin block height to start from
export MAX_HEADERS_PER_TX="10"  # Headers per transaction (default: 10)
```

### Alternative: Configuration File

Create `~/.binocular/config.conf`:

```hocon
bitcoin {
  rpc-url = "http://localhost:8332"
  rpc-user = "your_rpc_user"
  rpc-password = "your_rpc_password"
  network = "mainnet"
}

cardano {
  network = "preview"
  backend = "blockfrost"
  blockfrost-api-key = "your_api_key"
}

wallet {
  mnemonic = "your twenty four word mnemonic phrase here ..."
}

oracle {
  start-height = 866970
  max-headers-per-tx = 10
}
```

## Demo Steps

### Step 1: Initialize the Oracle

Create a new oracle UTxO on Cardano with the initial Bitcoin block state:

```bash
# Initialize oracle at a specific Bitcoin block height
sbt "runMain binocular.cli.Main init-oracle --start-block 866970"
```

**Expected Output:**
```
Initializing new oracle...

Start Block Height: 866970
Bitcoin Network: mainnet
Cardano Network: preview
Oracle Address: addr_test1...

✓ Wallet loaded: addr_test1...
✓ Connected to Cardano backend (blockfrost)

Step 1: Fetching initial block data from Bitcoin...
✓ Block hash: 000000000000000000020aebcf64dfe45ce4d021df9ac3094116bf531f9a6209

Step 2: Building initial ChainState...
✓ ChainState created

Step 3: Submitting initialization transaction...
✓ Oracle initialized successfully!
  Transaction Hash: abc123...
  Output Index: 0

Save your oracle UTxO: abc123...:0
```

**Save the UTxO ID** - you'll need it for all subsequent operations.

### Step 2: Update the Oracle with New Blocks

Add new Bitcoin blocks to the oracle. The command automatically batches if you request many blocks:

```bash
# Update oracle with blocks from current height to latest
sbt "runMain binocular.cli.Main update-oracle --utxo abc123...:0"

# Or specify a range
sbt "runMain binocular.cli.Main update-oracle --utxo abc123...:0 --from 866971 --to 867070"
```

**For 100+ blocks (required for promotion):**
```bash
# Add 105 blocks to trigger promotion
sbt "runMain binocular.cli.Main update-oracle --utxo abc123...:0 --to 867075"
```

The command will automatically split into batches of 10 blocks each.

**Expected Output:**
```
Updating oracle at abc123...:0...

Step 1: Loading configurations...
✓ Bitcoin Node: http://localhost:8332
✓ Cardano Network: preview
✓ Oracle Address: addr_test1...

Step 2: Fetching current oracle UTxO from Cardano...
✓ Found oracle UTxO: abc123...:0
✓ Current oracle state:
  Block Height: 866970
  Fork Tree Size: 0

Step 4: Processing 105 blocks in 11 batches of up to 10 headers each...

  Batch 1/11: blocks 866971 to 866980
    ✓ Batch 1 submitted: def456...
    Waiting for confirmation...

  Batch 2/11: blocks 866981 to 866990
    ✓ Batch 2 submitted: ghi789...
    ...

✓ Oracle updated successfully!
  Transaction Hash: xyz999...
  Updated from block 866971 to 867075 (105 blocks)
  Processed in 11 batches
```

### Step 3: Wait for Challenge Period (200 minutes)

For blocks to be promoted from the forks tree to confirmed state, they must:
1. Have 100+ confirmations (blocks built on top)
2. Be on-chain for 200+ minutes (challenge period)

**Wait approximately 3.5 hours** for the challenge period to pass.

You can check the oracle state periodically:

```bash
sbt "runMain binocular.cli.Main list-oracles"
```

### Step 4: Update Again to Trigger Promotion

After the challenge period, submit more blocks to trigger promotion:

```bash
# Add a few more blocks to trigger promotion check
sbt "runMain binocular.cli.Main update-oracle --utxo xyz999...:0 --to 867080"
```

**Expected Output (with promotion):**
```
✓ Oracle updated successfully!
  Transaction Hash: promo123...
  Updated from block 867076 to 867080 (5 blocks)

Note: Blocks were promoted to confirmed state!
  Previous confirmed height: 866970
  New confirmed height: 866975
```

### Step 5: Prove Bitcoin Transaction Inclusion

Now you can prove that a Bitcoin transaction is included in the confirmed blockchain:

```bash
# Find a transaction ID from one of the confirmed blocks
# For example, from block 866970

sbt "runMain binocular.cli.Main prove-transaction \
  --utxo promo123...:0 \
  --btc-tx f44ba1e992bc2cc4ac0da8d654600b80c8ca4f9f58fcd3e38c0dee1fae8f9ee5"
```

**Expected Output:**
```
Proving Bitcoin transaction inclusion...
  Oracle UTxO: promo123...:0
  Bitcoin TX: f44ba1e992bc2cc4ac0da8d654600b80c8ca4f9f58fcd3e38c0dee1fae8f9ee5

Step 1: Loading configurations...
✓ Bitcoin Node: http://localhost:8332
✓ Cardano Network: preview

Step 2: Fetching oracle UTxO from Cardano...
✓ Found oracle UTxO

Step 3: Parsing ChainState datum...
✓ Current confirmed height: 866975

Step 4: Finding transaction in Bitcoin blockchain...
✓ Transaction found in block 866970

Step 5: Building Merkle proof...
✓ Merkle proof generated
  Block Merkle Root: ad051a67e24ce3e89472a02d94b126a366cda8c4c3818ff9e4402af75b831552
  Proof size: 13 hashes
  Transaction index: 0

Step 6: Verifying proof...
✓ Proof verified successfully!

Merkle Proof Output:
{
  "txHash": "f44ba1e992bc2cc4ac0da8d654600b80c8ca4f9f58fcd3e38c0dee1fae8f9ee5",
  "blockHeight": 866970,
  "txIndex": 0,
  "merkleRoot": "ad051a67e24ce3e89472a02d94b126a366cda8c4c3818ff9e4402af75b831552",
  "proof": ["hash1...", "hash2...", ...]
}
```

### Step 6: Use BitcoinDependentLock Application

The `BitcoinDependentLock` example application demonstrates locking Cardano funds that can only be unlocked with a valid Bitcoin transaction Merkle proof.

#### Build the Example Application

```bash
cd example
sbt compile
```

#### 6a. Show Contract Information

```bash
sbt "runMain binocular.example.bitcoinDependentLock info"
```

**Expected Output:**
```
BitcoinDependentLock - TransactionVerifier Contract Info

Script Hash: ...
Contract Addresses:
  Mainnet: addr1...
  Testnet: addr_test1...

Usage:
  lock <BTC_TX_ID> --merkle-root <ROOT> --amount <LOVELACE>
  unlock <UTXO> --tx-index <INDEX> --proof <HASH1,HASH2,...>
```

#### 6b. Lock Funds Requiring Bitcoin TX Proof

Use the Merkle root from Step 5's output:

```bash
sbt "runMain binocular.example.bitcoinDependentLock lock \
  f44ba1e992bc2cc4ac0da8d654600b80c8ca4f9f58fcd3e38c0dee1fae8f9ee5 \
  --merkle-root ad051a67e24ce3e89472a02d94b126a366cda8c4c3818ff9e4402af75b831552 \
  --amount 10000000"
```

**Expected Output:**
```
Locking funds with Bitcoin transaction requirement...

  Bitcoin TX: f44ba1e992bc2cc4ac0da8d654600b80c8ca4f9f58fcd3e38c0dee1fae8f9ee5
  Merkle Root: ad051a67e24ce3e89472a02d94b126a366cda8c4c3818ff9e4402af75b831552
  Amount: 10000000 lovelace (10.0 ADA)

✓ Wallet loaded: addr_test1...
✓ Connected to Cardano backend
✓ Verifier address: addr_test1...

Step 1: Building lock transaction...

✓ Funds locked successfully!
  Transaction: locked123...
  Locked UTxO: locked123...:0

To unlock, use:
  bitcoin-dependent-lock unlock locked123...:0 --tx-index <INDEX> --proof <PROOF>
```

#### 6c. Unlock Funds with Merkle Proof

Use the proof from Step 5's output (the `proof` array):

```bash
sbt "runMain binocular.example.bitcoinDependentLock unlock \
  locked123...:0 \
  --tx-index 0 \
  --proof hash1,hash2,hash3,hash4,hash5,hash6,hash7,hash8,hash9,hash10,hash11,hash12,hash13"
```

**Expected Output:**
```
Unlocking funds with Merkle proof...

  UTxO: locked123...:0
  TX Index: 0
  Proof size: 13 hashes

✓ Wallet loaded: addr_test1...
✓ Connected to Cardano backend

Step 1: Fetching locked UTxO...
✓ Found locked UTxO

Step 2: Building unlock transaction...

✓ Funds unlocked successfully!
  Transaction: unlock456...

The Merkle proof was verified on-chain, proving the Bitcoin
transaction exists in the specified block.
```

#### What Happens On-Chain

1. **Lock**: Creates a UTxO at the verifier script address with datum containing:
   - Expected Bitcoin transaction hash
   - Block Merkle root

2. **Unlock**: Spends the UTxO by providing a redeemer with:
   - Transaction index in block
   - Merkle proof (sibling hashes)

3. **Verification**: The on-chain script:
   - Computes Merkle root from tx hash + proof
   - Verifies computed root matches datum's Merkle root
   - If valid, allows spending; otherwise fails

## Verification Commands

### List All Oracles

```bash
sbt "runMain binocular.cli.Main list-oracles"
```

### Get Oracle Info

```bash
sbt "runMain binocular.cli.Main info --utxo abc123...:0"
```

### Verify Oracle State

```bash
sbt "runMain binocular.cli.Main verify-oracle --utxo abc123...:0"
```

## Timeline Summary

| Step | Action | Duration |
|------|--------|----------|
| 1 | Initialize oracle | ~1 minute |
| 2 | Add 100+ blocks | ~5-10 minutes |
| 3 | Wait for challenge period | **~200 minutes (3.5 hours)** |
| 4 | Update to trigger promotion | ~1 minute |
| 5 | Prove transaction | ~30 seconds |
| 6 | Use verifier contract | ~2 minutes |

**Total Demo Time: ~4 hours** (mostly waiting for challenge period)

## Troubleshooting

### "UTxO not found"
- The oracle UTxO was spent by another transaction
- Use `list-oracles` to find the current UTxO

### "Error fetching Bitcoin headers"
- Check Bitcoin RPC connection
- Verify RPC credentials
- Ensure requested block heights exist

### "Transaction failed"
- Ensure wallet has sufficient ADA (at least 10 ADA recommended)
- Check that UTxO hasn't been spent

### "Proof verification failed"
- Transaction is not in a confirmed block yet
- Block hasn't been promoted (need to wait for challenge period)
- Transaction ID may be incorrect

## Architecture Notes

### Challenge Period (200 minutes)
The challenge period prevents pre-computation attacks where an attacker could prepare a malicious fork in advance. By requiring blocks to age on-chain, honest participants have time to submit the correct chain.

### Block Promotion
Blocks move from the `forksTree` to `confirmedBlocksTree` when:
1. They have 100+ confirmations (matches Bitcoin's coinbase maturity)
2. They've been on-chain for 200+ minutes

### Merkle Proofs
Transaction inclusion is proven using Bitcoin's standard Merkle tree:
1. Transaction hash is the leaf
2. Proof contains sibling hashes from leaf to root
3. Computed root must match block's merkle root
4. Block must be in the confirmed chain

## Next Steps

- Explore the whitepaper for detailed protocol specification
- Check integration tests for programmatic usage examples
- Build your own dApp using the TransactionVerifier pattern
