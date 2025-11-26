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
# Run it directly:
java -jar target/scala-3.3.7/binocular-assembly-*.jar --help

# Or run directly with sbt (no assembly needed):
sbt "runMain binocular.main --help"
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

Create `src/main/resources/application.conf` or use the existing `src/test/resources/application.conf` as a template:

```hocon
binocular {
  bitcoin-node {
    url = "http://localhost:8332"
    username = "your_rpc_user"
    password = "your_rpc_password"
    network = "mainnet"
  }

  cardano {
    network = "preview"
    backend = "blockfrost"
    blockfrost {
      project-id = "your_blockfrost_project_id"
    }
  }

  wallet {
    mnemonic = "your twenty four mnemonic phrase "
  }

  oracle {
    start-height = 866970
    max-headers-per-tx = 10
  }
}
```

Note: Environment variables take precedence over values in `application.conf` when using the `${?VAR_NAME}` syntax.

## Demo Steps

### Step 1: Initialize the Oracle

Create a new oracle UTxO on Cardano with the initial Bitcoin block state:

```bash
# Initialize oracle at a specific Bitcoin block height
# Option A: Start from an older block (866970)
sbt "runMain binocular.main init-oracle --start-block 866970"

# Option B: Start from a recent block (925000 - November 2025)
sbt "runMain binocular.main init-oracle --start-block 925000"
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
sbt "runMain binocular.main update-oracle abc123...:0"

# Or specify a range
sbt "runMain binocular.main update-oracle --from 866971 --to 867070 abc123...:0"
```

**For 100+ blocks (required for promotion):**
```bash
# Add 105 blocks to trigger promotion
sbt "runMain binocular.main update-oracle --to 867075 abc123...:0"
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
sbt "runMain binocular.main list-oracles"
```

### Step 4: Update Again to Trigger Promotion

After the challenge period, submit more blocks to trigger promotion:

```bash
# Add a few more blocks to trigger promotion check
sbt "runMain binocular.main update-oracle --to 867080 xyz999...:0"
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

Now you can prove that a Bitcoin transaction is included in the confirmed blockchain.

#### Finding a Bitcoin Transaction ID

First, you need to find a transaction ID from one of the confirmed blocks. There are several ways to do this:

**Option A: Using Bitcoin RPC (if you have a local node)**

```bash
# Get block hash for a specific height (e.g., 866970)
bitcoin-cli getblockhash 866970
# Returns: 000000000000000000020aebcf64dfe45ce4d021df9ac3094116bf531f9a6209

# Get block details with transaction list
bitcoin-cli getblock 000000000000000000020aebcf64dfe45ce4d021df9ac3094116bf531f9a6209

# The output includes a "tx" array with all transaction IDs in that block
# The first transaction (index 0) is always the coinbase transaction
```

**Option B: Using Block Explorers**

Visit any Bitcoin block explorer and navigate to your confirmed block:

- **Blockstream.info**: `https://blockstream.info/block-height/866970`
- **Mempool.space**: `https://mempool.space/block/866970`
- **Blockchain.com**: `https://www.blockchain.com/explorer/blocks/btc/866970`

On the block page, you'll see a list of all transactions. Click any transaction to get its full transaction ID (64 hex characters).

**Option C: Using curl with Bitcoin RPC**

```bash
# Get block hash
BLOCK_HASH=$(curl -s -u "$BITCOIN_RPC_USER:$BITCOIN_RPC_PASSWORD" \
  --data-binary '{"jsonrpc":"1.0","method":"getblockhash","params":[866970]}' \
  -H 'content-type: text/plain;' $BITCOIN_RPC_URL | jq -r '.result')

# Get block with transactions
curl -s -u "$BITCOIN_RPC_USER:$BITCOIN_RPC_PASSWORD" \
  --data-binary "{\"jsonrpc\":\"1.0\",\"method\":\"getblock\",\"params\":[\"$BLOCK_HASH\"]}" \
  -H 'content-type: text/plain;' $BITCOIN_RPC_URL | jq '.result.tx'
```

**Important Notes:**
- The transaction must be in a **confirmed** block (one that has been promoted from the forks tree)
- Use the `list-oracles` command to check the current confirmed block height
- The coinbase transaction (first tx in block, index 0) is always available and easy to use for testing

#### Example Transaction IDs

Here are real transaction IDs from various blocks that you can use for testing:

**Block 866970** (older block)
- Block Hash: `000000000000000000020aebcf64dfe45ce4d021df9ac3094116bf531f9a6209`

| Index | Transaction ID | Type |
|-------|----------------|------|
| 0 | `f44ba1e992bc2cc4ac0da8d654600b80c8ca4f9f58fcd3e38c0dee1fae8f9ee5` | Coinbase |
| 1 | `d4079be98efe3b266c1fffa6d03243020c321911b4b973cd5c83e1bb0a31a42a` | Regular |
| 2 | `7fdb2aee458c6dd02c4b27e4e43452bbaedbab1f537571712949b1cacb3591b0` | Regular |

[Blockstream Explorer - Block 866970](https://blockstream.info/block/000000000000000000020aebcf64dfe45ce4d021df9ac3094116bf531f9a6209)

**Block 925000** (recent block - November 2025)
- Block Hash: `0000000000000000000067f9f40ca6960173ebee423f6130138762dfc40630bf`
- Merkle Root: `40d416f2b888d5d5d5e3ad82633723a0c5fbbf2d19eae039bbf5e5a87380f3d4`

| Index | Transaction ID | Type |
|-------|----------------|------|
| 0 | `ff67bd82c5e65d9b906cc18acc586cd8b8d9093419990e40c5136c4115c27e65` | Coinbase |
| 1 | `d4f38073a8e5bb39d0ea192dfbbfc5a023376382ade3d5d5d588b8f216d40b35` | Regular |
| 2 | `9b03138953d42652b6258648036d21ebbc2a04ab68d945ba8e33f4e04250552b` | Regular |
| 3 | `0d751bfb1716814ec3233bcff532ac53d9f363af68ab193cb334843eeb34f904` | Regular |
| 4 | `d8258deb576ac0710cc8a206e2daa34a742718f756c39f06469f7f6f9140adf1` | Regular |

[Blockstream Explorer - Block 925000](https://blockstream.info/block/0000000000000000000067f9f40ca6960173ebee423f6130138762dfc40630bf)

**Block 925100** (recent block)
- Block Hash: `000000000000000000014939c0d3421925d169319f6f50b945f604dbb41e85a8`

| Index | Transaction ID | Type |
|-------|----------------|------|
| 0 | `2fde926a3c7ebb9aba7b019eeb716b0760db2ad018909a5c033d77e063dae20c` | Coinbase |
| 1 | `a187d3aa8e73d608fbeab88180e8d3a48aaa888f4b98a4a78401e044f66b8624` | Regular |
| 2 | `aae46bd835987b97d8cadb48edef0c12dfcb14d16021238fc028e6193edd5ccd` | Regular |

[Blockstream Explorer - Block 925100](https://blockstream.info/block/000000000000000000014939c0d3421925d169319f6f50b945f604dbb41e85a8)

**Block 925150** (recent block)
- Block Hash: `0000000000000000000126c79e39a80e5b08720fbb0d6de3ed811179ec7c4e93`

| Index | Transaction ID | Type |
|-------|----------------|------|
| 0 | `d569e3f75c677bc7daabbe780a4528d2d860e3ce39f7a1b66912ff2d217b97a3` | Coinbase |
| 1 | `460ad4733131a58f1196039204ca37f9558474cda848538955feda24059bf4d9` | Regular |
| 2 | `f1b04b7dd962c316df2afb299e153d13d4fb9ac35b40ce013561836f63a8b0c5` | Regular |

[Blockstream Explorer - Block 925150](https://blockstream.info/block/0000000000000000000126c79e39a80e5b08720fbb0d6de3ed811179ec7c4e93)

#### Proving the Transaction

Once you have a transaction ID from a confirmed block:

```bash
# Basic usage - looks up transaction to find its block
sbt "runMain binocular.main prove-transaction promo123...:0 f44ba1e992bc2cc4ac0da8d654600b80c8ca4f9f58fcd3e38c0dee1fae8f9ee5"

# With --block option - skip transaction lookup (useful for checking non-existent tx)
sbt "runMain binocular.main prove-transaction promo123...:0 f44ba1e992bc2cc4ac0da8d654600b80c8ca4f9f58fcd3e38c0dee1fae8f9ee5 --block 000000000000000000020aebcf64dfe45ce4d021df9ac3094116bf531f9a6209"
```

**Note:** The `--block` option takes a **block hash** (64 hex characters), not a block height.

#### Offline Verification (No Bitcoin RPC)

For fully offline verification without any Bitcoin RPC calls, provide all four options:

```bash
# Offline mode - verifies locally without Bitcoin RPC
sbt "runMain binocular.main prove-transaction <ORACLE_UTXO> <TX_ID> \
  --block <BLOCK_HASH> \
  --tx-index <TX_INDEX> \
  --merkle-root <MERKLE_ROOT> \
  --proof <HASH1,HASH2,...>"
```

**Tip:** Run online verification first - it outputs a ready-to-use offline command with all parameters.

#### Complete Example: Block 925000 Transaction Verification

```bash
# Step 1: Online verification (fetches data from Bitcoin RPC)
sbt "runMain binocular.main prove-transaction <ORACLE_UTXO> \
  ff67bd82c5e65d9b906cc18acc586cd8b8d9093419990e40c5136c4115c27e65"

# The output will include something like:
# Offline verification command:
#   binocular prove-transaction <ORACLE_UTXO> ff67bd82c5e65d9b906cc18acc586cd8b8d9093419990e40c5136c4115c27e65 \
#     --block 0000000000000000000067f9f40ca6960173ebee423f6130138762dfc40630bf \
#     --tx-index 0 \
#     --merkle-root <MERKLE_ROOT_FROM_BLOCK> \
#     --proof <PROOF_HASHES>

# Step 2: Copy and run the offline command (no Bitcoin RPC needed)
# This can be run anywhere without Bitcoin node access
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
sbt "runMain binocular.main list-oracles"
```

### Verify Oracle State

```bash
sbt "runMain binocular.main verify-oracle abc123...:0"
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
