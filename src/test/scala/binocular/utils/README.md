# Test Utilities

This directory contains utilities for testing Binocular Oracle.

## BitcoinDataFetcher

**Purpose:** Fetch real Bitcoin block data from RPC for integration testing.

**Usage:**

```bash
# Configure QuickNode (fastest - no local node needed)
export bitcoind_rpc_url="https://your-node.quicknode.pro/YOUR-API-KEY/"
export bitcoind_rpc_user=""
export bitcoind_rpc_password=""

# Fetch specific blocks
sbt "Test/runMain binocular.utils.BitcoinDataFetcher 170 286 100000"

# Fetch a range of consecutive blocks (for integration tests)
sbt "Test/runMain binocular.utils.BitcoinDataFetcher --range 866870 866970"

# Fetch 100+ blocks for comprehensive testing
sbt "Test/runMain binocular.utils.BitcoinDataFetcher --range 800000 800100"
```

**Output:**

Saves JSON files to `src/test/resources/bitcoin_blocks/`:
- `block_<height>.json` - Block metadata and all transaction IDs
- `block_<height>_merkle_proofs.json` - Merkle proof test cases for sample transactions

**Recommended blocks to fetch:**

**Individual blocks (testing specific cases):**
- `170` - First Bitcoin transaction (Satoshi to Hal Finney)
- `286` - Early block with simple merkle tree
- `100000` - Milestone block (~4 transactions)
- `125552` - Block from March 2011

**Block ranges (integration testing):**
- `--range 866870 866970` - 100 recent consecutive blocks (validation testing)
- `--range 800000 800100` - 100 blocks from height 800k
- `--range 170 270` - 100 early blocks (simple transactions)

## Configuration

The utility uses `BitcoinNodeConfig` which supports multiple sources:

### Option 1: QuickNode (Recommended for testing)

```bash
export bitcoind_rpc_url="https://your-node.quicknode.pro/YOUR-KEY/"
export bitcoind_rpc_user=""
export bitcoind_rpc_password=""
```

**Benefits:**
- No local Bitcoin Core installation needed
- No blockchain sync wait time
- Free tier available
- Works immediately

**Get started:** https://www.quicknode.com/

### Option 2: GetBlock

```bash
export bitcoind_rpc_url="https://btc.getblock.io/YOUR-KEY/mainnet/"
export bitcoind_rpc_user=""
export bitcoind_rpc_password=""
```

**Get started:** https://getblock.io/

### Option 3: Local Bitcoin Core

```bash
export bitcoind_rpc_url="http://localhost:8332"
export bitcoind_rpc_user="bitcoin"
export bitcoind_rpc_password="your_password"
```

See `docs/BITCOIN_SETUP.md` for Bitcoin Core setup instructions.

## Example Session

```bash
$ export bitcoind_rpc_url="https://my-node.quicknode.pro/abc123/"
$ sbt "Test/runMain binocular.utils.BitcoinDataFetcher 170"

Fetching 1 Bitcoin block(s)...
Output directory: src/test/resources/bitcoin_blocks

✓ Loaded Bitcoin RPC config: BitcoinNodeConfig(...)

Fetching block 170...
  ✓ Block hash: 00000000d1145790a8694403d4063f323d499e655c83426834d4ce2f8dd4a2ee
  ✓ Transactions: 2
  ✓ Merkle root: 7dac2c5666815c17a3b36427de37bb9d2e2c5ccec3f8633eb91a4205cb4c10ff
  ✓ Generated 2 merkle proof test cases
  ✓ Saved to src/test/resources/bitcoin_blocks/

============================================================
✅ Successfully fetched 1 block(s)!
============================================================

Block 170:
  Hash: 00000000d1145790a8694403d4063f323d499e655c83426834d4ce2f8dd4a2ee
  Transactions: 2
  Files saved to: src/test/resources/bitcoin_blocks/
    - block_170.json
    - block_170_merkle_proofs.json

You can now use these JSON files in integration tests!
```

## Using Generated JSON in Tests

```scala
import upickle.default._
import java.nio.file.{Files, Path}

class MyMerkleProofTest extends FunSuite {
  test("verify transaction inclusion in block 170") {
    // Load block data
    val blockJson = Files.readString(Path.of("src/test/resources/bitcoin_blocks/block_170.json"))
    val blockData = read[BitcoinDataFetcher.BlockData](blockJson)
    
    // Load merkle proofs
    val proofsJson = Files.readString(Path.of("src/test/resources/bitcoin_blocks/block_170_merkle_proofs.json"))
    val proofs = read[BitcoinDataFetcher.MerkleProofTestCase](proofsJson)
    
    // Build merkle tree
    val txHashes = blockData.transactions.map(tx => ByteString.fromHex(tx).reverse)
    val tree = MerkleTree.fromHashes(txHashes)
    
    // Verify coinbase transaction proof
    val coinbaseCase = proofs.testCases.head
    val txHash = ByteString.fromHex(coinbaseCase.txHash).reverse
    val proof = coinbaseCase.merkleProof.map(p => ByteString.fromHex(p).reverse)
    val computedRoot = MerkleTree.calculateMerkleRootFromProof(
      coinbaseCase.txIndex,
      txHash,
      proof
    )
    
    val expectedRoot = ByteString.fromHex(blockData.merkleroot).reverse
    assertEquals(computedRoot, expectedRoot)
  }
}
```

## Adding More Utilities

When adding new test utilities:

1. Place them in this directory
2. Use package `binocular.utils`
3. Update this README with usage instructions
4. Add examples to documentation
