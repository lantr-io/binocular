#!/bin/bash
# Binocular Two-Proof Transaction Verification Demo Script
#
# This script demonstrates the secure two-proof transaction verification
# using the Binocular Bitcoin oracle on Cardano.
#
# The two proofs are:
#   1. Transaction in Block - proves TX is in a specific Bitcoin block
#   2. Block in Oracle - proves the block is confirmed in the Oracle
#
# Prerequisites:
# - Bitcoin RPC configured (for online verification)
# - Cardano backend configured (Blockfrost or Kupo)
# - Oracle UTxO exists on Cardano with confirmed blocks
# - Wallet with testnet ADA configured

set -e

# =============================================================================
# Configuration
# =============================================================================

# Oracle script address (bypasses CBOR hash computation)
export ORACLE_SCRIPT_ADDRESS="addr_test1wr23gch0egtlxksmpj4f0jsxp5fq4qz7pgfyxnpmpwr356gxr6kht"

ORACLE_UTXO="1b40ab478de4aeffd29ad41239ef8c9084107db562ce80bdd3e73f00338eda99:0"

# Block 925000 data (November 2025)
BLOCK_HASH="0000000000000000000067f9f40ca6960173ebee423f6130138762dfc40630bf"
BLOCK_HEIGHT=925000

# Transaction from block 925000 (coinbase transaction at index 0)
TX_ID="ff67bd82c5e65d9b906cc18acc586cd8b8d9093419990e40c5136c4115c27e65"

# Amount to lock (5 ADA = 5000000 lovelace)
LOCK_AMOUNT=5000000

# Temp file for capturing output
TEMP_OUTPUT=$(mktemp)
trap "rm -f $TEMP_OUTPUT" EXIT

# =============================================================================
# Step 1: Generate Two-Proof Data
# =============================================================================

echo "============================================================================="
echo "STEP 1: Generate Two-Proof Transaction Verification Data"
echo "============================================================================="
echo ""
echo "This step fetches transaction data from Bitcoin RPC and generates BOTH proofs:"
echo "  - Proof 1: Transaction in Block (TX merkle proof)"
echo "  - Proof 2: Block in Oracle (block merkle proof)"
echo ""
echo "Running:"
echo "  sbt \"runMain binocular.main prove-transaction $ORACLE_UTXO $TX_ID\""
echo ""
echo "Press Enter to run, or Ctrl+C to exit..."
read

sbt "runMain binocular.main prove-transaction $ORACLE_UTXO $TX_ID" 2>&1 | tee $TEMP_OUTPUT

# Extract both proofs from output
TX_INDEX=$(grep "TX Index:" $TEMP_OUTPUT | head -1 | awk '{print $3}')
TX_PROOF=$(grep "\-\-tx-proof" $TEMP_OUTPUT | sed 's/.*--tx-proof //' | awk '{print $1}' | tr -d '\\')
BLOCK_INDEX=$(grep "\-\-block-index" $TEMP_OUTPUT | sed 's/.*--block-index //' | awk '{print $1}' | tr -d '\\')
BLOCK_PROOF=$(grep "\-\-block-proof" $TEMP_OUTPUT | sed 's/.*--block-proof //' | awk '{print $1}' | tr -d '\\')
BLOCK_HEADER=$(grep "\-\-block-header" $TEMP_OUTPUT | sed 's/.*--block-header //' | awk '{print $1}' | tr -d '\\')

echo ""
echo "============================================================================="
echo "Captured from Step 1:"
echo "  TX Index: $TX_INDEX"
echo "  TX Proof: ${TX_PROOF:0:60}..."
echo "  Block Index: $BLOCK_INDEX"
echo "  Block Proof: ${BLOCK_PROOF:0:60}..."
echo "  Block Header: ${BLOCK_HEADER:0:40}..."
echo "============================================================================="
echo ""

if [ -z "$TX_PROOF" ] || [ -z "$BLOCK_HEADER" ]; then
    echo "ERROR: Could not extract proof data from output."
    echo "Please check the output above for errors."
    exit 1
fi

# =============================================================================
# Step 2: Lock Funds with Bitcoin Transaction Requirement
# =============================================================================

echo "============================================================================="
echo "STEP 2: Lock Funds (BitcoinDependentLock with Oracle Verification)"
echo "============================================================================="
echo ""
echo "Lock $((LOCK_AMOUNT / 1000000)) ADA that can only be unlocked with:"
echo "  - Valid TX merkle proof (transaction in block)"
echo "  - Valid Block merkle proof (block in Oracle's confirmed tree)"
echo ""
echo "Running:"
echo "  sbt \"example/runMain binocular.example.bitcoinDependentLock lock \\"
echo "    $TX_ID \\"
echo "    --block-hash $BLOCK_HASH \\"
echo "    --amount $LOCK_AMOUNT\""
echo ""
echo "Press Enter to run, or Ctrl+C to skip..."
read

sbt "example/runMain binocular.example.bitcoinDependentLock lock \
    $TX_ID \
    --block-hash $BLOCK_HASH \
    --amount $LOCK_AMOUNT" 2>&1 | tee $TEMP_OUTPUT

# Extract locked UTXO from output
LOCKED_UTXO=$(grep "Locked UTxO:" $TEMP_OUTPUT | awk '{print $3}')

echo ""
echo "============================================================================="
echo "Captured from Step 2:"
echo "  Locked UTxO: $LOCKED_UTXO"
echo "============================================================================="
echo ""

if [ -z "$LOCKED_UTXO" ]; then
    echo "ERROR: Could not extract locked UTxO from output."
    echo "Please check the output above for errors."
    exit 1
fi

# =============================================================================
# Step 3: Unlock Funds with Two Merkle Proofs
# =============================================================================

echo "============================================================================="
echo "STEP 3: Unlock Funds (Two-Proof Verification)"
echo "============================================================================="
echo ""
echo "Unlock funds by providing BOTH merkle proofs:"
echo "  - TX Proof: Transaction is in the block"
echo "  - Block Proof: Block is in Oracle's confirmed tree"
echo ""
echo "Running:"
echo "  sbt \"example/runMain binocular.example.bitcoinDependentLock unlock \\"
echo "    $LOCKED_UTXO \\"
echo "    --tx-index $TX_INDEX \\"
echo "    --tx-proof $TX_PROOF \\"
echo "    --block-index $BLOCK_INDEX \\"
echo "    --block-proof $BLOCK_PROOF \\"
echo "    --block-header $BLOCK_HEADER \\"
echo "    --oracle-utxo $ORACLE_UTXO\""
echo ""
echo "Press Enter to run, or Ctrl+C to skip..."
read

sbt "example/runMain binocular.example.bitcoinDependentLock unlock \
    $LOCKED_UTXO \
    --tx-index $TX_INDEX \
    --tx-proof $TX_PROOF \
    --block-index $BLOCK_INDEX \
    --block-proof $BLOCK_PROOF \
    --block-header $BLOCK_HEADER \
    --oracle-utxo $ORACLE_UTXO"

echo ""
echo "============================================================================="
echo "DEMO COMPLETE!"
echo "============================================================================="
echo ""
echo "Summary:"
echo "  1. Generated TWO proofs from Bitcoin RPC:"
echo "     - TX Proof: Proves transaction is in block (merkle path)"
echo "     - Block Proof: Proves block is confirmed in Oracle"
echo "  2. Locked $((LOCK_AMOUNT / 1000000)) ADA requiring both proofs"
echo "  3. Unlocked funds by providing:"
echo "     - TX merkle proof"
echo "     - Block merkle proof"
echo "     - Block header (80 bytes)"
echo "     - Oracle reference input"
echo ""
echo "SECURITY: The on-chain validator verified:"
echo "  1. Block is in Oracle's confirmed blocks tree"
echo "  2. Block header hashes to expected block hash"
echo "  3. Transaction is included in the block"
echo ""
echo "This ensures the transaction ACTUALLY exists in Bitcoin as attested by the Oracle!"
echo "============================================================================="
