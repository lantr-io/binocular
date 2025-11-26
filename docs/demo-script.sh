#!/bin/bash
# Binocular Transaction Verification Demo Script
#
# This script demonstrates online and offline transaction verification
# using the Binocular Bitcoin oracle on Cardano.
#
# Prerequisites:
# - Bitcoin RPC configured (for online verification)
# - Cardano backend configured (Blockfrost or Kupo)
# - Oracle UTxO exists on Cardano
# - Wallet with testnet ADA configured

set -e

# =============================================================================
# Configuration
# =============================================================================

ORACLE_UTXO="1b40ab478de4aeffd29ad41239ef8c9084107db562ce80bdd3e73f00338eda99:0"

# Block 925000 data (November 2025)
BLOCK_HASH="0000000000000000000067f9f40ca6960173ebee423f6130138762dfc40630bf"
BLOCK_HEIGHT=925000

# Transaction from block 925000 (coinbase transaction at index 0)
TX_ID="ff67bd82c5e65d9b906cc18acc586cd8b8d9093419990e40c5136c4115c27e65"
TX_INDEX=0

# Amount to lock (5 ADA = 5000000 lovelace)
LOCK_AMOUNT=5000000

# Temp file for capturing output
TEMP_OUTPUT=$(mktemp)
trap "rm -f $TEMP_OUTPUT" EXIT

# =============================================================================
# Step 1: Online Verification (requires Bitcoin RPC)
# =============================================================================

echo "============================================================================="
echo "STEP 1: Online Transaction Verification"
echo "============================================================================="
echo ""
echo "This step fetches transaction data from Bitcoin RPC and generates a Merkle proof."
echo ""
echo "Running:"
echo "  sbt \"runMain binocular.main prove-transaction $ORACLE_UTXO $TX_ID\""
echo ""
echo "Press Enter to run, or Ctrl+C to exit..."
read

sbt "runMain binocular.main prove-transaction $ORACLE_UTXO $TX_ID" 2>&1 | tee $TEMP_OUTPUT

# Extract merkle root and proof from output
MERKLE_ROOT=$(grep "merkle-root" $TEMP_OUTPUT | sed 's/.*--merkle-root //' | awk '{print $1}')
PROOF=$(grep "proof" $TEMP_OUTPUT | tail -1 | sed 's/.*--proof //')

echo ""
echo "============================================================================="
echo "Captured from Step 1:"
echo "  Merkle Root: $MERKLE_ROOT"
echo "  Proof: ${PROOF:0:60}..."
echo "============================================================================="
echo ""

if [ -z "$MERKLE_ROOT" ] || [ -z "$PROOF" ]; then
    echo "ERROR: Could not extract merkle root or proof from output."
    echo "Please check the output above for errors."
    exit 1
fi

# =============================================================================
# Step 2: Offline Verification (no Bitcoin RPC needed)
# =============================================================================

echo "============================================================================="
echo "STEP 2: Offline Transaction Verification"
echo "============================================================================="
echo ""
echo "This verifies the transaction using only the proof data (no Bitcoin RPC)."
echo ""
echo "Running:"
echo "  sbt \"runMain binocular.main prove-transaction $ORACLE_UTXO $TX_ID \\"
echo "    --block $BLOCK_HASH \\"
echo "    --tx-index $TX_INDEX \\"
echo "    --merkle-root $MERKLE_ROOT \\"
echo "    --proof $PROOF\""
echo ""
echo "Press Enter to run, or Ctrl+C to skip..."
read

sbt "runMain binocular.main prove-transaction $ORACLE_UTXO $TX_ID \
    --block $BLOCK_HASH \
    --tx-index $TX_INDEX \
    --merkle-root $MERKLE_ROOT \
    --proof $PROOF"

echo ""

# =============================================================================
# Step 3: Lock Funds with Bitcoin Transaction Requirement
# =============================================================================

echo "============================================================================="
echo "STEP 3: Lock Funds (BitcoinDependentLock)"
echo "============================================================================="
echo ""
echo "Lock $((LOCK_AMOUNT / 1000000)) ADA that can only be unlocked with a valid Bitcoin TX proof."
echo ""
echo "Running:"
echo "  sbt \"example/runMain binocular.example.bitcoinDependentLock lock \\"
echo "    $TX_ID \\"
echo "    --merkle-root $MERKLE_ROOT \\"
echo "    --amount $LOCK_AMOUNT\""
echo ""
echo "Press Enter to run, or Ctrl+C to skip..."
read

sbt "example/runMain binocular.example.bitcoinDependentLock lock \
    $TX_ID \
    --merkle-root $MERKLE_ROOT \
    --amount $LOCK_AMOUNT" 2>&1 | tee $TEMP_OUTPUT

# Extract locked UTXO from output
LOCKED_UTXO=$(grep "Locked UTxO:" $TEMP_OUTPUT | awk '{print $3}')

echo ""
echo "============================================================================="
echo "Captured from Step 3:"
echo "  Locked UTxO: $LOCKED_UTXO"
echo "============================================================================="
echo ""

if [ -z "$LOCKED_UTXO" ]; then
    echo "ERROR: Could not extract locked UTxO from output."
    echo "Please check the output above for errors."
    exit 1
fi

# =============================================================================
# Step 4: Unlock Funds with Merkle Proof
# =============================================================================

echo "============================================================================="
echo "STEP 4: Unlock Funds (BitcoinDependentLock)"
echo "============================================================================="
echo ""
echo "Unlock funds by providing the Merkle proof."
echo ""
echo "Running:"
echo "  sbt \"example/runMain binocular.example.bitcoinDependentLock unlock \\"
echo "    $LOCKED_UTXO \\"
echo "    --tx-index $TX_INDEX \\"
echo "    --proof $PROOF\""
echo ""
echo "Press Enter to run, or Ctrl+C to skip..."
read

sbt "example/runMain binocular.example.bitcoinDependentLock unlock \
    $LOCKED_UTXO \
    --tx-index $TX_INDEX \
    --proof $PROOF"

echo ""
echo "============================================================================="
echo "DEMO COMPLETE!"
echo "============================================================================="
echo ""
echo "Summary:"
echo "  1. Online verification: Fetched proof from Bitcoin RPC"
echo "  2. Offline verification: Verified proof without Bitcoin RPC"
echo "  3. Lock: Locked $((LOCK_AMOUNT / 1000000)) ADA requiring Bitcoin TX proof"
echo "  4. Unlock: Unlocked funds by providing valid Merkle proof"
echo ""
echo "The Merkle proof was verified on-chain by the Plutus smart contract!"
echo "============================================================================="
