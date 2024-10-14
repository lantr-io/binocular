# Binocular

## Abstract

Binocular is a decentralized optimistic Bitcoin Oracle on Cardano.

## Overview

- Anyone can become a Miner by committing a deposit into a Fraud Bond contract and minting a Miner token.
- Miners can create and update Oracle TxOuts with a Bitcoin block header in the Datum.
- Block header hash, block height and block chainwork are verified by the Binocular contract.
- Miners can withdraw the Fraud Bond by buring the Miner token after the deposit timeout.
- Anyone can challenge the Datum by submitting a Fraud Proof and take the Fraud Bond.

Fraud is when a published Bitcoin block header is not in the longest chain.

Fraud Proof is a Bitcoin block header of the same height or
a timestamp within a same time range as the disputed block but with a higher chainwork.

- Mint a Miner token by depositing Fraud Bond into the contract.
- Minted Token name contains the deposit timeout.
- Miner token holders can submit Bitcoin a block header to the contract Datum.
- Miners can withdraw the Fraud Bond after the deposit timeout.
- Anyone can challenge the Datum by submitting a Fraud Proof and take the Fraud Bond.

## Design

### Fraud Bond

Fraud Bond is a UTXO locked by a validator script that can be spend if:

1. if timeout passed and the creator signature is valid
2. fraud proof is provided

### Miner Token

Minting a Miner token checks:

1. Fraud Bond is valid
   1. is unspent: the TxOut of `Bond` validator hash is referenced in the transaction
   2. has > `minDeposit` ADA locked
   3. has timeout > `minTimeout` in the future
2. Oracle TxOut is created
   1. it's a first TxOut
   2. TxOut contains the Miner token as a proof this Oracle TxOut was checked by the Miner contract
   3. Datum has a predefined valid `initial Block header`
   4. Datum has a `ownerPubKeyHash` and the transaction is signed by the owner

Burning a Miner token checks:

1. Oracle TxOut:
   1. is spent
   2. has a valid signature of the owner

### Updating the Oracle

Updating the Oracle checks:

1. Miner token is preserved
2. New TxOut Datum is updated
   1. Datum has a valid `block header hash`
   2. Datum has a valid `block height`
      1. is higher than the previous block height
      2. checks the Coinbase tx hash inclusion proof against the block header merkle root hash
   3. Datum has a valid `chainwork`
      1. block header hash is < `target` according to the block header
   4. Block `timestamp` is in the past
   5. Datum has a valid `ownerPubKeyHash`
   6. Datum is signed by the owner

### Fraud Proof

Submitting a Fraud Proof checks:

1.

## Security Assumptions

- Mining a Bitcoin block for block $height ∈ (initialBlockHight, currentBlockHight)$
is more expensive than the Fraud Bond deposit value
- SHA256 is a secure hash function
- Ed25519 is a secure signature scheme
