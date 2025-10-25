# Claude Code Guidelines for Binocular Project

## Project Overview

Binocular is a Bitcoin oracle for Cardano that enables smart contracts to access and verify Bitcoin
blockchain state. The protocol allows anyone to submit Bitcoin block headers to a single on-chain
Oracle UTxO without registration or bonding requirements. All blocks are validated against Bitcoin
consensus rules (proof-of-work, difficulty adjustment, timestamp constraints) enforced by a Plutus
smart contract written in Scalus.

**Key Features:**

- Complete on-chain Bitcoin validation (PoW, difficulty adjustment, timestamps)
- Permissionless participation - anyone can submit blocks
- Single-UTxO architecture with automatic canonical chain selection
- Challenge period mechanism (200 minutes) to prevent pre-computed attacks
- Transaction inclusion proofs for cross-chain applications

## Quick Reference for Claude Code

**Essential Commands:**

- `sbt compile` - Compile the project
- `sbt test` - Run all tests
- `./generate-pdfs.sh` - Generate PDF versions of whitepapers (requires LaTeX)

**Key Patterns:**

- Bitcoin consensus validation is in `BitcoinValidator.scala`
- All Bitcoin constants match Bitcoin Core (`chainparams.cpp`, `validation.cpp`)
- Reference Bitcoin Core source code comments for algorithm implementations
- Study Whitepaper.md for protocol specifications and algorithm details

## Key Principles for Claude Code

- **Always reference Bitcoin Core implementations** - Each algorithm includes Bitcoin Core source
  references
- **Use type aliases for semantic clarity** - `BlockHash` not `ByteString`, `CompactBits` not
  `ByteString`
- **Maintain Bitcoin consensus compatibility** - All validation rules must match Bitcoin Core
  exactly
- **Keep whitepaper and code synchronized** - Update both when making changes to data structures or
  algorithms
- **Prefer editing existing files over creating new ones** - Only create files when absolutely
  necessary
- **Test against real Bitcoin data** - Use `HeaderSyncWithRpc` to validate against actual Bitcoin
  blockchain
- **Follow implementation references** - Each algorithm in whitepaper references specific line
  numbers

## Essential Commands

### Build and Development

```bash
# Compile the project
sbt compile

# Clean and compile
sbt clean compile

# Interactive sbt shell (recommended for development)
sbt
# Then inside sbt shell:
> compile
> test
> testOnly *BitcoinValidatorSpec
```

### Testing

```bash
# Run all tests
sbt test

# Run tests with timeout (useful for validator tests)
timeout 60 sbt test
timeout 120 sbt test  # for longer tests

# Test with specific Bitcoin RPC connection (requires bitcoind)
export bitcoind_rpc_url="http://localhost:8332"
export bitcoind_rpc_user="bitcoin"
export bitcoind_rpc_password="your_password"
timeout 60s sbt "runMain binocular.HeaderSyncWithRpc"
```

### Documentation

```bash
# Generate PDF versions of Litepaper and Whitepaper
./make-pdfs.sh
```

## Architecture Overview

### Module Structure

- **`src/main/scala/binocular/`** - Core source code
    - `BitcoinValidator.scala` - Main Plutus validator with Bitcoin consensus validation
    - `bitcoin.scala` - Bitcoin RPC integration and header sync utilities
    - `merkle.scala` - Merkle tree operations for transaction inclusion proofs
    - `main.scala` - CLI interface (if applicable)

- **Documentation**
    - `Whitepaper.md` - Complete technical specification (97 pages)
    - `Litepaper.md` - Concise overview (8 pages)

### Data Structures

**Core Oracle State:**

```scala
case class ChainState(
                       // Confirmed state
                       blockHeight: BigInt,
                       blockHash: BlockHash,
                       currentTarget: CompactBits,
                       blockTimestamp: BigInt,
                       recentTimestamps: List[BigInt],
                       previousDifficultyAdjustmentTimestamp: BigInt,
                       confirmedBlocksRoot: MerkleRoot,

                       // Forks tree
                       forksTree: Map[BlockHash, BlockNode]
                     )

case class BlockNode(
                      prevBlockHash: BlockHash,
                      chainwork: BigInt,
                      addedTimestamp: BigInt,
                      children: List[BlockHash]
                    )
```

**Type Aliases:**

```scala
type BlockHash = ByteString // 32-byte SHA256d hash
type TxHash = ByteString // 32-byte transaction hash
type MerkleRoot = ByteString // 32-byte Merkle root
type CompactBits = ByteString // 4-byte difficulty target
type BlockHeaderBytes = ByteString // 80-byte raw header
```

### Validation Pipeline

1. **Block Submission** - Anyone submits Bitcoin block header(s)
2. **Validation** - On-chain validator checks:
    - Proof-of-Work (hash ≤ target)
    - Difficulty adjustment (matches expected retarget)
    - Timestamps (> median-time-past, < current + 2 hours)
    - Version (≥ 4)
    - Chain continuity
3. **Fork Tree Update** - Add to forks tree as `BlockNode`
4. **Canonical Selection** - Automatically select highest chainwork fork
5. **Block Promotion** - Move blocks meeting criteria (100+ confirmations, 200+ min age) to
   confirmed state

### Key Bitcoin Consensus Constants

All constants match Bitcoin Core exactly:

```scala
UnixEpoch: BigInt
= 1231006505 // Bitcoin genesis
TargetBlockTime: BigInt
= 600 // 10 minutes
DifficultyAdjustmentInterval: BigInt
= 2016 // Every 2016 blocks
MaxFutureBlockTime: BigInt
= 7200 // 2 hours
MedianTimeSpan: BigInt
= 11 // For median-time-past
PowLimit: BigInt
= 0x00000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffff
MaturationConfirmations: BigInt
= 100 // For promotion
ChallengeAging: BigInt
= 200 * 60 // 200 minutes
```

## Development Guidelines

### Working with Bitcoin Consensus Validation

**Always match Bitcoin Core exactly:**

- Reference comments indicate Bitcoin Core source:
  `// matches CBlockHeader::GetHash() in primitives/block.h`
- Use same variable names where possible
- Implement the same logic flow
- Test against real Bitcoin blockchain data

**Key Algorithms and Bitcoin Core References:**

1. **Compact Bits Conversion** (`BitcoinValidator.scala:120-137`)
    - Matches `arith_uint256::SetCompact()` in `arith_uint256.cpp`

2. **Target to Compact** (`BitcoinValidator.scala:145-186`)
    - Matches `arith_uint256::GetCompact()` in `arith_uint256.cpp`

3. **Block Header Hash** (`BitcoinValidator.scala:89-90`)
    - Matches `CBlockHeader::GetHash()` in `primitives/block.h`

4. **Proof-of-Work** (`BitcoinValidator.scala:357-361`)
    - Matches `CheckProofOfWork()` in `pow.cpp:140-163`

5. **Median Time Past** (`BitcoinValidator.scala:192-198`)
    - Matches `CBlockIndex::GetMedianTimePast()` in `chain.h:278-290`

6. **Difficulty Adjustment** (`BitcoinValidator.scala:315-343`)
    - Matches `GetNextWorkRequired()` and `CalculateNextWorkRequired()` in `pow.cpp:14-84`

### Working with Type Aliases

**Always use semantic type aliases instead of raw ByteString:**

❌ **Wrong:**

```scala
def blockHeaderHash(header: BlockHeader): ByteString
def getTxHash(rawTx: ByteString): ByteString
```

✅ **Correct:**

```scala
def blockHeaderHash(header: BlockHeader): BlockHash
def getTxHash(rawTx: ByteString): TxHash
```

This improves type safety and makes code self-documenting.

### Synchronizing Code and Documentation

**When modifying data structures or algorithms:**

1. Update `BitcoinValidator.scala` first
2. Update corresponding section in `Whitepaper.md`
3. Update `Litepaper.md` if it's a major change
4. Verify line number references in Whitepaper are still accurate
5. Update type signatures in pseudocode to match Scala code

**Example workflow:**

```bash
# 1. Edit BitcoinValidator.scala
# 2. Note the new line numbers
# 3. Update Whitepaper.md:
#    - Data structure definitions (lines 157-181)
#    - Algorithm pseudocode (various sections)
#    - Implementation references (e.g., "BitcoinValidator.scala:120-137")
```

### Testing with Bitcoin RPC

**Setup Bitcoin Core RPC connection:**

```bash
# Export environment variables
export bitcoind_rpc_url="http://localhost:8332"
export bitcoind_rpc_user="bitcoin"
export bitcoind_rpc_password="your_password"

# Run header sync validation
timeout 60s sbt "runMain binocular.HeaderSyncWithRpc"
```

This validates the on-chain implementation against real Bitcoin blockchain data.

## Important Files

### Source Code

- **`src/main/scala/binocular/BitcoinValidator.scala`** - Main Plutus validator (460+ lines)
    - Complete Bitcoin consensus validation
    - All core algorithms with Bitcoin Core references
    - Type aliases for semantic clarity

- **`src/main/scala/binocular/bitcoin.scala`** - Bitcoin integration utilities
    - RPC client integration (`HeaderSyncWithRpc`)
    - Bitcoin header parsing
    - Coinbase transaction handling

- **`src/main/scala/binocular/merkle.scala`** - Merkle tree operations
    - Merkle proof verification
    - Tree construction

- **`build.sbt`** - Build configuration
    - Scalus compiler plugin
    - Dependencies (bitcoin-s, cardano-client-lib)

### Documentation

- **`Whitepaper.md`** - Complete technical specification (97 pages)
    - Protocol specification with all data structures
    - All algorithms with mathematical specifications and pseudocode
    - Security analysis with formal proofs
    - Implementation references to source code

- **`Litepaper.md`** - Concise overview (8 pages)
    - High-level protocol description
    - Key concepts and security properties
    - Future work (BiFROST integration)

### Scripts

- **`make-pdfs.sh`** - Generate PDF versions of whitepapers

## Common Tasks

### Adding a New Bitcoin Validation Rule

1. Find the corresponding Bitcoin Core implementation
2. Add the validation logic to `BitcoinValidator.scala`
3. Add implementation references in comments
4. Add to `Whitepaper.md`:
    - Algorithm specification section
    - Mathematical specification
    - Pseudocode
    - Implementation reference
5. Update `Validation Rules Summary` section
6. Test against real Bitcoin data using `HeaderSyncWithRpc`

### Adding a New Type Alias

1. Add to type alias definitions in `BitcoinValidator.scala` (lines 21-28)
2. Update function signatures that use the type
3. Add to type aliases section in `Whitepaper.md` (lines 144-152)
4. Update data structure definitions in `Whitepaper.md`
5. Update algorithm pseudocode to use the new type

### Updating Protocol Parameters

1. Modify constants in `BitcoinValidator.scala` (lines 22-30)
2. Update constants in `Whitepaper.md` (lines 196-205)
3. Update parameter justification section if applicable
4. Update security analysis if parameters affect security properties

### Modifying Data Structures

1. Update case class in `BitcoinValidator.scala`
2. Ensure `derives FromData, ToData` is present
3. Update corresponding definition in `Whitepaper.md` (lines 157-181)
4. Update capacity analysis in `Whitepaper.md` if datum size changes
5. Update all functions that use the structure

## Testing Guidelines

### Unit Tests

- Test individual validation functions
- Use ScalaCheck for property-based testing
- Test edge cases and boundary conditions

### Integration Tests

- Use `HeaderSyncWithRpc` to validate against real Bitcoin blockchain
- Test fork scenarios
- Test difficulty adjustments at 2016-block boundaries
- Test timestamp edge cases

### Testing Checklist

Before considering work complete:

- [ ] Unit tests pass: `sbt test`
- [ ] Integration test with Bitcoin RPC passes (if applicable)
- [ ] No compilation errors or warnings
- [ ] Code follows existing patterns and conventions
- [ ] Type aliases used correctly throughout
- [ ] Whitepaper updated to match code changes
- [ ] Implementation references are accurate

## Before Submitting Changes

### Pre-submission Checklist

1. **Code compiles cleanly**: `sbt compile`
2. **Tests pass**: `sbt test`
3. **Code follows patterns**: Check similar existing code
4. **Type aliases used**: No raw `ByteString` where type aliases exist
5. **Documentation updated**:
    - [ ] Whitepaper.md updated
    - [ ] Litepaper.md updated (if major change)
    - [ ] Implementation references accurate
    - [ ] Line numbers in references updated
6. **Bitcoin Core references preserved**: All algorithm comments intact
7. **Testing completed**: Integration tests run if applicable

### Commit Messages

- Use clear, descriptive commit messages
- Reference specific algorithms or sections modified
- Examples:
    - "feat: add type aliases for BlockHash and CompactBits"
    - "fix: correct median-time-past calculation to match Bitcoin Core"
    - "docs: update whitepaper capacity analysis for new datum structure"
- Never add "Co-authored by Claude Code" or similar
- Focus on the "why" not just the "what"

## Protocol-Specific Guidelines

### Security-Critical Code

**Extra care required for:**

- Proof-of-work validation
- Difficulty adjustment calculations
- Timestamp validation
- Chainwork calculations
- Block promotion logic

These must match Bitcoin Core exactly and be thoroughly tested.

### Datum Size Considerations

**Be mindful of Cardano constraints:**

- Maximum transaction size: 16,384 bytes (16 KB)
- Transaction overhead: ~1,000 bytes
- Available for datum: ~15,384 bytes
- Forks tree capacity: ~172-210 blocks

When adding fields to data structures:

1. Calculate new per-block storage size
2. Update capacity analysis in Whitepaper.md
3. Ensure forks tree can still accommodate typical scenarios

### Challenge Period and Liveness

**Protocol relies on:**

- 100-block confirmation requirement (Bitcoin standard)
- 200-minute on-chain aging requirement (challenge period)
- 1-honest-party assumption

Changes affecting these parameters require security analysis updates.

## Resources

### Bitcoin Core References

- **Source Code**: https://github.com/bitcoin/bitcoin
- **Key Files**:
    - `src/arith_uint256.cpp` - Compact bits conversion
    - `src/pow.cpp` - Proof-of-work and difficulty
    - `src/validation.cpp` - Block header validation
    - `src/chain.h` - Median-time-past
    - `src/chainparams.cpp` - Consensus parameters

### Cardano & Scalus

- **Scalus**: https://scalus.org - Scala to Plutus compiler
- **Cardano Docs**: https://docs.cardano.org/
- **Plutus**: https://plutus.cardano.org/

### Project Documentation

- Start with `Litepaper.md` for high-level overview
- Read `Whitepaper.md` for complete technical details
- Study `BitcoinValidator.scala` for implementation patterns

## Development Environment

### Using Nix (Recommended)

```bash
nix develop
```

### Manual Setup

Ensure you have:

- Scala 3.3.7+
- SBT 1.x
- JDK 21 (for compatibility with bitcoin-s library)

### Bitcoin Core RPC (Optional)

For integration testing:

```bash
# Install Bitcoin Core
# Configure bitcoin.conf with RPC credentials
# Start bitcoind
# Export RPC credentials as environment variables
```

## Claude Code Specific Notes

### When Working with Validation Logic

- Study the corresponding Bitcoin Core implementation first
- Check Whitepaper.md for the algorithm specification
- Look for existing similar validation functions
- Preserve Bitcoin Core reference comments
- Test against real Bitcoin data when possible

### When Working with Documentation

- Whitepaper.md is the source of truth for protocol specification
- Keep code and documentation synchronized
- Update implementation references when code changes
- Maintain consistency in type usage (type aliases vs ByteString)

### File Organization Patterns

- Bitcoin validation: `BitcoinValidator.scala`
- Bitcoin integration: `bitcoin.scala`
- Cryptographic operations: `merkle.scala`
- Documentation: `Whitepaper.md`, `Litepaper.md`

### Common Pitfalls to Avoid

❌ **Don't:**

- Use `ByteString` where type aliases exist
- Modify Bitcoin constants without Bitcoin Core justification
- Skip updating implementation references in Whitepaper
- Change validation logic without checking Bitcoin Core
- Ignore datum size constraints

✅ **Do:**

- Use type aliases consistently
- Match Bitcoin Core implementations exactly
- Keep whitepaper and code synchronized
- Test against real Bitcoin blockchain data
- Consider Cardano transaction size limits

---

**Remember**: Binocular is security-critical infrastructure for cross-chain interoperability. Code
changes must be thoroughly validated against Bitcoin consensus rules and tested extensively.
