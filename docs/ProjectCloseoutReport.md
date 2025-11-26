# Binocular - Project Close Out Report

## Project Identification

| Field                 | Value                                               |
|-----------------------|-----------------------------------------------------|
| **Project Name**      | Binocular – Trustless Bitcoin Transaction Oracle    |
| **Project ID**        | 1100038                                             |
| **Fund**              | Fund 11                                             |
| **Challenge**         | Cardano Use Cases: Concept                          |
| **Proposal URL**      | https://cardano.ideascale.com/c/cardano/idea/114376 |
| **Project Manager**   | Alexander Nemish                                    |
| **Start Date**        | March 11, 2024                                      |
| **Completion Date**   | November 26, 2025                                   |
| **GitHub Repository** | https://github.com/lantr-io/binocular               |

---

## Project Summary

Binocular is a trustless Bitcoin oracle for Cardano that enables smart contracts to verify Bitcoin
blockchain state without relying on trusted third parties. The protocol allows anyone to submit
Bitcoin block headers to a single on-chain Oracle UTxO, with all blocks validated against Bitcoin
consensus rules (proof-of-work, difficulty adjustment, timestamp constraints) enforced entirely by a
Plutus smart contract.

### Problem Addressed

Before Binocular, there was no way to trustlessly verify Bitcoin transaction existence within a
Cardano smart contract. Existing solutions required trusted oracles or centralized data feeds,
introducing security vulnerabilities and trust assumptions that undermined the decentralized nature
of both blockchains.

### Solution Delivered

Binocular provides:

- **Complete on-chain Bitcoin validation** - PoW, difficulty adjustment, and timestamps verified by
  Plutus validator
- **Permissionless participation** - Anyone can submit blocks without registration or bonding
- **Single-UTxO architecture** - Efficient design with automatic canonical chain selection
- **Challenge period mechanism** - 200-minute aging requirement prevents pre-computed attacks
- **Transaction inclusion proofs** - Merkle proofs enable cross-chain applications

---

## Performance Against Goals

### Challenge KPIs

| Challenge KPI                           | How Addressed                                                                                                  |
|-----------------------------------------|----------------------------------------------------------------------------------------------------------------|
| Demonstrate innovative Cardano use case | Delivered first trustless Bitcoin oracle on Cardano, enabling cross-chain verification without trusted parties |
| Provide concept validation              | Complete proof-of-concept with working smart contract, off-chain client, and testnet demonstrations            |
| Open source deliverables                | All code, documentation, and specifications publicly available on GitHub under open source license             |

### Project KPIs

| Project KPI                   | Status     | Evidence                                                                                                                    |
|-------------------------------|------------|-----------------------------------------------------------------------------------------------------------------------------|
| Litepaper published           | ✅ Complete | [Litepaper.md](https://github.com/lantr-io/binocular/blob/v0.1.0/Litepaper.md)                                              |
| Whitepaper published          | ✅ Complete | [Whitepaper.md](https://github.com/lantr-io/binocular/blob/v0.1.0/Whitepaper.md)                                            |
| Smart contract implementation | ✅ Complete | [BitcoinValidator.scala](https://github.com/lantr-io/binocular/blob/v0.1.0/src/main/scala/binocular/BitcoinValidator.scala) |
| Off-chain client              | ✅ Complete | [CLI Application](https://github.com/lantr-io/binocular/blob/v0.1.0/src/main/scala/binocular/cli/CliApp.scala)              |
|

---

## Key Achievements

### Technical Deliverables

1. **Comprehensive Documentation**
    - Litepaper providing accessible protocol overview with security analysis
    - Whitepaper with complete technical specification, formal state machine, and security proofs
    - PDF versions generated for offline access

2. **Smart Contract Implementation**
    - Plutus validator written in Scalus (Scala to Plutus compiler)
    - Full Bitcoin consensus validation matching Bitcoin Core implementations
    - Efficient fork management with automatic canonical chain selection
    - Block promotion logic with 100-confirmation and 200-minute challenge period

3. **Off-Chain Client**
    - Command-line application for Oracle interaction
    - Commands: init-oracle, update-oracle, prove-transaction, list-oracles, verify-oracle
    - Support for multiple Cardano backends (Blockfrost, Koios, Ogmios)
    - Bitcoin Core RPC integration for block header fetching

4. **Test Suite**
    - Unit tests for validator logic
    - Integration tests using Yaci DevKit
    - Real Bitcoin block fixtures for validation testing

### Security Analysis

The whitepaper includes comprehensive security analysis:

- Economic attack cost analysis ($46M+ to attack)
- Formal security theorems for safety and liveness
- Analysis of 5 attack scenarios with mitigations
- Challenge period sufficiency proofs

---

## Impact & Community Value

### Innovation

Binocular introduces several innovations to the Cardano ecosystem:

1. **First Trustless Bitcoin Oracle** - No existing solution provides fully on-chain Bitcoin
   validation on Cardano
2. **1-Honest-Party Assumption** - Security requires only one honest participant, unlike multi-sig
   or committee approaches
3. **Permissionless Design** - Anyone can participate without staking, registration, or approval
4. **Economic Security** - Leverages Bitcoin's proof-of-work for security guarantees

### Use Cases Enabled

- **Cross-chain atomic swaps** - Trustless BTC/ADA exchanges
- **Bitcoin-backed assets** - Verifiable Bitcoin collateral on Cardano
- **Cross-chain bridges** - Secure asset transfers between chains.
- **Bitcoin payment verification** - Prove Bitcoin payments in Cardano contracts

### Ecosystem Contribution

- Open source codebase available for community use and extension
- Detailed documentation enabling developers to understand and build upon the protocol
- Reference implementation demonstrating Scalus for complex on-chain logic

---

## Lessons Learned

### Technical Challenges

1. **Datum Size Constraints** - Cardano's 16KB transaction limit required careful optimization of
   fork tree storage
2. **Bitcoin Consensus Complexity** - Matching Bitcoin Core's exact validation logic required
   careful implementation
3. **Time Handling** - Coordinating Bitcoin block timestamps with Cardano validity intervals
   required design consideration

### What Worked Well

- Scalus compiler enabled complex validation logic in readable Scala code
- Incremental development with comprehensive testing caught issues early
- Detailed whitepaper writing clarified edge cases before implementation

---

## Future Direction

### Planned Enhancements

1. **BiFROST Integration** - Extend to support BiFROST cross-chain protocol
2. **Participation Incentives** - Design tokenomics for oracle updaters
3. **Mainnet Deployment** - Production deployment with monitoring and tooling

### Sustainability

The project is designed for long-term sustainability:

- Permissionless operation requires no central coordinator
- Open source allows community maintenance and improvement
- Economic incentive design planned for future versions

---

## Project Resources

### Documentation

- [Litepaper](https://github.com/lantr-io/binocular/blob/v0.1.0/Litepaper.md)
- [Whitepaper](https://github.com/lantr-io/binocular/blob/v0.1.0/Whitepaper.md)
- [PDF Documents](https://github.com/lantr-io/binocular/tree/v0.1.0/pdfs)

### Source Code

- [GitHub Repository](https://github.com/lantr-io/binocular)
- [Smart Contract](https://github.com/lantr-io/binocular/blob/v0.1.0/src/main/scala/binocular/BitcoinValidator.scala)
- [CLI Application](https://github.com/lantr-io/binocular/blob/v0.1.0/src/main/scala/binocular/cli/CliApp.scala)

### Media

- [Project Close-Out Video](https://www.youtube.com/watch?v=ezGDQzug59A)

---

## Conclusion

Binocular successfully delivered a proof-of-concept trustless Bitcoin oracle for Cardano, meeting
all milestone objectives. The project provides comprehensive documentation, a working smart contract
implementation, and off-chain tooling that demonstrates the viability of cross-chain Bitcoin
verification on Cardano. The open source deliverables enable the community to build upon this work
and explore new cross-chain applications.
