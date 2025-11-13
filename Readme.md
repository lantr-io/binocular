# Binocular – a decentralized Bitcoin Oracle on Cardano.

## How to build

Using Nix with flakes and/or direnv is recommended.

    nix develop

or

    direnv allow

This will install all the dependencies and set up the environment.

    scala-cli setup-ide .
    scala-cli test .

## Bitcoin Core Setup

To test with real Bitcoin data, you need Bitcoin Core running locally or access to a remote node.

**Quick setup:**
```bash
export bitcoind_rpc_url="http://localhost:8332"
export bitcoind_rpc_user="bitcoin"
export bitcoind_rpc_password="your_password"
```

**For detailed setup instructions, see [docs/BITCOIN_SETUP.md](docs/BITCOIN_SETUP.md)**

This includes:
- Installing Bitcoin Core
- Configuration (`bitcoin.conf`)
- Running mainnet, testnet, or regtest
- Troubleshooting common issues

## Documentation

### Generating PDFs

To generate PDF versions of the Litepaper and Whitepaper with properly rendered Mermaid diagrams:

```bash
./generate-pdfs.sh
```

This script:
- Processes Mermaid diagrams and converts them to PNG images
- Generates PDFs using pandoc with proper formatting
- Outputs PDFs to the `./pdfs/` directory

**Requirements:**
- All dependencies are included in the Nix development environment
- The script uses `mermaid-cli` (mmdc) for diagram rendering, which requires a browser for headless rendering
- **On macOS**: You need to install Chrome or Chromium separately:
  ```bash
  brew install --cask google-chrome
  # or
  brew install --cask chromium
  ```
- **On Linux**: Chromium is provided through Nix
- PDFs are generated with table of contents, syntax highlighting, and professional formatting

**Output files:**
- `pdfs/Litepaper.pdf`
- `pdfs/Whitepaper.pdf`

### Development Environment

The Nix development environment includes all necessary tools:
- Scala development tools (scala-cli, scalafmt, openjdk23)
- Documentation tools (pandoc, texlive, mermaid-cli)
- Bitcoin RPC testing tools