# Binocular – a decentralized optimistic Bitcoin Oracle on Cardano.

## How to build

Using Nix with flakes and/or direnv is recommended.

    nix develop

or

    direnv allow

This will install all the dependencies and set up the environment.

    scala-cli setup-ide .
    scala-cli test .

Setup `bitcoind_rpc_url`, `bitcoind_rpc_user`, `bitcoind_rpc_password` environment variables to
download the Bitcoin headers.