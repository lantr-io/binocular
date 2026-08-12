# NixOS module: Dolos, a self-hosted Cardano data node serving a Blockfrost-compatible API.
#
# Replaces hosted blockfrost.io for binocular (and heimdall). Dolos syncs over Ouroboros from a
# public preprod relay, so no local cardano-node is required.
#
# Everything here is declarative: the binary is a pinned release tarball, the genesis files are
# pinned fetchurl inputs, and dolos.toml is generated into the Nix store. Only the chain data
# lives out of store, in /var/lib/dolos.
#
# ONE-TIME BOOTSTRAP (not automated — see the `bootstrap` note at the bottom of this file):
#   sudo -u dolos dolos-bootstrap
#
# Design note: the default bootstrap is the LEDGER variant, which restores current ledger state
# with no historical blocks; the archive then grows forward from that point. That is a deliberate
# trade — see `archiveFromNow` below for what it costs.
{ config, lib, pkgs, ... }:

let
  cfg = config.services.dolos;

  # Pinned release. Update `version` + `hash` together; hash comes from the published
  # dolos-x86_64-unknown-linux-gnu.tar.gz.sha256 asset, converted to SRI.
  dolosPkg = pkgs.stdenv.mkDerivation rec {
    pname = "dolos";
    version = "1.6.0";

    src = pkgs.fetchurl {
      url = "https://github.com/txpipe/dolos/releases/download/v${version}/dolos-x86_64-unknown-linux-gnu.tar.gz";
      hash = "sha256-50Q1+uC1rWBFU8R6HqexIarGo5HGCoc4G+80R0viWFc=";
    };

    # The tarball unpacks into dolos-x86_64-unknown-linux-gnu/, not the root.
    sourceRoot = "dolos-x86_64-unknown-linux-gnu";
    nativeBuildInputs = [ pkgs.autoPatchelfHook ];
    buildInputs = [ pkgs.stdenv.cc.cc.lib pkgs.openssl ];

    installPhase = ''
      runHook preInstall
      install -Dm755 dolos $out/bin/dolos
      runHook postInstall
    '';

    meta = {
      description = "Cardano data node with a Blockfrost-compatible API";
      homepage = "https://github.com/txpipe/dolos";
      platforms = [ "x86_64-linux" ];
    };
  };

  # Preprod genesis files, pinned by content hash from the official environments endpoint.
  genesisFile = name: hash:
    pkgs.fetchurl {
      url = "https://book.play.dev.cardano.org/environments/${cfg.network}/${name}-genesis.json";
      inherit hash;
    };

  genesis = {
    byron = genesisFile "byron" cfg.genesisHashes.byron;
    shelley = genesisFile "shelley" cfg.genesisHashes.shelley;
    alonzo = genesisFile "alonzo" cfg.genesisHashes.alonzo;
    conway = genesisFile "conway" cfg.genesisHashes.conway;
  };

  tomlFormat = pkgs.formats.toml { };

  settings = lib.recursiveUpdate {
    upstream.peer_address = cfg.upstreamPeer;

    chain = {
      type = "cardano";
      magic = cfg.networkMagic;
      is_testnet = cfg.networkMagic != 764824073;
    };

    genesis = {
      byron_path = "${genesis.byron}";
      shelley_path = "${genesis.shelley}";
      alonzo_path = "${genesis.alonzo}";
      conway_path = "${genesis.conway}";
    };

    storage = {
      path = cfg.stateDir;
      # MUST be set. Dolos 1.6.0 defaults to V0 when this is absent and then refuses to start with
      # "unsupported storage version V0, only V3 is supported". It also selects the snapshot URL:
      # https://dolos-snapshots.txpipe.cloud/${version}/${magic}/${variant}/${point}.tar.gz
      version = cfg.storageVersion;
      # storage.archive.backend MUST NOT be "no_op": the archive is what answers history queries
      # (/addresses/{addr}/transactions, /txs/{hash}/utxos for a spent output). Leaving the
      # default keeps it enabled.
      #
      # NOTE: `sync.max_history` is deliberately NOT set. Setting it prunes archive history to a
      # sliding window, which silently truncates the reads the CPO trie reconstruction depends on.
    };

    # WAL retention. Left unbounded, the WAL grows without limit during a long catch-up: restoring
    # a ledger snapshot ~57 days behind tip pushed it past 1.8 GB and nearly filled a 38 GB disk.
    #
    # Rollbacks cannot exceed the stability window, 3k/f slots. On preprod (k = 2160, f = 0.05,
    # 1 s slots) that is 129600 slots, or 36 hours. Retaining that much is sufficient and bounds
    # the WAL to tens of MB.
    #
    # This prunes the WAL only. It is NOT sync.max_history, which would prune the ARCHIVE and
    # silently truncate the history the CPO trie reconstruction reads.
    sync.max_rollback = cfg.maxRollback;

    serve.minibf = {
      listen_address = cfg.listenAddress;
      # Default is 3000, which caps the page scan on heavy endpoints. A long-lived bridge address
      # can exceed that, and the failure mode is silent truncation rather than an error.
      max_scan_items = cfg.maxScanItems;
    };

    logging.max_level = cfg.logLevel;
  } cfg.extraSettings;

  configFile = tomlFormat.generate "dolos.toml" settings;

  # Wrapper so the operator never has to remember the config path.
  bootstrapScript = pkgs.writeShellScriptBin "dolos-bootstrap" ''
    set -euo pipefail
    echo "Bootstrapping Dolos (${cfg.network}, variant: ${cfg.bootstrapVariant})."
    echo "This refuses to run if ${cfg.stateDir} already holds data."
    exec ${dolosPkg}/bin/dolos --config ${configFile} \
      bootstrap snapshot --variant ${cfg.bootstrapVariant}
  '';

  dolosCli = pkgs.writeShellScriptBin "dolos-cli" ''
    exec ${dolosPkg}/bin/dolos --config ${configFile} "$@"
  '';
in
{
  options.services.dolos = {
    enable = lib.mkEnableOption "Dolos Cardano data node";

    package = lib.mkOption {
      type = lib.types.package;
      default = dolosPkg;
      description = "Dolos package to run.";
    };

    network = lib.mkOption {
      type = lib.types.str;
      default = "preprod";
      description = "Cardano network name, used to fetch the matching genesis files.";
    };

    networkMagic = lib.mkOption {
      type = lib.types.int;
      default = 1;
      description = "Network magic. preprod = 1, preview = 2, mainnet = 764824073.";
    };

    upstreamPeer = lib.mkOption {
      type = lib.types.str;
      default = "preprod-node.play.dev.cardano.org:3001";
      description = ''
        Ouroboros peer Dolos syncs from, as host:port.

        The default is the bootstrap peer from the official preprod topology.json. An SPO running
        their own node SHOULD point this at that node instead, which is the production topology
        described in the bridge's SPO backend document.
      '';
    };

    listenAddress = lib.mkOption {
      type = lib.types.str;
      default = "127.0.0.1:3000";
      description = ''
        Listen address for the Blockfrost-compatible (mini-Blockfrost) HTTP API.

        Keep this on localhost. mini-Blockfrost has no authentication of its own.
      '';
    };

    maxScanItems = lib.mkOption {
      type = lib.types.int;
      default = 100000;
      description = ''
        Page scan cap for heavy mini-Blockfrost endpoints. Dolos defaults to 3000, which can
        silently truncate `/addresses/{addr}/transactions` for a long-lived bridge address.
      '';
    };

    bootstrapVariant = lib.mkOption {
      type = lib.types.enum [ "ledger" "full" ];
      default = "ledger";
      description = ''
        Which snapshot `dolos-bootstrap` restores.

        `ledger` restores current ledger state only, with no historical blocks; the archive then
        grows forward from the restore point. It is far smaller (preprod: about 3 GB compressed,
        against about 10 GB for `full`).

        `full` restores the complete chain history.

        The cost of `ledger` is that history BEFORE the restore point is absent. Every current-state
        read still works. What breaks is completed-peg-outs trie reconstruction, which reads the
        full transaction history of the two bridge addresses. A truncated history yields a trie
        whose root does not match the on-chain root, and the POR sweeper then HALTS rather than
        submitting bad proofs. That is fail-safe and loud, not silent corruption, but it does stop
        peg-out cleanup until an operator intervenes.

        ORDERING RULE. Bootstrap Dolos BEFORE the bridge protocol is deployed on this network.
        The archive then covers the whole lifetime of the bridge, every completed peg-out falls
        after the restore point, and `ledger` costs nothing at all. This is why `ledger` is the
        default here: preprod had no deployed protocol when this module was written.

        Two ways to lose that property later, both silent:

        - Re-running `dolos-bootstrap` with `--variant ledger` AFTER deployment. It discards the
          accumulated archive and restarts it at today's tip, orphaning every earlier peg-out.
          Re-bootstrap with `full`, or restore `stateDir` from a backup, instead.
        - Setting `sync.max_history`. That prunes the archive to a sliding window, which walks the
          cutoff forward until it passes the bridge's own history.

        Choose `full` when the bridge already has completed peg-outs predating the restore point.
      '';
    };

    maxRollback = lib.mkOption {
      type = lib.types.int;
      default = 129600;
      description = ''
        Slots of WAL history to retain (`sync.max_rollback`).

        The default is preprod's stability window, 3k/f = 3 × 2160 / 0.05 = 129600 slots (36
        hours), beyond which a rollback cannot occur. Leaving this unset lets the WAL grow without
        bound during catch-up.

        This bounds the WAL only. It does NOT prune the archive; `sync.max_history` does that, and
        is deliberately left unset.
      '';
    };

    storageVersion = lib.mkOption {
      type = lib.types.str;
      default = "v3";
      description = ''
        On-disk storage schema version. Dolos 1.6.0 supports only `v3`, and refuses to start if
        this is left unset (it then defaults to V0). It also forms part of the snapshot URL, so it
        must match a version TxPipe publishes snapshots for.
      '';
    };

    logLevel = lib.mkOption {
      type = lib.types.str;
      default = "info";
      description = "Dolos log level.";
    };

    stateDir = lib.mkOption {
      type = lib.types.str;
      default = "/var/lib/dolos";
      description = "Chain data directory. This is the only out-of-store state.";
    };

    user = lib.mkOption {
      type = lib.types.str;
      default = "dolos";
      description = "Service user owning stateDir.";
    };

    genesisHashes = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = {
        byron = "sha256-2I+//feNqsz63fUE6VhAxzzlJ8BvpBQK77VdP5HADO8=";
        shelley = "sha256-S50ywJFZwpSOQ4a6H1nbWiSaibQ7hN/YNo9GXmUAld4=";
        alonzo = "sha256-czO/r+MRWJ+gnov1mkfsDYWhlZ8AdIzAgAWR0sdkZAg=";
        conway = "sha256-wZaBT+Lo82rRkQ5cKHGElwM5ZXcp561Pw1TDThSb4/g=";
      };
      description = ''
        Content hashes of the genesis files for `network`. The defaults are for preprod.
        Changing `network` requires replacing these.
      '';
    };

    memoryMax = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = "1500M";
      description = ''
        systemd MemoryMax for the service. dev.lantr.io is a 4 GB box already running bitcoind and
        binocular, so Dolos is capped rather than left to compete with them. Set to null to remove
        the cap.
      '';
    };

    extraSettings = lib.mkOption {
      type = lib.types.attrs;
      default = { };
      description = "Extra dolos.toml settings, merged over the generated ones.";
    };
  };

  config = lib.mkIf cfg.enable {
    users.users.${cfg.user} = {
      isSystemUser = true;
      group = cfg.user;
      home = cfg.stateDir;
    };
    users.groups.${cfg.user} = { };

    environment.systemPackages = [ bootstrapScript dolosCli ];

    systemd.services.dolos = {
      description = "Dolos Cardano data node (mini-Blockfrost on ${cfg.listenAddress})";
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        Type = "simple";
        User = cfg.user;
        Group = cfg.user;
        StateDirectory = baseNameOf cfg.stateDir;
        ExecStart = "${cfg.package}/bin/dolos --config ${configFile} daemon";
        Restart = "always";
        RestartSec = 10;

        # Hardening, matching binocular-watchtower.nix.
        NoNewPrivileges = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        PrivateTmp = true;
        ReadWritePaths = [ cfg.stateDir ];
      } // lib.optionalAttrs (cfg.memoryMax != null) {
        MemoryMax = cfg.memoryMax;
      };
    };
  };
}
