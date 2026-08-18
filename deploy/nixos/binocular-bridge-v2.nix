# NixOS module: the Bifrost Bridge v2 TM relay + TM confirm loops.
#
# A SECOND bridge beside `binocular-watchtower`, on the same box, against the SAME oracle and the
# same bitcoind. It runs three units and NOT the `watchtower` command, because that command always
# starts the oracle sync worker too — and only one process on this box may write to the oracle
# UTxO, which is `binocular-watchtower`'s job. Those three are exactly what `watchtower` would
# have started minus that worker.
#
#   binocular-bridge-v2-relay    → `relay`         (Cardano → Bitcoin broadcast)
#   binocular-bridge-v2-confirm  → `confirm-tmtx`  (Bitcoin inclusion → Cardano Confirmed, and the
#                                                   POR sweeper that follows each confirm)
#   binocular-bridge-v2-proofs   → `serve-proofs`  (the [SPI-4]/[OB-13] REST API the frontend
#                                                   builds its complete-peg-in from)
#
# Out-of-store files, deployed by hand (deploy/deploy.sh ships v1's; this one is copied alongside):
#   /var/lib/binocular/binocular-v2.jar             (fat jar; NOT v1's binocular.jar)
#   /var/lib/binocular/application-preprod-v2.conf  (non-secret HOCON config)
#   /var/lib/binocular/secrets.env                  (shared with v1; WALLET_MNEMONIC et al)
#
# Nothing here manages bitcoind: `binocular-watchtower.nix` already runs the testnet4 node both
# bridges read.
{ config, lib, pkgs, ... }:

let
  cfg = config.services.binocular-bridge-v2;

  # The two loops differ only in their subcommand, so the unit is written once.
  mkLoop = name: subcommand: description: {
    inherit description;
    after = [ "network-online.target" "bitcoind-watchtower.service" ];
    wants = [ "network-online.target" ];
    wantedBy = [ "multi-user.target" ];

    serviceConfig = {
      Type = "simple";
      User = cfg.user;
      Group = cfg.user;
      # Creates /var/lib/binocular-v2 owned by the user, and makes it writable under
      # ProtectSystem=strict. It holds cpo-trie.json — v2's own mirror of the completed-peg-outs
      # root, which must never be shared with v1's (a mirror reconciled against the wrong root
      # HALTS the sweeper).
      StateDirectory = "binocular-v2";
      EnvironmentFile = cfg.secretsFile;
      ExecStart = ''
        ${cfg.jdk}/bin/java --sun-misc-unsafe-memory-access=allow \
          -Xmx${toString cfg.heapMb}m \
          -jar ${cfg.stateDir}/${cfg.jarFile} \
          --config ${cfg.stateDir}/${cfg.configFile} ${subcommand}
      '';
      Restart = "always";
      RestartSec = 10;
      # Exit code 3 = unrecoverable watchtower state. Restarting only re-detects it.
      RestartPreventExitStatus = "3";

      # Hardening, matching binocular-watchtower.nix.
      NoNewPrivileges = true;
      ProtectSystem = "strict";
      ProtectHome = true;
      PrivateTmp = true;
      ReadWritePaths = [ "/var/lib/binocular-v2" ];
    };
  };
in
{
  options.services.binocular-bridge-v2 = {
    enable = lib.mkEnableOption "Bifrost Bridge v2 relay + confirm loops";

    jdk = lib.mkOption {
      type = lib.types.package;
      default = pkgs.openjdk25;
      description = "JDK used to run the fat jar (needs JDK 23+ for --sun-misc-unsafe-memory-access).";
    };

    stateDir = lib.mkOption {
      type = lib.types.str;
      default = "/var/lib/binocular";
      description = "Directory holding the jar, config, and secrets. Shared with v1 on purpose: the secrets file is the same wallet.";
    };

    jarFile = lib.mkOption {
      type = lib.types.str;
      default = "binocular-v2.jar";
      description = "Jar filename within stateDir. Deliberately NOT binocular.jar — overwriting that one would change the running v1 demo on its next restart.";
    };

    configFile = lib.mkOption {
      type = lib.types.str;
      default = "application-preprod-v2.conf";
      description = "Config filename within stateDir passed to --config.";
    };

    secretsFile = lib.mkOption {
      type = lib.types.str;
      default = "/var/lib/binocular/secrets.env";
      description = "EnvironmentFile with WALLET_MNEMONIC and BLOCKFROST_PROJECT_ID (mode 600). Shared with v1.";
    };

    user = lib.mkOption {
      type = lib.types.str;
      default = "binocular";
      description = "Service user. Defaults to the user binocular-watchtower.nix already creates.";
    };

    heapMb = lib.mkOption {
      type = lib.types.int;
      default = 384;
      description = ''
        -Xmx for each loop, in MiB. The box has ~1.5 GB available with bitcoind, Dolos and v1's
        JVM (~310 MB RSS) resident, and this module adds two more JVMs. Lower it to 256 if the box
        starts swapping.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    # No users.users block: binocular-watchtower.nix creates this user, and both modules are
    # imported together. Declaring it twice is what a conflicting `home` would break.
    systemd.services.binocular-bridge-v2-relay =
      mkLoop "relay" "relay" "Bifrost Bridge v2 TM relay (Cardano to Bitcoin)";
    systemd.services.binocular-bridge-v2-confirm =
      mkLoop "confirm" "confirm-tmtx" "Bifrost Bridge v2 TM confirm (Bitcoin inclusion to Cardano)";

    # [SPI-4]/[OB-13]: the swept-peg-ins membership proof and the deposit-inclusion bundle, over
    # HTTP, which is what the frontend's complete-peg-in is built from. `watchtower` runs this
    # in-process, but v2 does not run `watchtower` - that would start the oracle worker too - so
    # `serve-proofs` is its own unit.
    #
    # Its port comes from bridge.proof-server-port in the config, not from a flag, and
    # ProofServer binds 0.0.0.0 with no option to narrow it (ProofServer.scala:73). That port
    # MUST therefore stay out of networking.firewall.allowedTCPPorts: the firewall is the only
    # thing keeping the API off the public internet, and Caddy reaches it over loopback, which
    # the firewall does not filter.
    systemd.services.binocular-bridge-v2-proofs =
      mkLoop "proofs" "serve-proofs" "Bifrost Bridge v2 proof API (SPI-4/OB-13)";
  };
}
