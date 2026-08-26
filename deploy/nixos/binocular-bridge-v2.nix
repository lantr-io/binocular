# NixOS module: the Bifrost Bridge v2 watchtower.
#
# ONE unit, `binocular-bridge-v2`, running the `watchtower` command from main. That command
# supervises four workers in a single JVM:
#
#   [oracle]   `run`            Bitcoin header submission to the oracle UTxO (and SetState recovery)
#   [relay]    `relay`          Cardano → Bitcoin broadcast
#   [confirm]  `confirm-tmtx`   Bitcoin inclusion → Cardano Confirmed, plus the POR sweeper
#   [proofs]   `serve-proofs`   the [SPI-4]/[OB-13] REST API the frontend's complete-peg-in uses
#
# History: v2 used to run three units (relay / confirm / proofs) and deliberately NOT `watchtower`,
# because the 2026-07-17 v1 deployment (`binocular-watchtower`, application-preprod.conf) owned the
# oracle UTxO and only one process on this box may write it. v1 is retired, so v2 takes the oracle
# worker back and the three loops collapse into the one command that supervises all four.
#
# binocular-watchtower.nix is still imported: it defines the testnet4 bitcoind both this unit and
# Dolos sit beside, plus the `binocular` user and /var/lib/binocular. Set its `runWatchtower =
# false` to retire the v1 unit while keeping those.
#
# Out-of-store files, deployed by deploy/deploy.sh --v2:
#   /var/lib/binocular/binocular-v2.jar             (fat jar; NOT v1's binocular.jar)
#   /var/lib/binocular/application-preprod-v2.conf  (non-secret HOCON config)
#   /var/lib/binocular/secrets.env                  (WALLET_MNEMONIC et al; mode 600)
{ config, lib, pkgs, ... }:

let
  cfg = config.services.binocular-bridge-v2;
in
{
  options.services.binocular-bridge-v2 = {
    enable = lib.mkEnableOption "Bifrost Bridge v2 watchtower (oracle + relay + confirm + proofs)";

    jdk = lib.mkOption {
      type = lib.types.package;
      default = pkgs.openjdk25;
      description = "JDK used to run the fat jar (needs JDK 23+ for --sun-misc-unsafe-memory-access).";
    };

    stateDir = lib.mkOption {
      type = lib.types.str;
      default = "/var/lib/binocular";
      description = "Directory holding the jar, config, and secrets. Shared with binocular-watchtower.nix on purpose: the secrets file is the same wallet.";
    };

    jarFile = lib.mkOption {
      type = lib.types.str;
      default = "binocular-v2.jar";
      description = "Jar filename within stateDir. Deliberately NOT binocular.jar — that one is v1's, kept on disk for rollback.";
    };

    configFile = lib.mkOption {
      type = lib.types.str;
      default = "application-preprod-v2.conf";
      description = "Config filename within stateDir passed to --config.";
    };

    secretsFile = lib.mkOption {
      type = lib.types.str;
      default = "/var/lib/binocular/secrets.env";
      description = "EnvironmentFile with WALLET_MNEMONIC and BLOCKFROST_PROJECT_ID (mode 600).";
    };

    user = lib.mkOption {
      type = lib.types.str;
      default = "binocular";
      description = "Service user. Defaults to the user binocular-watchtower.nix creates.";
    };

    heapMb = lib.mkOption {
      type = lib.types.int;
      default = 512;
      description = ''
        -Xmx for the watchtower JVM, in MiB.

        Was 256, spent three times over when relay, confirm and proofs were three units: measured
        resident use was 251 / 157 / 86 MB, i.e. ~494 MB plus three JVM runtimes. One JVM pays that
        overhead once and adds the oracle worker, which ran inside v1's watchtower on the default
        heap without incident. 512 in one process is therefore more headroom per worker than the old
        layout had, at a lower total footprint — which matters on this 3.7 GB box, where on
        2026-08-18 bitcoind was pushed 546 MB into zram and its RPC latency reached 180 s.

        Raise to 768 if [oracle] GC-thrashes while rebuilding the MPF from start-height.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    # No users.users block: binocular-watchtower.nix creates this user, and both modules are
    # imported together. Declaring it twice is what a conflicting `home` would break.
    systemd.services.binocular-bridge-v2 = {
      description = "Bifrost Bridge v2 watchtower (oracle sync + TM relay + TM confirm + proof API)";
      after = [ "network-online.target" "bitcoind-watchtower.service" ];
      # `wants`, not just `after`: the oracle and confirm workers are useless without bitcoind.
      wants = [ "network-online.target" "bitcoind-watchtower.service" ];
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
            --config ${cfg.stateDir}/${cfg.configFile} watchtower
        '';
        Restart = "always";
        RestartSec = 10;
        # Exit code 3 = unrecoverable watchtower state (deep reorg orphaned the oracle's confirmed
        # history; see Watchtower.UnrecoverableExitCode). Restarting only re-detects it, so leave
        # the service stopped for manual re-init.
        RestartPreventExitStatus = "3";

        # Hardening, matching binocular-watchtower.nix.
        NoNewPrivileges = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        PrivateTmp = true;
        # Only the v2 state dir is writable; the jar, config and secrets under stateDir are read.
        #
        # The proof API worker binds 0.0.0.0 with no option to narrow it (ProofServer.scala:73) on
        # bridge.proof-server-port (9061). That port MUST stay out of
        # networking.firewall.allowedTCPPorts: the firewall is the only thing keeping the API off
        # the public internet, and Caddy reaches it over loopback, which the firewall does not
        # filter.
        ReadWritePaths = [ "/var/lib/binocular-v2" ];
      };
    };
  };
}
