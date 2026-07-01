# NixOS module: Binocular watchtower + local testnet4 bitcoind.
#
# Import this into your host configuration and set the options under `services.binocular-watchtower`.
# The jar, config, and secrets live OUT of the Nix store (they are deployed with deploy.sh):
#   /var/lib/binocular/binocular.jar               (fat jar; built on your Mac)
#   /var/lib/binocular/application-preprod.conf    (non-secret HOCON config)
#   /var/lib/binocular/secrets.env                 (MNEMONIC + BLOCKFROST_PROJECT_ID; mode 600)
#
# Nothing secret is placed in the Nix store.
{ config, lib, pkgs, ... }:

let
  cfg = config.services.binocular-watchtower;
in
{
  options.services.binocular-watchtower = {
    enable = lib.mkEnableOption "Binocular watchtower daemon";

    jdk = lib.mkOption {
      type = lib.types.package;
      default = pkgs.openjdk25;
      description = "JDK used to run the fat jar (needs JDK 23+ for --sun-misc-unsafe-memory-access).";
    };

    stateDir = lib.mkOption {
      type = lib.types.str;
      default = "/var/lib/binocular";
      description = "Out-of-store directory holding the jar, config, and secrets.";
    };

    configFile = lib.mkOption {
      type = lib.types.str;
      default = "application-preprod.conf";
      description = "Config filename within stateDir passed to --config.";
    };

    secretsFile = lib.mkOption {
      type = lib.types.str;
      default = "/var/lib/binocular/secrets.env";
      description = "EnvironmentFile with MNEMONIC and BLOCKFROST_PROJECT_ID (mode 600).";
    };

    user = lib.mkOption {
      type = lib.types.str;
      default = "binocular";
      description = "Service user (also owns stateDir and reads the secrets file).";
    };

    manageBitcoind = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Run a local testnet4 bitcoind (RPC 127.0.0.1:48332, txindex=1) for the watchtower.";
    };
  };

  config = lib.mkIf cfg.enable {
    users.users.${cfg.user} = {
      isSystemUser = true;
      group = cfg.user;
      home = cfg.stateDir;
    };
    users.groups.${cfg.user} = { };

    # Local Bitcoin node the config points at (bitcoin-node.url = http://127.0.0.1:48332).
    # txindex=1 is REQUIRED: confirm-tmtx calls getrawtransaction on non-wallet TM txids.
    services.bitcoind.watchtower = lib.mkIf cfg.manageBitcoind {
      enable = true;
      extraConfig = ''
        chain=testnet4
        txindex=1
        [testnet4]
        rpcbind=127.0.0.1
        rpcport=48332
        rpcallowip=127.0.0.1
      '';
      # RPC credentials must match application-preprod.conf (bitcoin-node.username/password).
      # For a local-only testnet4 node these are low-stakes; move to rpcpasswordfile if desired.
      rpc.users.bitcoin.passwordHMACFromFile = false;
    };

    systemd.services.binocular-watchtower = {
      description = "Binocular watchtower (oracle sync + TM relay + TM confirm)";
      after = [ "network-online.target" ]
        ++ lib.optional cfg.manageBitcoind "bitcoind-watchtower.service";
      wants = [ "network-online.target" ]
        ++ lib.optional cfg.manageBitcoind "bitcoind-watchtower.service";
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        Type = "simple";
        User = cfg.user;
        Group = cfg.user;
        StateDirectory = "binocular"; # ensures /var/lib/binocular exists, owned by the user
        EnvironmentFile = cfg.secretsFile;
        ExecStart = ''
          ${cfg.jdk}/bin/java --sun-misc-unsafe-memory-access=allow \
            -jar ${cfg.stateDir}/binocular.jar \
            --config ${cfg.stateDir}/${cfg.configFile} watchtower
        '';
        Restart = "always";
        RestartSec = 10;

        # Hardening
        NoNewPrivileges = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        PrivateTmp = true;
        ReadWritePaths = [ cfg.stateDir ];
      };
    };
  };
}
