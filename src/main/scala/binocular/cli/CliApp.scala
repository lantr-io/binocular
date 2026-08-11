package binocular.cli

import binocular.*
import binocular.cli.commands.*
import com.monovore.decline.*
import cats.implicits.*
import scalus.uplc.builtin.ByteString

/** Binocular CLI Application
  *
  * Main entry point for the Binocular Bitcoin Oracle CLI.
  */
object CliApp {

    /** CLI command enum for routing */
    enum Cmd:
        case Version
        case Info
        case ListOracles(limit: Int)
        case VerifyOracle
        case Init(startBlock: Option[Long], confirmedUntil: Option[Long], dryRun: Boolean)
        case SetState(height: Long, dryRun: Boolean)
        case UpdateOracle(fromBlock: Option[Long], toBlock: Option[Long])
        case Run(dryRun: Boolean)
        case Attack(parent: String, rogueSprint: Int, blockSpacing: Long, dryRun: Boolean)
        case Close
        case DeployScript
        case ProveTransaction(
            btcTxId: String,
            blockHash: Option[String],
            txIndex: Option[Int],
            proof: Option[String],
            merkleRoot: Option[String]
        )
        case Relay(dryRun: Boolean)
        case ConfirmTmtx(dryRun: Boolean)
        case Watchtower(dryRun: Boolean)
        case TmScript
        case PegInRequest(btcTxId: String, dryRun: Boolean)
        case DeployBridge(dryRun: Boolean)
        case BootstrapBridgeState(
            oneShotRef: Option[String],
            anchor: Option[String],
            amountSat: Option[Long],
            spiRoot: Option[String],
            cpoRoot: Option[String],
            skipBtcCheck: Boolean,
            dryRun: Boolean
        )
        case UpdateConfig(
            bridgeStatePolicy: Option[String],
            tmScriptHash: Option[String],
            pegInWithdrawHash: Option[String],
            pegOutWithdrawHash: Option[String],
            params: binocular.cli.commands.UpdateConfigCommand.ParamEdits,
            dryRun: Boolean
        )
        case DeployScriptRefs(dryRun: Boolean)
        case RegisterBridgeCreds(dryRun: Boolean)
        case SignPeginMsg(keyPath: String, message: String)
        case PegInComplete(
            pir: String,
            recipient: String,
            signature: Option[String],
            priorPegins: List[String],
            dryRun: Boolean
        )
        case PegOutRequest(
            btcAddress: String,
            amountSat: Long,
            treasuryOutpoint: String,
            ownerPkh: Option[String],
            dryRun: Boolean
        )
        case PegOutComplete(
            pegOut: Option[String],
            dryRun: Boolean
        )
        case SpiProof(outpoint: String)
        case DepositProof(outpoint: String)
        case ServeProofs(port: Option[Int], dryRun: Boolean)

    /** CLI argument parsers */
    object CliParsers {

        val limitOpt: Opts[Int] = Opts
            .option[Int]("limit", help = "Maximum number of results to return", short = "n")
            .withDefault(10)

        val startBlockOpt: Opts[Option[Long]] = Opts
            .option[Long]("start-block", help = "Bitcoin block height to start from", short = "s")
            .orNone

        val confirmedUntilOpt: Opts[Option[Long]] = Opts
            .option[Long](
              "confirmed-until",
              help = "Seed confirmed blocks up to this height (default: single block at start)"
            )
            .orNone

        val dryRunFlag: Opts[Boolean] = Opts
            .flag("dry-run", help = "Build transaction but don't submit")
            .orFalse

        val fromBlockOpt: Opts[Option[Long]] = Opts
            .option[Long]("from", help = "Start Bitcoin block height", short = "f")
            .orNone

        val toBlockOpt: Opts[Option[Long]] = Opts
            .option[Long]("to", help = "End Bitcoin block height", short = "t")
            .orNone

        val btcTxIdArg: Opts[String] = Opts.argument[String](metavar = "BTC_TX_ID")

        val blockHashOpt: Opts[Option[String]] = Opts
            .option[String]("block", help = "Bitcoin block hash (64 hex chars)", short = "b")
            .orNone

        val txIndexOpt: Opts[Option[Int]] = Opts
            .option[Int]("tx-index", help = "Transaction index in block", short = "i")
            .orNone

        val proofOpt: Opts[Option[String]] = Opts
            .option[String]("proof", help = "Merkle proof hashes (comma-separated)", short = "p")
            .orNone

        val merkleRootOpt: Opts[Option[String]] = Opts
            .option[String](
              "merkle-root",
              help = "Expected merkle root (64 hex chars)",
              short = "m"
            )
            .orNone
    }

    /** Global --config option */
    val configOpt: Opts[Option[String]] = Opts
        .option[String]("config", help = "Path to HOCON config file")
        .orNone

    /** Main CLI command parser */
    val command: Command[(Option[String], Cmd)] = {
        import CliParsers.*

        val versionFlag = Opts
            .flag("version", help = "Print version and exit", short = "v")
            .map(_ => Cmd.Version)

        val infoCommand = Opts.subcommand("info", "Display oracle configuration and info") {
            Opts(Cmd.Info)
        }

        val listCommand = Opts.subcommand("list-oracles", "List oracle UTxOs") {
            limitOpt.map(Cmd.ListOracles.apply)
        }

        val verifyCommand = Opts.subcommand("verify-oracle", "Verify oracle state") {
            Opts(Cmd.VerifyOracle)
        }

        val initCommand = Opts.subcommand("init", "Initialize new oracle") {
            (startBlockOpt, confirmedUntilOpt, dryRunFlag).mapN(Cmd.Init.apply)
        }

        val setStateCommand = Opts.subcommand(
          "set-state",
          "Owner state reset of a STALE oracle (testing + deep-reorg recovery): replaces the " +
              "ChainState in one tx, preserving the NFT and all dependent contracts"
        ) {
            val heightOpt: Opts[Long] =
                Opts.option[Long]("height", "Bitcoin block height to anchor the new state at")
            (heightOpt, dryRunFlag).mapN(Cmd.SetState.apply)
        }

        val updateCommand = Opts.subcommand("update-oracle", "Update oracle with new blocks") {
            (fromBlockOpt, toBlockOpt).mapN(Cmd.UpdateOracle.apply)
        }

        val runCommand =
            Opts.subcommand("run", "Continuous daemon: submit oracle updates in a loop") {
                dryRunFlag.map(Cmd.Run.apply)
            }

        val attackCommand =
            Opts.subcommand(
              "attack",
              "ADVERSARIAL: mine rogue blocks with fake txs into the fork tree (Eve)"
            ) {
                val parentOpt = Opts
                    .option[String](
                      "parent",
                      help = "Fork anchor: 0=tip, 1-100=depth back from tip, or a 64-hex block hash"
                    )
                    .withDefault("0")
                val sprintOpt = Opts
                    .option[Int](
                      "rogue-sprint",
                      help = "Rogue blocks to front-load on the first cycle"
                    )
                    .withDefault(6)
                val spacingOpt = Opts
                    .option[Long](
                      "block-spacing",
                      help =
                          "Timestamp gap between rogue blocks in seconds (must be > 1200 for testnet4 min-difficulty)"
                    )
                    .withDefault(1201L)
                (parentOpt, sprintOpt, spacingOpt, dryRunFlag).mapN(Cmd.Attack.apply)
            }

        val closeCommand = Opts.subcommand("close", "Close oracle, burn NFT") {
            Opts(Cmd.Close)
        }

        val deployScriptCommand =
            Opts.subcommand("deploy-script", "Deploy oracle validator reference script") {
                Opts(Cmd.DeployScript)
            }

        val proveCommand =
            Opts.subcommand("prove-transaction", "Prove Bitcoin transaction inclusion") {
                (btcTxIdArg, blockHashOpt, txIndexOpt, proofOpt, merkleRootOpt).mapN(
                  Cmd.ProveTransaction.apply
                )
            }

        val relayCommand =
            Opts.subcommand(
              "relay",
              "Relay signed Bitcoin transactions from Cardano to Bitcoin"
            ) {
                dryRunFlag.map(Cmd.Relay.apply)
            }

        val confirmTmtxCommand =
            Opts.subcommand(
              "confirm-tmtx",
              "Confirm relayed Bitcoin transactions on-chain"
            ) {
                dryRunFlag.map(Cmd.ConfirmTmtx.apply)
            }

        val watchtowerCommand =
            Opts.subcommand(
              "watchtower",
              "Run the oracle sync, TM relay, and TM confirm daemons together in one process"
            ) {
                dryRunFlag.map(Cmd.Watchtower.apply)
            }

        val tmScriptCommand =
            Opts.subcommand(
              "tm-script",
              "Export the TreasuryMovementValidator policy id + address + CBOR (for heimdall to mint under)"
            ) {
                Opts(Cmd.TmScript)
            }

        val pegInRequestCommand =
            Opts.subcommand(
              "pegin-request",
              "Mint a PegInRequest on Cardano for a confirmed BTC peg-in tx"
            ) {
                // The retired legit_TM_verifier path used to read source_chain_treasury_utxo_id
                // from the PIR datum, so we accepted a `--tm` flag to derive it. The verifier was
                // removed (B1 references the Confirmed TM UTxO directly), the datum field is now
                // left empty, and `--tm` no longer affects anything — so the flag is gone too.
                (btcTxIdArg, dryRunFlag).mapN(Cmd.PegInRequest.apply)
            }

        val deployBridgeCommand =
            Opts.subcommand(
              "deploy-bridge",
              "Deploy the ft-bifrost-bridge completion contracts (config NFT + completed-peg-ins/outs MPFs)"
            ) {
                dryRunFlag.map(Cmd.DeployBridge.apply)
            }

        val registerBridgeCredsCommand =
            Opts.subcommand(
              "register-bridge-creds",
              "Register the peg_in and peg_out withdraw reward credentials (config fields 4 and 5)"
            ) {
                dryRunFlag.map(Cmd.RegisterBridgeCreds.apply)
            }

        val bootstrapBridgeStateCommand =
            Opts.subcommand(
              "bootstrap-bridge-state",
              "Mint a fresh bridge-state singleton against the LIVE bridge (config field 3 " +
                  "swap / recovery replacement)"
            ) {
                val oneShotOpt = Opts
                    .option[String](
                      "one-shot-ref",
                      help = "Wallet UTxO TX_HASH#INDEX to consume as the one-shot " +
                          "(default: auto-pick the largest clean pure-ADA UTxO)"
                    )
                    .orNone
                val anchorOpt = Opts
                    .option[String](
                      "anchor",
                      help = "Bitcoin treasury outpoint TXID:VOUT the singleton's head points at " +
                          "(default: bridge.initial-btc-treasury-utxo)"
                    )
                    .orNone
                val amountOpt = Opts
                    .option[Long](
                      "amount-sat",
                      help = "Satoshi amount of that outpoint " +
                          "(default: bridge.initial-btc-treasury-amount-sat)"
                    )
                    .orNone
                val spiRootOpt = Opts
                    .option[String](
                      "spi-root",
                      help = "Initial swept-peg-ins root, 64 hex chars (default: 32 zero bytes)"
                    )
                    .orNone
                val cpoRootOpt = Opts
                    .option[String](
                      "cpo-root",
                      help =
                          "Initial completed-peg-outs root, 64 hex chars (default: 32 zero bytes)"
                    )
                    .orNone
                val skipBtcCheckFlag = Opts
                    .flag(
                      "skip-btc-check",
                      help = "Skip the [DEP-2] gettxout verification of the anchor outpoint and " +
                          "amount. Only for an unreachable Bitcoin node AND a hand-verified anchor."
                    )
                    .orFalse
                (
                  oneShotOpt,
                  anchorOpt,
                  amountOpt,
                  spiRootOpt,
                  cpoRootOpt,
                  skipBtcCheckFlag,
                  dryRunFlag
                )
                    .mapN(Cmd.BootstrapBridgeState.apply)
            }

        val updateConfigCommand =
            Opts.subcommand(
              "update-config",
              "Update the deployed Config UTxO in place (governance): the script hashes in " +
                  "fields 3-6, the ban policy in fields 7-10, and the operational parameters " +
                  "nested in field 14. Only what you name changes"
            ) {
                val bridgeStatePolicyOpt = Opts
                    .option[String](
                      "bridge-state-policy",
                      help = "New bridge-state singleton NFT policy (56 hex) for config field 3 " +
                          "(spec §Recovery: replacing the singleton)"
                    )
                    .orNone
                val tmScriptHashOpt = Opts
                    .option[String](
                      "tm-script-hash",
                      help = "New TM validator script hash (56 hex) for config field 4 " +
                          "(spec [CFG-2]; swap field 3 with it on a TM redeploy)"
                    )
                    .orNone
                val pegInHashOpt = Opts
                    .option[String](
                      "peg-in-withdraw-hash",
                      help = "New peg_in withdraw script hash (56 hex) for config field 5"
                    )
                    .orNone
                val pegOutHashOpt = Opts
                    .option[String](
                      "peg-out-withdraw-hash",
                      help = "New peg_out withdraw script hash (56 hex) for config field 6"
                    )
                    .orNone
                // The operational parameters (config field 14, nested). Every SPO's TM builder
                // reads them at its batch snapshot slot, so these edits change the bytes every
                // heimdall builds.
                val feeRateOpt = Opts
                    .option[Long](
                      "fee-rate",
                      help = "params[0]: exact Bitcoin miner fee rate for TM construction (sat/vB)"
                    )
                    .orNone
                val perPegoutFeeOpt = Opts
                    .option[Long](
                      "per-pegout-fee",
                      help = "params[1]: floor for a peg-out's datum-pinned fee (satoshi)"
                    )
                    .orNone
                val minPegOutOpt = Opts
                    .option[Long](
                      "min-peg-out",
                      help = "params[2]: minimum fBTC a peg-out may lock (satoshi)"
                    )
                    .orNone
                val scheduleOpt: Opts[Map[String, BigInt]] = Opts
                    .options[String](
                      "schedule",
                      help = "params[3]: patch one schedule slot, NAME=VALUE (repeatable). " +
                          UpdateConfigCommand.ScheduleFields.mkString(", ")
                    )
                    .orEmpty
                    // A misspelled slot name is a usage error, not a crash: decline reports it
                    // alongside the rest of the help.
                    .mapValidated(args =>
                        UpdateConfigCommand.ParamEdits
                            .parseSchedule(args)
                            .fold(cats.data.Validated.invalidNel, cats.data.Validated.validNel)
                    )
                // The ban policy (config #7-#10). Publishing it is what removes the ban keys from
                // every SPO's heimdall.toml: they are inputs to the policy id, so no node can
                // derive the address it would read them from.
                val banPolicyOpt: Opts[Option[ByteString]] = Opts
                    .option[String](
                      "spo-bans-policy",
                      help = "config #7: the deployed spo_bans policy id (56 hex). Every SPO " +
                          "reads its ban list from this and needs no ban config of its own"
                    )
                    .mapValidated(arg =>
                        UpdateConfigCommand.ParamEdits
                            .parseBanPolicy(arg)
                            .fold(cats.data.Validated.invalidNel, cats.data.Validated.validNel)
                    )
                    .orNone
                val baseBanDurationOpt = Opts
                    .option[Long](
                      "base-ban-duration-ms",
                      help = "config #8: first ban's length (ms); the nth is base * 2^(n-1)"
                    )
                    .orNone
                val maxFaultsOpt = Opts
                    .option[Long](
                      "max-faults-before-permanent",
                      help = "config #9: fault count at which a pool is banned permanently"
                    )
                    .orNone
                val maxValidityWindowOpt = Opts
                    .option[Long](
                      "max-validity-window-ms",
                      help = "config #10: upper bound on an ApplyBan tx's validity interval (ms)"
                    )
                    .orNone
                val paramsOpt: Opts[UpdateConfigCommand.ParamEdits] = (
                  feeRateOpt,
                  perPegoutFeeOpt,
                  minPegOutOpt,
                  scheduleOpt,
                  banPolicyOpt,
                  baseBanDurationOpt,
                  maxFaultsOpt,
                  maxValidityWindowOpt
                ).mapN {
                    (
                        feeRate,
                        perPegoutFee,
                        minPegOut,
                        schedule,
                        banPolicy,
                        baseBanDuration,
                        maxFaults,
                        maxValidityWindow
                    ) =>
                        UpdateConfigCommand.ParamEdits(
                          feeRateSatPerVb = feeRate.map(BigInt.apply),
                          perPegoutFee = perPegoutFee.map(BigInt.apply),
                          minPegOutFbtc = minPegOut.map(BigInt.apply),
                          schedule = schedule,
                          spoBansPolicyId = banPolicy,
                          baseBanDurationMs = baseBanDuration.map(BigInt.apply),
                          maxFaultsBeforePermanent = maxFaults.map(BigInt.apply),
                          maxValidityWindowMs = maxValidityWindow.map(BigInt.apply)
                        )
                }
                // Every option is applied in ONE Update tx — a validator migration requires its
                // dependent fields to flip together, and a params update is one signed act.
                (
                  bridgeStatePolicyOpt,
                  tmScriptHashOpt,
                  pegInHashOpt,
                  pegOutHashOpt,
                  paramsOpt,
                  dryRunFlag
                )
                    .mapN(Cmd.UpdateConfig.apply)
            }

        val deployScriptRefsCommand =
            Opts.subcommand(
              "deploy-script-refs",
              "Publish peg_in / bridged_token / completed_peg_ins as CIP-33 reference scripts (shrinks pegin-complete tx)"
            ) {
                dryRunFlag.map(Cmd.DeployScriptRefs.apply)
            }

        val pegInCompleteCommand =
            Opts.subcommand(
              "pegin-complete",
              "Complete a peg-in: mint fBTC to --recipient and record it in the completed-peg-ins MPF"
            ) {
                val pirOpt = Opts.option[String]("pir", "PegInRequest UTxO (TX_HASH#INDEX)")
                val recipientOpt =
                    Opts.option[String]("recipient", "fBTC recipient Cardano address (bech32)")
                val signatureOpt = Opts
                    .option[String](
                      "signature",
                      "Depositor BIP-322 signature (64-byte hex from a bip322-simple wallet sig); omit with --dry-run to print the text to sign"
                    )
                    .orNone
                val priorOpt = Opts
                    .options[String](
                      "prior-pegin",
                      "peg_in_utxo_id of an earlier completion (repeatable, insertion order)"
                    )
                    .map(_.toList)
                    .withDefault(Nil)
                (pirOpt, recipientOpt, signatureOpt, priorOpt, dryRunFlag).mapN(
                  Cmd.PegInComplete.apply
                )
            }

        val signPeginMsgCommand =
            Opts.subcommand(
              "sign-pegin-msg",
              "BIP-322-sign the pegin-complete message with a depositor WIF (prints the --signature)"
            ) {
                val keyOpt = Opts
                    .option[String](
                      "key",
                      "Path to depositor WIF file (e.g. heimdall/.keys/alice.wif)"
                    )
                val msgOpt = Opts
                    .option[String](
                      "message",
                      "32-byte sha2_256 digest hex from pegin-complete --dry-run"
                    )
                (keyOpt, msgOpt).mapN(Cmd.SignPeginMsg.apply)
            }

        val pegOutRequestCommand =
            Opts.subcommand(
              "peg-out-request",
              "Create a peg-out: lock fBTC + MIN_ADA at peg_out.ak with a Bitcoin destination"
            ) {
                val btcAddrOpt = Opts
                    .option[String](
                      "btc-address",
                      "Bitcoin destination address (BTC paid here by the TM)"
                    )
                val amountOpt = Opts
                    .option[Long]("amount", "fBTC amount to peg out, in satoshis")
                val treasuryOpt = Opts
                    .option[String](
                      "treasury-outpoint",
                      "Treasury UTxO the peg-out TM will spend (BTC TXID:VOUT, display form)"
                    )
                val ownerOpt = Opts
                    .option[String](
                      "owner-pkh",
                      "owner_auth payment key hash (56 hex) for reclaim; default = sponsor pkh"
                    )
                    .orNone
                (btcAddrOpt, amountOpt, treasuryOpt, ownerOpt, dryRunFlag).mapN(
                  Cmd.PegOutRequest.apply
                )
            }

        val pegOutCompleteCommand =
            Opts.subcommand(
              "peg-out-complete",
              "Complete PAID peg-out requests: burn the locked fBTC against a membership proof and keep the MIN_ADA (permissionless)"
            ) {
                val pegOutOpt = Opts
                    .option[String](
                      "pegout",
                      "Complete only this PegOutRequest UTxO (TX_HASH#INDEX); default: every " +
                          "completable request at the peg-out address"
                    )
                    .orNone
                (pegOutOpt, dryRunFlag).mapN(Cmd.PegOutComplete.apply)
            }

        val outpointArg: Opts[String] =
            Opts.argument[String](metavar = "OUTPOINT")

        val spiProofCommand =
            Opts.subcommand(
              "spi-proof",
              "Serve the [CPI-9] swept-peg-ins membership proof for one deposit outpoint " +
                  "(TXID:VOUT or 72-hex) — [SPI-4]/[SPI-6]"
            ) {
                outpointArg.map(Cmd.SpiProof.apply)
            }

        val depositProofCommand =
            Opts.subcommand(
              "deposit-proof",
              "Serve the [OB-12] deposit-inclusion bundle (header, merkle proof, oracle MPF " +
                  "proof, raw tx) for one deposit outpoint (TXID:VOUT or 72-hex)"
            ) {
                outpointArg.map(Cmd.DepositProof.apply)
            }

        val serveProofsCommand =
            Opts.subcommand(
              "serve-proofs",
              "Run the proof-serving REST API ([SPI-4]/[OB-13]) standalone: " +
                  "/api/v1/spi-proof/{outpoint} and /api/v1/deposit-proof/{outpoint}"
            ) {
                val portOpt = Opts
                    .option[Int](
                      "port",
                      help = "HTTP port to bind (default: bridge.proof-server-port)"
                    )
                    .orNone
                (portOpt, dryRunFlag).mapN(Cmd.ServeProofs.apply)
            }

        val subcommands =
            versionFlag `orElse`
                infoCommand `orElse`
                listCommand `orElse`
                verifyCommand `orElse`
                initCommand `orElse`
                setStateCommand `orElse`
                updateCommand `orElse`
                runCommand `orElse`
                attackCommand `orElse`
                closeCommand `orElse`
                deployScriptCommand `orElse`
                proveCommand `orElse`
                relayCommand `orElse`
                confirmTmtxCommand `orElse`
                watchtowerCommand `orElse`
                tmScriptCommand `orElse`
                pegInRequestCommand `orElse`
                deployBridgeCommand `orElse`
                bootstrapBridgeStateCommand `orElse`
                updateConfigCommand `orElse`
                deployScriptRefsCommand `orElse`
                registerBridgeCredsCommand `orElse`
                pegInCompleteCommand `orElse`
                signPeginMsgCommand `orElse`
                pegOutRequestCommand `orElse`
                pegOutCompleteCommand `orElse`
                spiProofCommand `orElse`
                depositProofCommand `orElse`
                serveProofsCommand

        com.monovore.decline.Command(
          name = "binocular",
          header = "Binocular - Bitcoin Oracle for Cardano"
        )((configOpt, subcommands).tupled)
    }

    /** Execute the CLI application */
    def run(args: Seq[String]): Int = {
        val trimmedArgs = args.map(_.trim).filter(_.nonEmpty)
        command.parse(trimmedArgs) match {
            case Left(help) =>
                System.err.println(help)
                1

            case Right((configPath, Cmd.Version)) =>
                println(s"binocular ${BuildInfo.version}")
                0

            case Right((configPath, cmd)) =>
                try {
                    val config = BinocularConfig.load(configPath)

                    val commandImpl: binocular.cli.Command = cmd match {
                        case Cmd.Info =>
                            InfoCommand()
                        case Cmd.ListOracles(limit) =>
                            ListOraclesCommand(limit)
                        case Cmd.VerifyOracle =>
                            VerifyOracleCommand()
                        case Cmd.Init(startBlock, confirmedUntil, dryRun) =>
                            InitOracleCommand(startBlock, confirmedUntil, dryRun)
                        case Cmd.SetState(height, dryRun) =>
                            SetStateCommand(height, dryRun)
                        case Cmd.UpdateOracle(from, to) =>
                            UpdateOracleCommand(from, to)
                        case Cmd.Run(dryRun) =>
                            RunCommand(dryRun)
                        case Cmd.Attack(parent, rogueSprint, blockSpacing, dryRun) =>
                            AttackCommand(parent, rogueSprint, blockSpacing, dryRun)
                        case Cmd.Close =>
                            CloseCommand()
                        case Cmd.DeployScript =>
                            DeployScriptCommand()
                        case Cmd.ProveTransaction(
                              btcTxId,
                              blockHash,
                              txIndex,
                              proof,
                              merkleRoot
                            ) =>
                            ProveTransactionCommand(
                              btcTxId,
                              blockHash,
                              txIndex,
                              proof,
                              merkleRoot
                            )
                        case Cmd.Relay(dryRun) =>
                            RelayCommand(dryRun)
                        case Cmd.ConfirmTmtx(dryRun) =>
                            ConfirmTmtxCommand(dryRun)
                        case Cmd.Watchtower(dryRun) =>
                            WatchtowerCommand(dryRun)
                        case Cmd.TmScript =>
                            TmScriptCommand()
                        case Cmd.PegInRequest(btcTxId, dryRun) =>
                            PegInRequestCommand(btcTxId, dryRun)
                        case Cmd.DeployBridge(dryRun) =>
                            DeployBridgeCommand(dryRun)
                        case Cmd.BootstrapBridgeState(
                              oneShotRef,
                              anchor,
                              amountSat,
                              spiRoot,
                              cpoRoot,
                              skipBtcCheck,
                              dryRun
                            ) =>
                            BootstrapBridgeStateCommand(
                              oneShotRef,
                              anchor,
                              amountSat,
                              spiRoot,
                              cpoRoot,
                              skipBtcCheck,
                              dryRun
                            )
                        case Cmd.UpdateConfig(
                              bridgeStatePolicy,
                              tmScriptHash,
                              pegInHash,
                              pegOutHash,
                              params,
                              dryRun
                            ) =>
                            UpdateConfigCommand(
                              bridgeStatePolicy,
                              tmScriptHash,
                              pegInHash,
                              pegOutHash,
                              params,
                              dryRun
                            )
                        case Cmd.DeployScriptRefs(dryRun) =>
                            DeployScriptRefsCommand(dryRun)
                        case Cmd.RegisterBridgeCreds(dryRun) =>
                            RegisterBridgeCredsCommand(dryRun)
                        case Cmd.SignPeginMsg(keyPath, message) =>
                            SignPeginMsgCommand(keyPath, message)
                        case Cmd.PegInComplete(
                              pir,
                              recipient,
                              signature,
                              priorPegins,
                              dryRun
                            ) =>
                            PegInCompleteCommand(pir, recipient, signature, priorPegins, dryRun)
                        case Cmd.PegOutRequest(
                              btcAddress,
                              amountSat,
                              treasuryOutpoint,
                              ownerPkh,
                              dryRun
                            ) =>
                            PegOutRequestCommand(
                              btcAddress,
                              amountSat,
                              treasuryOutpoint,
                              ownerPkh,
                              dryRun = dryRun
                            )
                        case Cmd.PegOutComplete(pegOut, dryRun) =>
                            PegOutCompleteCommand(pegOut, dryRun)
                        case Cmd.SpiProof(outpoint) =>
                            SpiProofCommand(outpoint)
                        case Cmd.DepositProof(outpoint) =>
                            DepositProofCommand(outpoint)
                        case Cmd.ServeProofs(port, dryRun) =>
                            ServeProofsCommand(port, dryRun)
                        case Cmd.Version =>
                            return 0 // unreachable: handled above
                    }

                    commandImpl.execute(config)
                } catch {
                    // Unrecoverable state (e.g. a deep reorg orphaning confirmed history) — a
                    // known terminal condition, so report it plainly without a stack trace. The
                    // watchtower handles its own exit; this covers the standalone `run` command.
                    case e: binocular.watchtower.UnrecoverableWorkerError =>
                        System.err.println(s"Unrecoverable: ${e.getMessage}")
                        binocular.watchtower.Watchtower.UnrecoverableExitCode
                    case e: pureconfig.error.ConfigReaderException[?] =>
                        System.err.println(
                          s"Configuration error: ${e.failures.toList.map(_.description).mkString(", ")}"
                        )
                        1
                    case e: Exception =>
                        System.err.println(s"Error: ${e.getMessage}")
                        e.printStackTrace()
                        1
                }
        }
    }
}
