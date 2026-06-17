package binocular.cli

import binocular.*
import binocular.cli.commands.*
import com.monovore.decline.*
import cats.implicits.*

/** Binocular CLI Application
  *
  * Main entry point for the Binocular Bitcoin Oracle CLI.
  */
object CliApp {

    /** CLI command enum for routing */
    enum Cmd:
        case Version
        case Blueprint
        case Info
        case ListOracles(limit: Int)
        case VerifyOracle
        case Init(startBlock: Option[Long], dryRun: Boolean)
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
        case CreateTmtx(btcTxHex: String)
        case SpendTmtx
        case ConfirmTmtx(dryRun: Boolean)
        case TmScript
        case PegInRequest(btcTxId: String, dryRun: Boolean)
        case DeployBridge(authorizedMinter: Option[String], dryRun: Boolean)
        case DeployScriptRefs(dryRun: Boolean)
        case RegisterBridgeCreds(dryRun: Boolean)
        case SignPeginMsg(keyPath: String, message: String)
        case PegInComplete(
            pir: String,
            tm: String,
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
            pegOut: String,
            tm: String,
            priorPegouts: List[String],
            dryRun: Boolean
        )

    /** CLI argument parsers */
    object CliParsers {

        val limitOpt: Opts[Int] = Opts
            .option[Int]("limit", help = "Maximum number of results to return", short = "n")
            .withDefault(10)

        val startBlockOpt: Opts[Option[Long]] = Opts
            .option[Long]("start-block", help = "Bitcoin block height to start from", short = "s")
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

        val blueprintCommand = Opts.subcommand("blueprint", "Print CIP-57 Blueprint JSON") {
            Opts(Cmd.Blueprint)
        }

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
            (startBlockOpt, dryRunFlag).mapN(Cmd.Init.apply)
        }

        val updateCommand = Opts.subcommand("update-oracle", "Update oracle with new blocks") {
            (fromBlockOpt, toBlockOpt).mapN(Cmd.UpdateOracle.apply)
        }

        val runCommand =
            Opts.subcommand("run", "Continuous daemon: submit oracle updates in a loop") {
                dryRunFlag.map(Cmd.Run.apply)
            }

        val attackCommand =
            Opts.subcommand("attack", "ADVERSARIAL: mine rogue blocks with fake txs into the fork tree (Eve)") {
                val parentOpt = Opts
                    .option[String]("parent", help = "Fork anchor: 0=tip, 1-100=depth back from tip, or a 64-hex block hash")
                    .withDefault("0")
                val sprintOpt = Opts
                    .option[Int]("rogue-sprint", help = "Rogue blocks to front-load on the first cycle")
                    .withDefault(6)
                val spacingOpt = Opts
                    .option[Long]("block-spacing", help = "Timestamp gap between rogue blocks in seconds (>1200 for min-difficulty)")
                    .withDefault(1200L)
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

        val createTmtxCommand =
            Opts.subcommand(
              "create-tmtx",
              "Create a TMTx UTxO on Cardano (for testing relay)"
            ) {
                Opts
                    .argument[String](metavar = "BTC_TX_HEX")
                    .map(Cmd.CreateTmtx.apply)
            }

        val spendTmtxCommand =
            Opts.subcommand(
              "spend-tmtx",
              "Spend (destroy) all TMTx UTxOs at the script address"
            ) {
                Opts(Cmd.SpendTmtx)
            }

        val confirmTmtxCommand =
            Opts.subcommand(
              "confirm-tmtx",
              "Confirm relayed Bitcoin transactions on-chain"
            ) {
                dryRunFlag.map(Cmd.ConfirmTmtx.apply)
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
              "Deploy the ft-bifrost-bridge completion contracts (config NFT + completed-peg-ins MPF + TM-control NFT)"
            ) {
                val authorizedMinterOpt = Opts
                    .option[String](
                      "authorized-minter",
                      help =
                          "28-byte hex pkh allowed to mint TM NFTs (default: bridge.tm-authorized-minter)"
                    )
                    .orNone
                (authorizedMinterOpt, dryRunFlag).mapN(Cmd.DeployBridge.apply)
            }

        val registerBridgeCredsCommand =
            Opts.subcommand(
              "register-bridge-creds",
              "Register the 3 withdraw reward credentials (peg_in, owner_auth, legit_TM_verifier)"
            ) {
                dryRunFlag.map(Cmd.RegisterBridgeCreds.apply)
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
                val tmOpt =
                    Opts.option[String]("tm", "Confirmed Treasury Movement BTC txid (64 hex)")
                val recipientOpt =
                    Opts.option[String]("recipient", "fBTC recipient Cardano address (bech32)")
                val signatureOpt = Opts
                    .option[String](
                      "signature",
                      "Depositor BIP340 Schnorr signature (64-byte hex); omit with --dry-run to print the digest to sign"
                    )
                    .orNone
                val priorOpt = Opts
                    .options[String](
                      "prior-pegin",
                      "peg_in_utxo_id of an earlier completion (repeatable, insertion order)"
                    )
                    .map(_.toList)
                    .withDefault(Nil)
                (pirOpt, tmOpt, recipientOpt, signatureOpt, priorOpt, dryRunFlag).mapN(
                  Cmd.PegInComplete.apply
                )
            }

        val signPeginMsgCommand =
            Opts.subcommand(
              "sign-pegin-msg",
              "BIP340-sign the pegin-complete digest with a depositor WIF (prints the --signature)"
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
              "Complete a peg-out: burn the locked fBTC and record it in the completed-peg-outs MPF"
            ) {
                val pegOutOpt = Opts.option[String]("pegout", "PegOut UTxO (TX_HASH#INDEX)")
                val tmOpt =
                    Opts.option[String](
                      "tm",
                      "Treasury Movement BTC txid that paid the peg-out (64 hex)"
                    )
                val priorOpt = Opts
                    .options[String](
                      "prior-pegout",
                      "peg_out_utxo_id of an earlier completion (repeatable, insertion order)"
                    )
                    .map(_.toList)
                    .withDefault(Nil)
                (pegOutOpt, tmOpt, priorOpt, dryRunFlag).mapN(Cmd.PegOutComplete.apply)
            }

        val subcommands =
            versionFlag `orElse`
                blueprintCommand `orElse`
                infoCommand `orElse`
                listCommand `orElse`
                verifyCommand `orElse`
                initCommand `orElse`
                updateCommand `orElse`
                runCommand `orElse`
                attackCommand `orElse`
                closeCommand `orElse`
                deployScriptCommand `orElse`
                proveCommand `orElse`
                relayCommand `orElse`
                createTmtxCommand `orElse`
                spendTmtxCommand `orElse`
                confirmTmtxCommand `orElse`
                tmScriptCommand `orElse`
                pegInRequestCommand `orElse`
                deployBridgeCommand `orElse`
                deployScriptRefsCommand `orElse`
                registerBridgeCredsCommand `orElse`
                pegInCompleteCommand `orElse`
                signPeginMsgCommand `orElse`
                pegOutRequestCommand `orElse`
                pegOutCompleteCommand

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

            case Right((_, Cmd.Blueprint)) =>
                BlueprintCommand().execute(null)

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
                        case Cmd.Init(startBlock, dryRun) =>
                            InitOracleCommand(startBlock, dryRun)
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
                        case Cmd.CreateTmtx(btcTxHex) =>
                            CreateTmtxCommand(btcTxHex)
                        case Cmd.SpendTmtx =>
                            SpendTmtxCommand()
                        case Cmd.ConfirmTmtx(dryRun) =>
                            ConfirmTmtxCommand(dryRun)
                        case Cmd.TmScript =>
                            TmScriptCommand()
                        case Cmd.PegInRequest(btcTxId, dryRun) =>
                            PegInRequestCommand(btcTxId, dryRun)
                        case Cmd.DeployBridge(authorizedMinter, dryRun) =>
                            DeployBridgeCommand(authorizedMinter, dryRun)
                        case Cmd.DeployScriptRefs(dryRun) =>
                            DeployScriptRefsCommand(dryRun)
                        case Cmd.RegisterBridgeCreds(dryRun) =>
                            RegisterBridgeCredsCommand(dryRun)
                        case Cmd.SignPeginMsg(keyPath, message) =>
                            SignPeginMsgCommand(keyPath, message)
                        case Cmd.PegInComplete(
                              pir,
                              tm,
                              recipient,
                              signature,
                              priorPegins,
                              dryRun
                            ) =>
                            PegInCompleteCommand(pir, tm, recipient, signature, priorPegins, dryRun)
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
                        case Cmd.PegOutComplete(pegOut, tm, priorPegouts, dryRun) =>
                            PegOutCompleteCommand(pegOut, tm, priorPegouts, dryRun)
                        case Cmd.Version | Cmd.Blueprint =>
                            return 0 // unreachable: handled above
                    }

                    commandImpl.execute(config)
                } catch {
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
