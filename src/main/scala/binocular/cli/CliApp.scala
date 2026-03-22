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
        case Close
        case DeployScript
        case ProveTransaction(
            btcTxId: String,
            blockHash: Option[String],
            txIndex: Option[Int],
            proof: Option[String],
            merkleRoot: Option[String]
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

        val subcommands =
            versionFlag `orElse`
                blueprintCommand `orElse`
                infoCommand `orElse`
                listCommand `orElse`
                verifyCommand `orElse`
                initCommand `orElse`
                updateCommand `orElse`
                runCommand `orElse`
                closeCommand `orElse`
                deployScriptCommand `orElse`
                proveCommand

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
