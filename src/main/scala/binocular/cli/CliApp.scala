package binocular.cli

import binocular.cli.commands.*
import com.monovore.decline.*
import cats.implicits.*

/** Binocular CLI Application
  *
  * Main entry point for the Binocular Bitcoin Oracle CLI. Uses the Decline library for argument
  * parsing and command routing.
  */
object CliApp {

    /** CLI command enum for routing */
    enum Cmd:
        case Info
        case Config
        case ListOracles(limit: Int)
        case VerifyOracle(utxo: String)
        case InitOracle(startBlock: Option[Long])
        case UpdateOracle(utxo: String, fromBlock: Option[Long], toBlock: Option[Long])
        case ProveTransaction(
            utxo: String,
            btcTxId: String,
            blockHash: Option[String],
            txIndex: Option[Int],
            proof: Option[String],
            merkleRoot: Option[String]
        )

    /** CLI argument parsers */
    object CliParsers {

        val limitOpt: Opts[Int] = Opts
            .option[Int](
              "limit",
              help = "Maximum number of results to return",
              short = "n"
            )
            .withDefault(10)

        val utxoArg: Opts[String] = Opts.argument[String](
          metavar = "UTXO"
        )

        val startBlockOpt: Opts[Option[Long]] = Opts
            .option[Long](
              "start-block",
              help = "Bitcoin block height to start from",
              short = "s"
            )
            .orNone

        val fromBlockOpt: Opts[Option[Long]] = Opts
            .option[Long](
              "from",
              help = "Start Bitcoin block height",
              short = "f"
            )
            .orNone

        val toBlockOpt: Opts[Option[Long]] = Opts
            .option[Long](
              "to",
              help = "End Bitcoin block height",
              short = "t"
            )
            .orNone

        val btcTxIdArg: Opts[String] = Opts.argument[String](
          metavar = "BTC_TX_ID"
        )

        val blockHashOpt: Opts[Option[String]] = Opts
            .option[String](
              "block",
              help = "Bitcoin block hash (64 hex chars)",
              short = "b"
            )
            .orNone

        val txIndexOpt: Opts[Option[Int]] = Opts
            .option[Int](
              "tx-index",
              help = "Transaction index in block (for offline verification)",
              short = "i"
            )
            .orNone

        val proofOpt: Opts[Option[String]] = Opts
            .option[String](
              "proof",
              help = "Merkle proof hashes (comma-separated, for offline verification)",
              short = "p"
            )
            .orNone

        val merkleRootOpt: Opts[Option[String]] = Opts
            .option[String](
              "merkle-root",
              help = "Expected merkle root (64 hex chars, for offline verification)",
              short = "m"
            )
            .orNone
    }

    /** Main CLI command parser */
    val command: Command[Cmd] = {
        import CliParsers.*

        val infoCommand = Opts.subcommand("info", "Display oracle information") {
            Opts(Cmd.Info)
        }

        val configCommand = Opts.subcommand("config", "Show complete configuration") {
            Opts(Cmd.Config)
        }

        val listCommand = Opts.subcommand("list-oracles", "List oracle UTxOs") {
            limitOpt.map(Cmd.ListOracles.apply)
        }

        val verifyCommand = Opts.subcommand("verify-oracle", "Verify oracle state") {
            utxoArg.map(Cmd.VerifyOracle.apply)
        }

        val initCommand = Opts.subcommand("init-oracle", "Initialize new oracle") {
            startBlockOpt.map(Cmd.InitOracle.apply)
        }

        val updateCommand = Opts.subcommand("update-oracle", "Update oracle with new blocks") {
            (utxoArg, fromBlockOpt, toBlockOpt).mapN(Cmd.UpdateOracle.apply)
        }

        val proveCommand =
            Opts.subcommand("prove-transaction", "Prove Bitcoin transaction inclusion") {
                (utxoArg, btcTxIdArg, blockHashOpt, txIndexOpt, proofOpt, merkleRootOpt).mapN(
                  Cmd.ProveTransaction.apply
                )
            }

        com.monovore.decline.Command(
          name = "binocular",
          header = "Binocular - Bitcoin Oracle for Cardano"
        )(
          infoCommand orElse
              configCommand orElse
              listCommand orElse
              verifyCommand orElse
              initCommand orElse
              updateCommand orElse
              proveCommand
        )
    }

    /** Execute the CLI application
      *
      * @param args
      *   Command line arguments
      * @return
      *   Exit code (0 for success, non-zero for error)
      */
    def run(args: Seq[String]): Int = {
        // Trim whitespace from arguments to handle extra spaces
        val trimmedArgs = args.map(_.trim).filter(_.nonEmpty)
        command.parse(trimmedArgs) match {
            case Left(help) =>
                System.err.println(help)
                1

            case Right(cmd) =>
                try {
                    // Route to appropriate command implementation
                    val commandImpl: binocular.cli.Command = cmd match {
                        case Cmd.Info =>
                            InfoCommand()
                        case Cmd.Config =>
                            ConfigCommand()
                        case Cmd.ListOracles(limit) =>
                            ListOraclesCommand(limit)
                        case Cmd.VerifyOracle(utxo) =>
                            VerifyOracleCommand(utxo)
                        case Cmd.InitOracle(startBlock) =>
                            InitOracleCommand(startBlock)
                        case Cmd.UpdateOracle(utxo, from, to) =>
                            UpdateOracleCommand(utxo, from, to)
                        case Cmd.ProveTransaction(
                              utxo,
                              btcTxId,
                              blockHash,
                              txIndex,
                              proof,
                              merkleRoot
                            ) =>
                            ProveTransactionCommand(
                              utxo,
                              btcTxId,
                              blockHash,
                              txIndex,
                              proof,
                              merkleRoot
                            )
                    }

                    // Execute command and return exit code
                    commandImpl.execute()
                } catch {
                    case e: Exception =>
                        System.err.println(s"Error: ${e.getMessage}")
                        e.printStackTrace()
                        1
                }
        }
    }
}
