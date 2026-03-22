package binocular.cli.commands

import binocular.*
import binocular.cli.{Command, CommandHelpers, Console}
import scalus.cardano.ledger.Utxo

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.util.boundary
import boundary.break
import scalus.utils.await

/** Close oracle, burn NFT, return min_ada */
case class CloseCommand() extends Command {

    override def execute(config: BinocularConfig): Int = {
        Console.header("Close Oracle")
        println()

        closeOracle(config)
    }

    private def closeOracle(config: BinocularConfig): Int = boundary {
        given ec: ExecutionContext = ExecutionContext.global
        val timeout = config.oracle.transactionTimeout.seconds

        val setup = CommandHelpers.setupOracle(config) match {
            case Right(s) => s
            case Left(err) =>
                Console.error(err)
                break(1)
        }

        Console.info("Oracle", setup.scriptAddress.encode.getOrElse("?"))
        Console.info(
          "Wallet",
          setup.hdAccount.baseAddress(config.cardano.scalusNetwork).toBech32.getOrElse("?")
        )

        // Find oracle UTxO by NFT
        val oracleUtxo: Utxo =
            try {
                CommandHelpers
                    .findOracleUtxo(setup.provider, setup.script.scriptHash)
                    .await(timeout)
            } catch {
                case e: Exception =>
                    Console.error(e.getMessage)
                    break(1)
            }

        val oracleRef = s"${oracleUtxo.input.transactionId.toHex}#${oracleUtxo.input.index}"
        Console.info("Oracle UTxO", oracleRef)

        // Find reference script
        val referenceScriptUtxo = CommandHelpers.findReferenceScriptUtxo(
          setup.provider,
          setup.scriptAddress,
          setup.script,
          timeout
        ) match {
            case Some(utxo) => utxo
            case None =>
                Console.error("Reference script not found. Run 'binocular init' first.")
                break(1)
        }

        Console.log("Building close transaction...")

        OracleTransactions.buildAndSubmitCloseTransaction(
          setup.provider,
          setup.hdAccount,
          oracleUtxo,
          referenceScriptUtxo,
          setup.compiled,
          timeout
        ) match {
            case Right(txHash) =>
                Console.logSuccess(s"Oracle closed | tx: $txHash")
                Console.info("NFT", "burned")
                Console.info("ADA", "returned to wallet")
                0
            case Left(err) =>
                Console.error(s"Failed to close oracle: $err")
                1
        }
    }
}
