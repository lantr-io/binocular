package binocular.cli.commands

import binocular.*
import binocular.cli.{Command, CommandHelpers}
import com.typesafe.scalalogging.LazyLogging
import scalus.cardano.ledger.Utxo

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scalus.utils.await

/** Close oracle, burn NFT, return min_ada */
case class CloseCommand() extends Command with LazyLogging {

    override def execute(config: BinocularConfig): Int = {
        logger.info(s"Closing oracle...")

        closeOracle(config)
    }

    private def closeOracle(config: BinocularConfig): Int = {
        given ec: ExecutionContext = ExecutionContext.global
        val timeout = config.oracle.transactionTimeout.seconds

        val setup = CommandHelpers.setupOracle(config) match {
            case Right(s) => s
            case Left(err) =>
                System.err.println(s"Error: $err")
                return 1
        }

        // Find oracle UTxO by NFT
        val oracleUtxo: Utxo =
            try {
                CommandHelpers
                    .findOracleUtxo(setup.provider, setup.script.scriptHash)
                    .await(timeout)
            } catch {
                case e: Exception =>
                    System.err.println(s"Error: ${e.getMessage}")
                    return 1
            }

        val oracleRef = s"${oracleUtxo.input.transactionId.toHex}:${oracleUtxo.input.index}"
        logger.info(s"Found oracle UTxO: $oracleRef")

        // Find reference script
        val referenceScriptUtxo = CommandHelpers.findReferenceScriptUtxo(
          setup.provider,
          setup.scriptAddress,
          setup.script.scriptHash,
          timeout
        )

        logger.info("Building close transaction...")

        OracleTransactions.buildAndSubmitCloseTransaction(
          setup.signer,
          setup.provider,
          setup.scriptAddress,
          setup.sponsorAddress,
          oracleUtxo,
          setup.script,
          referenceScriptUtxo,
          timeout
        ) match {
            case Right(closeTxHash) =>
                println()
                println("Oracle closed successfully!")
                println(s"  Transaction Hash: $closeTxHash")
                println(s"  NFT burned, min_ada returned to wallet")
                0
            case Left(err) =>
                System.err.println(s"Error closing oracle: $err")
                1
        }
    }
}
