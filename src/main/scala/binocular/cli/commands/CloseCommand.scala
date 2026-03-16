package binocular.cli.commands

import binocular.*
import binocular.cli.{Command, CommandHelpers}
import com.typesafe.scalalogging.LazyLogging
import scalus.cardano.ledger.{TransactionHash, TransactionInput, Utxo}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scalus.utils.await

/** Close oracle, burn NFT, return min_ada */
case class CloseCommand(utxo: String) extends Command with LazyLogging {

    override def execute(config: BinocularConfig): Int = {
        logger.info(s"Closing oracle at $utxo...")

        CommandHelpers.parseUtxo(utxo) match {
            case Left(err) =>
                System.err.println(s"Error: $err")
                return 1
            case Right((txHash, outputIndex)) =>
                closeOracle(txHash, outputIndex, config)
        }
    }

    private def closeOracle(txHash: String, outputIndex: Int, config: BinocularConfig): Int = {
        given ec: ExecutionContext = ExecutionContext.global
        val timeout = config.oracle.transactionTimeout.seconds

        val setup = CommandHelpers.setupOracle(config) match {
            case Right(s) => s
            case Left(err) =>
                System.err.println(s"Error: $err")
                return 1
        }

        // Fetch oracle UTxO
        val input = TransactionInput(TransactionHash.fromHex(txHash), outputIndex)
        val oracleUtxo: Utxo = setup.provider.findUtxo(input).await(timeout) match {
            case Right(u) => u
            case Left(_) =>
                System.err.println(s"Error: Oracle UTxO not found at $txHash:$outputIndex")
                return 1
        }

        logger.info(s"Found oracle UTxO: $txHash:$outputIndex")

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
