package binocular.cli.commands

import binocular.*
import binocular.cli.{Command, CommandHelpers}
import com.typesafe.scalalogging.LazyLogging

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import cats.syntax.either.*

/** Deploy the oracle validator as a reference script UTxO */
case class DeployScriptCommand() extends Command with LazyLogging {

    override def execute(config: BinocularConfig): Int = {
        logger.info("Deploying oracle validator reference script...")

        given ec: ExecutionContext = ExecutionContext.global
        val timeout = config.oracle.transactionTimeout.seconds

        val setup = CommandHelpers.setupOracle(config).valueOr { err =>
            System.err.println(s"Error: $err")
            return 1
        }

        // Check if already deployed
        CommandHelpers.findReferenceScriptUtxo(
          setup.provider,
          setup.scriptAddress,
          setup.script,
          timeout
        ) match {
            case Some(u) =>
                val ref = s"${u.input.transactionId.toHex}:${u.input.index}"
                println(s"Reference script already deployed at $ref")
                return 0
            case None =>
        }

        logger.info("No existing reference script found. Deploying...")

        OracleTransactions.deployReferenceScript(
          setup.signer,
          setup.provider,
          setup.sponsorAddress,
          setup.scriptAddress,
          setup.script,
          timeout
        ) match {
            case Right((deployTxHash, deployOutputIdx, _)) =>
                println()
                println("Reference script deployed successfully!")
                println(s"  TX Hash: $deployTxHash")
                println(s"  Output Index: $deployOutputIdx")
                println(s"  Reference: $deployTxHash:$deployOutputIdx")
                0
            case Left(err) =>
                System.err.println(s"Error deploying reference script: $err")
                1
        }
    }
}
