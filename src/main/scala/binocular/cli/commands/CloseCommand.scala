package binocular.cli.commands

import binocular.*
import binocular.cli.{Command, CommandHelpers, Console}
import cats.syntax.either.*
import scalus.cardano.ledger.Utxo
import scalus.utils.await

import java.time.Instant
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.util.{boundary, Try}
import scala.util.boundary.break

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

        val setup = CommandHelpers.setupOracle(config).valueOr { err =>
            Console.error(err)
            break(1)
        }

        Console.info("Oracle", setup.scriptAddress.encode.getOrElse("?"))
        Console.info(
          "Wallet",
          setup.hdAccount.baseAddress(config.cardano.scalusNetwork).toBech32.getOrElse("?")
        )
        CommandHelpers.printParams(setup.params)

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

        // Pre-flight staleness check: replicate the validator's rule
        //   intervalEndInSeconds - chainState.ctx.timestamps.head > closureTimeout
        // so the user sees exactly why a close will/won't succeed before paying for a
        // failing tx. Uses the close tx's validity-interval END (the upper bound that
        // ends up as on-chain currentTime), not just `now`, because the validator
        // evaluates against intervalEnd.
        val stalenessReport: Option[Boolean] = Try {
            val state = oracleUtxo.output.requireInlineDatum.to[ChainState]
            val headTs = state.ctx.timestamps.head.toLong
            val (_, validityIntervalTimeSeconds) =
                OracleTransactions.computeValidityIntervalTime(setup.provider.cardanoInfo)
            val intervalEnd = validityIntervalTimeSeconds.toLong
            val gap = intervalEnd - headTs
            val limit = setup.params.closureTimeout.toLong
            val canClose = gap > limit
            Console.info(
              "Last confirmed block ts",
              s"$headTs (${Instant.ofEpochSecond(headTs)})"
            )
            Console.info(
              "Validity interval end",
              s"$intervalEnd (${Instant.ofEpochSecond(intervalEnd)})"
            )
            Console.info(
              "Age vs closure timeout",
              s"${gap}s / ${limit}s — ${
                      if canClose then "stale (can close)" else "fresh (cannot close)"
                  }"
            )
            canClose
        }.toOption

        if stalenessReport.contains(false) then {
            Console.error(
              "Oracle is not yet stale. Wait until no new confirmed-block timestamp " +
                  "is within the closure-timeout window, then retry."
            )
            break(1)
        }

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
