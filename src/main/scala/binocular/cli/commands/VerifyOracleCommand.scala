package binocular.cli.commands

import binocular.*
import binocular.ForkTreePretty.*
import binocular.cli.{Command, CommandHelpers}
import scalus.cardano.ledger.Utxo
import scalus.uplc.builtin.Data.fromData

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.util.{Failure, Success, Try}
import scalus.utils.await

/** Verify oracle UTxO and validate state */
case class VerifyOracleCommand() extends Command {

    override def execute(config: BinocularConfig): Int = {
        println(s"Verifying oracle...")
        println()

        verifyOracle(config)
    }

    private def verifyOracle(config: BinocularConfig): Int = {
        val cardanoConf = config.cardano
        val oracleConf = config.oracle

        val params = oracleConf.toBitcoinValidatorParams() match {
            case Right(p) => p
            case Left(err) =>
                System.err.println(s"Error deriving params: $err")
                return 1
        }

        val script = BitcoinContract.makeContract(params).script

        given ec: ExecutionContext = ExecutionContext.global

        cardanoConf.createBlockchainProvider() match {
            case Right(provider) =>
                try {
                    val foundUtxo: Utxo = CommandHelpers
                        .findOracleUtxo(provider, script.scriptHash)
                        .await(30.seconds)

                    val oracleRef =
                        s"${foundUtxo.input.transactionId.toHex}:${foundUtxo.input.index}"
                    println(s"Oracle UTxO found: $oracleRef")
                    val addr = foundUtxo.output.address.encode.getOrElse("?")
                    println(s"  Address: $addr")

                    val lovelace = foundUtxo.output.value.coin.value
                    println(s"  Lovelace: $lovelace")

                    foundUtxo.output.inlineDatum match {
                        case Some(data) =>
                            println()
                            println(s"Inline datum found")

                            Try {
                                fromData[ChainState](data)
                            } match {
                                case Success(chainState) =>
                                    println()
                                    println("ChainState parsed successfully:")
                                    println(s"  Block Height: ${chainState.ctx.height}")
                                    println(s"  Block Hash: ${chainState.ctx.lastBlockHash.toHex}")
                                    println(
                                      s"  Current Target: ${chainState.ctx.currentBits.toHex}"
                                    )
                                    println(
                                      s"  Recent Timestamps: ${chainState.ctx.timestamps.size} entries"
                                    )

                                    val timestamps =
                                        chainState.ctx.timestamps.toScalaList
                                    println(s"    Values: ${timestamps.mkString(", ")}")
                                    val isSorted = timestamps.sliding(2).forall {
                                        case Seq(a, b) => a >= b
                                        case _         => true
                                    }
                                    println(s"    Sorted (descending): $isSorted")
                                    if timestamps.nonEmpty then {
                                        val median = timestamps(timestamps.size / 2)
                                        println(s"    Median: $median")
                                    }
                                    println(
                                      s"  Previous Diff Adjustment: ${chainState.ctx.prevDiffAdjTimestamp}"
                                    )
                                    println(
                                      s"  Confirmed Blocks Root: ${chainState.confirmedBlocksRoot.toHex}"
                                    )
                                    println(
                                      s"  Forks Tree: ${chainState.forkTree.blockCount} block(s)"
                                    )

                                    if chainState.forkTree.nonEmpty then {
                                        println()
                                        println(
                                          chainState.forkTree
                                              .pretty(chainState.ctx.height)
                                        )
                                    }

                                case Failure(ex) =>
                                    println()
                                    println(s"Error parsing ChainState: ${ex.getMessage}")
                                    ex.printStackTrace()
                            }

                        case None =>
                            println()
                            println(s"Warning: No inline datum found")
                            println("  Oracle UTxOs should have inline datums")
                    }

                    0
                } catch {
                    case e: Exception =>
                        System.err.println(s"Error verifying oracle: ${e.getMessage}")
                        e.printStackTrace()
                        1
                }
            case Left(err) =>
                System.err.println(s"Error creating blockchain provider: $err")
                1
        }
    }
}
