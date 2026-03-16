package binocular.cli.commands

import binocular.*
import binocular.cli.{Command, CommandHelpers}
import scalus.cardano.ledger.{TransactionHash, TransactionInput, Utxo}
import scalus.uplc.builtin.Data.fromData

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.util.{Failure, Success, Try}
import scalus.utils.await

/** Verify oracle UTxO and validate state */
case class VerifyOracleCommand(utxo: String) extends Command {

    override def execute(config: BinocularConfig): Int = {
        println(s"Verifying oracle at $utxo...")
        println()

        CommandHelpers.parseUtxo(utxo) match {
            case Left(err) =>
                System.err.println(s"Error: $err")
                return 1
            case Right((txHash, outputIndex)) =>
                verifyOracle(txHash, outputIndex, config)
        }
    }

    private def verifyOracle(txHash: String, outputIndex: Int, config: BinocularConfig): Int = {
        val cardanoConf = config.cardano
        val oracleConf = config.oracle

        val oracleScriptAddress = oracleConf.scriptAddress(cardanoConf.cardanoNetwork) match {
            case Right(addr) => addr
            case Left(err) =>
                System.err.println(s"Error deriving script address: $err")
                return 1
        }

        given ec: ExecutionContext = ExecutionContext.global

        cardanoConf.createBlockchainProvider() match {
            case Right(provider) =>
                try {
                    val input =
                        TransactionInput(TransactionHash.fromHex(txHash), outputIndex)
                    val utxoResult =
                        provider.findUtxo(input).await(30.seconds)

                    val foundUtxo: Utxo = utxoResult match {
                        case Right(u) => u
                        case Left(_) =>
                            System.err.println(
                              s"Error: UTxO not found at $txHash:$outputIndex"
                            )
                            return 1
                    }

                    println(s"UTxO found")
                    val addr = foundUtxo.output.address.encode.getOrElse("?")
                    println(s"  Address: $addr")

                    val lovelace = foundUtxo.output.value.coin.value
                    println(s"  Lovelace: $lovelace")

                    if addr != oracleScriptAddress then {
                        println()
                        println(s"Warning: UTxO is not at the oracle script address")
                        println(s"  Expected: $oracleScriptAddress")
                        println(s"  Found:    $addr")
                    } else {
                        println(s"UTxO is at correct oracle script address")
                    }

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
                                    println(s"  Block Height: ${chainState.blockHeight}")
                                    println(s"  Block Hash: ${chainState.blockHash.toHex}")
                                    println(
                                      s"  Current Target: ${chainState.currentTarget.toHex}"
                                    )
                                    println(
                                      s"  Recent Timestamps: ${chainState.recentTimestamps.size} entries"
                                    )

                                    val timestamps =
                                        chainState.recentTimestamps.toScalaList
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
                                      s"  Previous Diff Adjustment: ${chainState.previousDifficultyAdjustmentTimestamp}"
                                    )
                                    println(
                                      s"  Confirmed Blocks Root: ${chainState.confirmedBlocksRoot.toHex}"
                                    )
                                    println(
                                      s"  Forks Tree: ${chainState.forkTree.blockCount} block(s)"
                                    )

                                    if chainState.forkTree.nonEmpty then {
                                        println()
                                        println("  Fork tree:")
                                        println(
                                          chainState.forkTree
                                              .displayTree(chainState.blockHeight, "    ")
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
