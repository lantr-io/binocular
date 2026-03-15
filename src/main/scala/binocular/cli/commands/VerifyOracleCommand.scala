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

    override def execute(): Int = {
        println(s"Verifying oracle at $utxo...")
        println()

        // Parse UTxO string
        CommandHelpers.parseUtxo(utxo) match {
            case Left(err) =>
                System.err.println(s"Error: $err")
                return 1
            case Right((txHash, outputIndex)) =>
                verifyOracle(txHash, outputIndex)
        }
    }

    private def verifyOracle(txHash: String, outputIndex: Int): Int = {
        // Load configurations
        val oracleConfig = OracleConfig.load()
        val cardanoConfig = CardanoConfig.load()

        (oracleConfig, cardanoConfig) match {
            case (Right(oracle), Right(cardano)) =>
                given ec: ExecutionContext = ExecutionContext.global

                cardano.createBlockchainProvider() match {
                    case Right(provider) =>
                        try {
                            // Fetch specific UTxO
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

                            // Verify it's at the oracle script address
                            if addr != oracle.scriptAddress then {
                                println()
                                println(s"Warning: UTxO is not at the oracle script address")
                                println(s"  Expected: ${oracle.scriptAddress}")
                                println(s"  Found:    $addr")
                            } else {
                                println(s"UTxO is at correct oracle script address")
                            }

                            // Try to parse datum
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
            case (Left(err), _) =>
                System.err.println(s"Error loading oracle config: $err")
                1
            case (_, Left(err)) =>
                System.err.println(s"Error loading cardano config: $err")
                1
        }
    }
}
