package binocular.cli.commands

import binocular.{CardanoConfig, ChainState, OracleConfig}
import binocular.cli.{Command, CommandHelpers}
import scalus.cardano.address.Address
import scalus.cardano.ledger.{Utxo, Utxos}
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.fromData
import scalus.uplc.builtin.ByteString.given

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration.*
import scala.util.{Failure, Success, Try}

/** List oracle UTxOs on Cardano */
case class ListOraclesCommand(limit: Int) extends Command {

    override def execute(): Int = {
        println(s"Listing oracle UTxOs (limit: $limit)...")
        println()

        // Load configurations
        val oracleConfig = OracleConfig.load()
        val cardanoConfig = CardanoConfig.load()

        (oracleConfig, cardanoConfig) match {
            case (Right(oracle), Right(cardano)) =>
                given ec: ExecutionContext = ExecutionContext.global

                // Create blockchain provider
                cardano.createBlockchainProvider() match {
                    case Right(provider) =>
                        try {
                            val scriptAddress = oracle.scriptAddress
                            println(s"Oracle Script Address: $scriptAddress")
                            println(s"Network: ${oracle.network}")
                            println()

                            val address = Address.fromBech32(scriptAddress)
                            val utxosResult = Await.result(
                              provider.findUtxos(address),
                              30.seconds
                            )

                            val allUtxos: List[Utxo] = utxosResult match {
                                case Right(u) =>
                                    u.map { case (input, output) => Utxo(input, output) }.toList
                                case Left(err) =>
                                    System.err.println(s"Error fetching UTxOs: $err")
                                    return 1
                            }

                            val validOracles = CommandHelpers.filterValidOracleUtxos(allUtxos)

                            if validOracles.isEmpty then {
                                val oracleLikeUtxos = allUtxos.filter(CommandHelpers.isOracleUtxo)
                                if oracleLikeUtxos.nonEmpty then {
                                    println("No valid oracle UTxOs found.")
                                    println(
                                      s"Found ${oracleLikeUtxos.size} invalid oracle UTxO(s):"
                                    )
                                    oracleLikeUtxos.foreach { utxo =>
                                        println(
                                          s"  - ${utxo.input.transactionId.toHex}:${utxo.input.index}"
                                        )
                                        CommandHelpers.parseChainState(utxo) match {
                                            case Some(cs) =>
                                                val timestampCount = cs.recentTimestamps.size
                                                if timestampCount < 11 then
                                                    println(
                                                      s"    Only $timestampCount/11 timestamps"
                                                    )
                                                else if !CommandHelpers.isValidChainState(cs) then
                                                    println(s"    Timestamps not sorted")
                                            case None =>
                                                println(s"    Cannot parse ChainState")
                                        }
                                    }
                                } else if allUtxos.nonEmpty then {
                                    println(
                                      "No oracle UTxOs found (only reference script UTxOs present)."
                                    )
                                } else {
                                    println("No oracle UTxOs found.")
                                }
                                println()
                                println("To initialize a new oracle, run:")
                                println(
                                  "  binocular init-oracle --start-block <BITCOIN_BLOCK_HEIGHT>"
                                )
                            } else {
                                println(s"Found ${validOracles.size} valid oracle UTxO(s):")
                                println()

                                validOracles.foreach { vo =>
                                    val lovelace = vo.utxo.output.value.coin.value

                                    println(s"  - ${vo.utxoRef}")
                                    println(s"    Lovelace: $lovelace")
                                    println(s"    Block Height: ${vo.chainState.blockHeight}")
                                    println(
                                      s"    Block Hash: ${vo.chainState.blockHash.toHex.take(16)}..."
                                    )
                                    if vo.chainState.forksTree.nonEmpty then {
                                        val maxForkHeight = vo.chainState.forksTree.foldLeft(0L) {
                                            (max, branch) =>
                                                math.max(max, branch.tipHeight.toLong)
                                        }
                                        println(
                                          s"    Fork Tree: ${vo.chainState.forksTree.size} branch(es), highest at $maxForkHeight"
                                        )
                                    }
                                    println()
                                }
                            }
                            0
                        } catch {
                            case e: Exception =>
                                System.err.println(s"Error querying oracle UTxOs: ${e.getMessage}")
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
