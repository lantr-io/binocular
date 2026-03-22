package binocular.cli.commands

import binocular.*
import binocular.cli.{Command, CommandHelpers}
import scalus.cardano.address.Address
import scalus.cardano.ledger.Utxo

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scalus.utils.await
import cats.syntax.either.*

/** List oracle UTxOs on Cardano */
case class ListOraclesCommand(limit: Int) extends Command {

    override def execute(config: BinocularConfig): Int = {
        println(s"Listing oracle UTxOs (limit: $limit)...")
        println()

        val cardanoConf = config.cardano
        val oracleConf = config.oracle

        val oracleScriptAddress =
            oracleConf.scriptAddress(cardanoConf.cardanoNetwork).valueOr { err =>
                System.err.println(s"Error deriving script address: $err")
                return 1
            }

        given ec: ExecutionContext = ExecutionContext.global

        cardanoConf.createBlockchainProvider() match {
            case Right(provider) =>
                try {
                    println(s"Oracle Script Address: $oracleScriptAddress")
                    println(s"Network: ${cardanoConf.network}")
                    println()

                    val address = Address.fromBech32(oracleScriptAddress)
                    val utxosResult =
                        provider.findUtxos(address).await(30.seconds)

                    val allUtxos: List[Utxo] = utxosResult
                        .valueOr { err =>
                            System.err.println(s"Error fetching UTxOs: $err")
                            return 1
                        }
                        .map { case (input, output) => Utxo(input, output) }
                        .toList

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
                                        val timestampCount = cs.ctx.timestamps.size
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
                          "  binocular init --start-block <BITCOIN_BLOCK_HEIGHT>"
                        )
                    } else {
                        println(s"Found ${validOracles.size} valid oracle UTxO(s):")
                        println()

                        validOracles.foreach { vo =>
                            val lovelace = vo.utxo.output.value.coin.value

                            println(s"  - ${vo.utxoRef}")
                            println(s"    Lovelace: $lovelace")
                            println(s"    Block Height: ${vo.chainState.ctx.height}")
                            println(
                              s"    Block Hash: ${vo.chainState.ctx.lastBlockHash.toHex.take(16)}..."
                            )
                            if vo.chainState.forkTree.nonEmpty then {
                                val maxForkHeight =
                                    vo.chainState.forkTree
                                        .highestHeight(vo.chainState.ctx.height)
                                        .toLong
                                println(
                                  s"    Fork Tree: ${vo.chainState.forkTree.blockCount} block(s), highest at $maxForkHeight"
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
    }
}
