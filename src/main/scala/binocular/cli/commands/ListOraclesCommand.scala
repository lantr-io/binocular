package binocular.cli.commands

import binocular.{CardanoConfig, ChainState, OracleConfig}
import binocular.cli.{Command, CommandHelpers}
import scalus.builtin.Data
import scalus.builtin.Data.fromData
import scalus.builtin.ByteString.given

import scala.jdk.CollectionConverters.*
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
                // Create backend service
                cardano.createBackendService() match {
                    case Right(backendService) =>
                        try {
                            val scriptAddress = oracle.scriptAddress
                            println(s"Oracle Script Address: $scriptAddress")
                            println(s"Network: ${oracle.network}")
                            println()

                            // Query UTxOs at script address
                            val utxosResult = backendService.getUtxoService
                                .getUtxos(scriptAddress, limit, 1)
                            val utxos = Option(utxosResult.getValue).getOrElse(java.util.Collections.emptyList())

                            val allUtxos = utxos.asScala.toList
                            val validOracles = CommandHelpers.filterValidOracleUtxos(allUtxos)

                            if validOracles.isEmpty then {
                                // Check if there are any oracle-like UTxOs (with datum) that are invalid
                                val oracleLikeUtxos = allUtxos.filter(CommandHelpers.isOracleUtxo)
                                if oracleLikeUtxos.nonEmpty then {
                                    println("No valid oracle UTxOs found.")
                                    println(s"Found ${oracleLikeUtxos.size} invalid oracle UTxO(s):")
                                    oracleLikeUtxos.foreach { utxo =>
                                        println(s"  • ${utxo.getTxHash}:${utxo.getOutputIndex}")
                                        CommandHelpers.parseChainState(utxo) match {
                                            case Some(cs) =>
                                                val timestampCount = cs.recentTimestamps.size
                                                if timestampCount < 11 then
                                                    println(s"    ⚠ Only $timestampCount/11 timestamps")
                                                else if !CommandHelpers.isValidChainState(cs) then
                                                    println(s"    ⚠ Timestamps not sorted")
                                            case None =>
                                                println(s"    ⚠ Cannot parse ChainState")
                                        }
                                    }
                                } else if allUtxos.nonEmpty then {
                                    println("No oracle UTxOs found (only reference script UTxOs present).")
                                } else {
                                    println("No oracle UTxOs found.")
                                }
                                println()
                                println("To initialize a new oracle, run:")
                                println("  binocular init-oracle --start-block <BITCOIN_BLOCK_HEIGHT>")
                            } else {
                                println(s"Found ${validOracles.size} valid oracle UTxO(s):")
                                println()

                                validOracles.foreach { vo =>
                                    val lovelace = vo.utxo.getAmount.asScala.headOption
                                        .map(_.getQuantity)
                                        .getOrElse("0")

                                    println(s"  • ${vo.utxoRef}")
                                    println(s"    Lovelace: $lovelace")
                                    println(s"    Block Height: ${vo.chainState.blockHeight}")
                                    println(s"    Block Hash: ${vo.chainState.blockHash.toHex.take(16)}...")
                                    if vo.chainState.forksTree.nonEmpty then {
                                        val maxForkHeight = vo.chainState.forksTree.foldLeft(0L) { (max, branch) =>
                                            math.max(max, branch.tipHeight.toLong)
                                        }
                                        println(s"    Fork Tree: ${vo.chainState.forksTree.size} branch(es), highest at $maxForkHeight")
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
                        System.err.println(s"Error creating backend service: $err")
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
