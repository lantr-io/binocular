package binocular.cli.commands

import binocular.{CardanoConfig, OracleConfig}
import binocular.cli.Command

import scala.jdk.CollectionConverters.*

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
                            val utxos = backendService.getUtxoService
                                .getUtxos(scriptAddress, limit, 1)
                                .getValue

                            if utxos.isEmpty then {
                                println("No oracle UTxOs found.")
                                println()
                                println("To initialize a new oracle, run:")
                                println(
                                  "  binocular init-oracle --start-block <BITCOIN_BLOCK_HEIGHT>"
                                )
                            } else {
                                val utxoList = utxos.asScala.toList

                                println(s"Found ${utxoList.size} oracle UTxO(s):")
                                println()

                                utxoList.foreach { utxo =>
                                    val txHash = utxo.getTxHash
                                    val outputIndex = utxo.getOutputIndex
                                    val lovelace = utxo.getAmount.asScala.headOption
                                        .map(_.getQuantity)
                                        .getOrElse("0")

                                    println(s"  • $txHash:$outputIndex")
                                    println(s"    Lovelace: $lovelace")

                                    // Try to show datum info if available
                                    Option(utxo.getInlineDatum).foreach { datum =>
                                        println(s"    Datum: ${datum.take(100)}...")
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
