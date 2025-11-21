package binocular.cli.commands

import binocular.{CardanoConfig, ChainState, OracleConfig}
import binocular.cli.Command
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

                            if utxos.isEmpty then {
                                println("No oracle UTxOs found.")
                                println()
                                println("To initialize a new oracle, run:")
                                println(
                                  "  binocular init-oracle --start-block <BITCOIN_BLOCK_HEIGHT>"
                                )
                            } else {
                                // Filter out reference script UTxOs (they have scriptRef but no inline datum)
                                val oracleUtxos = utxos.asScala.toList.filter { utxo =>
                                    // Oracle UTxOs must have inline datum and no reference script
                                    utxo.getInlineDatum != null && utxo.getReferenceScriptHash == null
                                }

                                if oracleUtxos.isEmpty then {
                                    println("No oracle UTxOs found (only reference script UTxOs present).")
                                    println()
                                    println("To initialize a new oracle, run:")
                                    println(
                                      "  binocular init-oracle --start-block <BITCOIN_BLOCK_HEIGHT>"
                                    )
                                } else {
                                    println(s"Found ${oracleUtxos.size} oracle UTxO(s):")
                                    println()

                                    oracleUtxos.foreach { utxo =>
                                    val txHash = utxo.getTxHash
                                    val outputIndex = utxo.getOutputIndex
                                    val lovelace = utxo.getAmount.asScala.headOption
                                        .map(_.getQuantity)
                                        .getOrElse("0")

                                    println(s"  • $txHash:$outputIndex")
                                    println(s"    Lovelace: $lovelace")

                                    // Try to parse and show ChainState info
                                    Option(utxo.getInlineDatum) match {
                                        case Some(datumHex) =>
                                            Try {
                                                import scalus.builtin.ByteString
                                                val bs = ByteString.fromHex(datumHex)
                                                val data = Data.fromCbor(bs)
                                                val chainState = fromData[ChainState](data)
                                                chainState
                                            } match {
                                                case Success(chainState) =>
                                                    println(s"    Block Height: ${chainState.blockHeight}")
                                                    println(s"    Block Hash: ${chainState.blockHash.toHex.take(16)}...")
                                                    if chainState.forksTree.nonEmpty then {
                                                        val maxForkHeight = chainState.forksTree.foldLeft(0L) { (max, branch) =>
                                                            math.max(max, branch.tipHeight.toLong)
                                                        }
                                                        println(s"    Fork Tree: ${chainState.forksTree.size} branch(es), highest at $maxForkHeight")
                                                    }
                                                    // Validate datum integrity
                                                    val timestampCount = chainState.recentTimestamps.size
                                                    if timestampCount < 11 then {
                                                        println(s"    ⚠ INVALID: Only $timestampCount/11 timestamps (oracle unusable)")
                                                    } else {
                                                        // Check if timestamps are properly sorted (descending)
                                                        val timestamps = chainState.recentTimestamps.toList
                                                        val isSorted = timestamps.sliding(2).forall {
                                                            case Seq(a, b) => a >= b
                                                            case _ => true
                                                        }
                                                        if !isSorted then {
                                                            println(s"    ⚠ INVALID: Timestamps not sorted (oracle unusable)")
                                                        }
                                                    }
                                                case Failure(e) =>
                                                    println(s"    ⚠ INVALID: Cannot parse datum as ChainState")
                                                    println(s"    Error: ${e.getMessage}")
                                            }
                                        case None =>
                                            println(s"    No inline datum")
                                    }
                                    println()
                                }
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
