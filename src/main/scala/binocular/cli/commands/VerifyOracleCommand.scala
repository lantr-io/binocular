package binocular.cli.commands

import binocular.{CardanoConfig, ChainState, OracleConfig}
import binocular.cli.{Command, CommandHelpers}
import scalus.builtin.Data
import scalus.builtin.Data.{FromData, fromData}
import scalus.builtin.ByteString.given

import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try}

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
                cardano.createBackendService() match {
                    case Right(backendService) =>
                        try {
                            import scala.jdk.CollectionConverters.*

                            // Fetch specific UTxO
                            val utxoService = backendService.getUtxoService
                            val utxos = utxoService.getTxOutput(txHash, outputIndex)

                            if !utxos.isSuccessful || utxos.getValue == null then {
                                System.err.println(s"Error: UTxO not found at $txHash:$outputIndex")
                                return 1
                            }

                            val foundUtxo = utxos.getValue
                            println(s"✓ UTxO found")
                            println(s"  Address: ${foundUtxo.getAddress}")

                            val lovelace = foundUtxo.getAmount.asScala.headOption
                                .map(_.getQuantity)
                                .getOrElse("0")
                            println(s"  Lovelace: $lovelace")

                            // Verify it's at the oracle script address
                            if foundUtxo.getAddress != oracle.scriptAddress then {
                                println()
                                println(s"⚠ Warning: UTxO is not at the oracle script address")
                                println(s"  Expected: ${oracle.scriptAddress}")
                                println(s"  Found:    ${foundUtxo.getAddress}")
                            } else {
                                println(s"✓ UTxO is at correct oracle script address")
                            }

                            // Try to parse datum
                            val datumCbor = Option(foundUtxo.getInlineDatum)
                            datumCbor match {
                                case Some(datumHex) =>
                                    println()
                                    println(s"✓ Inline datum found")

                                    // Parse ChainState from datum
                                    Try {
                                        import scalus.builtin.ByteString
                                        val bs = ByteString.fromHex(datumHex)
                                        val data = Data.fromCbor(bs)
                                        val chainState = fromData[ChainState](data)
                                        chainState
                                    } match {
                                        case Success(chainState) =>
                                            println()
                                            println("✓ ChainState parsed successfully:")
                                            println(s"  Block Height: ${chainState.blockHeight}")
                                            println(s"  Block Hash: ${chainState.blockHash.toHex}")
                                            println(s"  Block Timestamp: ${chainState.blockTimestamp}")
                                            println(s"  Current Target: ${chainState.currentTarget.toHex}")
                                            println(s"  Recent Timestamps: ${chainState.recentTimestamps.size} entries")
                                            println(s"  Previous Diff Adjustment: ${chainState.previousDifficultyAdjustmentTimestamp}")
                                            println(s"  Confirmed Blocks Tree: ${chainState.confirmedBlocksTree.size} levels")
                                            println(s"  Forks Tree: ${chainState.forksTree.size} branches")

                                            if chainState.forksTree.nonEmpty then {
                                                println()
                                                println("  Fork tree branches:")
                                                chainState.forksTree.take(5).foreach { branch =>
                                                    println(s"    - Branch tip: ${branch.tipHash.toHex.take(16)}..., height: ${branch.tipHeight}, chainwork: ${branch.tipChainwork}, blocks: ${branch.recentBlocks.size}")
                                                }
                                                if chainState.forksTree.size > 5 then {
                                                    println(s"    ... and ${chainState.forksTree.size - 5} more branches")
                                                }
                                            }

                                        case Failure(ex) =>
                                            println()
                                            println(s"✗ Error parsing ChainState: ${ex.getMessage}")
                                            println(s"  CBOR (truncated): ${datumHex.take(100)}...")
                                            ex.printStackTrace()
                                    }

                                case None =>
                                    println()
                                    println(s"⚠ Warning: No inline datum found")
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
