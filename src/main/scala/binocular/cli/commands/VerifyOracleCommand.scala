package binocular.cli.commands

import binocular.{OracleConfig, CardanoConfig}
import binocular.cli.{Command, CommandHelpers}

import scala.jdk.CollectionConverters.*

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

                            if (!utxos.isSuccessful || utxos.getValue == null) {
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
                            if (foundUtxo.getAddress != oracle.scriptAddress) {
                                println()
                                println(s"⚠ Warning: UTxO is not at the oracle script address")
                                println(s"  Expected: ${oracle.scriptAddress}")
                                println(s"  Found:    ${foundUtxo.getAddress}")
                            } else {
                                println(s"✓ UTxO is at correct oracle script address")
                            }

                            // Try to parse datum
                            val datumCbor = Option(foundUtxo.getInlineDatum).orElse(Option(foundUtxo.getDataHash))
                            datumCbor match {
                                case Some(datum) =>
                                    println()
                                    println(s"✓ Datum found")
                                    println(s"  CBOR (truncated): ${datum.take(100)}...")

                                    println()
                                    println("✓ Oracle UTxO has datum (ChainState parsing not yet implemented)")

                                case None =>
                                    println()
                                    println(s"⚠ Warning: No datum found")
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
