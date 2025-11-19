package binocular.cli.commands

import binocular.cli.{Command, CommandHelpers}
import binocular.*
import com.bloxbean.cardano.client.address.Address
import scalus.utils.Hex.hexToBytes
import scalus.bloxbean.Interop.toScalusData
import scalus.builtin.Data.fromData
import scalus.builtin.{ByteString, Data}

import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.jdk.CollectionConverters.*

/** Update oracle with new Bitcoin blocks */
case class UpdateOracleCommand(
    utxo: String,
    fromBlock: Option[Long],
    toBlock: Option[Long]
) extends Command {

    override def execute(): Int = {
        println(s"Updating oracle at $utxo...")
        println()

        // Parse UTxO string
        CommandHelpers.parseUtxo(utxo) match {
            case Left(err) =>
                System.err.println(s"Error: $err")
                return 1
            case Right((txHash, outputIndex)) =>
                updateOracle(txHash, outputIndex)
        }
    }

    private def updateOracle(txHash: String, outputIndex: Int): Int = {
        // Load configurations
        val bitcoinConfig = BitcoinNodeConfig.load()
        val cardanoConfig = CardanoConfig.load()
        val oracleConfig = OracleConfig.load()
        val walletConfig = WalletConfig.load()

        (bitcoinConfig, cardanoConfig, oracleConfig, walletConfig) match {
            case (Right(btcConf), Right(cardanoConf), Right(oracleConf), Right(walletConf)) =>
                println("Step 1: Loading configurations...")
                println(s"✓ Bitcoin Node: ${btcConf.url}")
                println(s"✓ Cardano Network: ${cardanoConf.network}")
                println(s"✓ Oracle Address: ${oracleConf.scriptAddress}")
                println()

                // Create account for signing
                val account = walletConf.createAccount() match {
                    case Right(acc) =>
                        println(s"✓ Wallet loaded: ${acc.baseAddress()}")
                        acc
                    case Left(err) =>
                        System.err.println(s"Error creating wallet account: $err")
                        return 1
                }

                // Create Cardano backend service
                val backendService = cardanoConf.createBackendService() match {
                    case Right(service) =>
                        println(s"✓ Connected to Cardano backend (${cardanoConf.backend})")
                        service
                    case Left(err) =>
                        System.err.println(s"Error creating backend service: $err")
                        return 1
                }

                println()
                println("Step 2: Fetching current oracle UTxO from Cardano...")

                // Fetch the specific UTxO
                val scriptAddress = new Address(oracleConf.scriptAddress)
                val utxoService = backendService.getUtxoService
                val utxos =
                    try {
                        utxoService.getUtxos(scriptAddress.getAddress, 100, 1)
                    } catch {
                        case e: Exception =>
                            System.err.println(s"✗ Error fetching UTxOs: ${e.getMessage}")
                            return 1
                    }

                if !utxos.isSuccessful then {
                    System.err.println(s"✗ Error fetching UTxOs: ${utxos.getResponse}")
                    return 1
                }

                import scala.jdk.CollectionConverters.*
                val allUtxos = utxos.getValue.asScala.toList
                val targetUtxo =
                    allUtxos.find(u => u.getTxHash == txHash && u.getOutputIndex == outputIndex)

                targetUtxo match {
                    case None =>
                        System.err.println(s"✗ UTxO not found: $txHash:$outputIndex")
                        System.err.println(s"  Available UTxOs:")
                        allUtxos.foreach { u =>
                            System.err.println(s"    ${u.getTxHash}:${u.getOutputIndex}")
                        }
                        return 1

                    case Some(utxo) =>
                        println(s"✓ Found oracle UTxO: $txHash:$outputIndex")

                        // Parse ChainState datum
                        println()
                        println("Step 3: Parsing current ChainState datum...")

                        val inlineDatumHex = utxo.getInlineDatum
                        if inlineDatumHex == null || inlineDatumHex.isEmpty then {
                            System.err.println("✗ UTxO has no inline datum")
                            return 1
                        }

                        val currentChainState =
                            try {
                                val data = Data.fromCbor(inlineDatumHex.hexToBytes)
                                // Parse as ChainState - use extension method on Data
                                data.to[ChainState]
                            } catch {
                                case e: Exception =>
                                    System.err.println(
                                      s"✗ Error parsing ChainState datum: ${e.getMessage}"
                                    )
                                    e.printStackTrace()
                                    return 1
                            }

                        println(s"✓ Current oracle state:")
                        println(s"  Block Height: ${currentChainState.blockHeight}")
                        println(s"  Block Hash: ${currentChainState.blockHash.toHex}")
                        println(s"  Fork Tree Size: ${currentChainState.forksTree.size}")

                        // Determine block range
                        val startHeight =
                            fromBlock.getOrElse(currentChainState.blockHeight.toLong + 1)
                        val endHeight = toBlock match {
                            case Some(h) => h
                            case None    =>
                                // Fetch current Bitcoin chain tip
                                given ec: ExecutionContext = ExecutionContext.global
                                val rpc = new SimpleBitcoinRpc(btcConf)
                                val infoFuture = rpc.getBlockchainInfo()
                                try {
                                    val info = Await.result(infoFuture, 30.seconds)
                                    info.blocks.toLong
                                } catch {
                                    case e: Exception =>
                                        System.err.println(
                                          s"✗ Error fetching blockchain info: ${e.getMessage}"
                                        )
                                        return 1
                                }
                        }

                        if startHeight > endHeight then {
                            System.err.println(s"✗ Invalid block range: $startHeight to $endHeight")
                            return 1
                        }

                        val numBlocks = (endHeight - startHeight + 1).toInt
                        val batchSize = oracleConf.maxHeadersPerTx

                        // Create execution context for async operations
                        given ec: ExecutionContext = ExecutionContext.global
                        val rpc = new SimpleBitcoinRpc(btcConf)

                        // Split into batches if needed
                        val batches = (startHeight to endHeight).grouped(batchSize).toList
                        val totalBatches = batches.size

                        if totalBatches > 1 then {
                            println()
                            println(
                              s"Step 4: Processing $numBlocks blocks in $totalBatches batches of up to $batchSize headers each..."
                            )
                        } else {
                            println()
                            println(
                              s"Step 4: Fetching Bitcoin headers from block $startHeight to $endHeight ($numBlocks headers)..."
                            )
                        }

                        // Track current state and UTxO across batches
                        var currentState = currentChainState
                        var currentTxHash = txHash
                        var currentOutputIndex = outputIndex

                        for (batch, batchIndex) <- batches.zipWithIndex do {
                            val batchStart = batch.head
                            val batchEnd = batch.last
                            val batchNum = batchIndex + 1

                            if totalBatches > 1 then {
                                println()
                                println(
                                  s"  Batch $batchNum/$totalBatches: blocks $batchStart to $batchEnd"
                                )
                            }

                            // Fetch Bitcoin headers for this batch
                            val headersFuture: Future[Seq[BlockHeader]] =
                                Future.sequence(
                                  batch.map { height =>
                                      for {
                                          hashHex <- rpc.getBlockHash(height.toInt)
                                          headerInfo <- rpc.getBlockHeader(hashHex)
                                      } yield BitcoinChainState.convertHeader(headerInfo)
                                  }
                                )

                            val headers: Seq[BlockHeader] =
                                try {
                                    Await.result(headersFuture, 60.seconds)
                                } catch {
                                    case e: Exception =>
                                        System.err.println(
                                          s"✗ Error fetching Bitcoin headers: ${e.getMessage}"
                                        )
                                        return 1
                                }

                            if totalBatches == 1 then {
                                println(s"✓ Fetched $numBlocks Bitcoin headers")
                            }

                            // Convert to Scalus list
                            val headersList = scalus.prelude.List.from(headers.toList)

                            // Calculate new ChainState using shared validator logic
                            val validityTime =
                                OracleTransactions.computeValidityIntervalTime(backendService)

                            if totalBatches == 1 then {
                                println()
                                println("Step 5: Calculating new ChainState after update...")
                                println(s"  Using validity interval time: $validityTime")
                            }

                            val newChainState =
                                try {
                                    BitcoinValidator.computeUpdateOracleState(
                                      currentState,
                                      headersList,
                                      validityTime
                                    )
                                } catch {
                                    case e: Exception =>
                                        System.err.println(
                                          s"✗ Error computing new state: ${e.getMessage}"
                                        )
                                        e.printStackTrace()
                                        return 1
                                }

                            if totalBatches == 1 then {
                                println(s"✓ New oracle state calculated:")
                                println(s"  Block Height: ${newChainState.blockHeight}")
                                println(s"  Block Hash: ${newChainState.blockHash.toHex}")
                                println()
                                println(
                                  "Step 6: Building and submitting UpdateOracle transaction..."
                                )
                            }

                            // Build and submit transaction with pre-computed state and validity time
                            val txResult = OracleTransactions.buildAndSubmitUpdateTransaction(
                              account,
                              backendService,
                              scriptAddress,
                              currentTxHash,
                              currentOutputIndex,
                              currentState,
                              newChainState,
                              headersList,
                              validityTime
                            )

                            txResult match {
                                case Right(resultTxHash) =>
                                    if totalBatches > 1 then {
                                        println(s"    ✓ Batch $batchNum submitted: $resultTxHash")
                                    }
                                    // Update state and UTxO for next batch
                                    currentState = newChainState
                                    currentTxHash = resultTxHash
                                    currentOutputIndex = 0 // Oracle output is always at index 0

                                    // Wait for confirmation before next batch
                                    if batchIndex < batches.size - 1 then {
                                        println(s"    Waiting for confirmation...")
                                        Thread.sleep(3000) // Wait 3 seconds between batches
                                    }

                                case Left(errorMsg) =>
                                    println()
                                    System.err.println(s"✗ Error submitting transaction: $errorMsg")
                                    if totalBatches > 1 then {
                                        System.err.println(
                                          s"  Failed at batch $batchNum (blocks $batchStart to $batchEnd)"
                                        )
                                        System.err.println(
                                          s"  Successfully processed ${batchIndex * batchSize} blocks before failure"
                                        )
                                    }
                                    return 1
                            }
                        }

                        // Final success message
                        println()
                        println("✓ Oracle updated successfully!")
                        println(s"  Transaction Hash: $currentTxHash")
                        println(
                          s"  Updated from block $startHeight to $endHeight ($numBlocks blocks)"
                        )
                        if totalBatches > 1 then {
                            println(s"  Processed in $totalBatches batches")
                        }
                        println()
                        println("Next steps:")
                        println(s"  1. Wait for transaction confirmation")
                        println(
                          s"  2. Verify oracle: binocular verify-oracle $currentTxHash:0"
                        )
                        0
                }

            case (Left(err), _, _, _) =>
                System.err.println(s"Error loading Bitcoin config: $err")
                1
            case (_, Left(err), _, _) =>
                System.err.println(s"Error loading Cardano config: $err")
                1
            case (_, _, Left(err), _) =>
                System.err.println(s"Error loading Oracle config: $err")
                1
            case (_, _, _, Left(err)) =>
                System.err.println(s"Error loading Wallet config: $err")
                1
        }
    }
}
