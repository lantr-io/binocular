package binocular.cli.commands

import binocular.{BitcoinChainState, BitcoinNodeConfig, BitcoinValidator, CardanoConfig, OracleConfig, OracleTransactions, SimpleBitcoinRpc, WalletConfig}
import binocular.cli.{Command, CommandHelpers}
import com.bloxbean.cardano.client.address.Address
import scalus.builtin.ByteString
import scalus.builtin.Data.fromData

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.*
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
                val utxos = try {
                    utxoService.getUtxos(scriptAddress.getAddress, 100, 1)
                } catch {
                    case e: Exception =>
                        System.err.println(s"✗ Error fetching UTxOs: ${e.getMessage}")
                        return 1
                }

                if (!utxos.isSuccessful) {
                    System.err.println(s"✗ Error fetching UTxOs: ${utxos.getResponse}")
                    return 1
                }

                import scala.jdk.CollectionConverters.*
                val allUtxos = utxos.getValue.asScala.toList
                val targetUtxo = allUtxos.find(u => u.getTxHash == txHash && u.getOutputIndex == outputIndex)

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
                        if (inlineDatumHex == null || inlineDatumHex.isEmpty) {
                            System.err.println("✗ UTxO has no inline datum")
                            return 1
                        }

                        val currentChainState = try {
                            // Deserialize hex string to PlutusData
                            val plutusData = com.bloxbean.cardano.client.plutus.spec.PlutusData.deserialize(
                                com.bloxbean.cardano.client.util.HexUtil.decodeHexString(inlineDatumHex)
                            )
                            // Convert to Scalus Data
                            val data = OracleTransactions.plutusDataToScalusData(plutusData)
                            // Parse as ChainState - use extension method on Data
                            data.to[BitcoinValidator.ChainState]
                        } catch {
                            case e: Exception =>
                                System.err.println(s"✗ Error parsing ChainState datum: ${e.getMessage}")
                                e.printStackTrace()
                                return 1
                        }

                        println(s"✓ Current oracle state:")
                        println(s"  Block Height: ${currentChainState.blockHeight}")
                        println(s"  Block Hash: ${currentChainState.blockHash.toHex}")
                        println(s"  Fork Tree Size: ${currentChainState.forksTree.size}")

                        // Determine block range
                        val startHeight = fromBlock.getOrElse(currentChainState.blockHeight.toLong + 1)
                        val endHeight = toBlock match {
                            case Some(h) => h
                            case None =>
                                // Fetch current Bitcoin chain tip
                                given ec: ExecutionContext = ExecutionContext.global
                                val rpc = new SimpleBitcoinRpc(btcConf)
                                val infoFuture = rpc.getBlockchainInfo()
                                try {
                                    val info = Await.result(infoFuture, 30.seconds)
                                    info.blocks.toLong
                                } catch {
                                    case e: Exception =>
                                        System.err.println(s"✗ Error fetching blockchain info: ${e.getMessage}")
                                        return 1
                                }
                        }

                        if (startHeight > endHeight) {
                            System.err.println(s"✗ Invalid block range: $startHeight to $endHeight")
                            return 1
                        }

                        val numBlocks = (endHeight - startHeight + 1).toInt
                        if (numBlocks > oracleConf.maxHeadersPerTx) {
                            System.err.println(s"✗ Too many blocks requested: $numBlocks (max: ${oracleConf.maxHeadersPerTx})")
                            System.err.println(s"  Use --from and --to options to limit the range")
                            return 1
                        }

                        println()
                        println(s"Step 4: Fetching Bitcoin headers from block $startHeight to $endHeight ($numBlocks headers)...")

                        // Fetch Bitcoin headers
                        given ec: ExecutionContext = ExecutionContext.global
                        val rpc = new SimpleBitcoinRpc(btcConf)

                        val headersFuture: Future[Seq[BitcoinValidator.BlockHeader]] = Future.sequence(
                            (startHeight to endHeight).map { height =>
                                for {
                                    hashHex <- rpc.getBlockHash(height.toInt)
                                    headerInfo <- rpc.getBlockHeader(hashHex)
                                } yield BitcoinChainState.convertHeader(headerInfo)
                            }
                        )

                        val headers: Seq[BitcoinValidator.BlockHeader] = try {
                            Await.result(headersFuture, 60.seconds)
                        } catch {
                            case e: Exception =>
                                System.err.println(s"✗ Error fetching Bitcoin headers: ${e.getMessage}")
                                return 1
                        }

                        println(s"✓ Fetched $numBlocks Bitcoin headers")
                        println(s" headers: ${headers.map(h => h.bytes.toHex).mkString(", ")}")

                        // Convert to Scalus list
                        val headersList = scalus.prelude.List.from(headers.toList)

                        println()
                        println("Step 5: Calculating new ChainState after update...")

                        // Calculate new ChainState
                        val currentTime = BigInt(System.currentTimeMillis() / 1000)
                        val newChainState = try {
                            OracleTransactions.applyHeaders(currentChainState, headersList, currentTime)
                        } catch {
                            case e: Exception =>
                                System.err.println(s"✗ Error applying headers to ChainState: ${e.getMessage}")
                                e.printStackTrace()
                                return 1
                        }

                        println(s"✓ New oracle state calculated:")
                        println(s"  Block Height: ${newChainState.blockHeight}")
                        println(s"  Block Hash: ${newChainState.blockHash.toHex}")

                        println()
                        println("Step 6: Building and submitting UpdateOracle transaction...")
                        
                        
                        // Build and submit transaction
                        val txResult = OracleTransactions.buildAndSubmitUpdateTransaction(
                            account,
                            backendService,
                            scriptAddress,
                            txHash,
                            outputIndex,
                            currentChainState,
                            newChainState,
                            headersList
                        )

                        txResult match {
                            case Right(resultTxHash) =>
                                println()
                                println("✓ Oracle updated successfully!")
                                println(s"  Transaction Hash: $resultTxHash")
                                println(s"  Updated from block $startHeight to $endHeight")
                                println()
                                println("Next steps:")
                                println(s"  1. Wait for transaction confirmation")
                                println(s"  2. Verify oracle: binocular verify-oracle $resultTxHash:0")
                                0
                            case Left(errorMsg) =>
                                println()
                                System.err.println(s"✗ Error submitting transaction: $errorMsg")
                                1
                        }
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
