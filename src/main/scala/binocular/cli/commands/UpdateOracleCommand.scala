package binocular.cli.commands

import binocular.cli.{Command, CommandHelpers}
import binocular.*
import com.bloxbean.cardano.client.address.Address
import com.bloxbean.cardano.client.api.model.Amount
import com.bloxbean.cardano.client.function.helper.SignerProviders
import com.bloxbean.cardano.client.quicktx.{QuickTxBuilder, Tx}
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

                // Skip wallet splitting - deploy reference script to script address instead
                // This avoids collateral conflicts (ref script at script address won't be selected as collateral)

                println()
                println("Step 3: Checking for reference script...")

                // Check if reference script exists at script address (like integration test does)
                // Deploying to script address avoids collateral conflicts
                val scriptAddress = new Address(oracleConf.scriptAddress)
                val referenceScriptUtxo = try {
                    val existingRefs = OracleTransactions.findReferenceScriptUtxos(
                      backendService,
                      scriptAddress.getAddress
                    )

                    existingRefs.headOption match {
                        case Some((txHash, outputIdx)) =>
                            println(s"✓ Found existing reference script at $txHash:$outputIdx")
                            Some((txHash, outputIdx))
                        case None =>
                            println(s"✗ No reference script found, deploying one...")
                            println(s"  This is a one-time operation to reduce transaction sizes")
                            println(s"  Deploying to script address (like integration test)")

                            OracleTransactions.deployReferenceScript(
                              account,
                              backendService,
                              scriptAddress.getAddress  // Deploy to script address, not wallet
                            ) match {
                                case Right((txHash, outputIdx)) =>
                                    println(s"✓ Reference script deployed at $txHash:$outputIdx")
                                    println(s"  This will save ~10KB per transaction")

                                    // Wait for the new wallet UTxO (change from ref script deployment) to be indexed
                                    println(s"  Waiting for new wallet UTxO to be indexed...")
                                    val walletUtxoIndexed = {
                                        var found = false
                                        var attempts = 0
                                        val maxAttempts = 30 // 30 attempts * 2 seconds = 60 seconds max
                                        while (!found && attempts < maxAttempts) {
                                            Thread.sleep(2000) // Wait 2 seconds between polls
                                            attempts += 1
                                            try {
                                                val utxos = backendService.getUtxoService.getUtxos(account.baseAddress(), 10, 1)
                                                if (utxos.isSuccessful) {
                                                    // Look for the new wallet UTxO from the ref script deployment
                                                    found = utxos.getValue.asScala.exists(u =>
                                                        u.getTxHash == txHash && u.getOutputIndex == 1
                                                    )
                                                    if (found) {
                                                        println(s"  ✓ Wallet UTxO indexed after ${attempts * 2} seconds")
                                                    } else if (attempts % 5 == 0) {
                                                        println(s"  Still waiting... (${attempts * 2}s elapsed)")
                                                    }
                                                }
                                            } catch {
                                                case _: Exception => // Ignore query errors, keep trying
                                            }
                                        }
                                        if (!found) {
                                            System.err.println(s"  ⚠ Warning: Wallet UTxO not indexed after ${maxAttempts * 2} seconds, continuing anyway")
                                        }
                                        found
                                    }

                                    Some((txHash, outputIdx))
                                case Left(err) =>
                                    System.err.println(s"✗ Failed to deploy reference script: $err")
                                    System.err.println(s"  Cannot proceed without reference script (transactions would exceed 16KB limit)")
                                    return 1
                            }
                    }
                } catch {
                    case e: Exception =>
                        System.err.println(s"✗ Error checking for reference script: ${e.getMessage}")
                        e.printStackTrace()
                        return 1
                }

                println()
                println("Step 4: Fetching current oracle UTxO from Cardano...")
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
                        println("Step 5: Parsing current ChainState datum...")

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

                        // Determine the highest block we have (either confirmed or in fork tree)
                        val highestBlock = if currentChainState.forksTree.nonEmpty then {
                            // Find the highest block in fork tree
                            val maxForkHeight = currentChainState.forksTree.foldLeft(0L) { (max, branch) =>
                                math.max(max, branch.tipHeight.toLong)
                            }
                            println(s"  Highest fork tree block: $maxForkHeight")
                            maxForkHeight
                        } else {
                            currentChainState.blockHeight.toLong
                        }

                        // Determine block range
                        val startHeight =
                            fromBlock.getOrElse(highestBlock + 1)
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

                        // Warn if too many blocks to fetch (likely to hit rate limits)
                        val maxRecommendedBlocks = 500
                        if numBlocks > maxRecommendedBlocks then {
                            println()
                            println(s"⚠ Warning: Attempting to fetch $numBlocks blocks")
                            println(s"  This may hit Bitcoin RPC rate limits and fail.")
                            println(s"  Recommended: Use --to parameter to limit range to ~$maxRecommendedBlocks blocks")
                            println(s"  Example: --to ${startHeight + maxRecommendedBlocks - 1}")
                            println()
                            println("  Press Ctrl+C to cancel, or continuing in 5 seconds...")
                            Thread.sleep(5000)
                        }

                        // Create execution context for async operations
                        given ec: ExecutionContext = ExecutionContext.global
                        val rpc = new SimpleBitcoinRpc(btcConf)

                        // Split into batches if needed
                        val batches = (startHeight to endHeight).grouped(batchSize).toList
                        val totalBatches = batches.size

                        if totalBatches > 1 then {
                            println()
                            println(
                              s"Step 5: Processing $numBlocks blocks in $totalBatches batches of up to $batchSize headers each..."
                            )
                        } else {
                            println()
                            println(
                              s"Step 5: Fetching Bitcoin headers from block $startHeight to $endHeight ($numBlocks headers)..."
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

                            // Fetch Bitcoin headers for this batch sequentially to avoid rate limiting
                            def fetchHeadersSequentially(heights: List[Long], acc: List[BlockHeader]): Future[List[BlockHeader]] = {
                                heights match {
                                    case Nil => Future.successful(acc.reverse)
                                    case h :: tail =>
                                        for {
                                            hashHex <- rpc.getBlockHash(h.toInt)
                                            headerInfo <- rpc.getBlockHeader(hashHex)
                                            header = BitcoinChainState.convertHeader(headerInfo)
                                            rest <- fetchHeadersSequentially(tail, header :: acc)
                                        } yield rest
                                }
                            }
                            val headersFuture: Future[Seq[BlockHeader]] =
                                fetchHeadersSequentially(batch.toList, Nil)

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
                                println("Step 6: Calculating new ChainState after update...")
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
                                  "Step 7: Building and submitting UpdateOracle transaction..."
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
                              validityTime,
                              referenceScriptUtxo
                            )

                            txResult match {
                                case Right(resultTxHash) =>
                                    if totalBatches > 1 then {
                                        println(s"    ✓ Batch $batchNum submitted: $resultTxHash")
                                    }
                                    // Update state and UTxO reference for next batch
                                    currentState = newChainState
                                    currentTxHash = resultTxHash
                                    currentOutputIndex = 0 // Oracle output is always at index 0

                                    // Wait for UTxO to be indexed before next batch
                                    if batchIndex < batches.size - 1 then {
                                        println(s"    Waiting for UTxO to be indexed...")

                                        // Poll Blockfrost until the UTxO is available using the SAME API
                                        // that the transaction builder uses (getUtxos by address)
                                        var utxoAvailable = false
                                        var attempts = 0
                                        val maxAttempts = 30 // Try for up to 30 seconds

                                        while !utxoAvailable && attempts < maxAttempts do {
                                            Thread.sleep(1000)
                                            attempts += 1

                                            // Use same API as OracleTransactions.buildAndSubmitUpdateTransaction
                                            val scriptAddr = oracleConf.scriptAddress
                                            val utxosCheck = backendService.getUtxoService.getUtxos(scriptAddr, 100, 1)
                                            if utxosCheck.isSuccessful && utxosCheck.getValue != null then {
                                                val utxoList = utxosCheck.getValue.asScala.toList
                                                val found = utxoList.exists(u =>
                                                    u.getTxHash == resultTxHash && u.getOutputIndex == 0
                                                )
                                                if found then {
                                                    utxoAvailable = true
                                                    println(s"    ✓ UTxO indexed after ${attempts}s")
                                                }
                                            }
                                        }

                                        if !utxoAvailable then {
                                            System.err.println(s"    ✗ UTxO not available after ${maxAttempts}s")
                                            return 1
                                        }

                                        // Wait for NEW wallet UTxOs from this transaction to be indexed
                                        // We need the change output to be available for next batch's collateral
                                        println(s"    Waiting for new wallet UTxOs from batch ${batchNum}...")
                                        var newUtxoFound = false
                                        var walletAttempts = 0
                                        val maxWalletAttempts = 30

                                        while !newUtxoFound && walletAttempts < maxWalletAttempts do {
                                            Thread.sleep(1000)
                                            walletAttempts += 1

                                            // Check if wallet has the NEW change output from resultTxHash
                                            val walletUtxosCheck = backendService.getUtxoService.getUtxos(
                                                account.baseAddress(), 100, 1
                                            )
                                            if walletUtxosCheck.isSuccessful && walletUtxosCheck.getValue != null then {
                                                val walletUtxos = walletUtxosCheck.getValue.asScala.toList
                                                // Look for UTxO from the transaction we just submitted
                                                val hasNewUtxo = walletUtxos.exists(u => u.getTxHash == resultTxHash)
                                                if hasNewUtxo then {
                                                    newUtxoFound = true
                                                    println(s"    ✓ New wallet UTxO indexed (after ${walletAttempts}s)")
                                                }
                                            }
                                        }

                                        if !newUtxoFound then {
                                            System.err.println(s"    ✗ New wallet UTxO not indexed after ${maxWalletAttempts}s")
                                            return 1
                                        }
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
