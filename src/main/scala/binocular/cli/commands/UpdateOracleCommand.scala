package binocular.cli.commands

import binocular.cli.{Command, CommandHelpers}
import binocular.*
import scalus.cardano.address.Address
import scalus.cardano.ledger.{TransactionHash, TransactionInput, Utxo, Utxos}
import scalus.cardano.node.{BlockchainProvider, UtxoQuery, UtxoSource}
import scalus.cardano.txbuilder.TransactionSigner
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.fromData
import scalus.cardano.onchain.plutus.prelude.List as ScalusList

import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext, Future}

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
                println(s"  Bitcoin Node: ${btcConf.url}")
                println(s"  Cardano Network: ${cardanoConf.network}")
                println(s"  Oracle Address: ${oracleConf.scriptAddress}")
                println()

                given ec: ExecutionContext = ExecutionContext.global

                val timeout = oracleConf.transactionTimeout.seconds

                // Create HD account and signer
                val hdAccount = walletConf.createHdAccount() match {
                    case Right(acc) =>
                        val addr =
                            acc.baseAddress(cardanoConf.scalusNetwork).toBech32.getOrElse("?")
                        println(s"  Wallet loaded: $addr")
                        acc
                    case Left(err) =>
                        System.err.println(s"Error creating wallet account: $err")
                        return 1
                }
                val signer = new TransactionSigner(Set(hdAccount.paymentKeyPair))
                val sponsorAddress = hdAccount.baseAddress(cardanoConf.scalusNetwork)

                // Create Cardano blockchain provider
                val provider = cardanoConf.createBlockchainProvider() match {
                    case Right(p) =>
                        println(s"  Connected to Cardano backend (${cardanoConf.backend})")
                        p
                    case Left(err) =>
                        System.err.println(s"Error creating blockchain provider: $err")
                        return 1
                }

                println()
                println("Step 3: Checking for reference script...")

                val scriptAddress = Address.fromBech32(oracleConf.scriptAddress)
                val referenceScriptUtxo: Option[Utxo] =
                    try {
                        val existingRefs = OracleTransactions.findReferenceScriptUtxos(
                          provider,
                          scriptAddress,
                          timeout
                        )

                        existingRefs.headOption match {
                            case Some((refTxHash, refOutputIdx)) =>
                                println(
                                  s"  Found existing reference script at $refTxHash:$refOutputIdx"
                                )
                                // Fetch the actual Utxo
                                val refInput = TransactionInput(
                                  TransactionHash.fromHex(refTxHash),
                                  refOutputIdx
                                )
                                val utxoResult = Await.result(
                                  provider.findUtxo(refInput),
                                  timeout
                                )
                                utxoResult match {
                                    case Right(u) => Some(u)
                                    case Left(_)  => None
                                }

                            case None =>
                                println(s"  No reference script found, deploying one...")
                                println(
                                  s"  This is a one-time operation to reduce transaction sizes"
                                )

                                OracleTransactions.deployReferenceScript(
                                  signer,
                                  provider,
                                  sponsorAddress,
                                  scriptAddress,
                                  timeout
                                ) match {
                                    case Right((deployTxHash, deployOutputIdx, savedOutput)) =>
                                        println(
                                          s"  Reference script deployed at $deployTxHash:$deployOutputIdx"
                                        )
                                        println(s"  This will save ~10KB per transaction")

                                        // Wait for the reference script tx to be confirmed
                                        println(
                                          s"  Waiting for reference script tx to be confirmed..."
                                        )
                                        val refInput = TransactionInput(
                                          TransactionHash.fromHex(deployTxHash),
                                          deployOutputIdx
                                        )
                                        var confirmed = false
                                        var attempts = 0
                                        val maxAttempts = 30

                                        while !confirmed && attempts < maxAttempts do {
                                            Thread.sleep(2000)
                                            attempts += 1
                                            try {
                                                val result = Await.result(
                                                  provider.findUtxo(refInput),
                                                  timeout
                                                )
                                                result match {
                                                    case Right(_) =>
                                                        confirmed = true
                                                        println(
                                                          s"  Reference script tx confirmed after ${attempts * 2} seconds"
                                                        )
                                                    case Left(_) =>
                                                        if attempts % 5 == 0 then
                                                            println(
                                                              s"  Still waiting... (${attempts * 2}s elapsed)"
                                                            )
                                                }
                                            } catch {
                                                case _: Exception => // Ignore, keep trying
                                            }
                                        }

                                        if !confirmed then {
                                            System.err.println(
                                              s"  Warning: Reference script tx not confirmed after ${maxAttempts * 2} seconds"
                                            )
                                        }

                                        // Construct Utxo from saved output (which has scriptRef populated)
                                        Some(Utxo(refInput, savedOutput))

                                    case Left(err) =>
                                        System.err.println(
                                          s"  Failed to deploy reference script: $err"
                                        )
                                        System.err.println(
                                          s"  Cannot proceed without reference script"
                                        )
                                        return 1
                                }
                        }
                    } catch {
                        case e: Exception =>
                            System.err.println(
                              s"  Error checking for reference script: ${e.getMessage}"
                            )
                            e.printStackTrace()
                            return 1
                    }

                println()
                println("Step 4: Fetching current oracle UTxO from Cardano...")

                val utxosResult =
                    try {
                        Await.result(provider.findUtxos(scriptAddress), timeout)
                    } catch {
                        case e: Exception =>
                            System.err.println(s"  Error fetching UTxOs: ${e.getMessage}")
                            return 1
                    }

                val allUtxos: List[Utxo] = utxosResult match {
                    case Right(u) => u.map { case (input, output) => Utxo(input, output) }.toList
                    case Left(err) =>
                        System.err.println(s"  Error fetching UTxOs: $err")
                        return 1
                }

                val targetUtxo = allUtxos.find { u =>
                    u.input.transactionId.toHex == txHash && u.input.index == outputIndex
                }

                targetUtxo match {
                    case None =>
                        System.err.println(s"  UTxO not found: $txHash:$outputIndex")
                        val validOracles = CommandHelpers.filterValidOracleUtxos(allUtxos)
                        if validOracles.nonEmpty then {
                            System.err.println(s"  Available valid oracle UTxOs:")
                            validOracles.foreach { vo =>
                                System.err.println(
                                  s"    ${vo.utxoRef} (height: ${vo.chainState.blockHeight})"
                                )
                            }
                        } else {
                            System.err.println(s"  No valid oracle UTxOs found at script address")
                        }
                        return 1

                    case Some(oracleUtxo) =>
                        println(s"  Found oracle UTxO: $txHash:$outputIndex")

                        println()
                        println("Step 5: Parsing current ChainState datum...")

                        val currentChainState =
                            try {
                                val data = oracleUtxo.output.requireInlineDatum
                                data.to[ChainState]
                            } catch {
                                case e: Exception =>
                                    System.err.println(
                                      s"  Error parsing ChainState datum: ${e.getMessage}"
                                    )
                                    e.printStackTrace()
                                    return 1
                            }

                        println(s"  Current oracle state:")
                        println(s"  Confirmed Height: ${currentChainState.blockHeight}")
                        println(s"  Block Hash: ${currentChainState.blockHash.toHex}")
                        println(s"  Fork Tree Size: ${currentChainState.forksTree.size}")

                        // Determine the highest block we have
                        val highestBlock = if currentChainState.forksTree.nonEmpty then {
                            val maxForkHeight = currentChainState.forksTree.foldLeft(0L) {
                                (max, branch) => math.max(max, branch.tipHeight.toLong)
                            }
                            println(s"  Fork Tree Tip: $maxForkHeight")
                            maxForkHeight
                        } else {
                            println(s"  Fork Tree: empty (oracle at confirmed height)")
                            currentChainState.blockHeight.toLong
                        }

                        // Determine block range
                        val startHeight = fromBlock.getOrElse(highestBlock + 1)
                        println(s"  Will start from: $startHeight")
                        val endHeight = toBlock match {
                            case Some(h) => h
                            case None =>
                                val rpc = new SimpleBitcoinRpc(btcConf)
                                val infoFuture = rpc.getBlockchainInfo()
                                try {
                                    val info = Await.result(infoFuture, 30.seconds)
                                    info.blocks.toLong
                                } catch {
                                    case e: Exception =>
                                        System.err.println(
                                          s"  Error fetching blockchain info: ${e.getMessage}"
                                        )
                                        return 1
                                }
                        }

                        if startHeight > endHeight then {
                            System.err.println(s"  Invalid block range: $startHeight to $endHeight")
                            return 1
                        }

                        val numBlocks = (endHeight - startHeight + 1).toInt

                        // Limit blocks per command
                        val maxBlocksPerCommand = 100
                        if numBlocks > maxBlocksPerCommand then {
                            val currentTime = System.currentTimeMillis() / 1000
                            val challengeAgingSeconds = BitcoinValidator.ChallengeAging.toLong

                            def toScalaList[A](l: ScalusList[A]): scala.List[A] = l match {
                                case ScalusList.Nil        => scala.Nil
                                case ScalusList.Cons(h, t) => h :: toScalaList(t)
                            }

                            val forksTreeScala = toScalaList(currentChainState.forksTree)

                            val canPromote = forksTreeScala.exists { branch =>
                                val blocksScala = toScalaList(branch.recentBlocks)
                                blocksScala.lastOption match {
                                    case Some(oldestBlock) =>
                                        val age = currentTime - oldestBlock.addedTime.toLong
                                        age >= challengeAgingSeconds
                                    case scala.None => false
                                }
                            }

                            if !canPromote then {
                                System.err.println(
                                  s"  Too many blocks: $numBlocks (max $maxBlocksPerCommand per command)"
                                )
                                System.err.println(
                                  s"  The fork tree grows with each block until promotion,"
                                )
                                System.err.println(
                                  s"  causing transactions to exceed Cardano's 16KB limit."
                                )
                                System.err.println()
                                if forksTreeScala.nonEmpty then {
                                    val oldestAddedTime = forksTreeScala
                                        .flatMap(b => toScalaList(b.recentBlocks).lastOption)
                                        .map(_.addedTime.toLong)
                                        .minOption
                                        .getOrElse(currentTime)
                                    val timeUntilPromotion =
                                        challengeAgingSeconds - (currentTime - oldestAddedTime)
                                    if timeUntilPromotion > 0 then {
                                        val minutes = timeUntilPromotion / 60
                                        System.err.println(
                                          s"  Promotion will be possible in ~$minutes minutes."
                                        )
                                        System.err.println(
                                          s"  Wait for promotion, then run this command again."
                                        )
                                    }
                                }
                                System.err.println()
                                System.err.println(s"  Or update in smaller chunks:")
                                val suggestedEnd = startHeight + maxBlocksPerCommand - 1
                                System.err.println(
                                  s"    binocular update-oracle --to $suggestedEnd <utxo>"
                                )
                                return 1
                            } else {
                                println(
                                  s"  Note: Processing $numBlocks blocks (promotion will occur)"
                                )
                            }
                        }

                        val batchSize = oracleConf.maxHeadersPerTx

                        // Warn if too many blocks
                        val maxRecommendedBlocks = 500
                        if numBlocks > maxRecommendedBlocks then {
                            println()
                            println(s"Warning: Attempting to fetch $numBlocks blocks")
                            println(s"  This may hit Bitcoin RPC rate limits and fail.")
                            println(
                              s"  Recommended: Use --to parameter to limit range to ~$maxRecommendedBlocks blocks"
                            )
                            println(s"  Example: --to ${startHeight + maxRecommendedBlocks - 1}")
                            println()
                            println("  Press Ctrl+C to cancel, or continuing in 5 seconds...")
                            Thread.sleep(5000)
                        }

                        val rpc = new SimpleBitcoinRpc(btcConf)

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
                        var currentOracleUtxo = oracleUtxo

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

                            // Fetch Bitcoin headers
                            def fetchHeadersSequentially(
                                heights: List[Long],
                                acc: List[BlockHeader]
                            ): Future[List[BlockHeader]] = {
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
                                          s"  Error fetching Bitcoin headers: ${e.getMessage}"
                                        )
                                        return 1
                                }

                            if totalBatches == 1 then {
                                println(s"  Fetched $numBlocks Bitcoin headers")
                            }

                            val headersList = ScalusList.from(headers.toList)

                            val (_, validityTime) = OracleTransactions.computeValidityIntervalTime(
                              provider.cardanoInfo
                            )

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
                                          s"  Error computing new state: ${e.getMessage}"
                                        )
                                        e.printStackTrace()
                                        return 1
                                }

                            if totalBatches == 1 then {
                                println(s"  New oracle state calculated:")
                                println(s"  Block Height: ${newChainState.blockHeight}")
                                println(s"  Block Hash: ${newChainState.blockHash.toHex}")
                                println()
                                println(
                                  "Step 7: Building and submitting UpdateOracle transaction..."
                                )
                            }

                            val txResult = OracleTransactions.buildAndSubmitUpdateTransaction(
                              signer,
                              provider,
                              scriptAddress,
                              sponsorAddress,
                              currentOracleUtxo,
                              currentState,
                              newChainState,
                              headersList,
                              validityTime,
                              referenceScriptUtxo,
                              timeout
                            )

                            txResult match {
                                case Right(resultTxHash) =>
                                    if totalBatches > 1 then {
                                        println(s"    Batch $batchNum submitted: $resultTxHash")
                                    }

                                    // Update state for next batch
                                    currentState = newChainState

                                    // Wait for UTxO to be indexed before next batch
                                    if batchIndex < batches.size - 1 then {
                                        println(s"    Waiting for UTxO to be indexed...")

                                        val newOracleInput = TransactionInput(
                                          TransactionHash.fromHex(resultTxHash),
                                          0
                                        )
                                        var utxoAvailable: Option[Utxo] = None
                                        var attempts = 0
                                        val maxAttempts = 30

                                        while utxoAvailable.isEmpty && attempts < maxAttempts do {
                                            Thread.sleep(1000)
                                            attempts += 1

                                            try {
                                                val result = Await.result(
                                                  provider.findUtxo(newOracleInput),
                                                  timeout
                                                )
                                                result match {
                                                    case Right(u) =>
                                                        utxoAvailable = Some(u)
                                                        println(
                                                          s"    UTxO indexed after ${attempts}s"
                                                        )
                                                    case Left(_) => // keep trying
                                                }
                                            } catch {
                                                case _: Exception => // keep trying
                                            }
                                        }

                                        if utxoAvailable.isEmpty then {
                                            System.err.println(
                                              s"    UTxO not available after ${maxAttempts}s"
                                            )
                                            return 1
                                        }

                                        currentOracleUtxo = utxoAvailable.get

                                        // Wait for wallet UTxOs to be indexed
                                        println(
                                          s"    Waiting for new wallet UTxOs from batch ${batchNum}..."
                                        )
                                        var newUtxoFound = false
                                        var walletAttempts = 0
                                        val maxWalletAttempts = 30

                                        while !newUtxoFound && walletAttempts < maxWalletAttempts
                                        do {
                                            Thread.sleep(1000)
                                            walletAttempts += 1

                                            try {
                                                val walletUtxosResult = Await.result(
                                                  provider.findUtxos(sponsorAddress),
                                                  timeout
                                                )
                                                walletUtxosResult match {
                                                    case Right(utxos) =>
                                                        val hasNewUtxo = utxos.exists {
                                                            case (input, _) =>
                                                                input.transactionId.toHex == resultTxHash
                                                        }
                                                        if hasNewUtxo then {
                                                            newUtxoFound = true
                                                            println(
                                                              s"    New wallet UTxO indexed (after ${walletAttempts}s)"
                                                            )
                                                        }
                                                    case Left(_) => // keep trying
                                                }
                                            } catch {
                                                case _: Exception => // keep trying
                                            }
                                        }

                                        if !newUtxoFound then {
                                            System.err.println(
                                              s"    New wallet UTxO not indexed after ${maxWalletAttempts}s"
                                            )
                                            return 1
                                        }
                                    }

                                case Left(errorMsg) =>
                                    println()
                                    System.err.println(s"  Error submitting transaction: $errorMsg")
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
                        val currentTxHash = currentOracleUtxo.input.transactionId.toHex
                        println()
                        println("Oracle updated successfully!")
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
                        println(s"  2. Verify oracle: binocular verify-oracle $currentTxHash:0")
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
