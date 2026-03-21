package binocular.cli.commands

import binocular.*
import binocular.cli.{Command, CommandHelpers}
import com.typesafe.scalalogging.LazyLogging
import scalus.cardano.ledger.{TransactionHash, TransactionInput, Utxo}
import scalus.cardano.node.TransactionStatus
import scalus.cardano.onchain.plutus.prelude.List as ScalusList
import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF

import scala.concurrent.duration.*
import scala.concurrent.{ExecutionContext, Future}
import scala.util.boundary, boundary.break
import scalus.utils.await

/** Update oracle with new Bitcoin blocks */
case class UpdateOracleCommand(
    fromBlock: Option[Long],
    toBlock: Option[Long]
) extends Command
    with LazyLogging {

    override def execute(config: BinocularConfig): Int = {
        println(s"Updating oracle...")
        println()

        updateOracle(config)
    }

    private def updateOracle(config: BinocularConfig): Int =
        boundary {
            given ec: ExecutionContext = ExecutionContext.global
            val timeout = config.oracle.transactionTimeout.seconds

            println("Step 1: Loading configurations...")
            val setup = CommandHelpers.setupOracle(config) match {
                case Right(s) => s
                case Left(err) =>
                    System.err.println(s"Error: $err")
                    break(1)
            }

            val walletAddr = setup.hdAccount
                .baseAddress(config.cardano.scalusNetwork)
                .toBech32
                .getOrElse("?")
            println(s"  Bitcoin Node: ${config.bitcoinNode.url}")
            println(s"  Cardano Network: ${config.cardano.network}")
            println(s"  Oracle Address: ${setup.scriptAddressBech32}")
            println(s"  Wallet loaded: $walletAddr")
            println()

            println("Step 2: Checking for reference script...")
            val referenceScriptUtxo: Option[Utxo] =
                CommandHelpers.findReferenceScriptUtxo(
                  setup.provider,
                  setup.scriptAddress,
                  setup.script.scriptHash,
                  timeout
                ) match {
                    case some @ Some(_) =>
                        println(s"  Found existing reference script")
                        some
                    case None =>
                        println(s"  No reference script found, deploying one...")
                        println(s"  This is a one-time operation to reduce transaction sizes")
                        OracleTransactions.deployReferenceScript(
                          setup.signer,
                          setup.provider,
                          setup.sponsorAddress,
                          setup.scriptAddress,
                          setup.script,
                          timeout
                        ) match {
                            case Right((deployTxHash, deployOutputIdx, savedOutput)) =>
                                println(
                                  s"  Reference script deployed at $deployTxHash:$deployOutputIdx"
                                )
                                println(s"  Waiting for reference script tx to be confirmed...")
                                val refInput = TransactionInput(
                                  TransactionHash.fromHex(deployTxHash),
                                  deployOutputIdx
                                )
                                val refStatus = setup.provider
                                    .pollForConfirmation(
                                      TransactionHash.fromHex(deployTxHash),
                                      delayMs = 2000
                                    )
                                    .await(timeout)
                                if refStatus == TransactionStatus.Confirmed then
                                    println(s"  Reference script tx confirmed")
                                else
                                    System.err.println(
                                      s"  Warning: Reference script tx not confirmed after 60s"
                                    )
                                Some(Utxo(refInput, savedOutput))
                            case Left(err) =>
                                System.err.println(s"  Failed to deploy reference script: $err")
                                System.err.println(s"  Cannot proceed without reference script")
                                break(1)
                        }
                }

            println()
            println("Step 3: Finding oracle UTxO by NFT...")

            val scriptAddress = setup.scriptAddress
            val oracleUtxo: Utxo =
                try {
                    CommandHelpers
                        .findOracleUtxo(setup.provider, setup.script.scriptHash)
                        .await(timeout)
                } catch {
                    case e: Exception =>
                        System.err.println(s"  Error: ${e.getMessage}")
                        break(1)
                }

            val oracleRef =
                s"${oracleUtxo.input.transactionId.toHex}:${oracleUtxo.input.index}"
            println(s"  Found oracle UTxO: $oracleRef")
            println()
            println("Step 4: Parsing current ChainState datum...")

            val currentChainState =
                try {
                    oracleUtxo.output.requireInlineDatum.to[ChainState]
                } catch {
                    case e: Exception =>
                        System.err.println(
                          s"  Error parsing ChainState datum: ${e.getMessage}"
                        )
                        e.printStackTrace()
                        break(1)
                }

            println(s"  Current oracle state:")
            println(s"  Confirmed Height: ${currentChainState.blockHeight}")
            println(s"  Block Hash: ${currentChainState.blockHash.toHex}")
            println(s"  Fork Tree Size: ${currentChainState.forkTree.blockCount}")

            val offChainMpfInit: OffChainMPF = CommandHelpers.reconstructMpf(
              new SimpleBitcoinRpc(config.bitcoinNode),
              currentChainState,
              config.oracle.startHeight
            ) match {
                case Right(mpf) =>
                    logger.info("MPF reconstructed successfully")
                    mpf
                case Left(err) =>
                    System.err.println(s"  Error: $err")
                    break(1)
            }

            val highestBlock = if currentChainState.forkTree.nonEmpty then {
                val maxForkHeight =
                    currentChainState.forkTree
                        .highestHeight(currentChainState.blockHeight)
                        .toLong
                println(s"  Fork Tree Tip: $maxForkHeight")
                maxForkHeight
            } else {
                println(s"  Fork Tree: empty (oracle at confirmed height)")
                currentChainState.blockHeight.toLong
            }

            val startHeight = fromBlock.getOrElse(highestBlock + 1)
            println(s"  Will start from: $startHeight")
            val endHeight = toBlock match {
                case Some(h) => h
                case None =>
                    val rpc = new SimpleBitcoinRpc(config.bitcoinNode)
                    try {
                        rpc.getBlockchainInfo().await(30.seconds).blocks.toLong
                    } catch {
                        case e: Exception =>
                            System.err.println(
                              s"  Error fetching blockchain info: ${e.getMessage}"
                            )
                            break(1)
                    }
            }

            if startHeight > endHeight then {
                System.err.println(s"  Invalid block range: $startHeight to $endHeight")
                break(1)
            }

            val numBlocks = (endHeight - startHeight + 1).toInt
            val params = setup.params

            val maxBlocksPerCommand = 100
            if numBlocks > maxBlocksPerCommand then {
                val currentTime = System.currentTimeMillis() / 1000
                val challengeAgingSeconds = params.challengeAging.toLong

                val canPromote =
                    currentChainState.forkTree.oldestBlockTime match {
                        case Some(oldest) =>
                            (currentTime - oldest.toLong) >= challengeAgingSeconds
                        case None => false
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
                    currentChainState.forkTree.oldestBlockTime.foreach { oldestAddedTime =>
                        val timeUntilPromotion =
                            challengeAgingSeconds - (currentTime - oldestAddedTime.toLong)
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
                      s"    binocular update-oracle --to $suggestedEnd"
                    )
                    break(1)
                } else {
                    println(
                      s"  Note: Processing $numBlocks blocks (promotion will occur)"
                    )
                }
            }

            val batchSize = config.oracle.maxHeadersPerTx

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

            val rpc = new SimpleBitcoinRpc(config.bitcoinNode)

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

            var currentState = currentChainState
            var currentOracleUtxo = oracleUtxo
            var currentMpf = offChainMpfInit

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

                val headers: Seq[BlockHeader] =
                    try {
                        fetchHeadersSequentially(batch.toList, Nil).await(60.seconds)
                    } catch {
                        case e: Exception =>
                            System.err.println(
                              s"  Error fetching Bitcoin headers: ${e.getMessage}"
                            )
                            break(1)
                    }

                if totalBatches == 1 then {
                    println(s"  Fetched $numBlocks Bitcoin headers")
                }

                val headersList = ScalusList.from(headers.toList)

                val (_, validityTime) =
                    OracleTransactions.computeValidityIntervalTime(
                      setup.provider.cardanoInfo
                    )
                val parentPath = currentState.forkTree.findTipPath

                if totalBatches == 1 then {
                    println()
                    println("Step 6: Calculating new ChainState after update...")
                    println(s"  Using validity interval time: $validityTime")
                }

                val (newChainState, mpfProofs, updatedMpf) =
                    try {
                        OracleTransactions.computeUpdateWithProofs(
                          currentState,
                          headersList,
                          parentPath,
                          validityTime,
                          currentMpf,
                          params
                        )
                    } catch {
                        case e: Exception =>
                            System.err.println(
                              s"  Error computing new state: ${e.getMessage}"
                            )
                            e.printStackTrace()
                            break(1)
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
                  setup.signer,
                  setup.provider,
                  scriptAddress,
                  setup.sponsorAddress,
                  currentOracleUtxo,
                  currentState,
                  newChainState,
                  headersList,
                  parentPath,
                  validityTime,
                  setup.script,
                  referenceScriptUtxo,
                  timeout,
                  mpfProofs
                )

                txResult match {
                    case Right(resultTxHash) =>
                        if totalBatches > 1 then {
                            println(s"    Batch $batchNum submitted: $resultTxHash")
                        }

                        currentState = newChainState
                        currentMpf = updatedMpf

                        if batchIndex < batches.size - 1 then {
                            println(s"    Waiting for UTxO to be indexed...")
                            val txHash = TransactionHash.fromHex(resultTxHash)
                            val status = setup.provider
                                .pollForConfirmation(txHash)
                                .await(timeout)
                            if status == TransactionStatus.Confirmed then {
                                val newOracleInput = TransactionInput(txHash, 0)
                                setup.provider
                                    .findUtxo(newOracleInput)
                                    .await(timeout) match {
                                    case Right(u) =>
                                        currentOracleUtxo = u
                                        println(s"    UTxO indexed")
                                    case Left(_) =>
                                        System.err.println(
                                          s"    UTxO not available after confirmation"
                                        )
                                        break(1)
                                }
                            } else {
                                System.err.println(
                                  s"    UTxO not available after 30s"
                                )
                                break(1)
                            }

                            // Wait for wallet UTxOs to be indexed
                            println(
                              s"    Waiting for new wallet UTxOs from batch $batchNum..."
                            )
                            var newUtxoFound = false
                            var walletAttempts = 0
                            val maxWalletAttempts = 30
                            while !newUtxoFound && walletAttempts < maxWalletAttempts do {
                                Thread.sleep(1000)
                                walletAttempts += 1
                                try {
                                    setup.provider
                                        .findUtxos(setup.sponsorAddress)
                                        .await(timeout) match {
                                        case Right(utxos) =>
                                            if utxos.exists { case (input, _) =>
                                                    input.transactionId.toHex == resultTxHash
                                                }
                                            then {
                                                newUtxoFound = true
                                                println(
                                                  s"    New wallet UTxO indexed (after ${walletAttempts}s)"
                                                )
                                            }
                                        case Left(_) =>
                                    }
                                } catch { case _: Exception => }
                            }

                            if !newUtxoFound then {
                                System.err.println(
                                  s"    New wallet UTxO not indexed after ${maxWalletAttempts}s"
                                )
                                break(1)
                            }
                        }

                    case Left(errorMsg) =>
                        println()
                        System.err.println(
                          s"  Error submitting transaction: $errorMsg"
                        )
                        if totalBatches > 1 then {
                            System.err.println(
                              s"  Failed at batch $batchNum (blocks $batchStart to $batchEnd)"
                            )
                            System.err.println(
                              s"  Successfully processed ${batchIndex * batchSize} blocks before failure"
                            )
                        }
                        break(1)
                }
            }

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
            println(s"  2. Verify oracle: binocular verify-oracle")
            0
        }
}
