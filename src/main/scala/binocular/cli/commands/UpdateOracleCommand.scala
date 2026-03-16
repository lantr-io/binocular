package binocular.cli.commands

import binocular.cli.{Command, CommandHelpers}
import binocular.*
import scalus.cardano.address.Address
import scalus.cardano.ledger.{TransactionHash, TransactionInput, Utxo}
import scalus.cardano.txbuilder.TransactionSigner
import scalus.uplc.builtin.{ByteString, Data}
import scalus.cardano.onchain.plutus.prelude.List as ScalusList
import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF
import scalus.utils.Hex.hexToBytes

import scala.concurrent.duration.*
import scala.concurrent.{ExecutionContext, Future}
import scala.util.boundary
import scala.util.boundary.break
import scalus.utils.await

/** Update oracle with new Bitcoin blocks */
case class UpdateOracleCommand(
    utxo: String,
    fromBlock: Option[Long],
    toBlock: Option[Long]
) extends Command {

    override def execute(config: BinocularConfig): Int = {
        println(s"Updating oracle at $utxo...")
        println()

        CommandHelpers.parseUtxo(utxo) match {
            case Left(err) =>
                System.err.println(s"Error: $err")
                return 1
            case Right((txHash, outputIndex)) =>
                updateOracle(txHash, outputIndex, config)
        }
    }

    private def updateOracle(txHash: String, outputIndex: Int, config: BinocularConfig): Int = {
        val btcConf = config.bitcoinNode
        val cardanoConf = config.cardano
        val oracleConf = config.oracle
        val walletConf = config.wallet

        val params = oracleConf.toBitcoinValidatorParams() match {
            case Right(p) => p
            case Left(err) =>
                System.err.println(s"Error: $err")
                return 1
        }

        val oracleScriptAddress = oracleConf.scriptAddress(cardanoConf.cardanoNetwork) match {
            case Right(addr) => addr
            case Left(err) =>
                System.err.println(s"Error deriving script address: $err")
                return 1
        }

        println("Step 1: Loading configurations...")
        println(s"  Bitcoin Node: ${btcConf.url}")
        println(s"  Cardano Network: ${cardanoConf.network}")
        println(s"  Oracle Address: $oracleScriptAddress")
        println()

        given ec: ExecutionContext = ExecutionContext.global

        val timeout = oracleConf.transactionTimeout.seconds

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

        val scriptAddress = Address.fromBech32(oracleScriptAddress)
        val script = BitcoinContract.makeContract(params).script
        val referenceScriptUtxo: Option[Utxo] =
            try {
                val existingRefs = OracleTransactions.findReferenceScriptUtxos(
                  provider,
                  scriptAddress,
                  script.scriptHash,
                  timeout
                )

                existingRefs.headOption match {
                    case Some((refTxHash, refOutputIdx)) =>
                        println(
                          s"  Found existing reference script at $refTxHash:$refOutputIdx"
                        )
                        val refInput = TransactionInput(
                          TransactionHash.fromHex(refTxHash),
                          refOutputIdx
                        )
                        val utxoResult =
                            provider.findUtxo(refInput).await(timeout)
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
                          script,
                          timeout
                        ) match {
                            case Right((deployTxHash, deployOutputIdx, savedOutput)) =>
                                println(
                                  s"  Reference script deployed at $deployTxHash:$deployOutputIdx"
                                )
                                println(s"  This will save ~10KB per transaction")

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
                                        val result =
                                            provider.findUtxo(refInput).await(timeout)
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
                provider.findUtxos(scriptAddress).await(timeout)
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
                println(s"  Fork Tree Size: ${currentChainState.forkTree.blockCount}")

                val initialMpf = OffChainMPF.empty
                    .insert(currentChainState.blockHash, currentChainState.blockHash)
                val offChainMpfInit: OffChainMPF =
                    if initialMpf.rootHash == currentChainState.confirmedBlocksRoot then
                        println(
                          s"  MPF state: single confirmed block (no previous promotions)"
                        )
                        initialMpf
                    else
                        val startHeight = oracleConf.startHeight match {
                            case Some(h) => h
                            case None =>
                                System.err.println(
                                  s"  Error: Previous promotions detected but oracle start-height not configured."
                                )
                                System.err.println(
                                  s"  Set ORACLE_START_HEIGHT or start-height in config to rebuild MPF."
                                )
                                return 1
                        }
                        println(
                          s"  MPF state: rebuilding from blocks $startHeight to ${currentChainState.blockHeight}"
                        )
                        val rpcForMpf = new SimpleBitcoinRpc(btcConf)
                        def rebuildMpf(
                            heights: List[Long],
                            mpf: OffChainMPF
                        ): Future[OffChainMPF] = {
                            heights match {
                                case Nil => Future.successful(mpf)
                                case h :: tail =>
                                    for {
                                        hashHex <- rpcForMpf.getBlockHash(h.toInt)
                                        blockHash = ByteString
                                            .fromArray(hashHex.hexToBytes.reverse)
                                        updatedMpf = mpf.insert(blockHash, blockHash)
                                        result <- rebuildMpf(tail, updatedMpf)
                                    } yield result
                            }
                        }
                        val heights =
                            (startHeight to currentChainState.blockHeight.toLong).toList
                        val rebuiltMpf =
                            try {
                                rebuildMpf(heights, OffChainMPF.empty).await(120.seconds)
                            } catch {
                                case e: Exception =>
                                    System.err.println(
                                      s"  Error rebuilding MPF: ${e.getMessage}"
                                    )
                                    return 1
                            }
                        if rebuiltMpf.rootHash != currentChainState.confirmedBlocksRoot
                        then {
                            System.err.println(
                              s"  Error: Rebuilt MPF root does not match on-chain confirmedBlocksRoot."
                            )
                            System.err.println(
                              s"  Expected: ${currentChainState.confirmedBlocksRoot.toHex}"
                            )
                            System.err.println(
                              s"  Got: ${rebuiltMpf.rootHash.toHex}"
                            )
                            System.err.println(
                              s"  Check that ORACLE_START_HEIGHT is correct."
                            )
                            return 1
                        }
                        println(s"  MPF rebuilt successfully (${heights.size} blocks)")
                        rebuiltMpf

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
                        val rpc = new SimpleBitcoinRpc(btcConf)
                        val infoFuture = rpc.getBlockchainInfo()
                        try {
                            val info = infoFuture.await(30.seconds)
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

                val maxBlocksPerCommand = 100
                if numBlocks > maxBlocksPerCommand then {
                    val currentTime = System.currentTimeMillis() / 1000
                    val challengeAgingSeconds = params.challengeAging.toLong

                    val canPromote =
                        currentChainState.forkTree.oldestBlockTime match {
                            case Some(oldest) =>
                                val age = currentTime - oldest.toLong
                                age >= challengeAgingSeconds
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

                var currentState = currentChainState
                var currentOracleUtxo = oracleUtxo
                var currentMpf = offChainMpfInit

                boundary[Int] {
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
                                        rest <- fetchHeadersSequentially(
                                          tail,
                                          header :: acc
                                        )
                                    } yield rest
                            }
                        }
                        val headersFuture: Future[Seq[BlockHeader]] =
                            fetchHeadersSequentially(batch.toList, Nil)

                        val headers: Seq[BlockHeader] =
                            try {
                                headersFuture.await(60.seconds)
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
                              provider.cardanoInfo
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
                          signer,
                          provider,
                          scriptAddress,
                          sponsorAddress,
                          currentOracleUtxo,
                          currentState,
                          newChainState,
                          headersList,
                          parentPath,
                          validityTime,
                          script,
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

                                    val newOracleInput = TransactionInput(
                                      TransactionHash.fromHex(resultTxHash),
                                      0
                                    )
                                    var utxoAvailable: Option[Utxo] = None
                                    var attempts = 0
                                    val maxAttempts = 30

                                    while utxoAvailable.isEmpty && attempts < maxAttempts
                                    do {
                                        Thread.sleep(1000)
                                        attempts += 1

                                        try {
                                            val result = provider
                                                .findUtxo(newOracleInput)
                                                .await(timeout)
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
                                        break(1)
                                    }

                                    currentOracleUtxo = utxoAvailable.get

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
                                            val walletUtxosResult = provider
                                                .findUtxos(sponsorAddress)
                                                .await(timeout)
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
                    println(s"  2. Verify oracle: binocular verify-oracle $currentTxHash:0")
                    0
                }
        }
    }
}
