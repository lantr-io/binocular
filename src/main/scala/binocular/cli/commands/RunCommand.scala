package binocular.cli.commands

import binocular.*
import binocular.cli.{Command, CommandHelpers}
import com.typesafe.scalalogging.LazyLogging
import scalus.cardano.address.Address
import scalus.cardano.ledger.{TransactionHash, TransactionInput, Utxo}
import scalus.cardano.txbuilder.TransactionSigner
import scalus.uplc.builtin.ByteString
import scalus.cardano.onchain.plutus.prelude.List as ScalusList
import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF
import scalus.utils.Hex.hexToBytes

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.*
import scalus.utils.await

/** Continuous daemon: read oracle state, submit updates in a loop */
case class RunCommand(utxo: String, dryRun: Boolean = false) extends Command with LazyLogging {

    override def execute(config: BinocularConfig): Int = {
        if dryRun then {
            println("Running in dry-run mode (will compute one update and exit)")
        } else {
            println("Starting oracle update daemon...")
        }
        println()

        CommandHelpers.parseUtxo(utxo) match {
            case Left(err) =>
                System.err.println(s"Error: $err")
                return 1
            case Right((txHash, outputIndex)) =>
                runDaemon(txHash, outputIndex, config)
        }
    }

    private def runDaemon(
        initialTxHash: String,
        initialOutputIndex: Int,
        config: BinocularConfig
    ): Int = {
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

        given ec: ExecutionContext = ExecutionContext.global
        val timeout = oracleConf.transactionTimeout.seconds
        val pollInterval = oracleConf.pollInterval
        val retryInterval = oracleConf.retryInterval

        // Set up wallet
        val hdAccount = walletConf.createHdAccount() match {
            case Right(acc) =>
                val addr = acc.baseAddress(cardanoConf.scalusNetwork).toBech32.getOrElse("?")
                logger.info(s"Wallet loaded: $addr")
                acc
            case Left(err) =>
                System.err.println(s"Error creating wallet account: $err")
                return 1
        }
        val signer = new TransactionSigner(Set(hdAccount.paymentKeyPair))
        val sponsorAddress = hdAccount.baseAddress(cardanoConf.scalusNetwork)

        // Set up Cardano provider
        val provider = cardanoConf.createBlockchainProvider() match {
            case Right(p) =>
                logger.info(s"Connected to Cardano backend (${cardanoConf.backend})")
                p
            case Left(err) =>
                System.err.println(s"Error creating blockchain provider: $err")
                return 1
        }

        val scriptAddress = Address.fromBech32(oracleScriptAddress)
        val script = BitcoinContract.makeContract(params).script

        // Find or deploy reference script
        val referenceScriptUtxo: Option[Utxo] = {
            val refs = OracleTransactions.findReferenceScriptUtxos(
              provider,
              scriptAddress,
              script.scriptHash,
              timeout
            )
            refs.headOption
                .flatMap { case (refHash, refIdx) =>
                    val refInput = TransactionInput(TransactionHash.fromHex(refHash), refIdx)
                    provider.findUtxo(refInput).await(timeout) match {
                        case Right(u) =>
                            logger.info(s"Found reference script at $refHash:$refIdx")
                            Some(u)
                        case Left(_) => None
                    }
                }
                .orElse {
                    logger.info("Deploying reference script...")
                    OracleTransactions.deployReferenceScript(
                      signer,
                      provider,
                      sponsorAddress,
                      scriptAddress,
                      script,
                      timeout
                    ) match {
                        case Right((hash, idx, output)) =>
                            logger.info(s"Reference script deployed at $hash:$idx")
                            // Wait for confirmation
                            val refInput = TransactionInput(TransactionHash.fromHex(hash), idx)
                            var confirmed = false
                            var attempts = 0
                            while !confirmed && attempts < 30 do {
                                Thread.sleep(2000)
                                attempts += 1
                                try {
                                    provider.findUtxo(refInput).await(timeout) match {
                                        case Right(_) => confirmed = true
                                        case Left(_)  =>
                                    }
                                } catch { case _: Exception => }
                            }
                            Some(Utxo(refInput, output))
                        case Left(err) =>
                            System.err.println(s"Error deploying reference script: $err")
                            return 1
                    }
                }
        }

        // Fetch initial oracle UTxO and parse state
        val initialInput = TransactionInput(
          TransactionHash.fromHex(initialTxHash),
          initialOutputIndex
        )
        var currentOracleUtxo: Utxo = provider.findUtxo(initialInput).await(timeout) match {
            case Right(u) => u
            case Left(_) =>
                System.err.println(
                  s"Error: Oracle UTxO not found at $initialTxHash:$initialOutputIndex"
                )
                return 1
        }

        var currentChainState: ChainState =
            try {
                currentOracleUtxo.output.requireInlineDatum.to[ChainState]
            } catch {
                case e: Exception =>
                    System.err.println(s"Error parsing ChainState: ${e.getMessage}")
                    return 1
            }

        logger.info(
          s"Oracle state: height=${currentChainState.blockHeight}, forkTree=${currentChainState.forkTree.blockCount} blocks"
        )

        // Reconstruct off-chain MPF
        var currentMpf: OffChainMPF = {
            val initialMpf = OffChainMPF.empty
                .insert(currentChainState.blockHash, currentChainState.blockHash)
            if initialMpf.rootHash == currentChainState.confirmedBlocksRoot then {
                logger.info("MPF state: single confirmed block")
                initialMpf
            } else {
                val startHeight = oracleConf.startHeight match {
                    case Some(h) => h
                    case None =>
                        System.err.println(
                          "Error: Previous promotions detected but ORACLE_START_HEIGHT not configured"
                        )
                        return 1
                }
                logger.info(
                  s"Rebuilding MPF from blocks $startHeight to ${currentChainState.blockHeight}"
                )
                val rpcForMpf = new SimpleBitcoinRpc(btcConf)
                def rebuildMpf(heights: List[Long], mpf: OffChainMPF): Future[OffChainMPF] = {
                    heights match {
                        case Nil => Future.successful(mpf)
                        case h :: tail =>
                            for {
                                hashHex <- rpcForMpf.getBlockHash(h.toInt)
                                blockHash = ByteString.fromArray(hashHex.hexToBytes.reverse)
                                updatedMpf = mpf.insert(blockHash, blockHash)
                                result <- rebuildMpf(tail, updatedMpf)
                            } yield result
                    }
                }
                val heights = (startHeight to currentChainState.blockHeight.toLong).toList
                val rebuiltMpf = rebuildMpf(heights, OffChainMPF.empty).await(120.seconds)
                if rebuiltMpf.rootHash != currentChainState.confirmedBlocksRoot then {
                    System.err.println(
                      "Error: Rebuilt MPF root does not match on-chain confirmedBlocksRoot"
                    )
                    return 1
                }
                logger.info(s"MPF rebuilt successfully (${heights.size} blocks)")
                rebuiltMpf
            }
        }

        val rpc = new SimpleBitcoinRpc(btcConf)
        val batchSize = oracleConf.maxHeadersPerTx

        // Main loop
        while true do {
            try {
                // Get Bitcoin tip
                val bitcoinInfo = rpc.getBlockchainInfo().await(30.seconds)
                val bitcoinTip = bitcoinInfo.blocks.toLong

                // Determine highest known block
                val highestKnown = if currentChainState.forkTree.nonEmpty then {
                    currentChainState.forkTree.highestHeight(currentChainState.blockHeight).toLong
                } else {
                    currentChainState.blockHeight.toLong
                }

                if bitcoinTip > highestKnown then {
                    val startHeight = highestKnown + 1
                    val endHeight = Math.min(bitcoinTip, startHeight + batchSize - 1)

                    logger.info(
                      s"New blocks available: $startHeight to $endHeight (Bitcoin tip: $bitcoinTip)"
                    )

                    // Fetch headers
                    def fetchHeaders(
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
                                    rest <- fetchHeaders(tail, header :: acc)
                                } yield rest
                        }
                    }

                    val headers =
                        fetchHeaders((startHeight to endHeight).toList, Nil).await(60.seconds)
                    val headersList = ScalusList.from(headers)

                    val (_, validityTime) =
                        OracleTransactions.computeValidityIntervalTime(provider.cardanoInfo)
                    val parentPath = currentChainState.forkTree.findTipPath

                    val (newChainState, mpfProofs, updatedMpf) =
                        OracleTransactions.computeUpdateWithProofs(
                          currentChainState,
                          headersList,
                          parentPath,
                          validityTime,
                          currentMpf,
                          params
                        )

                    if dryRun then {
                        println()
                        println("Dry-run: computed update")
                        println(s"  Current Height: ${currentChainState.blockHeight}")
                        println(s"  New Height: ${newChainState.blockHeight}")
                        println(s"  Headers: ${headers.size}")
                        println(s"  Fork Tree: ${newChainState.forkTree.blockCount} blocks")
                        return 0
                    }

                    val txResult = OracleTransactions.buildAndSubmitUpdateTransaction(
                      signer,
                      provider,
                      scriptAddress,
                      sponsorAddress,
                      currentOracleUtxo,
                      currentChainState,
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
                            logger.info(
                              s"Transaction submitted: $resultTxHash (blocks $startHeight-$endHeight)"
                            )

                            // Wait for new UTxO
                            val newInput =
                                TransactionInput(TransactionHash.fromHex(resultTxHash), 0)
                            var utxoAvailable: Option[Utxo] = None
                            var attempts = 0
                            while utxoAvailable.isEmpty && attempts < 60 do {
                                Thread.sleep(1000)
                                attempts += 1
                                try {
                                    provider.findUtxo(newInput).await(timeout) match {
                                        case Right(u) => utxoAvailable = Some(u)
                                        case Left(_)  =>
                                    }
                                } catch { case _: Exception => }
                            }

                            utxoAvailable match {
                                case Some(u) =>
                                    currentOracleUtxo = u
                                    currentChainState = newChainState
                                    currentMpf = updatedMpf
                                    logger.info(s"UTxO confirmed after ${attempts}s")
                                case None =>
                                    logger.warn(
                                      s"UTxO not confirmed after 60s, re-reading state..."
                                    )
                                // Re-read oracle state on next iteration
                            }

                        case Left(err) =>
                            logger.error(s"Transaction failed: $err")
                            Thread.sleep(retryInterval * 1000L)
                    }
                } else {
                    // No new blocks, check if promotion is possible
                    Thread.sleep(pollInterval * 1000L)
                }
            } catch {
                case e: Exception =>
                    logger.error(s"Error in main loop: ${e.getMessage}")
                    Thread.sleep(retryInterval * 1000L)
            }
        }
        0 // unreachable
    }
}
