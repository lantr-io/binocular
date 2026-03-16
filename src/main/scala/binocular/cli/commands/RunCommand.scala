package binocular.cli.commands

import binocular.*
import binocular.cli.{Command, CommandHelpers}
import com.typesafe.scalalogging.LazyLogging
import scalus.cardano.ledger.{TransactionHash, TransactionInput, Utxo}
import scalus.cardano.onchain.plutus.prelude.List as ScalusList
import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.*
import scala.util.boundary, boundary.break
import scalus.utils.await

/** Continuous daemon: read oracle state, submit updates in a loop */
case class RunCommand(utxo: String, dryRun: Boolean = false) extends Command with LazyLogging {

    override def execute(config: BinocularConfig): Int = {
        if dryRun then println("Running in dry-run mode (will compute one update and exit)")
        else println("Starting oracle update daemon...")
        println()

        CommandHelpers.parseUtxo(utxo) match {
            case Left(err) =>
                System.err.println(s"Error: $err")
                1
            case Right((txHash, outputIndex)) =>
                runDaemon(txHash, outputIndex, config)
        }
    }

    private def runDaemon(
        initialTxHash: String,
        initialOutputIndex: Int,
        config: BinocularConfig
    ): Int = boundary {
        given ec: ExecutionContext = ExecutionContext.global
        val timeout = config.oracle.transactionTimeout.seconds
        val pollInterval = config.oracle.pollInterval
        val retryInterval = config.oracle.retryInterval

        val setup = CommandHelpers.setupOracle(config) match {
            case Right(s) => s
            case Left(err) =>
                System.err.println(s"Error: $err")
                break(1)
        }

        // Find or deploy reference script
        var referenceScriptUtxo = CommandHelpers.findReferenceScriptUtxo(
          setup.provider,
          setup.scriptAddress,
          setup.script.scriptHash,
          timeout
        )
        if referenceScriptUtxo.isEmpty then {
            logger.info("Deploying reference script...")
            OracleTransactions.deployReferenceScript(
              setup.signer,
              setup.provider,
              setup.sponsorAddress,
              setup.scriptAddress,
              setup.script,
              timeout
            ) match {
                case Right((hash, idx, output)) =>
                    logger.info(s"Reference script deployed at $hash:$idx")
                    val refInput = TransactionInput(TransactionHash.fromHex(hash), idx)
                    CommandHelpers.waitForUtxo(setup.provider, refInput, timeout)
                    referenceScriptUtxo = Some(Utxo(refInput, output))
                case Left(err) =>
                    System.err.println(s"Error deploying reference script: $err")
                    break(1)
            }
        }

        // Fetch initial oracle UTxO and parse state
        val initialInput = TransactionInput(
          TransactionHash.fromHex(initialTxHash),
          initialOutputIndex
        )
        var currentOracleUtxo: Utxo = setup.provider.findUtxo(initialInput).await(timeout) match {
            case Right(u) => u
            case Left(_) =>
                System.err.println(
                  s"Error: Oracle UTxO not found at $initialTxHash:$initialOutputIndex"
                )
                break(1)
        }

        var currentChainState: ChainState =
            try {
                currentOracleUtxo.output.requireInlineDatum.to[ChainState]
            } catch {
                case e: Exception =>
                    System.err.println(s"Error parsing ChainState: ${e.getMessage}")
                    break(1)
            }

        logger.info(
          s"Oracle state: height=${currentChainState.blockHeight}, forkTree=${currentChainState.forkTree.blockCount} blocks"
        )

        // Reconstruct off-chain MPF
        val rpc = new SimpleBitcoinRpc(config.bitcoinNode)
        var currentMpf: OffChainMPF = CommandHelpers.reconstructMpf(
          rpc,
          currentChainState,
          config.oracle.startHeight
        ) match {
            case Right(mpf) =>
                logger.info("MPF reconstructed successfully")
                mpf
            case Left(err) =>
                System.err.println(s"Error: $err")
                break(1)
        }

        val batchSize = config.oracle.maxHeadersPerTx

        // Main loop
        while true do {
            try {
                val bitcoinInfo = rpc.getBlockchainInfo().await(30.seconds)
                val bitcoinTip = bitcoinInfo.blocks.toLong

                val highestKnown =
                    if currentChainState.forkTree.nonEmpty then
                        currentChainState.forkTree
                            .highestHeight(currentChainState.blockHeight)
                            .toLong
                    else currentChainState.blockHeight.toLong

                if bitcoinTip > highestKnown then {
                    val startHeight = highestKnown + 1
                    val endHeight = Math.min(bitcoinTip, startHeight + batchSize - 1)

                    logger.info(
                      s"New blocks available: $startHeight to $endHeight (Bitcoin tip: $bitcoinTip)"
                    )

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
                        OracleTransactions.computeValidityIntervalTime(setup.provider.cardanoInfo)
                    val parentPath = currentChainState.forkTree.findTipPath

                    val (newChainState, mpfProofs, updatedMpf) =
                        OracleTransactions.computeUpdateWithProofs(
                          currentChainState,
                          headersList,
                          parentPath,
                          validityTime,
                          currentMpf,
                          setup.params
                        )

                    if dryRun then {
                        println()
                        println("Dry-run: computed update")
                        println(s"  Current Height: ${currentChainState.blockHeight}")
                        println(s"  New Height: ${newChainState.blockHeight}")
                        println(s"  Headers: ${headers.size}")
                        println(s"  Fork Tree: ${newChainState.forkTree.blockCount} blocks")
                        break(0)
                    }

                    val txResult = OracleTransactions.buildAndSubmitUpdateTransaction(
                      setup.signer,
                      setup.provider,
                      setup.scriptAddress,
                      setup.sponsorAddress,
                      currentOracleUtxo,
                      currentChainState,
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
                            logger.info(
                              s"Transaction submitted: $resultTxHash (blocks $startHeight-$endHeight)"
                            )

                            val newInput =
                                TransactionInput(TransactionHash.fromHex(resultTxHash), 0)
                            CommandHelpers.waitForUtxo(
                              setup.provider,
                              newInput,
                              timeout,
                              maxAttempts = 60
                            ) match {
                                case Some(u) =>
                                    currentOracleUtxo = u
                                    currentChainState = newChainState
                                    currentMpf = updatedMpf
                                    logger.info("UTxO confirmed")
                                case None =>
                                    logger.warn(
                                      "UTxO not confirmed after 60s, re-reading state..."
                                    )
                            }

                        case Left(err) =>
                            logger.error(s"Transaction failed: $err")
                            Thread.sleep(retryInterval * 1000L)
                    }
                } else {
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
