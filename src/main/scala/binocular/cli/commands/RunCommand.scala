package binocular.cli.commands

import binocular.*
import binocular.ForkTreePretty.*
import binocular.cli.{Command, CommandHelpers, Console}
import scalus.cardano.ledger.{TransactionHash, TransactionInput, Utxo}
import scalus.cardano.node.TransactionStatus
import scalus.cardano.onchain.plutus.prelude.List as ScalusList
import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.*
import scala.util.boundary
import boundary.break
import scalus.utils.await
import cats.syntax.either.*

/** Continuous daemon: read oracle state, submit updates in a loop */
case class RunCommand(dryRun: Boolean = false) extends Command {

    override def execute(config: BinocularConfig): Int = {
        Console.header("Binocular Oracle Daemon")
        if dryRun then Console.warn("Dry-run mode — will compute one update and exit")
        println()

        runDaemon(config)
    }

    private def runDaemon(
        config: BinocularConfig
    ): Int = boundary {
        given ec: ExecutionContext = ExecutionContext.global
        val timeout = config.oracle.transactionTimeout.seconds
        val pollInterval = config.oracle.pollInterval
        val retryInterval = config.oracle.retryInterval

        val setup = CommandHelpers.setupOracle(config).valueOr { err =>
            Console.error(err)
            break(1)
        }

        Console.info("Bitcoin", s"${config.bitcoinNode.url} (${config.bitcoinNode.network})")
        Console.info("Cardano", config.cardano.network)
        Console.info(
          "Wallet",
          setup.hdAccount.baseAddress(config.cardano.scalusNetwork).toBech32.getOrElse("?")
        )

        // Find reference script (deployed by init to script address)
        var referenceScriptUtxo: Utxo = CommandHelpers
            .findReferenceScriptUtxo(
              setup.provider,
              setup.scriptAddress,
              setup.script,
              timeout
            )
            .getOrElse {
                Console.error("Reference script not found. Run 'binocular init' first.")
                break(1)
            }

        // Find oracle UTxO by NFT
        var currentOracleUtxo: Utxo =
            try {
                CommandHelpers
                    .findOracleUtxo(setup.provider, setup.script.scriptHash)
                    .await(timeout)
            } catch {
                case e: Exception =>
                    Console.error(e.getMessage)
                    break(1)
            }

        var currentChainState: ChainState =
            try {
                currentOracleUtxo.output.requireInlineDatum.to[ChainState]
            } catch {
                case e: Exception =>
                    Console.error(s"Parsing ChainState: ${e.getMessage}")
                    break(1)
            }

        Console.info(
          "Oracle",
          setup.scriptAddress.encode.getOrElse("?")
        )
        Console.info("Height", currentChainState.ctx.height)
        Console.info(
          "Fork tree",
          s"${currentChainState.forkTree.blockCount} blocks"
        )
        // Reconstruct off-chain MPF
        val rpc = new SimpleBitcoinRpc(config.bitcoinNode)
        var currentMpf: OffChainMPF = CommandHelpers
            .reconstructMpf(
              rpc,
              currentChainState,
              config.oracle.startHeight
            )
            .valueOr { err =>
                Console.error(err)
                break(1)
            }
        Console.success(s"MPF reconstructed: ${currentMpf.size} confirmed blocks")
        println(
          currentChainState.forkTree
              .pretty(currentChainState.ctx.height, confirmedBlocks = Some(currentMpf.size))
        )

        Console.separator()
        println()

        val batchSize = config.oracle.maxHeadersPerTx

        /** Re-read oracle UTxO and chain state from the blockchain. Called after tx failure or
          * uncertain confirmation to recover from stale state.
          */
        def refreshOracleState(): Unit = {
            try {
                Console.log("Re-reading oracle state...")
                val utxo = CommandHelpers
                    .findOracleUtxo(setup.provider, setup.script.scriptHash)
                    .await(timeout)
                val state = utxo.output.requireInlineDatum.to[ChainState]
                currentOracleUtxo = utxo
                currentChainState = state
                currentMpf = CommandHelpers
                    .reconstructMpf(rpc, state, config.oracle.startHeight)
                    .valueOr { err =>
                        Console.logError(s"MPF reconstruction failed: $err")
                        currentMpf // keep existing
                    }
                Console.logSuccess(
                  s"State refreshed: height=${state.ctx.height}, tree=${state.forkTree.blockCount} blocks"
                )
            } catch {
                case e: Exception =>
                    Console.logError(s"Failed to refresh state: ${e.getMessage}")
            }
        }

        // Main loop
        while true do {
            try {
                val bitcoinInfo = rpc.getBlockchainInfo().await(30.seconds)
                val bitcoinTip = bitcoinInfo.blocks.toLong

                val highestKnown =
                    if currentChainState.forkTree.nonEmpty then
                        currentChainState.forkTree
                            .highestHeight(currentChainState.ctx.height)
                            .toLong
                    else currentChainState.ctx.height.toLong

                // Cap fork tree best chain at 150 blocks to prevent datum overflow
                val maxForkTreeBestChain = 150
                val bestChainBlocks =
                    if currentChainState.forkTree.nonEmpty then
                        (highestKnown - currentChainState.ctx.height.toLong).toInt
                    else 0
                val maxNewBlocks = maxForkTreeBestChain - bestChainBlocks

                // Fetch new headers if available and fork tree has room
                val headers = if bitcoinTip > highestKnown && maxNewBlocks > 0 then {
                    val startHeight = highestKnown + 1
                    val endHeight = Math.min(
                      Math.min(bitcoinTip, startHeight + batchSize - 1),
                      startHeight + maxNewBlocks - 1
                    )

                    Console.log(
                      s"Fetching blocks $startHeight..$endHeight (${endHeight - startHeight + 1} headers)"
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

                    fetchHeaders((startHeight to endHeight).toList, Nil).await(60.seconds)
                } else Nil

                val headersList = ScalusList.from(headers)

                val (_, validityTime) =
                    OracleTransactions.computeValidityIntervalTime(setup.provider.cardanoInfo)
                val parentPath = currentChainState.forkTree.findTipPath

                // Quick check: is there anything to do?
                val totalPromotable = OracleTransactions
                    .computePromotedBlocks(
                      currentChainState,
                      headersList,
                      parentPath,
                      validityTime,
                      setup.params
                    )
                    .length
                val stateChanged = headers.nonEmpty || totalPromotable > 0

                if !stateChanged then {
                    val behind = bitcoinTip - highestKnown
                    val status =
                        if behind == 0 then "up to date"
                        else s"behind: $behind"
                    Console.logInPlace(
                      s"Polling... tip: $bitcoinTip | oracle: $highestKnown | $status"
                    )
                    Thread.sleep(pollInterval * 1000L)
                } else {
                    if dryRun then {
                        Console.success("Dry-run: computed update")
                        Console.info("Current Height", currentChainState.ctx.height)
                        Console.info("Promotable", totalPromotable)
                        Console.info("Headers", headers.size)
                        Console.info(
                          "Fork Tree",
                          s"${currentChainState.forkTree.blockCount} blocks"
                        )
                        break(0)
                    }

                    val result = OracleTransactions.buildOptimalUpdateTransaction(
                      setup.provider,
                      setup.hdAccount,
                      setup.compiled,
                      currentOracleUtxo,
                      currentChainState,
                      headersList,
                      parentPath,
                      validityTime,
                      referenceScriptUtxo,
                      currentMpf,
                      setup.params,
                      timeout
                    )

                    result match {
                        case Right((txResult, newChainState, updatedMpf, promotions)) =>
                            Console.log(
                              s"Update: +${headers.size} headers, $promotions promoted | tree: ${newChainState.forkTree.blockCount} blocks"
                            )
                            Console.log(
                              s"Submitting... datum: ${txResult.datumSize} B, tx: ${txResult.txSize} B"
                            )

                            val txHash = TransactionHash.fromHex(txResult.txHash)
                            val status = setup.provider
                                .pollForConfirmation(txHash, maxAttempts = 60)
                                .await(timeout)
                            if status == TransactionStatus.Confirmed then {
                                val newInput = TransactionInput(txHash, 0)
                                setup.provider.findUtxo(newInput).await(timeout) match {
                                    case Right(u) =>
                                        currentOracleUtxo = u
                                        currentChainState = newChainState
                                        currentMpf = updatedMpf
                                        Console.logSuccess(
                                          s"Confirmed ${txResult.txHash} | height: ${newChainState.ctx.height}"
                                        )
                                        println(
                                          newChainState.forkTree.pretty(
                                            newChainState.ctx.height,
                                            confirmedBlocks = Some(updatedMpf.size)
                                          )
                                        )
                                        // Wait for wallet UTxOs to be indexed before next iteration
                                        var walletReady = false
                                        var walletAttempts = 0
                                        while !walletReady && walletAttempts < 30 do {
                                            Thread.sleep(1000)
                                            walletAttempts += 1
                                            try {
                                                setup.provider
                                                    .findUtxos(setup.sponsorAddress)
                                                    .await(timeout) match {
                                                    case Right(utxos) =>
                                                        if utxos.exists { case (input, _) =>
                                                                input.transactionId.toHex == txResult.txHash
                                                            }
                                                        then walletReady = true
                                                    case Left(_) =>
                                                }
                                            } catch { case _: Exception => }
                                        }
                                        if !walletReady then
                                            Console.logWarn(
                                              "Wallet UTxOs not indexed after 30s, continuing anyway"
                                            )
                                    case Left(_) =>
                                        Console.logWarn(
                                          "UTxO confirmed but not found, re-reading state..."
                                        )
                                        refreshOracleState()
                                }
                            } else {
                                Console.logWarn(
                                  "Tx not confirmed after 60s, re-reading state..."
                                )
                                refreshOracleState()
                            }

                        case Left(err) =>
                            Console.logError(s"Tx failed: $err")
                            refreshOracleState()
                    }
                }
            } catch {
                case e: Exception =>
                    Console.logError(
                      s"Error: ${e.getMessage} — retrying in ${retryInterval}s"
                    )
                    Thread.sleep(retryInterval * 1000L)
            }
        }
        0 // unreachable
    }
}
