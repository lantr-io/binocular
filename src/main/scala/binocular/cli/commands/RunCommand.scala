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

        val setup = CommandHelpers.setupOracle(config) match {
            case Right(s) => s
            case Left(err) =>
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
        println(currentChainState.forkTree.pretty(currentChainState.ctx.height))

        // Reconstruct off-chain MPF
        val rpc = new SimpleBitcoinRpc(config.bitcoinNode)
        var currentMpf: OffChainMPF = CommandHelpers.reconstructMpf(
          rpc,
          currentChainState,
          config.oracle.startHeight
        ) match {
            case Right(mpf) =>
                Console.success("MPF reconstructed")
                mpf
            case Left(err) =>
                Console.error(err)
                break(1)
        }

        Console.separator()
        println()

        val batchSize = config.oracle.maxHeadersPerTx

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

                // Fetch new headers if available
                val headers = if bitcoinTip > highestKnown then {
                    val startHeight = highestKnown + 1
                    val endHeight = Math.min(bitcoinTip, startHeight + batchSize - 1)

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
                      setup.signer,
                      setup.provider,
                      setup.scriptAddress,
                      setup.sponsorAddress,
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
                                          newChainState.forkTree.pretty(newChainState.ctx.height)
                                        )
                                    case Left(_) =>
                                        Console.logWarn(
                                          "UTxO confirmed but not found, re-reading state..."
                                        )
                                }
                            } else {
                                Console.logWarn(
                                  "UTxO not confirmed after 60s, re-reading state..."
                                )
                            }

                        case Left(err) =>
                            Console.logError(s"Tx failed: $err — retrying in ${retryInterval}s")
                            Thread.sleep(retryInterval * 1000L)
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
