package binocular.cli.commands

import binocular.*
import binocular.ForkTreePretty.*
import binocular.cli.{Command, CommandHelpers, Console}
import org.bouncycastle.crypto.digests.Blake2bDigest
import scalus.uplc.builtin.ByteString
import scalus.cardano.ledger.{Bech32, TransactionHash, TransactionInput, Utxo}
import scalus.cardano.node.TransactionStatus
import scalus.cardano.onchain.plutus.prelude.List as ScalusList
import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.*
import scala.util.boundary
import boundary.break
import scalus.utils.await
import cats.syntax.either.*

/** CIP-14 native-asset fingerprint: bech32("asset", blake2b-160(policyId ++ assetName)). */
private def assetFingerprint(policyId: ByteString, assetName: ByteString): String = {
    val concatenated = policyId.bytes ++ assetName.bytes
    val digest = new Blake2bDigest(160)
    digest.update(concatenated, 0, concatenated.length)
    val hash = new Array[Byte](digest.getDigestSize)
    digest.doFinal(hash, 0)
    Bech32.encode("asset", hash)
}

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
        val nftPolicyId = setup.script.scriptHash
        // Oracle NFT uses an empty asset name (see findOracleUtxo / mint logic).
        val nftAssetName = ByteString.empty
        Console.info("NFT policy", nftPolicyId.toHex)
        Console.info(
          "NFT asset",
          assetFingerprint(ByteString.fromArray(nftPolicyId.bytes), nftAssetName)
        )
        Console.info(
          "Oracle UTxO",
          s"${currentOracleUtxo.input.transactionId.toHex}#${currentOracleUtxo.input.index}"
        )
        Console.info("Height", currentChainState.ctx.height)
        Console.info(
          "Fork tree",
          s"${currentChainState.forkTree.blockCount} blocks"
        )
        CommandHelpers.printParams(setup.params)
        // Reconstruct off-chain MPF
        val rpc = new SimpleBitcoinRpc(config.bitcoinNode)
        var currentMpf: OffChainMPF =
            try
                CommandHelpers
                    .reconstructMpf(
                      rpc,
                      currentChainState,
                      config.oracle.startHeight
                    )
                    .valueOr { err =>
                        Console.error(err)
                        break(1)
                    }
            catch {
                case e: CommandHelpers.DeepReorgException =>
                    Console.error(e.getMessage)
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

        /** Poll the script address for an oracle UTxO whose input differs from `previousInput`.
          *
          * Used after a Pending-timeout or an "inputs already spent" submission error: those are
          * signals that the previously-submitted tx may have actually been included even though we
          * gave up waiting for it. If a *new* oracle UTxO appears we adopt it (returning Some). If
          * after the full timeout the oracle UTxO at the script address is still the same one we
          * had cached, the previous tx was almost certainly dropped (returning None) and the caller
          * can safely keep using the cached state.
          */
        def waitForOracleUtxoChange(
            previousInput: TransactionInput,
            maxAttempts: Int = 60,
            backoffMs: Long = 2000L
        ): Option[Utxo] = {
            var attempt = 0
            while attempt < maxAttempts do {
                attempt += 1
                try {
                    val fresh = CommandHelpers
                        .findOracleUtxo(setup.provider, setup.script.scriptHash)
                        .await(timeout)
                    if fresh.input != previousInput then return Some(fresh)
                } catch { case _: Exception => () }
                Thread.sleep(backoffMs)
            }
            None
        }

        /** Adopt a newly-observed oracle UTxO as the current state. Reconstructs the off-chain MPF;
          * if reconstruction fails, keeps the existing MPF (the next iteration will fail loudly
          * rather than silently corrupting state).
          */
        def adoptOracleUtxo(utxo: Utxo, source: String): Unit = {
            val state = utxo.output.requireInlineDatum.to[ChainState]
            currentOracleUtxo = utxo
            currentChainState = state
            currentMpf = CommandHelpers
                .reconstructMpf(rpc, state, config.oracle.startHeight)
                .valueOr { err =>
                    Console.logError(s"MPF reconstruction failed: $err")
                    currentMpf
                }
            Console.logSuccess(
              s"Adopted oracle state from $source: height=${state.ctx.height}, " +
                  s"tree=${state.forkTree.blockCount} blocks"
            )
            println(
              state.forkTree.pretty(
                state.ctx.height,
                confirmedBlocks = Some(currentMpf.size)
              )
            )
        }

        /** Re-read oracle UTxO and chain state from the blockchain. Called after tx failure or
          * uncertain confirmation to recover from stale state.
          *
          * Guards against indexer lag by rejecting reads that go backwards from the cached
          * in-memory state — lower height, or same height with a smaller fork tree. In that case
          * the read is treated as stale, and we retry up to a few times with a short backoff before
          * giving up and keeping the cached state.
          */
        def refreshOracleState(): Unit = {
            val maxAttempts = 10
            val backoffMs = 1000L
            var attempt = 0
            var done = false
            Console.log("Re-reading oracle state...")
            while !done && attempt < maxAttempts do {
                attempt += 1
                try {
                    val utxo = CommandHelpers
                        .findOracleUtxo(setup.provider, setup.script.scriptHash)
                        .await(timeout)
                    val state = utxo.output.requireInlineDatum.to[ChainState]

                    val cachedHeight = currentChainState.ctx.height
                    val cachedTreeBlocks = currentChainState.forkTree.blockCount
                    val newHeight = state.ctx.height
                    val newTreeBlocks = state.forkTree.blockCount

                    val isStale =
                        newHeight < cachedHeight ||
                            (newHeight == cachedHeight && newTreeBlocks < cachedTreeBlocks)

                    if isStale then {
                        Console.logWarn(
                          s"Stale read (attempt $attempt/$maxAttempts): " +
                              s"got height=$newHeight tree=$newTreeBlocks, " +
                              s"cached height=$cachedHeight tree=$cachedTreeBlocks — retrying"
                        )
                        Thread.sleep(backoffMs)
                    } else {
                        currentOracleUtxo = utxo
                        currentChainState = state
                        currentMpf = CommandHelpers
                            .reconstructMpf(rpc, state, config.oracle.startHeight)
                            .valueOr { err =>
                                Console.logError(s"MPF reconstruction failed: $err")
                                currentMpf // keep existing
                            }
                        Console.logSuccess(
                          s"State refreshed: height=$newHeight, tree=$newTreeBlocks blocks"
                        )
                        println(
                          state.forkTree.pretty(
                            state.ctx.height,
                            confirmedBlocks = Some(currentMpf.size)
                          )
                        )
                        done = true
                    }
                } catch {
                    case e: Exception =>
                        Console.logError(
                          s"Failed to refresh state (attempt $attempt/$maxAttempts): ${e.getMessage}"
                        )
                        Thread.sleep(backoffMs)
                }
            }
            if !done then
                Console.logWarn(
                  s"Could not obtain a non-stale oracle state after $maxAttempts attempts; keeping cached state"
                )
        }

        // Main loop
        while true do {
            try {
                val bitcoinInfo = rpc.getBlockchainInfo().await(30.seconds)
                val bitcoinTip = bitcoinInfo.blocks.toLong

                // Detect reorg and compute correct parentPath
                val (parentPath, reorgStartHeight) =
                    CommandHelpers.detectReorgAndComputePath(
                      rpc,
                      currentChainState,
                      currentMpf
                    )

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

                // Use reorgStartHeight if reorg detected, otherwise highestKnown + 1
                val effectiveStart = reorgStartHeight

                // Fetch new headers if available and fork tree has room
                val headers = if bitcoinTip >= effectiveStart && maxNewBlocks > 0 then {
                    val startHeight = effectiveStart
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
                            val appliedHeaders =
                                newChainState.forkTree.blockCount - currentChainState.forkTree.blockCount + promotions
                            Console.log(
                              s"Update: +$appliedHeaders/${headers.size} headers, $promotions promoted | tree: ${newChainState.forkTree.blockCount} blocks"
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
                                        // Wait for both wallet UTxOs AND the new oracle UTxO to
                                        // be indexed before the next iteration. Without the
                                        // oracle-side check, the next iteration's
                                        // findOracleUtxo (used by refresh paths) can return the
                                        // already-spent previous oracle UTxO and we will build a
                                        // tx referencing a stale input.
                                        var walletReady = false
                                        var oracleReady = false
                                        var attempts = 0
                                        while (!walletReady || !oracleReady) && attempts < 30 do {
                                            Thread.sleep(1000)
                                            attempts += 1
                                            if !walletReady then
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
                                            if !oracleReady then
                                                try {
                                                    val freshOracle = CommandHelpers
                                                        .findOracleUtxo(
                                                          setup.provider,
                                                          setup.script.scriptHash
                                                        )
                                                        .await(timeout)
                                                    if freshOracle.input == newInput then
                                                        oracleReady = true
                                                } catch { case _: Exception => }
                                        }
                                        if !walletReady then
                                            Console.logWarn(
                                              "Wallet UTxOs not indexed after 30s, continuing anyway"
                                            )
                                        if !oracleReady then
                                            Console.logWarn(
                                              "New oracle UTxO not indexed after 30s, continuing anyway"
                                            )
                                    case Left(_) =>
                                        Console.logWarn(
                                          "UTxO confirmed but not found, re-reading state..."
                                        )
                                        refreshOracleState()
                                }
                            } else {
                                // Pending after the polling window. The tx may still land —
                                // wait for the script-address oracle UTxO to change before
                                // assuming it failed.
                                Console.logWarn(
                                  "Tx not confirmed after 60s — waiting for oracle UTxO to change..."
                                )
                                waitForOracleUtxoChange(currentOracleUtxo.input) match {
                                    case Some(newUtxo) =>
                                        Console.logSuccess(
                                          "Late confirmation detected: oracle UTxO advanced"
                                        )
                                        adoptOracleUtxo(newUtxo, "late confirmation")
                                    case None =>
                                        Console.logWarn(
                                          "Oracle UTxO unchanged after wait — assuming tx was dropped"
                                        )
                                        refreshOracleState()
                                }
                            }

                        case Left(err) =>
                            Console.logError(s"Tx failed: $err")
                            // "All inputs are spent" / "BadInputsUTxO" mean a previous tx of
                            // ours was actually included. Wait for the new oracle UTxO before
                            // doing anything else, otherwise we will keep rebuilding against
                            // the now-spent cached input.
                            val inputsSpent =
                                err.contains("All inputs are spent") ||
                                    err.contains("BadInputsUTxO")
                            if inputsSpent then {
                                Console.logWarn(
                                  "Inputs already spent — waiting for new oracle UTxO..."
                                )
                                waitForOracleUtxoChange(currentOracleUtxo.input) match {
                                    case Some(newUtxo) =>
                                        adoptOracleUtxo(newUtxo, "post-spent recovery")
                                    case None =>
                                        Console.logWarn(
                                          "No new oracle UTxO observed; falling back to refresh"
                                        )
                                        refreshOracleState()
                                }
                            } else refreshOracleState()
                    }
                }
            } catch {
                case e: CommandHelpers.DeepReorgException =>
                    Console.logError(e.getMessage)
                    break(1)
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
