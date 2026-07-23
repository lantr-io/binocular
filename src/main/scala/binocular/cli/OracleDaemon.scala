package binocular.cli

import binocular.*
import binocular.bitcoin.*
import binocular.notify.Notifier
import binocular.oracle.*
import binocular.oracle.ForkTreePretty.*
import org.bouncycastle.crypto.digests.Blake2bDigest
import scalus.uplc.builtin.ByteString
import scalus.cardano.ledger.{Bech32, TransactionHash, TransactionInput, Utxo}
import scalus.cardano.node.TransactionStatus
import scalus.cardano.onchain.plutus.prelude.List as ScalusList
import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF

import scala.concurrent.ExecutionContext
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

/** Oracle daemon loop. Calls `planner.planUpdate` each cycle to decide what to submit, then handles
  * submission, confirmation, adoption, and retry logic.
  *
  * `planner` controls the strategy (honest vs. rogue); all other logic is shared.
  */
class OracleDaemon(
    planner: UpdatePlanner,
    dryRun: Boolean,
    notifier: Notifier = binocular.notify.NoopNotifier
) {

    /** How long to wait for the deep-reorg alert to be delivered before letting the exception
      * propagate and the watchtower `System.exit`. Covers the Discord HTTP POST round-trip.
      */
    private val DeepReorgAlertFlushMs = 10_000L

    /** Big-endian (block-explorer) hex of a confirmed tip hash, which is stored little-endian. */
    private def displayHash(hash: ByteString): String =
        ByteString.fromArray(hash.bytes.reverse).toHex

    /** A compact, structured deep-reorg alert for the notifier.
      *
      * Leads with the reorg length (how many confirmed blocks were orphaned), then the fork point
      * and both tip hashes in big-endian (block-explorer) order so an operator can paste them
      * straight into a Bitcoin explorer.
      *
      * @param phase
      *   where it fired (e.g. "startup MPF reconstruction" / "update loop") for context
      */
    private def deepReorgAlert(
        e: CommandHelpers.DeepReorgException,
        phase: String,
        network: String
    ): String = {
        val (depthLabel, forkLine) = e.deepestConfirmedAncestor match {
            case Some((h, _)) =>
                val intoConfirmed = e.confirmedHeight - h
                // Full reorg depth = forkTipHeight - ancestor, of which `intoConfirmed` reaches into
                // confirmed history. Fall back to the into-confirmed count if the fork tip is absent.
                val label = e.forkTipHeight match {
                    case Some(tip)  => s"~${tip - h}-block reorg"
                    case scala.None => s"$intoConfirmed-block reorg"
                }
                (
                  label,
                  s"• Height ${e.confirmedHeight}, forked at $h — $intoConfirmed into confirmed history"
                )
            case None =>
                e.searchedDepth match {
                    case Some(n) =>
                        (
                          s"reorg ≥$n blocks",
                          s"• Height ${e.confirmedHeight}, no fork within $n blocks"
                        )
                    case None =>
                        (
                          "deep reorg",
                          s"• Height ${e.confirmedHeight}, detected before MPF rebuild"
                        )
                }
        }
        s"""UNRECOVERABLE $depthLabel — oracle tip off canonical chain ($phase)
           |$forkLine
           |• Oracle tip:    ${displayHash(e.oracleHash)}
           |• Canonical tip: ${displayHash(e.canonicalHash)}
           |• Network: $network
           |Action: watchtower stopped (no auto-restart). Resume with `set-state`, or re-init from a canonical height, then restart.""".stripMargin
    }

    def run(config: BinocularConfig): Int = boundary {
        given ec: ExecutionContext = binocular.cli.DaemonExecution.ec
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
                // Unrecoverable — let it propagate out of the daemon so the watchtower supervisor
                // stops the whole process (do-not-restart exit code) rather than restarting us into
                // the same deep reorg every retry interval.
                case e: CommandHelpers.DeepReorgException =>
                    Console.error(e.getMessage)
                    notifier.error(
                      "oracle",
                      deepReorgAlert(e, "startup MPF reconstruction", config.bitcoinNode.network)
                    )
                    // The watchtower exits (System.exit) right after this propagates; drain the
                    // async post first or the alert is killed mid-flight and never delivered.
                    notifier.flush(DeepReorgAlertFlushMs)
                    throw e
            }
        Console.success(s"MPF reconstructed: ${currentMpf.size} confirmed blocks")
        println(
          currentChainState.forkTree
              .pretty(currentChainState.ctx.height, confirmedBlocks = Some(currentMpf.size))
        )

        Console.separator()
        println()

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
            notifier.newBlock(
              state.forkTree.highestHeight(state.ctx.height),
              state.ctx.height,
              displayHash(state.ctx.lastBlockHash),
              TimeFmt.iso(state.ctx.timestamps.head),
              0,
              state.forkTree.blockCount.toInt,
              currentMpf.size
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
                        notifier.newBlock(
                          state.forkTree.highestHeight(state.ctx.height),
                          state.ctx.height,
                          displayHash(state.ctx.lastBlockHash),
                          TimeFmt.iso(state.ctx.timestamps.head),
                          0,
                          state.forkTree.blockCount.toInt,
                          currentMpf.size
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
                val (_, validityTime) =
                    OracleTransactions.computeValidityIntervalTime(setup.provider.cardanoInfo)

                val plan = planner.planUpdate(
                  rpc,
                  currentChainState,
                  currentMpf,
                  validityTime,
                  setup.params
                )
                val headersList = plan.headers
                val parentPath = plan.parentPath

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
                val stateChanged = headersList != ScalusList.Nil || totalPromotable > 0

                if !stateChanged then {
                    Console.logInPlace(plan.logContext)
                    Thread.sleep(pollInterval * 1000L)
                } else {
                    if dryRun then {
                        Console.success("Dry-run: computed update")
                        Console.info("Current Height", currentChainState.ctx.height)
                        Console.info("Promotable", totalPromotable)
                        Console.info("Headers", headersList.length)
                        Console.info(
                          "Fork Tree",
                          s"${currentChainState.forkTree.blockCount} blocks"
                        )
                        break(0)
                    }

                    val result = OracleTransactions.buildOptimalUpdateTransaction(
                      setup.provider,
                      setup.hdAccount,
                      setup.script,
                      currentOracleUtxo,
                      currentChainState,
                      headersList,
                      parentPath,
                      validityTime,
                      referenceScriptUtxo,
                      currentMpf,
                      setup.params,
                      timeout,
                      // Exclude EVERY reference-script UTxO at the sponsor address from fee selection —
                      // BlockfrostProvider drops their scriptRef, so the builder under-estimates the
                      // fee by the Conway ref-script surcharge (→ FeeTooSmallUTxO, which stalled the
                      // daemon at h135997). The scan catches every deployed ref UTxO by its
                      // reference_script_hash, including duplicates from earlier deploy-script-refs runs.
                      excludeInputs = binocular.cli.CommandHelpers.refScriptOutpoints(
                        config,
                        setup.sponsorAddress.encode.getOrElse("")
                      )
                    )

                    result match {
                        case Right((txResult, newChainState, updatedMpf, promotions)) =>
                            val appliedHeaders =
                                newChainState.forkTree.blockCount - currentChainState.forkTree.blockCount + promotions
                            Console.log(
                              s"Update: +$appliedHeaders/${headersList.length} headers, $promotions promoted | tree: ${newChainState.forkTree.blockCount} blocks"
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
                                        notifier.newBlock(
                                          newChainState.forkTree.highestHeight(
                                            newChainState.ctx.height
                                          ),
                                          newChainState.ctx.height,
                                          displayHash(newChainState.ctx.lastBlockHash),
                                          TimeFmt.iso(newChainState.ctx.timestamps.head),
                                          appliedHeaders.toInt,
                                          newChainState.forkTree.blockCount.toInt,
                                          updatedMpf.size
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
                            // "All inputs are spent" / "BadInputsUTxO" mean another submitter
                            // (typically another watchtower instance) spent the oracle input
                            // before our tx landed. This is expected in multi-instance
                            // deployments — adopt the new oracle UTxO and continue.
                            val inputsSpent =
                                err.contains("All inputs are spent") ||
                                    err.contains("BadInputsUTxO")
                            if inputsSpent then {
                                Console.log(
                                  "Another submitter advanced the oracle first — adopting their UTxO and continuing"
                                )
                                waitForOracleUtxoChange(currentOracleUtxo.input) match {
                                    case Some(newUtxo) =>
                                        adoptOracleUtxo(newUtxo, "lost submission race")
                                    case None =>
                                        Console.logWarn(
                                          "No new oracle UTxO observed; falling back to refresh"
                                        )
                                        refreshOracleState()
                                }
                            } else {
                                Console.logError(s"Tx failed: $err")
                                notifier.error("oracle", s"Tx failed: $err")
                                refreshOracleState()
                            }
                    }
                }
            } catch {
                // `break` (e.g. the dry-run break(0) above) is implemented as a control
                // exception; let it propagate to the `boundary` instead of being swallowed
                // by the generic `case e: Exception` below (which would log "Error: null"
                // and loop forever).
                case b: boundary.Break[?] =>
                    throw b
                // Unrecoverable — confirmed history orphaned; manual re-init required. Propagate
                // out of the daemon (rather than break(1)) so the watchtower supervisor stops the
                // whole process with a do-not-restart exit code, instead of the Supervisor
                // restarting us every retryInterval only to re-detect the same reorg (the 5s spam).
                case e: CommandHelpers.DeepReorgException =>
                    Console.logError(e.getMessage)
                    notifier.error(
                      "oracle",
                      deepReorgAlert(e, "update loop", config.bitcoinNode.network)
                    )
                    // The watchtower exits (System.exit) right after this propagates; drain the
                    // async post first or the alert is killed mid-flight and never delivered.
                    notifier.flush(DeepReorgAlertFlushMs)
                    throw e
                case e: Exception =>
                    Console.logError(
                      s"Error: ${e.getMessage} — retrying in ${retryInterval}s"
                    )
                    notifier.error("oracle", s"Error: ${e.getMessage}")
                    Thread.sleep(retryInterval * 1000L)
            }
        }
        0 // unreachable
    }
}
