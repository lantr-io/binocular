package binocular.cli.commands

import binocular.*
import binocular.bitcoin.*
import binocular.oracle.*
import binocular.watchtower.*
import binocular.cli.{Command, CommandHelpers, Console}
import binocular.notify.Notifier
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.node.BlockchainProvider
import scalus.cardano.onchain.plutus.prelude.List as ScalusList
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.util.Try
import scala.util.boundary
import boundary.break
import scalus.utils.await
import cats.syntax.either.*

/** Confirm posted Treasury Movement (TM) transactions on Cardano — the validated Confirm transition
  * guarded on-chain by [[TreasuryMovementValidator]] (spec §Confirm TM tx, rev 5.4).
  *
  * Polls the TM validator address for `Unconfirmed` UTxOs (datum
  * `Constr(0, [signed_btc_tx, creator, created, fulfilled_por_outpoints])`, as posted by heimdall's
  * `publish.rs`). For each, once the TM is confirmed on Bitcoin and the block is in the Binocular
  * oracle's confirmed-blocks root, it builds the inclusion proof and submits the Confirm tx: spend
  * the `Unconfirmed` UTxO and the bridge-state singleton, reference the oracle and the Config, burn
  * the TM NFT ([CTM-24]), produce no output at the TM address ([CTM-25]), and recreate the
  * singleton carrying the [[BridgeState]] the TM's single `"BTMR1"` commitment output attests
  * ([CTM-27]): both roots, the new head `txid ‖ 00000000` ([CTM-19]) and its satoshi amount
  * ([CTM-21]). This command generates no MPF proofs and keeps no trie state of its own — both roots
  * are read straight out of the signed Bitcoin bytes ([[SweptPegInsTrie.committedRoots]], the same
  * exactly-one rule the validator applies).
  *
  * A TM without exactly one commitment output can never confirm, so it is reported and skipped
  * permanently rather than retried. A TM whose input 0 does not spend the singleton's CURRENT head
  * can never confirm either ([CTM-18]) — the head moved, so the record is dead weight until its
  * creator garbage-collects it.
  */
case class ConfirmTmtxCommand(dryRun: Boolean = false, notifier: Option[Notifier] = None)
    extends Command {

    override def execute(config: BinocularConfig): Int = {
        Console.header("Binocular TM Confirm (validated)")
        if dryRun then Console.warn("Dry-run mode — will check once and not submit")
        println()
        runConfirm(config, notifier.getOrElse(Notifier.fromConfig(config.notifications)))
    }

    private def runConfirm(config: BinocularConfig, notifier: Notifier): Int = boundary {
        given ec: ExecutionContext = binocular.cli.DaemonExecution.ec
        val pollInterval = config.relay.pollInterval
        val retryInterval = config.relay.retryInterval
        val timeout = config.oracle.transactionTimeout.seconds

        val setup = CommandHelpers.setupOracle(config).valueOr { err =>
            Console.error(err); break(1)
        }
        val provider: BlockchainProvider = setup.provider
        val network = setup.network
        val hdAccount = setup.hdAccount
        val oraclePolicyId = setup.script.scriptHash
        val oracleScriptHashBS = ByteString.fromArray(oraclePolicyId.bytes)

        // The TM UTxO lives at the validator address (parameterized by the oracle script hash + the
        // config NFT pair).
        val tmScript = TreasuryMovementContract.script(
          oracleScriptHashBS,
          ByteString.fromHex(config.bridge.configNftPolicyId),
          ByteString.fromHex(config.bridge.configNftAssetName)
        )
        val tmAddress = Address(network, Credential.ScriptHash(tmScript.scriptHash))
        // The TM NFT: policy = the validator's own script hash, empty asset name (minted by the
        // validator's mint branch). Only UTxOs carrying it are genuine TM UTxOs.
        val tmNftPolicy = tmScript.scriptHash
        val tmNftAsset = AssetName.empty

        // Diagnostic (TM_DEBUG_TRACE=1): a trace-compiled twin of the TM validator, registered under
        // the deployed TM hash during Confirm-tx build so a failing eval reports WHICH `require`
        // fails — the release compile strips trace strings, leaving only "Error evaluated". Costs a
        // one-off in-code compile; only built when the env var is set. Needs a non-dry-run to fire.
        val debugTmScript: Option[scalus.cardano.ledger.Script.PlutusV3] =
            if sys.env.get("TM_DEBUG_TRACE").exists(v => v == "1" || v.equalsIgnoreCase("true"))
            then
                Console.warn(
                  "TM_DEBUG_TRACE set — registering trace-compiled TM twin for diagnostic replay"
                )
                Some(
                  TreasuryMovementDebugContract.script(
                    oracleScriptHashBS,
                    ByteString.fromHex(config.bridge.configNftPolicyId),
                    ByteString.fromHex(config.bridge.configNftAssetName)
                  )
                )
            else None

        // --- bridge-state singleton wiring (the scripts are fixed for the process's life) ---
        // The bridge_state validator takes (TM script hash, one-shot ref). Its hash must equal
        // Config field 3 (`bridge_state_policy`), which is checked against the live config each
        // cycle. Defaults to the blueprint vendored in binocular's own jar, so a Docker image or a
        // systemd unit with no sibling ft checkout still starts. `bridge.plutus-json` /
        // BIFROST_PLUTUS_JSON overrides it when the file exists (development).
        val (bridgeBlueprint, blueprintSource) =
            try BifrostBlueprint.resolve(config.bridge.plutusJson)
            catch {
                case e: Exception =>
                    Console.error(s"Loading bridge blueprint: ${e.getMessage}"); break(1)
            }
        val bssOneShot = config.bridge.bridgeStateOneShotRef
            .map(_.trim)
            .filter(_.nonEmpty)
            .flatMap(s =>
                s.split('#') match {
                    case Array(h, i) if h.length == 64 && i.toIntOption.isDefined =>
                        Some(TxOutRef(TxId(ByteString.fromHex(h)), BigInt(i.toInt)))
                    case _ => None
                }
            )
            .getOrElse {
                Console.error(
                  "bridge.bridge-state-one-shot-ref must be TX_HASH#INDEX (the one-shot that " +
                      "parameterizes the bridge_state validator). Confirm cannot build the " +
                      "singleton spend without it."
                )
                break(1)
            }
        val bssScript = BridgeStateContract(
          bridgeBlueprint,
          ByteString.fromArray(tmScript.scriptHash.bytes),
          bssOneShot
        ).script
        val bssAssetName = AssetName(BridgeStateContract.assetName)
        val bssAddress = Address(network, Credential.ScriptHash(bssScript.scriptHash))
        val configNftPolicy = ScriptHash.fromHex(config.bridge.configNftPolicyId)
        val configNftAsset = AssetName(ByteString.fromHex(config.bridge.configNftAssetName))

        // --- POR sweeper (rev 5.2): chain peg-out Complete after Confirm ---
        // Built once; its scripts are fixed for the life of the process, and it is checked against
        // the live Config on every sweep. A construction failure disables SWEEPING only — confirming
        // a TM must never depend on the cleanup path.
        val sweeper: Option[PorSweeper] =
            if !config.bridge.porSweeper then {
                Console.info("POR sweeper", "disabled (bridge.por-sweeper = false)")
                None
            } else
                BridgeSweepSetup.buildPorSweeper(
                  config,
                  provider,
                  hdAccount,
                  bridgeBlueprint,
                  configNftPolicy,
                  configNftAsset,
                  tmAddress,
                  network,
                  notifier,
                  dryRun,
                  timeout
                ) match {
                    case Right(s) => Some(s)
                    case Left(err) =>
                        Console.logWarn(s"POR sweeper disabled: $err")
                        None
                }
        // 0 so the first cycle always sweeps: that is what makes `--dry-run` a real preflight of the
        // completion path (script hashes, reward account, trie mirror) and not just of confirming.
        var lastSweepMs = 0L
        // The Config UTxO lives at the config policy's own script address (config.ak is both the
        // one-shot minting policy and the spend validator).
        val configAddress = Address(network, Credential.ScriptHash(configNftPolicy))

        /** Resolve the live Config + singleton pair via [[BridgeSweepSetup.loadSingletonContext]],
          * then check the config's `bridge_state_policy` equals the locally derived script hash —
          * confirm must SPEND the singleton, so a mismatch means a wrong one-shot ref or an
          * unapplied config Update. Re-read every cycle: each Confirm spends the singleton, so a
          * cached UTxO is stale after one submit.
          */
        def loadSingletonContext(): Either[String, BridgeSweepSetup.SingletonContext] =
            BridgeSweepSetup
                .loadSingletonContext(
                  provider,
                  configAddress,
                  configNftPolicy,
                  configNftAsset,
                  network,
                  timeout
                )
                .flatMap(ctx =>
                    Either.cond(
                      ctx.config.bridgeStatePolicy.toHex == bssScript.scriptHash.toHex,
                      ctx,
                      s"config publishes bridge_state policy " +
                          s"${ctx.config.bridgeStatePolicy.toHex}, but the bridge_state validator " +
                          s"derived from (TM hash, one-shot) hashes to " +
                          s"${bssScript.scriptHash.toHex}. Check bridge.bridge-state-one-shot-ref, " +
                          "or run the config Update that publishes the new policy."
                    )
                )

        // Operator-declared dead TMs (relay.skip-btc-txids): match on the display (big-endian) btc
        // txid, lower-cased so config casing doesn't matter.
        val skipBtcTxids: Set[String] = config.relay.skipBtcTxids.map(_.toLowerCase).toSet
        if skipBtcTxids.nonEmpty then Console.info("Skip btc txids", skipBtcTxids.mkString(", "))

        val rpc = new SimpleBitcoinRpc(config.bitcoinNode)
        try {
            val info = rpc.getBlockchainInfo().await(30.seconds)
            Console.info("Bitcoin", s"${config.bitcoinNode.url} (${info.chain})")
        } catch {
            case e: Exception => Console.error(s"Bitcoin RPC: ${e.getMessage}"); break(1)
        }
        Console.info("Cardano", config.cardano.network)
        Console.info("Oracle policy", oraclePolicyId.toHex)
        Console.info("TM validator", tmScript.scriptHash.toHex)
        Console.info("TM address", tmAddress.encode.getOrElse("?"))
        Console.info("bridge blueprint", blueprintSource)
        Console.info("bridge-state policy", bssScript.scriptHash.toHex)
        Console.separator()
        println()

        // utxoRef -> Cardano tx hash (or "dry-run"); avoids reprocessing within a run.
        val processed = scala.collection.mutable.Map[String, String]()

        while true do {
            try {
                // Re-read the oracle each cycle: its confirmed-blocks root advances as Bitcoin does.
                val oracleUtxo =
                    CommandHelpers.findOracleUtxo(provider, oraclePolicyId).await(timeout)
                val chainState = CommandHelpers
                    .parseChainState(oracleUtxo)
                    .getOrElse {
                        Console.logWarn("Oracle UTxO has no valid ChainState");
                        throw new RuntimeException("no chainstate")
                    }
                val obMpf = CommandHelpers
                    .reconstructMpf(rpc, chainState, config.oracle.startHeight)
                    .valueOr { err =>
                        Console.logWarn(s"Rebuilding confirmed-blocks MPF: $err");
                        throw new RuntimeException(err)
                    }

                provider.findUtxos(tmAddress).await(timeout) match {
                    case Left(err) => Console.logWarn(s"UTxO query: $err")
                    case Right(utxos) =>
                        val unconfirmed = utxos.toList
                            .collect { case (in, out) =>
                                out.inlineDatum match
                                    case Some(d @ Data.Constr(0, args))
                                        if args.nonEmpty && out.value.hasAsset(
                                          tmNftPolicy,
                                          tmNftAsset
                                        ) =>
                                        Try(d.to[UnconfirmedTm]).toOption
                                            .map(u => (Utxo(in, out), u))
                                    case _ => None
                            }
                            .flatten
                            .filterNot { case (u, _) =>
                                processed.contains(
                                  s"${u.input.transactionId.toHex}#${u.input.index}"
                                )
                            }

                        if unconfirmed.isEmpty then
                            Console.logInPlace(
                              s"Polling... ${utxos.size} UTxO(s) at TM address, ${processed.size} processed"
                            )
                        else
                            Console.log(s"Found ${unconfirmed.size} Unconfirmed TM UTxO(s)")
                            loadSingletonContext() match {
                                case Left(err) =>
                                    // This halts confirming for EVERY TM, indefinitely, and every
                                    // cause needs an operator (bootstrap the singleton, publish the
                                    // policy in Config field 3, fix the one-shot ref). So it is
                                    // paged, not only logged. The notifier debounces repeats, so a
                                    // stuck watchtower does not spam.
                                    Console.logError(
                                      s"  bridge-state singleton unavailable: $err — will retry"
                                    )
                                    notifier.error(
                                      "confirm",
                                      s"bridge-state singleton unavailable — TM confirming is " +
                                          s"HALTED until this is fixed: $err"
                                    )
                                    // --dry-run is a preflight check, so a broken singleton context
                                    // must be a non-zero exit: it is exactly the state that says
                                    // the bridge-state migration has not been applied yet.
                                    if dryRun then break(1)
                                case Right(ctx) =>
                                    // The singleton is a single shared input, so at most ONE TM can
                                    // be confirmed per tx. After a submitted confirm the singleton
                                    // is spent and every remaining context is stale, so stop and
                                    // let the next poll re-read the chain.
                                    //
                                    // --dry-run submits nothing, so it previews EVERY pending TM
                                    // against the same singleton. At most one of them chains from
                                    // the current head ([CTM-18]), so the others report that.
                                    var singletonSpent = false
                                    for
                                        (utxo, unconfirmedDatum) <- unconfirmed
                                        if !singletonSpent
                                    do
                                        val submitted = confirmOne(
                                          provider,
                                          hdAccount,
                                          tmScript,
                                          oracleUtxo,
                                          obMpf,
                                          rpc,
                                          utxo,
                                          unconfirmedDatum,
                                          ctx,
                                          bssScript,
                                          timeout,
                                          skipBtcTxids,
                                          processed,
                                          notifier,
                                          debugTmScript,
                                          sweeper
                                        )
                                        if submitted then singletonSpent = true
                            }
                }

                // --- chain peg-out Complete after Confirm ---
                // Runs OUTSIDE the confirm branch, and re-reads its context, because the Confirm
                // just submitted spent the singleton: the sweeper references the RECREATED
                // singleton, which is only visible once that transaction settles. Until then the
                // mirror already matches the on-chain root and the sweep is a cheap no-op.
                sweeper.foreach { s =>
                    val idleDue =
                        System.currentTimeMillis() - lastSweepMs >=
                            config.bridge.porSweepIntervalSeconds * 1000L
                    if (s.hasPending || idleDue) && !s.isHalted then {
                        lastSweepMs = System.currentTimeMillis()
                        // Contained: peg-out cleanup must never cost a confirm cycle. Without this
                        // guard a sweeper throw would land in the outer handler, page as a confirm
                        // error, and sleep the retry interval before the next TM is even looked at.
                        try
                            loadSingletonContext() match {
                                case Right(ctx) => s.sweep(ctx.configUtxo, ctx.singletonUtxo)
                                case Left(err) =>
                                    Console.logWarn(
                                      s"  sweeper: singleton context unavailable: $err"
                                    )
                            }
                        catch {
                            case e: Exception =>
                                Console.logError(s"  sweeper: sweep failed: ${e.getMessage}")
                        }
                    }
                }

                if dryRun then break(0)
                Thread.sleep(pollInterval * 1000L)
            } catch {
                case e: boundary.Break[?] =>
                    // Control-flow escape (the dry-run `break(0)` above), not an operational error:
                    // `boundary.break` throws a `Break` that extends RuntimeException, so without
                    // this guard the generic handler swallows it, logs a spurious "Error: null",
                    // and exits via `break(1)`. Re-throw so `--dry-run` unwinds cleanly.
                    throw e
                case e: Exception =>
                    Console.logError(s"Error: ${e.getMessage} — retrying in ${retryInterval}s")
                    notifier.error("confirm", s"Error: ${e.getMessage}")
                    if dryRun then break(1)
                    Thread.sleep(retryInterval * 1000L)
            }
        }
        0
    }

    /** Build + submit the Confirm tx for one Unconfirmed UTxO (or report why it's not ready).
      *
      * Returns `true` only when a Confirm tx was actually submitted, which tells the caller the
      * shared singleton UTxO is now spent and no further TM may be confirmed this cycle.
      */
    private def confirmOne(
        provider: BlockchainProvider,
        hdAccount: scalus.cardano.wallet.hd.HdAccount,
        tmScript: scalus.cardano.ledger.Script.PlutusV3,
        oracleUtxo: Utxo,
        obMpf: scalus.crypto.trie.MerklePatriciaForestry,
        rpc: SimpleBitcoinRpc,
        utxo: Utxo,
        unconfirmedDatum: UnconfirmedTm,
        ctx: BridgeSweepSetup.SingletonContext,
        bssScript: scalus.cardano.ledger.Script.PlutusV3,
        timeout: Duration,
        skipBtcTxids: Set[String],
        processed: scala.collection.mutable.Map[String, String],
        notifier: Notifier,
        debugTmScript: Option[scalus.cardano.ledger.Script.PlutusV3],
        sweeper: Option[PorSweeper]
    )(using ExecutionContext): Boolean = {
        val signedBtcTx = unconfirmedDatum.signedBtcTx
        val utxoRef = s"${utxo.input.transactionId.toHex}#${utxo.input.index}"
        // Parse the (attacker-placeable) datum bytes defensively: getTxHash/allInputOutpoints/
        // allOutputs recurse on a tx-declared count, so a crafted UTxO at the TM address could
        // StackOverflow/OOM. A parse failure is deterministic → mark the UTxO skipped so it neither
        // crashes the watchtower nor is retried forever. RPC errors stay outside this guard (the
        // outer loop retries those).
        val parsed: Option[(ByteString, ScalusList[ByteString], ScalusList[PegOutEntry])] =
            try
                Some(
                  (
                    BitcoinHelpers.getTxHash(signedBtcTx), // internal (LE) — the new head's txid
                    TreasuryMovementValidator.allInputOutpoints(signedBtcTx),
                    TreasuryMovementValidator.allOutputs(signedBtcTx)
                  )
                )
            catch {
                case t: Throwable =>
                    Console.logError(
                      s"  $utxoRef: malformed/poison TM bytes — skipping (${t.getClass.getSimpleName})"
                    )
                    processed(utxoRef) = "skip:malformed"
                    None
            }

        var submitted = false
        parsed.foreach { case (txid, swept, fulfilled) =>
            val displayTxid = txid.reverse.toHex
            Console.log(s"  $utxoRef: TM btc txid=$displayTxid")

            if skipBtcTxids.contains(displayTxid.toLowerCase) then
                Console.logWarn(s"    $utxoRef: skipped (relay.skip-btc-txids)")
                processed(utxoRef) = "skip:config"
            else {
                // Both roots THIS TM attests: the payload of its single "BTMR1" OP_RETURN output.
                // Zero or several such outputs mean the validator will reject the TM whatever we
                // build ([CTM-26]), so it can never confirm.
                //
                // Read BEFORE the Bitcoin proof: it is deterministic and free, so a TM that can
                // never confirm is reported and skipped without a node round-trip. The match below
                // is NESTED for exactly that reason — a single match on a `(rootsResult,
                // proofResult)` tuple would build the tuple first and defeat the ordering.
                val rootsResult = SweptPegInsTrie.committedRoots(fulfilled.asScala.toSeq)

                // [CTM-18] preflight: the TM must spend the singleton's CURRENT head as its
                // input 0. A TM chaining from any other outpoint can never confirm against this
                // singleton state. NOT marked processed: after the head advances past it the
                // record stays dead, but until this cycle's context settles a competing view is
                // possible, so it is re-checked (cheaply) next poll.
                val chainsFromHead = swept.asScala.headOption.contains(ctx.state.treasuryUtxoId)

                // Proof construction fetches the TM's signed BTC tx from the node. If the node
                // doesn't know the txid (bitcoind -5), the TM is in neither the mempool nor the
                // chain (txindex=1): either the relay hasn't broadcast it yet (a confirm/relay
                // race — MUST retry, the tx will mine shortly), or an input was already spent by
                // a competing confirmed tx so it can never mine (permanently dead — skip, or it
                // would be retried forever). Only on-chain evidence of a spent input marks it
                // dead; transport errors and failed liveness checks always stay retryable.
                // Catch per-UTxO so one bad TM never aborts the whole confirm batch.
                def proofResult =
                    try TmProofBundle.produce(rpc, obMpf, displayTxid).await(timeout)
                    catch {
                        case t: Throwable if TmLiveness.isTxUnknown(t) =>
                            TmLiveness.firstDeadInput(rpc, swept.asScala.toSeq, timeout) match {
                                case Some(outpoint) =>
                                    processed(utxoRef) = "skip:input-spent"
                                    Left(
                                      s"BTC tx $displayTxid can never be mined (input $outpoint already spent) — skipping permanently"
                                    )
                                case None =>
                                    Left(
                                      s"BTC tx $displayTxid not on this node yet (awaiting relay broadcast/mining) — will retry"
                                    )
                            }
                        case t: Throwable =>
                            Left(
                              s"BTC tx $displayTxid lookup failed (${t.getMessage}) — will retry"
                            )
                    }
                rootsResult match {
                    case Left(err) =>
                        Console.logError(s"    $utxoRef: unconfirmable TM — $err")
                        processed(utxoRef) = "skip:root-commitment"
                    case Right((spiRoot, cpoRoot)) if !chainsFromHead =>
                        Console.log(
                          s"    not chained from the current head " +
                              s"${ctx.state.treasuryUtxoId.toHex} — cannot confirm ([CTM-18])"
                        )
                    case Right((spiRoot, cpoRoot)) =>
                        proofResult match {
                            case Left(err) =>
                                Console.log(s"    not ready: $err")
                            case Right(tm) =>
                                val redeemer: Data = (TmSpendRedeemer.Confirm(
                                  TmConfirmProof(
                                    txIndex = BigInt(tm.txIndex),
                                    txMerkleProof = ScalusList.from(tm.txInBlockMerklePath.toList),
                                    blockMpfProof = tm.mpfHeaderInclusionProof,
                                    blockHeader = binocular.oracle.BlockHeader(tm.blockHeader)
                                  )
                                ): TmSpendRedeemer).toData
                                // The state the validator will rebuild and pin ([CTM-27]):
                                // both attested roots, head = txid ‖ 00000000 ([CTM-19]),
                                // amount = the TM's output 0 ([CTM-21]).
                                val newState = BridgeState(
                                  spiRoot = spiRoot,
                                  cpoRoot = cpoRoot,
                                  treasuryUtxoId = txid ++ ByteString.fromHex("00000000"),
                                  treasuryAmount = fulfilled.head.amount
                                )
                                val singletonSpend = TreasuryMovementTx.SingletonSpend(
                                  utxo = ctx.singletonUtxo,
                                  script = bssScript,
                                  newDatum = newState.toData
                                )

                                Console.log(
                                  s"    singleton: spi ${ctx.state.spiRoot.toHex} -> ${spiRoot.toHex}"
                                )
                                Console.log(
                                  s"    singleton: cpo ${ctx.state.cpoRoot.toHex} -> ${cpoRoot.toHex}"
                                )
                                Console.log(
                                  s"    singleton: head ${ctx.state.treasuryUtxoId.toHex} -> " +
                                      s"${newState.treasuryUtxoId.toHex} (${newState.treasuryAmount} sat)"
                                )

                                if dryRun then {
                                    Console.logSuccess(
                                      s"    [dry-run] would confirm  spent=$utxoRef"
                                    )
                                    confirmSummaryLines(
                                      displayTxid,
                                      tm,
                                      swept,
                                      fulfilled,
                                      cardanoTx = None
                                    )
                                        .foreach(l => Console.log(s"    $l"))
                                    processed(utxoRef) = "dry-run"
                                } else
                                    TreasuryMovementTx.buildAndSubmitConfirm(
                                      provider,
                                      hdAccount,
                                      tmScript,
                                      utxo,
                                      oracleUtxo,
                                      ctx.configUtxo,
                                      singletonSpend,
                                      redeemer,
                                      timeout,
                                      debugTmScript
                                    ) match {
                                        case Right(hash) =>
                                            submitted = true
                                            Console.logSuccess(s"    TM confirmed  spent=$utxoRef")
                                            confirmSummaryLines(
                                              displayTxid,
                                              tm,
                                              swept,
                                              fulfilled,
                                              cardanoTx = Some(hash)
                                            ).foreach(l => Console.log(s"    $l"))
                                            processed(utxoRef) = hash
                                            notifier.success(
                                              "confirm",
                                              s"TM confirmed on Cardano — btc txid `$displayTxid`, " +
                                                  s"cardano tx `$hash` ($utxoRef)"
                                            )
                                            // Hand the sweeper this TM's data-availability hint
                                            // NOW, while the requests it names are still unspent:
                                            // that is the only moment their datums are readable
                                            // from current state rather than from history.
                                            sweeper.foreach(
                                              _.recordConfirmed(
                                                displayTxid,
                                                cpoRoot,
                                                unconfirmedDatum.fulfilledPorOutpoints.asScala.toSeq
                                              )
                                            )
                                        case Left(err) =>
                                            // The singleton may or may not have been consumed: a
                                            // build or submit failure never spends it, and a
                                            // submitted-then-rejected tx does not either. Treat it
                                            // as unspent and let the next cycle re-read the chain.
                                            Console.logError(
                                              s"    Confirm failed: $err — will retry"
                                            )
                                    }
                        }
                }
            }
        }
        submitted
    }

    /** Max per-entry `pegin=`/`pegout=` lines to emit; a bigger sweep gets a `*_omitted=N` line
      * instead (never a silent cap).
      */
    private val MaxEntryLines = 20

    /** Render a Bitcoin outpoint (36 bytes = 32-byte txid in internal/LE order + 4-byte LE vout) as
      * the human `displaytxid:vout` form.
      */
    private def outpointDisplay(op: ByteString): String = {
        val (txid, vout) = TmLiveness.parseOutpoint(op)
        s"$txid:$vout"
    }

    /** Grep-able detail lines for a confirmed (or would-confirm) TM: every line carries the
      * `confirm-tm` anchor and `key=value` fields, so `journalctl | grep confirm-tm` yields the
      * whole event and individual facts are machine-extractable. Peg-ins are listed before
      * peg-outs. `cardanoTx` is `None` for the dry-run preview (no submitted tx yet).
      *
      * The recreated singleton is the Confirm tx's first output (`payTo(singleton…)` in
      * [[TreasuryMovementTx.buildAndSubmitConfirm]]), hence `new_state=<hash>#0`.
      */
    private def confirmSummaryLines(
        displayTxid: String,
        tm: TmProofBundle,
        swept: ScalusList[ByteString],
        fulfilled: ScalusList[PegOutEntry],
        cardanoTx: Option[String]
    ): Seq[String] = {
        val pegins = swept.asScala.toIndexedSeq
        val pegouts = fulfilled.asScala.toIndexedSeq
        val satTotal = pegouts.map(_.amount).sum
        val lines = scala.collection.mutable.ListBuffer.empty[String]
        cardanoTx.foreach(h => lines += s"confirm-tm cardano_tx=$h  new_state=$h#0")
        lines += s"confirm-tm btc_tx=$displayTxid  block=${tm.blockHeight}  index=${tm.txIndex}"
        lines += s"confirm-tm pegins=${pegins.size}"
        pegins.take(MaxEntryLines).zipWithIndex.foreach { case (op, i) =>
            lines += s"confirm-tm pegin=$i  outpoint=${outpointDisplay(op)}"
        }
        if pegins.size > MaxEntryLines then
            lines += s"confirm-tm pegins_omitted=${pegins.size - MaxEntryLines}"
        lines += s"confirm-tm pegouts=${pegouts.size}  sat_total=$satTotal"
        pegouts.take(MaxEntryLines).zipWithIndex.foreach { case (e, j) =>
            lines += s"confirm-tm pegout=$j  sat=${e.amount}  spk=${e.scriptPubKey.toHex}"
        }
        if pegouts.size > MaxEntryLines then
            lines += s"confirm-tm pegouts_omitted=${pegouts.size - MaxEntryLines}"
        lines.toList
    }
}
