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
import scala.util.boundary
import boundary.break
import scalus.utils.await
import cats.syntax.either.*

/** Confirm posted Treasury Movement (TM) transactions on Cardano — the validated `Unconfirmed ->
  * Confirmed` transition guarded on-chain by [[TreasuryMovementValidator]].
  *
  * Polls the TM validator address for `Unconfirmed` UTxOs (datum
  * `Constr(0, [signed_btc_tx, creator, created, epoch, leader_reward, fulfilled_por_outpoints])`,
  * as posted by heimdall's `publish.rs` or `create-tmtx`). For each, once the TM is confirmed on
  * Bitcoin and the block is in the Binocular oracle's confirmed-blocks root, it builds the
  * inclusion proof and submits the Confirm tx: spend the `Unconfirmed` UTxO, reference the oracle,
  * and recreate it with the `Confirmed` datum
  * `{ btc_txid, swept_peg_in_utxo_ids, fulfilled_peg_outs }` that the validator re-parses and
  * verifies on-chain.
  *
  * Unlike the old always-ok scaffold, the datum flip is now only accepted if the Bitcoin
  * confirmation is *proven* against the oracle.
  *
  * The Confirm tx ALSO carries the completed-peg-outs trie update: it references the Config UTxO
  * (the validator reads the trie policy from field 3), spends the trie UTxO, and recreates it with
  * the root the TM's single `"CPOR1"` OP_RETURN output attests. That root is read straight out of
  * the signed Bitcoin bytes ([[CompletedPegOutsTrie.committedRoot]], the same exactly-one rule the
  * validator applies) — this command generates no MPF proofs and keeps no trie state of its own.
  *
  * A TM without exactly one commitment output can never confirm, so it is reported and skipped
  * permanently rather than retried. Every other well-formed TM confirms.
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
        // TM-control NFT that authenticates the authorized-minter datum).
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

        // --- completed-peg-outs trie wiring (rebuilt per cycle; the scripts are fixed) ---
        // The trie validator takes (TM script hash, one-shot ref). Its hash must equal Config
        // field 3, which is checked against the live config each cycle.
        // Defaults to the blueprint vendored in binocular's own jar, so a Docker image or a
        // systemd unit with no sibling ft checkout still starts. `bridge.plutus-json` /
        // BIFROST_PLUTUS_JSON overrides it when the file exists (development).
        val (bridgeBlueprint, blueprintSource) =
            try BifrostBlueprint.resolve(config.bridge.plutusJson)
            catch {
                case e: Exception =>
                    Console.error(s"Loading bridge blueprint: ${e.getMessage}"); break(1)
            }
        val cpoOneShot = config.bridge.completedPegOutsOneShotRef
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
                  "bridge.completed-peg-outs-one-shot-ref must be TX_HASH#INDEX (the deploy-bridge " +
                      "one-shot that parameterizes the completed-peg-outs trie validator). Confirm " +
                      "cannot build the trie spend without it."
                )
                break(1)
            }
        val trieScript = CompletedPegOutsContract(
          bridgeBlueprint,
          ByteString.fromArray(tmScript.scriptHash.bytes),
          cpoOneShot
        ).script
        val trieAssetName = AssetName(CompletedPegOutsContract.assetName)
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
        Console.info("completed-peg-outs policy", trieScript.scriptHash.toHex)
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
                                        // Full typed decode (signedBtcTx, creator, created): the
                                        // creator/created fields must be carried verbatim into the
                                        // Confirmed datum the validator expects.
                                        scala.util.Try(d.to[TmDatum]).toOption.collect {
                                            case u: TmDatum.Unconfirmed => (Utxo(in, out), u)
                                        }
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
                            BridgeSweepSetup.loadTrieContext(
                              provider,
                              configAddress,
                              configNftPolicy,
                              configNftAsset,
                              trieScript,
                              trieAssetName,
                              network,
                              timeout
                            ) match {
                                case Left(err) =>
                                    // This halts confirming for EVERY TM, indefinitely, and every
                                    // cause needs an operator (run the migration Update, bootstrap
                                    // the trie singleton, fix the one-shot ref). So it is paged, not
                                    // only logged. The notifier debounces repeats, so a stuck
                                    // watchtower does not spam.
                                    Console.logError(
                                      s"  completed-peg-outs trie unavailable: $err — will retry"
                                    )
                                    notifier.error(
                                      "confirm",
                                      s"completed-peg-outs trie unavailable — TM confirming is " +
                                          s"HALTED until this is fixed: $err"
                                    )
                                    // --dry-run is a preflight check, so a broken trie context must
                                    // be a non-zero exit: it is exactly the state that says the
                                    // field-3 migration has not been applied yet.
                                    if dryRun then break(1)
                                case Right(trieCtx) =>
                                    // The trie UTxO is a single shared input, so at most ONE TM can
                                    // be confirmed per tx. After a submitted confirm the trie UTxO
                                    // is spent and every remaining context is stale, so stop and let
                                    // the next poll re-read the chain.
                                    //
                                    // --dry-run submits nothing, so it previews EVERY pending TM
                                    // against the same trie UTxO. Each TM's committed root is fixed
                                    // in its own signed bytes, so the previews are independent; only
                                    // the "from" root in the log line is shared.
                                    var trieSpent = false
                                    for
                                        (utxo, unconfirmedDatum) <- unconfirmed
                                        if !trieSpent
                                    do
                                        val submitted = confirmOne(
                                          provider,
                                          hdAccount,
                                          tmScript,
                                          tmAddress,
                                          oracleUtxo,
                                          obMpf,
                                          rpc,
                                          utxo,
                                          unconfirmedDatum,
                                          trieCtx,
                                          timeout,
                                          skipBtcTxids,
                                          processed,
                                          notifier,
                                          debugTmScript,
                                          sweeper
                                        )
                                        if submitted then trieSpent = true
                            }
                }

                // --- chain peg-out Complete after Confirm ---
                // Runs OUTSIDE the confirm branch, and re-reads the trie context, because the
                // Confirm just submitted spent the trie UTxO: the sweeper references the RECREATED
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
                            BridgeSweepSetup.loadTrieContext(
                              provider,
                              configAddress,
                              configNftPolicy,
                              configNftAsset,
                              trieScript,
                              trieAssetName,
                              network,
                              timeout
                            ) match {
                                case Right(ctx) => s.sweep(ctx.configUtxo, ctx.trieUtxo)
                                case Left(err) =>
                                    Console.logWarn(s"  sweeper: trie context unavailable: $err")
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
      * shared trie UTxO is now spent and no further TM may be confirmed this cycle.
      */
    private def confirmOne(
        provider: BlockchainProvider,
        hdAccount: scalus.cardano.wallet.hd.HdAccount,
        tmScript: scalus.cardano.ledger.Script.PlutusV3,
        tmAddress: Address,
        oracleUtxo: Utxo,
        obMpf: scalus.crypto.trie.MerklePatriciaForestry,
        rpc: SimpleBitcoinRpc,
        utxo: Utxo,
        unconfirmedDatum: TmDatum.Unconfirmed,
        trieCtx: BridgeSweepSetup.TrieContext,
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
        val parsed: Option[(ByteString, ScalusList[ByteString], ScalusList[PegOutEntry], Boolean)] =
            try
                Some(
                  (
                    BitcoinHelpers.getTxHash(signedBtcTx), // internal (LE) — the Confirmed btc_txid
                    TreasuryMovementValidator.allInputOutpoints(signedBtcTx),
                    TreasuryMovementValidator.allOutputs(signedBtcTx),
                    // N10b: treasury (input 0) swept via the federation CSV leaf? Coarse = a 3-item
                    // script-path witness on input 0 (the treasury tree is single-leaf, so that IS
                    // the federation leaf). MUST equal what the on-chain Confirm branch computes,
                    // else `exp === contOut.datum` fails. See TreasuryMovementValidator.TmDatum.
                    BitcoinHelpers.isValidScriptPathWitness(signedBtcTx, BigInt(0))
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
        parsed.foreach { case (txid, swept, fulfilled, spentViaFederationLeaf) =>
            val displayTxid = txid.reverse.toHex
            Console.log(s"  $utxoRef: TM btc txid=$displayTxid")
            if spentViaFederationLeaf then
                Console.logWarn(
                  s"    $utxoRef: treasury swept via the FEDERATION CSV leaf — " +
                      "Confirmed record will carry spent_via_federation_leaf=true (N10b reset evidence)"
                )

            if skipBtcTxids.contains(displayTxid.toLowerCase) then
                Console.logWarn(s"    $utxoRef: skipped (relay.skip-btc-txids)")
                processed(utxoRef) = "skip:config"
            else {
                // The completed-peg-outs root THIS TM attests: the payload of its single "CPOR1"
                // OP_RETURN output. Zero or several such outputs mean the validator will reject the
                // TM whatever we build, so it can never confirm.
                //
                // Read BEFORE the Bitcoin proof: it is deterministic and free, so a TM that can
                // never confirm is reported and skipped without a node round-trip. The match below
                // is NESTED for exactly that reason — a single match on a `(rootResult, proofResult)`
                // tuple would build the tuple first and defeat the ordering.
                val rootResult = CompletedPegOutsTrie.committedRoot(fulfilled.asScala.toSeq)

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
                rootResult match {
                    case Left(err) =>
                        Console.logError(s"    $utxoRef: unconfirmable TM — $err")
                        processed(utxoRef) = "skip:root-commitment"
                    case Right(newRoot) =>
                        proofResult match {
                            case Left(err) =>
                                Console.log(s"    not ready: $err")
                            case Right(tm) =>
                                val redeemer: Data = TmConfirmRedeemer(
                                  txIndex = BigInt(tm.txIndex),
                                  txMerkleProof = ScalusList.from(tm.txInBlockMerklePath.toList),
                                  blockMpfProof = tm.mpfHeaderInclusionProof,
                                  blockHeader = binocular.oracle.BlockHeader(tm.blockHeader)
                                ).toData
                                val trieSpend = TreasuryMovementTx.TrieSpend(
                                  utxo = trieCtx.trieUtxo,
                                  script = trieCtx.trieScript,
                                  newDatum = CompletedPegOutsTrieDatum(newRoot).toData
                                )
                                val confirmed: Data =
                                    (TmDatum.Confirmed(
                                      txid,
                                      swept,
                                      fulfilled,
                                      spentViaFederationLeaf,
                                      unconfirmedDatum.creator,
                                      unconfirmedDatum.created,
                                      unconfirmedDatum.epoch,
                                      unconfirmedDatum.leaderReward
                                    ): TmDatum).toData

                                Console.log(
                                  s"    trie: attested root " +
                                      s"${trieCtx.currentRoot.toHex} -> ${newRoot.toHex}"
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
                                      tmAddress,
                                      utxo,
                                      oracleUtxo,
                                      trieCtx.configUtxo,
                                      trieSpend,
                                      redeemer,
                                      confirmed,
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
                                                newRoot,
                                                unconfirmedDatum.fulfilledPorOutpoints.asScala.toSeq
                                              )
                                            )
                                        case Left(err) =>
                                            // The trie UTxO may or may not have been consumed: a build or
                                            // submit failure never spends it, and a submitted-then-rejected
                                            // tx does not either. Treat it as unspent and let the next
                                            // cycle re-read the chain.
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
      * The recreated Confirmed-TM UTxO is the Confirm tx's first output (`payTo(tmAddress, …)` in
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
