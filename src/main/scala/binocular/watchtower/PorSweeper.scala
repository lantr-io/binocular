package binocular.watchtower

import binocular.cli.Console
import binocular.notify.Notifier
import binocular.oracle.OracleTransactions
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.{AssetName, Script, ScriptHash, Utxo}
import scalus.cardano.node.BlockchainProvider
import scalus.cardano.wallet.hd.HdAccount
import scalus.uplc.builtin.{Builtins, ByteString, Data}
import scalus.utils.await

import java.nio.file.{Files, Path}
import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration
import scala.util.Try

/** Chains peg-out **Complete** after TM **Confirm**: the watchtower's PegOutRequest sweeper.
  *
  * Completion is permissionless cleanup since spec rev 5.1 — anyone may burn a paid request's
  * locked fBTC against a membership proof and keep its MIN_ADA. The watchtower is the natural party
  * to do it: it already confirms the Treasury Movement that made those requests completable, so it
  * learns about them first and the cleanup incentive pays for the transaction it submits. Left
  * undone, paid PegOutRequest UTxOs accumulate forever.
  *
  * ==What one sweep does==
  *   1. CATCH UP the local trie mirror to the root the on-chain CPO singleton holds, using the data
  *      -availability hints recorded at each confirm ([[recordConfirmed]]). If the hints cannot
  *      explain the on-chain root, reconstruct from chain history; if that fails too, HALT.
  *   1. COMPLETE every request at the peg-out address whose POR id the mirror records, one
  *      transaction each.
  *
  * ==Why catch-up is driven by the chain, not by our own confirms==
  * Confirming is permissionless, so another party's confirm can advance the singleton past us.
  * Comparing the mirror against the SINGLETON (rather than against the TM we just confirmed) makes
  * the sweeper notice that, and reconstruction repairs it. It also solves the settlement race for
  * free: right after our own confirm is submitted the singleton still holds the old root, the
  * mirror matches it, and there is simply nothing to catch up to until the transaction lands.
  *
  * ==Why the mirror root is HARD-verified, and when that halts anything==
  * A mirror whose entry set is wrong produces membership proofs `peg-out.ak` rejects. Submitting
  * them burns fees and, worse, hides the divergence behind a stream of script failures. So a mirror
  * that PROVABLY cannot be reconciled with the on-chain root halts sweeping and pages the operator.
  *
  * A halt is reserved for that. A backend that timed out, rate-limited us, or was briefly
  * unreachable is reported and retried on the next tick, because it says nothing about the trie —
  * latching there would trade a seconds-long outage for a process-lifetime one. The distinction is
  * carried by [[HistoryError.transient]] rather than inferred from the message.
  *
  * Confirming is NOT halted either way: peg-out cleanup is not on the critical path of the bridge.
  *
  * ==What survives a restart==
  * The trie mirror AND the pending hint queue are persisted in the state directory. The queue
  * matters as much as the mirror: a process that dies between a Confirm and the next catch-up would
  * otherwise lose the hints that explain the new on-chain root and fall back to a full two-address
  * reconstruction.
  *
  * ==Why completions are submitted sequentially==
  * They are independent ON-CHAIN — the trie is a reference input and each transaction spends one
  * request — so nothing forces an order. They are NOT independent off-chain: every one of them
  * draws its fee and collateral from the same sponsor wallet, and parallel builders would select
  * the same wallet UTxO and lose all but one to `ValueNotConserved`/`BadInputsUTxO`. Sequential
  * submission removes the only contention that actually exists, and per-request error isolation
  * keeps a failure from stopping the rest. A submitted completion is also suppressed until it
  * settles: its request stays listed at the peg-out address until then, so without that the next
  * poll rebuilds the same transaction and logs its inevitable failure.
  */
final class PorSweeper(
    provider: BlockchainProvider,
    hdAccount: HdAccount,
    ctx: PorSweeper.Context,
    stateDir: Path,
    historySource: Option[CpoHistorySource],
    notifier: Notifier,
    dryRun: Boolean,
    timeout: Duration
)(using ExecutionContext) {

    import PorSweeper.*

    /** `None` until the first sweep loads or reconstructs it. */
    private var mirror: Option[CpoTrieMirror] = None

    /** Set once the mirror is provably irreconcilable with the chain. Sweeping stays off until an
      * operator intervenes; a process restart on a fixed state directory clears it.
      *
      * ONLY an integrity failure latches here. A backend that timed out or rate-limited us is
      * reported and retried on the next tick, because latching on one HTTP 429 would disable
      * peg-out cleanup for the rest of the process's life over a condition that fixes itself in
      * seconds.
      */
    private var haltReason: Option[String] = None

    /** Hints recorded at confirm, oldest first, each already resolved to trie entries. */
    private val pending = mutable.Queue.empty[PendingTm]

    /** POR ref -> the wall-clock ms after which an unsettled completion may be rebuilt.
      *
      * A submitted completion does not remove its request from the peg-out address until it
      * settles, so without this the next poll (seconds later) rebuilds the same transaction and
      * logs its inevitable failure. The entry expires so a transaction that was dropped from the
      * mempool is eventually retried rather than abandoned.
      */
    private val inFlight = mutable.Map.empty[String, Long]

    /** True once the state directory has been read; see [[ensureLoaded]]. */
    private var loaded: Boolean = false

    def isHalted: Boolean = haltReason.isDefined

    /** True while a confirmed TM's hints have not yet been folded into the mirror. */
    def hasPending: Boolean = pending.nonEmpty

    /** Snapshot of the mirror, for logging and tests. */
    def currentMirror: Option[CpoTrieMirror] = mirror

    /** Record a TM the watchtower just confirmed.
      *
      * The hint outpoints are resolved to trie entries HERE, while the requests they name are still
      * unspent, rather than at sweep time when a competing completer may already have burned one.
      * The hint is unverified attacker-placeable data; nothing is trusted until the resulting
      * entries reproduce the attested root during catch-up.
      */
    def recordConfirmed(
        btcTxidDisplay: String,
        attestedRoot: ByteString,
        hintOutpoints: Seq[ByteString]
    ): Unit = {
        val pegOutUtxos = provider.findUtxos(ctx.pegOutAddress).await(timeout) match {
            case Right(us) => us.toList.map { case (i, o) => Utxo(i, o) }
            case Left(err) =>
                Console.logWarn(s"    sweeper: listing peg-out UTxOs: $err")
                List.empty
        }
        val byOutpoint = pegOutUtxos.flatMap { u =>
            historicalPor(u, ctx).map(p => p.outpoint.toHex -> p)
        }.toMap
        val entries = hintOutpoints.flatMap(op => byOutpoint.get(op.toHex)).map(_.entry)
        if entries.size < hintOutpoints.size then
            Console.logWarn(
              s"    sweeper: ${hintOutpoints.size - entries.size} of ${hintOutpoints.size} hinted " +
                  "peg-out outpoint(s) could not be resolved — catch-up may need reconstruction"
            )
        ensureLoaded().left.foreach(e => Console.logWarn(s"    sweeper: $e"))
        pending.enqueue(PendingTm(btcTxidDisplay, attestedRoot, entries))
        persistPending()
    }

    /** Catch the mirror up to `trieUtxo`'s root, then complete every completable request.
      *
      * `configUtxo` and `trieUtxo` are the LIVE reference inputs, re-read by the caller each cycle.
      */
    def sweep(configUtxo: Utxo, trieUtxo: Utxo, only: Option[String] = None): Unit = {
        haltReason match {
            case Some(why) => Console.logWarn(s"    sweeper: HALTED — skipping. $why")
            case None      => sweepUnhalted(configUtxo, trieUtxo, only)
        }
    }

    private def sweepUnhalted(configUtxo: Utxo, trieUtxo: Utxo, only: Option[String]): Unit =
        verifyAgainstConfig(configUtxo, ctx) match {
            // A deployment/migration state, not a defect: the deployed Config still publishes other
            // scripts than the ones derived here, so any completion we built would be rejected.
            // Report and skip; confirming is unaffected, and the next config Update fixes it with no
            // restart.
            case Left(err) => Console.logWarn(s"    sweeper: not sweeping — $err")
            case Right(()) => sweepVerified(configUtxo, trieUtxo, only)
        }

    private def sweepVerified(configUtxo: Utxo, trieUtxo: Utxo, only: Option[String]): Unit =
        onChainRoot(trieUtxo) match {
            case Left(err) => Console.logError(s"    sweeper: $err")
            case Right(root) =>
                catchUp(root) match {
                    // Transient: the backend was busy or unreachable. It says nothing about the
                    // trie, so the next tick simply tries again. Latching here would trade a
                    // seconds-long outage for a process-lifetime one.
                    case Left(err) if err.transient =>
                        Console.logWarn(
                          s"    sweeper: catch-up deferred (transient) — ${err.message}"
                        )
                    case Left(err) =>
                        haltReason = Some(err.message)
                        Console.logError(s"    sweeper: HALTING — ${err.message}")
                        notifier.error(
                          "sweeper",
                          s"peg-out sweeping HALTED: the local completed-peg-outs mirror " +
                              s"cannot be reconciled with the on-chain root. ${err.message}"
                        )
                    case Right(m) =>
                        mirror = Some(m)
                        m.save(stateDir)
                            .left
                            .foreach(e =>
                                Console.logWarn(s"    sweeper: persisting the trie mirror: $e")
                            )
                        completeAll(m, configUtxo, trieUtxo, only)
                }
        }

    /** The mirror, advanced until its root equals `target`.
      *
      * The queued hints are folded WITHOUT being consumed, and dropped only once the catch-up they
      * belong to has succeeded. A failure part-way through must leave the queue exactly as it found
      * it: consuming first would mean a transient backend error costs the cheap path back and
      * forces the next attempt into a full reconstruction.
      */
    private def catchUp(target: ByteString): Either[HistoryError, CpoTrieMirror] =
        ensureLoaded().left.map(HistoryError.permanent).flatMap { start =>
            val queued = pending.toIndexedSeq
            var current = start
            var consumed = 0
            var error: Option[String] = None
            while error.isEmpty && current.root != target && consumed < queued.size do {
                val tm = queued(consumed)
                consumed += 1
                current.applied(tm.entries) match {
                    case Right(next) =>
                        if next.root == tm.attestedRoot then {
                            current = next
                            Console.log(
                              s"    sweeper: mirror advanced by TM ${tm.btcTxidDisplay} to " +
                                  s"${next.root.toHex} (${next.size} entries)"
                            )
                        } else
                            error = Some(
                              s"TM ${tm.btcTxidDisplay} attested root ${tm.attestedRoot.toHex} " +
                                  s"but its ${tm.entries.size} hinted entr(y|ies) produce " +
                                  s"${next.root.toHex}"
                            )
                    case Left(err) => error = Some(s"TM ${tm.btcTxidDisplay}: $err")
                }
            }
            error match {
                case Some(err) => recover(target, err)
                case None if current.root == target =>
                    if consumed > 0 then {
                        pending.remove(0, consumed)
                        persistPending()
                    }
                    Right(current)
                case None =>
                    recover(
                      target,
                      s"the mirror holds ${current.root.toHex} and no recorded hint explains the " +
                          s"on-chain root ${target.toHex} (a TM confirmed by another party)"
                    )
            }
        }

    /** Rebuild the mirror from chain history when the recorded hints cannot explain `target`.
      *
      * A TRANSIENT failure is passed through as transient, so the caller waits instead of latching.
      * The pending queue is cleared only once reconstruction SUCCEEDS: the hints are the cheap path
      * back, and discarding them before knowing the expensive path worked would guarantee a full
      * reconstruction on the next attempt too.
      */
    private def recover(target: ByteString, why: String): Either[HistoryError, CpoTrieMirror] =
        historySource match {
            case None =>
                Left(
                  HistoryError.permanent(
                    s"$why, and no chain-history backend is configured to reconstruct from"
                  )
                )
            case Some(source) =>
                Console.logWarn(s"    sweeper: reconstructing the trie mirror — $why")
                reconstructConfig(target) match {
                    case Left(err) => Left(HistoryError.permanent(err))
                    case Right(cfg) =>
                        CpoReconstruction.reconstruct(
                          source,
                          cfg,
                          s => Console.log(s"    $s")
                        ) match {
                            case Right(m) =>
                                pending.clear()
                                persistPending()
                                Right(m)
                            case Left(err) =>
                                Left(
                                  HistoryError(
                                    s"$why; reconstruction also failed: ${err.message}",
                                    err.transient
                                  )
                                )
                        }
                }
        }

    /** The reconstruction inputs, or the reason they cannot be formed.
      *
      * The bech32 encodings are checked rather than defaulted: an address that will not encode used
      * to become the empty string, which the backend answers with an EMPTY history — and an empty
      * history reconstructs a confidently empty trie. Fail loudly instead.
      */
    private def reconstructConfig(target: ByteString): Either[String, CpoReconstruction.Config] =
        for {
            tm <- ctx.tmAddress.encode.toEither.left
                .map(e => s"the TM address does not encode to bech32: ${e.getMessage}")
            por <- ctx.pegOutAddress.encode.toEither.left
                .map(e => s"the peg-out address does not encode to bech32: ${e.getMessage}")
        } yield CpoReconstruction.Config(
          tmAddress = tm,
          pegOutAddress = por,
          fbtcPolicyHex = ctx.bridgedTokenPolicy.toHex,
          fbtcAssetNameHex = ctx.bridgedTokenAsset.bytes.toHex,
          onChainRoot = Some(target)
        )

    /** Read the state directory once: the persisted mirror and the pending hint queue.
      *
      * Both are read together because they are one logical checkpoint. The queue matters as much as
      * the mirror: a restart between a Confirm and the next catch-up would otherwise drop the hints
      * that explain the new on-chain root, forcing a full two-address reconstruction (or, when the
      * backend is unavailable, no catch-up at all).
      */
    private def ensureLoaded(): Either[String, CpoTrieMirror] =
        if loaded then Right(mirror.getOrElse(CpoTrieMirror.empty))
        else
            CpoTrieMirror.load(stateDir).map { persisted =>
                loaded = true
                val m = persisted match {
                    case Some(found) =>
                        Console.log(
                          s"    sweeper: loaded trie mirror ${found.root.toHex} " +
                              s"(${found.size} entries) from ${CpoTrieMirror.stateFile(stateDir)}"
                        )
                        found
                    case None =>
                        Console.log("    sweeper: no persisted trie mirror — starting from empty")
                        CpoTrieMirror.empty
                }
                mirror = Some(m)
                // Hints are unverified either way — every batch is checked against its TM's attested
                // root before it is folded in — so a stale or unreadable queue costs a reconstruction
                // at worst, never correctness. Hence a warning, not a failure.
                PendingTm.load(stateDir) match {
                    case Right(restored) =>
                        if restored.nonEmpty then {
                            pending.enqueueAll(restored)
                            Console.log(
                              s"    sweeper: restored ${restored.size} pending TM hint(s)"
                            )
                        }
                    case Left(err) => Console.logWarn(s"    sweeper: pending hints: $err")
                }
                m
            }

    private def persistPending(): Unit =
        PendingTm
            .save(stateDir, pending.toSeq)
            .left
            .foreach(e => Console.logWarn(s"    sweeper: persisting pending hints: $e"))

    /** Build and submit one Complete transaction per completable request. */
    private def completeAll(
        m: CpoTrieMirror,
        configUtxo: Utxo,
        trieUtxo: Utxo,
        only: Option[String]
    ): Unit = {
        val utxos = provider.findUtxos(ctx.pegOutAddress).await(timeout) match {
            case Right(us) => us.toList.map { case (i, o) => Utxo(i, o) }
            case Left(err) =>
                Console.logWarn(s"    sweeper: listing peg-out UTxOs: $err")
                List.empty
        }
        val (all, skipped) = candidates(utxos, ctx, m)
        skipped
            .filter(s => only.forall(_ == s.ref))
            .foreach(s => Console.logWarn(s"    sweeper: ${s.ref} skipped — ${s.reason}"))
        // `only` restricts the SUBMISSION set, never the mirror catch-up above: the manual command
        // must leave the mirror in exactly the state an automatic sweep would.
        val selected = only.fold(all)(ref => all.filter(_.ref == ref))
        // Drop anything already submitted and not yet settled. Its request is still listed at the
        // peg-out address until the completion is on-chain, so without this every poll rebuilds the
        // same transaction and logs its inevitable failure at error level.
        val now = System.currentTimeMillis()
        inFlight.filterInPlace((_, deadline) => deadline > now)
        val ready = selected.filterNot(c => inFlight.contains(c.ref))
        if selected.size > ready.size then
            Console.log(
              s"    sweeper: ${selected.size - ready.size} completion(s) still in flight — waiting"
            )
        if ready.isEmpty then
            if utxos.nonEmpty && selected.isEmpty then
                Console.log(
                  s"    sweeper: no completable peg-out request (${utxos.size} at address)"
                )
        else {
            Console.log(s"    sweeper: ${ready.size} completable peg-out request(s)")
            // Resolved ONCE per sweep, not once per process: a reference script deployed after
            // startup is picked up on the next sweep, and a discovery failure only costs this sweep
            // the smaller transaction, never the completion itself.
            val scriptRefs =
                try ctx.resolveScriptRefs()
                catch {
                    case e: Exception =>
                        Console.logWarn(
                          s"    sweeper: reference-script discovery failed (${e.getMessage}) — " +
                              "inlining the scripts this sweep"
                        )
                        PegOutCompleteTx.ScriptRefs(None, None)
                }
            ready.foreach(c => completeOne(c, m, configUtxo, trieUtxo, scriptRefs))
        }
    }

    /** One request. Every failure is caught HERE so one bad request cannot stop the others. */
    private def completeOne(
        c: Completable,
        m: CpoTrieMirror,
        configUtxo: Utxo,
        trieUtxo: Utxo,
        scriptRefs: PegOutCompleteTx.ScriptRefs
    ): Unit = {
        val ref = c.ref
        m.proveMembership(c.porId) match {
            case Left(err) => Console.logError(s"    sweeper: $ref — $err")
            case Right(proof) if dryRun =>
                Console.logSuccess(
                  s"    sweeper: [dry-run] would complete $ref  por_id=${c.porId.toHex}  " +
                      s"burn=${c.locked} sat  proof_steps=${proof.asScala.size}"
                )
            case Right(proof) =>
                try {
                    val tx = PegOutCompleteTx
                        .build(
                          provider = provider,
                          sponsor = hdAccount,
                          scripts =
                              PegOutCompleteTx.Scripts(ctx.pegOutScript, ctx.bridgedTokenScript),
                          scriptRefs = scriptRefs,
                          inputs = PegOutCompleteTx.Inputs(c.utxo, configUtxo, trieUtxo),
                          membershipProof = proof,
                          lockedFbtc = c.locked,
                          bridgedTokenPolicy = ctx.bridgedTokenPolicy,
                          bridgedTokenAsset = ctx.bridgedTokenAsset,
                          pegOutHash = ctx.pegOutScript.scriptHash
                        )
                        .await(timeout)
                    OracleTransactions.submitTx(provider, tx, timeout) match {
                        case Right(hash) =>
                            inFlight(ref) = System.currentTimeMillis() + InFlightTtlMs
                            Console.logSuccess(
                              s"    sweeper: completed $ref  burn=${c.locked} sat  cardano_tx=$hash"
                            )
                            notifier.success(
                              "sweeper",
                              s"peg-out completed — request `$ref`, ${c.locked} sat burned, " +
                                  s"cardano tx `$hash`"
                            )
                        case Left(err) =>
                            Console.logError(s"    sweeper: $ref submit failed: $err")
                    }
                } catch {
                    case e: Throwable =>
                        // A request another completer just took loses a harmless race here; a real
                        // defect surfaces as a repeated identical message. Either way the remaining
                        // requests must still be attempted, so nothing escapes.
                        Console.logError(s"    sweeper: $ref build/submit failed: ${e.getMessage}")
                }
        }
    }

    private def onChainRoot(trieUtxo: Utxo): Either[String, ByteString] =
        trieUtxo.output.inlineDatum
            .flatMap(d => Try(d.to[CompletedPegOutsTrieDatum].root).toOption)
            .toRight("the CPO singleton UTxO has no decodable inline root datum")
}

object PorSweeper {

    /** How long a submitted completion is left alone before it may be rebuilt: 10 minutes, an order
      * of magnitude above Cardano's settlement time, so a normal completion always disappears from
      * the peg-out address first and a mempool-dropped one is still retried the same hour.
      */
    val InFlightTtlMs: Long = 10 * 60 * 1000L

    /** Everything the sweeper needs that does not change while the process runs.
      *
      * `resolveScriptRefs` is a FUNCTION, not a value: CIP-33 reference UTxOs are discovered over
      * the network, so resolving them once at startup made a transient failure permanent and left a
      * reference script deployed later unused forever. It is called per sweep and may return
      * `ScriptRefs(None, None)`, which simply inlines the scripts in the witness set.
      */
    final case class Context(
        network: Network,
        tmAddress: Address,
        pegOutScript: Script.PlutusV3,
        pegOutAddress: Address,
        bridgedTokenScript: Script.PlutusV3,
        bridgedTokenPolicy: ScriptHash,
        bridgedTokenAsset: AssetName,
        resolveScriptRefs: () => PegOutCompleteTx.ScriptRefs
    )

    /** A confirmed TM whose hinted entries have not yet been folded into the mirror. */
    final case class PendingTm(
        btcTxidDisplay: String,
        attestedRoot: ByteString,
        entries: Seq[(ByteString, ByteString)]
    )

    /** Persistence for the pending hint queue.
      *
      * A sibling of the trie mirror rather than a field inside it: the mirror is a verified
      * artifact (its root is checked against its own entries on load) while the queue is unverified
      * attacker-influenced input, and mixing the two would let a bad hint make the mirror
      * unreadable.
      */
    object PendingTm {

        val FileName = "cpo-pending.json"

        val Version = 1

        def file(stateDir: Path): Path = stateDir.resolve(FileName)

        def save(stateDir: Path, queue: Seq[PendingTm]): Either[String, Unit] =
            JsonState.write(
              file(stateDir),
              ujson.Obj(
                "version" -> ujson.Num(Version),
                "pending" -> ujson.Arr(
                  queue.map { tm =>
                      ujson.Obj(
                        "btc_txid" -> ujson.Str(tm.btcTxidDisplay),
                        "attested_root" -> ujson.Str(tm.attestedRoot.toHex),
                        "entries" -> ujson.Arr(
                          tm.entries.map { case (k, v) =>
                              ujson.Obj(
                                "por_id" -> ujson.Str(k.toHex),
                                "value" -> ujson.Str(v.toHex)
                              )
                          }*
                        )
                      )
                  }*
                )
              )
            )

        /** Restore the queue. An absent file is an empty queue, not an error. */
        def load(stateDir: Path): Either[String, Seq[PendingTm]] = {
            val path = file(stateDir)
            if !Files.isReadable(path) then Right(Seq.empty)
            else
                JsonState.read(path).flatMap { json =>
                    try {
                        val version = json.obj.get("version").map(_.num.toInt).getOrElse(-1)
                        if version != Version then
                            Left(s"$path has state version $version, expected $Version — ignoring")
                        else
                            Right(
                              json("pending").arr.toSeq.map { tm =>
                                  PendingTm(
                                    btcTxidDisplay = tm("btc_txid").str,
                                    attestedRoot = ByteString.fromHex(tm("attested_root").str),
                                    entries = tm("entries").arr.toSeq.map { e =>
                                        (
                                          ByteString.fromHex(e("por_id").str),
                                          ByteString.fromHex(e("value").str)
                                        )
                                    }
                                  )
                              }
                            )
                    } catch { case e: Exception => Left(s"$path is unreadable: ${e.getMessage}") }
                }
        }
    }

    /** A request the mirror says is paid, with everything the builder needs. */
    final case class Completable(utxo: Utxo, porId: ByteString, locked: Long) {
        def ref: String = s"${utxo.input.transactionId.toHex}#${utxo.input.index}"
    }

    /** A request that will NOT be completed this pass, and why. */
    final case class Skipped(ref: String, reason: String)

    /** Field `i` of a Config UTxO's inline datum, as raw bytes.
      *
      * A RAW positional read, not a typed [[ConfigDatum]] decode, for the same reason
      * `ConfirmTmtxCommand.loadTrieContext` reads field 3 raw: a deployed pre-migration datum with
      * a different field count must still be readable, so the sweeper can report the migration
      * state instead of throwing.
      */
    def configField(configUtxo: Utxo, i: Int): Either[String, ByteString] =
        configUtxo.output.inlineDatum match {
            case Some(Data.Constr(0, fields)) =>
                fields.asScala.toIndexedSeq.lift(i) match {
                    case Some(Data.B(b)) => Right(b)
                    case other           => Left(s"config field $i is not a byte string: $other")
                }
            case other => Left(s"config datum is not a Constr 0 inline datum: $other")
        }

    /** The scripts this sweeper would use MUST be the ones the deployed Config publishes.
      *
      * Rev-5.4 layout: field 6 is `peg_out_script_hash` — the script whose reward account the
      * Complete transaction withdraws from and whose address holds the requests. Field 1 is the
      * bridged-token policy, the token the completion burns; its asset name is the [CFG-1] constant
      * `"fSAT"` ([[ConfigDatum.BridgedTokenAssetName]]), not a Config field. A mismatch on any of
      * them means the derived scripts are not the live ones and every completion would be rejected.
      */
    def verifyAgainstConfig(configUtxo: Utxo, ctx: Context): Either[String, Unit] =
        for {
            pegOut <- configField(configUtxo, 6)
            policy <- configField(configUtxo, 1)
            _ <- Either.cond(
              pegOut.toHex == ctx.pegOutScript.scriptHash.toHex,
              (),
              s"config field 6 publishes peg-out withdraw hash ${pegOut.toHex}, but the derived " +
                  s"peg_out validator hashes to ${ctx.pegOutScript.scriptHash.toHex} — run the " +
                  "`update-config --peg-out-withdraw-hash` migration before sweeping"
            )
            _ <- Either.cond(
              policy.toHex == ctx.bridgedTokenPolicy.toHex &&
                  ctx.bridgedTokenAsset.bytes.toHex == ConfigDatum.BridgedTokenAssetName.toHex,
              (),
              s"config publishes bridged token policy ${policy.toHex} (asset is the [CFG-1] " +
                  s"constant ${ConfigDatum.BridgedTokenAssetName.toHex}), but the sweeper is " +
                  s"configured for ${ctx.bridgedTokenPolicy.toHex}.${ctx.bridgedTokenAsset.bytes.toHex}"
            )
        } yield ()

    /** Read a peg-out request UTxO as a trie entry source.
      *
      * `None` when the output is not a request at all: no fBTC, or no decodable [[PegOutDatum]].
      * The peg-out address is permissionlessly payable, so junk outputs are expected and are not
      * errors.
      */
    def historicalPor(u: Utxo, ctx: Context): Option[CpoReconstruction.HistoricalPor] = {
        val locked = u.output.value.asset(ctx.bridgedTokenPolicy, ctx.bridgedTokenAsset)
        if locked <= 0 then None
        else
            u.output.inlineDatum
                .flatMap(d => Try(d.to[PegOutDatum]).toOption)
                .map { por =>
                    val txHash = ByteString.fromArray(u.input.transactionId.bytes)
                    CpoReconstruction.HistoricalPor(
                      porId = CpoTrieMirror.porId(txHash, u.input.index.toLong),
                      outpoint = CpoTrieMirror.hintBytes(txHash, u.input.index.toLong),
                      scriptPubKey = por.sourceChainDestinationAddress,
                      netSat = (BigInt(locked) - por.perPegoutFee).max(0)
                    )
                }
    }

    /** Split the peg-out address's UTxOs into what this sweep will complete and what it will not.
      *
      * PURE, so the decision rule is testable without a chain. The rules:
      *   - no fBTC / no `PegOutDatum` — not a request, ignored silently.
      *   - POR id absent from the mirror — not paid yet, ignored silently. This is the normal state
      *     of an open request and must never be reported as a problem.
      *   - POR id present with a DIFFERENT value — reported. `peg-out.ak` binds the trie value to
      *     `dest_spk ++ le8(locked − per_pegout_fee)` computed from THIS UTxO, so a mismatch means
      *     the paid amount and the request disagree; submitting would burn a fee on a script the
      *     ledger will reject.
      *   - otherwise — completable.
      */
    def candidates(
        utxos: Seq[Utxo],
        ctx: Context,
        mirror: CpoTrieMirror
    ): (Seq[Completable], Seq[Skipped]) = {
        val ready = Vector.newBuilder[Completable]
        val skipped = Vector.newBuilder[Skipped]
        utxos.foreach { u =>
            val locked = u.output.value.asset(ctx.bridgedTokenPolicy, ctx.bridgedTokenAsset)
            historicalPor(u, ctx).foreach { por =>
                val expected =
                    por.scriptPubKey ++ Builtins.integerToByteString(false, 8, por.netSat)
                val ref = s"${u.input.transactionId.toHex}#${u.input.index}"
                mirror.valueOf(por.porId) match {
                    case None => () // open request, not paid yet
                    case Some(v) if v != expected =>
                        skipped += Skipped(
                          ref,
                          s"the trie records ${v.toHex} for POR id ${por.porId.toHex}, but this " +
                              s"request binds ${expected.toHex} (locked $locked sat)"
                        )
                    case Some(_) =>
                        ready += Completable(u, por.porId, locked)
                }
            }
        }
        (ready.result(), skipped.result())
    }
}
