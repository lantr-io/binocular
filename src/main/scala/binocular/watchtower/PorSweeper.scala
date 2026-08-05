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

import java.nio.file.Path
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
  * ==Why the mirror root is HARD-verified==
  * A mirror whose entry set is wrong produces membership proofs `peg-out.ak` rejects. Submitting
  * them burns fees and, worse, hides the divergence behind a stream of script failures. So a mirror
  * that cannot be reconciled with the on-chain root halts sweeping and pages the operator instead.
  * Confirming is NOT halted: peg-out cleanup is not on the critical path of the bridge.
  *
  * ==Why completions are submitted sequentially==
  * They are independent ON-CHAIN — the trie is a reference input and each transaction spends one
  * request — so nothing forces an order. They are NOT independent off-chain: every one of them
  * draws its fee and collateral from the same sponsor wallet, and parallel builders would select
  * the same wallet UTxO and lose all but one to `ValueNotConserved`/`BadInputsUTxO`. Sequential
  * submission removes the only contention that actually exists, and per-request error isolation
  * keeps a failure from stopping the rest.
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

    /** Set once the mirror cannot be reconciled with the chain. Sweeping stays off until an
      * operator intervenes (the process restart that follows a fixed state directory clears it).
      */
    private var halted: Boolean = false

    /** Hints recorded at confirm, oldest first, each already resolved to trie entries. */
    private val pending = mutable.Queue.empty[PendingTm]

    def isHalted: Boolean = halted

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
        pending.enqueue(PendingTm(btcTxidDisplay, attestedRoot, entries))
    }

    /** Catch the mirror up to `trieUtxo`'s root, then complete every completable request.
      *
      * `configUtxo` and `trieUtxo` are the LIVE reference inputs, re-read by the caller each cycle.
      */
    def sweep(configUtxo: Utxo, trieUtxo: Utxo, only: Option[String] = None): Unit = {
        if halted then Console.logWarn("    sweeper: HALTED (trie mirror unreconciled) — skipping")
        else
            verifyAgainstConfig(configUtxo, ctx) match {
                // A deployment/migration state, not a defect: the deployed Config still publishes
                // other scripts than the ones derived here, so any completion we built would be
                // rejected. Report and skip; confirming is unaffected, and the next config Update
                // fixes it without a restart.
                case Left(err) =>
                    Console.logWarn(s"    sweeper: not sweeping — $err")
                case Right(()) => sweepVerified(configUtxo, trieUtxo, only)
            }
    }

    private def sweepVerified(configUtxo: Utxo, trieUtxo: Utxo, only: Option[String]): Unit =
        onChainRoot(trieUtxo) match {
            case Left(err) => Console.logError(s"    sweeper: $err")
            case Right(root) =>
                catchUp(root) match {
                    case Left(err) =>
                        halted = true
                        Console.logError(s"    sweeper: HALTING — $err")
                        notifier.error(
                          "sweeper",
                          s"peg-out sweeping HALTED: the local completed-peg-outs mirror " +
                              s"cannot be reconciled with the on-chain root. $err"
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

    /** The mirror, advanced until its root equals `target`. */
    private def catchUp(target: ByteString): Either[String, CpoTrieMirror] =
        ensureMirror().flatMap { start =>
            var current = start
            var error: Option[String] = None
            while error.isEmpty && current.root != target && pending.nonEmpty do {
                val tm = pending.dequeue()
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
                case Some(err)                      => recover(target, err)
                case None if current.root == target => Right(current)
                case None =>
                    recover(
                      target,
                      s"the mirror holds ${current.root.toHex} and no recorded hint explains the " +
                          s"on-chain root ${target.toHex} (a TM confirmed by another party)"
                    )
            }
        }

    /** Rebuild the mirror from chain history when the recorded hints cannot explain `target`. */
    private def recover(target: ByteString, why: String): Either[String, CpoTrieMirror] =
        historySource match {
            case None =>
                Left(s"$why, and no chain-history backend is configured to reconstruct from")
            case Some(source) =>
                Console.logWarn(s"    sweeper: reconstructing the trie mirror — $why")
                pending.clear()
                CpoReconstruction
                    .reconstruct(source, reconstructConfig(target), s => Console.log(s"    $s"))
                    .left
                    .map(err => s"$why; reconstruction also failed: $err")
        }

    private def reconstructConfig(target: ByteString) = CpoReconstruction.Config(
      tmAddress = ctx.tmAddress.encode.getOrElse(""),
      pegOutAddress = ctx.pegOutAddress.encode.getOrElse(""),
      fbtcPolicyHex = ctx.bridgedTokenPolicy.toHex,
      fbtcAssetNameHex = ctx.bridgedTokenAsset.bytes.toHex,
      onChainRoot = Some(target)
    )

    /** Load the persisted mirror, or start from the empty one (which catch-up then reconciles). */
    private def ensureMirror(): Either[String, CpoTrieMirror] = mirror match {
        case Some(m) => Right(m)
        case None =>
            CpoTrieMirror.load(stateDir).map {
                case Some(m) =>
                    Console.log(
                      s"    sweeper: loaded trie mirror ${m.root.toHex} (${m.size} entries) from " +
                          s"${CpoTrieMirror.stateFile(stateDir)}"
                    )
                    m
                case None =>
                    Console.log("    sweeper: no persisted trie mirror — starting from empty")
                    CpoTrieMirror.empty
            }
    }

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
        val ready = only.fold(all)(ref => all.filter(_.ref == ref))
        if ready.isEmpty then
            if utxos.nonEmpty then
                Console.log(
                  s"    sweeper: no completable peg-out request (${utxos.size} at address)"
                )
        else {
            Console.log(s"    sweeper: ${ready.size} completable peg-out request(s)")
            ready.foreach(c => completeOne(c, m, configUtxo, trieUtxo))
        }
    }

    /** One request. Every failure is caught HERE so one bad request cannot stop the others. */
    private def completeOne(
        c: Completable,
        m: CpoTrieMirror,
        configUtxo: Utxo,
        trieUtxo: Utxo
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
                          scriptRefs = ctx.scriptRefs,
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

    /** Everything the sweeper needs that does not change while the process runs. */
    final case class Context(
        network: Network,
        tmAddress: Address,
        pegOutScript: Script.PlutusV3,
        pegOutAddress: Address,
        bridgedTokenScript: Script.PlutusV3,
        bridgedTokenPolicy: ScriptHash,
        bridgedTokenAsset: AssetName,
        scriptRefs: PegOutCompleteTx.ScriptRefs
    )

    /** A confirmed TM whose hinted entries have not yet been folded into the mirror. */
    private final case class PendingTm(
        btcTxidDisplay: String,
        attestedRoot: ByteString,
        entries: Seq[(ByteString, ByteString)]
    )

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
      * Field 5 is `peg_out_withdraw_script_hash` — the script whose reward account the Complete
      * transaction withdraws from and whose address holds the requests. Fields 0/1 are the
      * bridged-token policy and asset name, the tokens the completion burns. A mismatch on any of
      * them means the derived scripts are not the live ones and every completion would be rejected.
      */
    def verifyAgainstConfig(configUtxo: Utxo, ctx: Context): Either[String, Unit] =
        for {
            pegOut <- configField(configUtxo, 5)
            policy <- configField(configUtxo, 0)
            asset <- configField(configUtxo, 1)
            _ <- Either.cond(
              pegOut.toHex == ctx.pegOutScript.scriptHash.toHex,
              (),
              s"config field 5 publishes peg-out withdraw hash ${pegOut.toHex}, but the derived " +
                  s"peg_out validator hashes to ${ctx.pegOutScript.scriptHash.toHex} — run the " +
                  "`update-config --peg-out-withdraw-hash` migration before sweeping"
            )
            _ <- Either.cond(
              policy.toHex == ctx.bridgedTokenPolicy.toHex &&
                  asset.toHex == ctx.bridgedTokenAsset.bytes.toHex,
              (),
              s"config publishes bridged token ${policy.toHex}.${asset.toHex}, but the sweeper is " +
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
