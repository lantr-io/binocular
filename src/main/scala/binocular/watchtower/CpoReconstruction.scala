package binocular.watchtower

import binocular.bitcoin.BitcoinHelpers
import binocular.oracle.reverse
import scalus.uplc.builtin.{ByteString, Data}

import scala.collection.mutable
import scala.util.Try

/** Rebuild the completed-peg-outs trie from Cardano history alone ([OB-2], [OB-9]).
  *
  * This is the cold-start / recovery path: it exists so a watchtower that has lost (or never had) a
  * local mirror can produce membership proofs again without asking anyone for the entry set. It is
  * the Scala mirror of heimdall's `src/cardano/cpo_trie.rs::reconstruct`.
  *
  * Rev 5.4 produces NO `Confirmed` record — Confirm burns the TM NFT and leaves nothing at the TM
  * address ([CTM-24], [CTM-25]) — so the confirmed chain is recovered exactly the way the SPI proof
  * server recovers it ([SPI-6]): harvest every spent `UnconfirmedTm` record's `signed_btc_tx`, key
  * it by the txid RECOMPUTED from the bytes ([SPI-7]/[OB-9]), and walk the treasury spend chain
  * BACKWARD from the singleton's `treasury_utxo_id` via input-0 ancestry
  * ([[SweptPegInsProofService.walkConfirmedChain]]). A TM mined but not yet confirmed SPENDS the
  * head, so the walk never visits it — the walk's result is exactly the set the singleton's
  * attested roots cover. Invariants kept from the pre-walk implementation:
  *
  *   1. NO SILENT SKIPS at the TM address. An output there whose datum EXISTS but cannot be read is
  *      a hard error: if it hides a confirmed TM's `UnconfirmedTm` record, the walk would stop
  *      short and the replay would fail with an unexplained gap — better to name the output.
  *   1. PER-TM ROOT ASSERTION. After each confirmed TM the running root MUST equal the `cpo_root`
  *      that TM committed in its `"BTMR1"` output. A mismatch names the offending TM instead of
  *      surfacing later as an unexplained wrong root.
  *   1. ALL HINTS TRIED. Posting a TM record is permissionless, so several `UnconfirmedTm` records
  *      may claim the same Bitcoin txid with different hints. Every candidate is tried and accepted
  *      only if it reproduces the attested root; a hostile one simply fails.
  *   1. FINAL CROSS-CHECK. The finished trie is compared against the bridge-state singleton's
  *      `cpo_root`. Only this catches a replay that stopped early — the per-TM assertion has no
  *      later TM to fail against at the tip.
  *
  * Invariant 1 turns on a THREE-way reading of a datum, not a two-way one. An output carrying NO
  * datum at all is skipped even at the TM address: every TM record is created with an inline datum,
  * so a datum-less output provably is not one. Only a datum that exists and cannot be READ is
  * fatal. Both addresses are permissionlessly payable, so treating "no datum" as fatal let a single
  * junk payment block every reconstruction forever — which is a denial of service, not a safety
  * property. A datum that resolves but is not an `UnconfirmedTm` record (junk, or a legacy rev-5.1
  * `Confirmed` Constr-1 shape) is skipped: it is not a byte carrier for the walk.
  *
  * The asymmetry between the two addresses is intentional. An UNREADABLE output at the PEG-OUT
  * address is skipped rather than fatal, because a missing request cannot shrink the trie silently
  * — it makes some TM fail its root assertion by name.
  *
  * The committed root is what converts every step from trust into search-and-check: a garbled hint
  * cannot corrupt the result, only make reconstruction slower.
  */
object CpoReconstruction {

    /** Max candidate assignments the fallback matcher tries per TM before giving up. Reconstruction
      * is a rare, operator-visible operation, so a bounded search that reports failure beats an
      * unbounded one that hangs.
      */
    val FallbackSearchBudget: Int = 200_000

    /** Everything reconstruction needs to identify the protocol's UTxOs.
      *
      * @param headUtxoId
      *   the bridge-state singleton's `treasury_utxo_id` (36 bytes, `btc_txid ‖ vout LE`) — where
      *   the backward walk starts. The head bounds the confirmed set structurally ([SPI-6]).
      * @param onChainRoot
      *   the `cpo_root` the singleton holds. `Some` turns on the final safety net. `None` skips it
      *   and MUST be logged loudly — only for a bridge whose singleton is not deployed yet, and for
      *   tests.
      */
    final case class Config(
        tmAddress: String,
        pegOutAddress: String,
        fbtcPolicyHex: String,
        fbtcAssetNameHex: String,
        headUtxoId: ByteString,
        onChainRoot: Option[ByteString]
    )

    /** A confirmed Treasury Movement, recovered by the treasury-chain walk.
      *
      * `outputs` is EVERY output of the signed Bitcoin transaction, root commitment included,
      * parsed from the raw bytes the spent `UnconfirmedTm` record carries ([OB-9]).
      */
    final case class ConfirmedTm(
        btcTxid: ByteString,
        outputs: Seq[PegOutEntry]
    )

    /** A peg-out request as chain history remembers it — open, completed, or cancelled. */
    final case class HistoricalPor(
        porId: ByteString,
        outpoint: ByteString,
        scriptPubKey: ByteString,
        netSat: BigInt
    ) {
        def entry: (ByteString, ByteString) =
            (porId, CompletedPegOutsTrie.trieValue(PegOutEntry(scriptPubKey, netSat)))
    }

    /** Reconstruct the mirror. `log` receives one progress line per phase.
      *
      * The error carries [[HistoryError.transient]], so the caller can tell "the backend was busy"
      * from "the chain and this trie disagree". Only the latter justifies latching a halt.
      */
    def reconstruct(
        source: CpoHistorySource,
        cfg: Config,
        log: String => Unit = _ => ()
    ): Either[HistoryError, CpoTrieMirror] = {
        log(s"cpo reconstruction backend: ${source.backend}")
        for {
            tmOutputs <- source.addressHistory(cfg.tmAddress)
            _ = log(s"cpo reconstruction: ${tmOutputs.size} output(s) ever at the TM address")
            scanned <- scanTmAddress(tmOutputs, cfg.tmAddress).left.map(HistoryError.permanent)
            (rawByTxid, hints) = scanned
            chain <- SweptPegInsProofService
                .walkConfirmedChain(cfg.headUtxoId, txid => rawByTxid.get(txid.toHex))
                .left
                .map(HistoryError.permanent)
            ordered = chain.tms.map(tm =>
                ConfirmedTm(
                  tm.txidLE,
                  TreasuryMovementValidator.allOutputs(tm.raw).asScala.toSeq
                )
            )
            porOutputs <- source.addressHistory(cfg.pegOutAddress)
            history = pegOutHistory(porOutputs, cfg)
            _ = log(
              s"cpo reconstruction: ${ordered.size} confirmed TM(s) on the walk from the head, " +
                  s"${history.size} peg-out request(s) in history"
            )
            mirror <- replay(ordered, hints, history, log).left
                .map(err => HistoryError.permanent(err + incompleteHistoryHint(chain)))
            _ <- crossCheck(mirror, cfg.onChainRoot, log).left
                .map(err => HistoryError.permanent(err + incompleteHistoryHint(chain)))
        } yield mirror
    }

    /** Appended to a replay/cross-check failure when the walk stopped at an unresolved txid: the
      * probable cause is then a history source missing a confirmed TM's record, not a lying chain.
      * An unresolved origin alone is NORMAL — the genesis funding transaction has no
      * `UnconfirmedTm` record and terminates every complete walk this way.
      */
    private def incompleteHistoryHint(
        chain: SweptPegInsProofService.ConfirmedChain
    ): String =
        chain.unresolvedOrigin.fold("")(txid =>
            s". The walk stopped at txid ${txid.reverse.toHex}, which the TM-address history " +
                "could not resolve — if that transaction is a confirmed TM, its record's datum " +
                "is missing from the history source"
        )

    /** Split the TM address's history into raw-TM byte carriers and hint candidates.
      *
      * Both maps are keyed by the btc txid RECOMPUTED from the record's own signed bytes, never by
      * a self-declared field ([SPI-7]). Raw bytes: first record in wins (same key means a
      * byte-identical preimage). Hints: EVERY candidate for a txid is kept (see the object doc).
      */
    def scanTmAddress(
        outputs: Seq[ChainOutput],
        tmAddress: String
    ): Either[String, (Map[String, ByteString], Map[String, Seq[Seq[ByteString]]])] = {
        val rawByTxid = mutable.LinkedHashMap.empty[String, ByteString]
        val hints = mutable.LinkedHashMap.empty[String, Vector[Seq[ByteString]]]
        var error: Option[String] = None
        val it = outputs.iterator
        while error.isEmpty && it.hasNext do {
            val out = it.next()
            out.inlineDatum match {
                // No datum AT ALL: provably not a TM record, because every TM record is created
                // with an inline datum. The TM address is permissionlessly payable, so a bare
                // payment to it is ordinary junk and skipping it costs nothing. Conflating this
                // with the unresolvable case below let ONE junk UTxO block every reconstruction
                // forever.
                case None if out.unresolvedDatum.isEmpty => ()
                case None                                =>
                    // A datum EXISTS and could not be read. This one might be the UnconfirmedTm
                    // record of a confirmed TM, and dropping it stops the treasury-chain walk
                    // short of that movement — the replay then fails with an unexplained gap. So:
                    // name the output and stop.
                    error = Some(
                      s"cannot resolve the datum of ${out.ref} at the TM address $tmAddress " +
                          s"(${out.unresolvedDatum.getOrElse("unknown reason")}) — refusing to " +
                          "reconstruct with an unexplained gap: if that output is the record of " +
                          "a confirmed TM, the walk would silently stop short of it"
                    )
                case Some(datum) =>
                    // Not an UnconfirmedTm record (junk, or a legacy rev-5.1 Confirmed shape) is
                    // skipped: its datum resolved, it just is not a byte carrier for the walk.
                    parseUnconfirmedHint(datum).foreach { case (txid, raw, hint) =>
                        rawByTxid.getOrElseUpdate(txid.toHex, raw)
                        hints.updateWith(txid.toHex) {
                            case Some(seen) => Some(seen :+ hint)
                            case None       => Some(Vector(hint))
                        }
                    }
            }
        }
        error.toLeft((rawByTxid.toMap, hints.view.mapValues(_.toSeq).toMap))
    }

    /** An `UnconfirmedTm` record's byte payload and data-availability hint (rev 5.4: Constr 0,
      * `signed_btc_tx` at field 0, `fulfilled_por_outpoints` at field 3), keyed by the txid
      * recomputed from the signed Bitcoin bytes ([SPI-7]).
      *
      * Tolerates a shorter datum, or one whose field 3 is not a list (the retired 6-field shape
      * carried `epoch` there), by returning an empty hint: reconstruction must read the chain
      * rather than refuse it. A malformed entry (not 36 bytes) is dropped — the hint is unverified,
      * attacker-supplied data. Every failure path is safe because a hint is only ever ACCEPTED
      * after it reproduces the attested root, and the raw bytes are only ever USED when the
      * attested chain walk asks for their hash.
      */
    def parseUnconfirmedHint(datum: Data): Option[(ByteString, ByteString, Seq[ByteString])] =
        datum match {
            case Data.Constr(0, fields) =>
                val f = fields.asScala.toIndexedSeq
                if f.isEmpty then None
                else
                    try {
                        val signedTx = f(0).toByteString
                        // Parsing attacker-placeable bytes: getTxHash walks a tx-declared count, so
                        // a crafted datum could recurse deeply. Guarded by the same catch that
                        // covers the decode.
                        val txid = BitcoinHelpers.getTxHash(signedTx)
                        val hint = f.lift(3) match {
                            case Some(Data.List(items)) =>
                                items.asScala.toSeq
                                    .flatMap(d => Try(d.toByteString).toOption)
                                    .filter(_.size == 36)
                            case _ => Seq.empty
                        }
                        Some((txid, signedTx, hint))
                    } catch { case _: Throwable => None }
            case _ => None
        }

    /** Every peg-out request ever created at the peg-out address, keyed by its 36-byte outpoint.
      *
      * Spent ones are included: a completed request's UTxO is gone, but its entry is exactly what
      * the trie must contain. An output with no fBTC is not a request; an undecodable datum is
      * skipped (see the object doc for why this address is treated differently from the TM one).
      */
    def pegOutHistory(
        outputs: Seq[ChainOutput],
        cfg: Config
    ): Map[String, HistoricalPor] = {
        val out = mutable.LinkedHashMap.empty[String, HistoricalPor]
        outputs.foreach { o =>
            val gross = o.quantityOf(cfg.fbtcPolicyHex, cfg.fbtcAssetNameHex)
            if gross > 0 then
                o.inlineDatum
                    .flatMap(d => Try(d.to[PegOutDatum]).toOption)
                    .foreach { por =>
                        val outpoint = CpoTrieMirror.hintBytes(o.txHash, o.outputIndex)
                        out.put(
                          outpoint.toHex,
                          HistoricalPor(
                            porId = CpoTrieMirror.porId(o.txHash, o.outputIndex),
                            outpoint = outpoint,
                            scriptPubKey = por.sourceChainDestinationAddress,
                            netSat = (gross - por.perPegoutFee).max(0)
                          )
                        )
                    }
        }
        out.toMap
    }

    /** Replay the confirmed chain into a mirror, asserting the running root after every TM. */
    def replay(
        ordered: Seq[ConfirmedTm],
        hints: Map[String, Seq[Seq[ByteString]]],
        history: Map[String, HistoricalPor],
        log: String => Unit = _ => ()
    ): Either[String, CpoTrieMirror] = {
        var mirror = CpoTrieMirror.empty
        var error: Option[String] = None
        val it = ordered.iterator
        while error.isEmpty && it.hasNext do {
            val tm = it.next()
            SweptPegInsTrie.committedRoots(tm.outputs).map(_._2) match {
                // A pre-rev-5.4 TM has no "BTMR1" commitment output. It also fulfilled nothing
                // under this regime, so it moves the root not at all. Backstopped by the final
                // cross-check: a TM wrongly treated as inert shows up as a root mismatch.
                case Left(_) => ()
                case Right(committed) if committed == mirror.root =>
                    () // zero-peg-out TM re-committing the unchanged root
                case Right(committed) =>
                    val fromHint = hints
                        .getOrElse(tm.btcTxid.toHex, Seq.empty)
                        .view
                        .map(_.flatMap(op => history.get(op.toHex)).map(_.entry))
                        .find(es => es.nonEmpty && mirror.rootAfter(es) == Right(committed))
                    val chosen = fromHint match {
                        case Some(entries) => Right(entries)
                        case None          => fallbackMatch(tm, committed, mirror, history)
                    }
                    chosen.flatMap(mirror.applied) match {
                        case Right(next) =>
                            mirror = next
                            log(
                              s"cpo reconstruction: TM ${tm.btcTxid.reverse.toHex} -> root " +
                                  s"${mirror.root.toHex} (${mirror.size} entries)"
                            )
                        case Left(err) =>
                            error = Some(
                              s"cannot reconstruct TM ${tm.btcTxid.reverse.toHex} " +
                                  s"(commits root ${committed.toHex}): $err"
                            )
                    }
            }
        }
        error.toLeft(mirror)
    }

    /** Find the peg-out set that explains `tm`'s committed root when no hint does.
      *
      * Each payment output `(spk, amount)` is matched against the requests with the same pair that
      * the trie does not already record. Ambiguity is a cost, not a correctness risk: every
      * candidate assignment is checked against the quorum-attested root, so a wrong guess is
      * rejected.
      */
    def fallbackMatch(
        tm: ConfirmedTm,
        committed: ByteString,
        mirror: CpoTrieMirror,
        history: Map[String, HistoricalPor]
    ): Either[String, Seq[(ByteString, ByteString)]] = {
        // The TM's actual PAYMENTS: every output except the treasury continuation (index 0) and the
        // root commitment.
        val payments = tm.outputs.drop(1).filterNot { o =>
            TreasuryMovementValidator.isTwoRootCommitment(o.scriptPubKey)
        }
        val candidates = payments.map { p =>
            history.values
                .filter(h =>
                    h.scriptPubKey == p.scriptPubKey && h.netSat == p.amount &&
                        !mirror.contains(h.porId)
                )
                .toSeq
                .sortBy(_.outpoint.toHex)
        }
        payments.zip(candidates).collectFirst { case (p, Seq()) =>
            s"no unrecorded peg-out request matches the payment of ${p.amount} sat to " +
                s"${p.scriptPubKey.toHex}"
        } match {
            case Some(err) => Left(err)
            case None =>
                var budget = FallbackSearchBudget
                def search(
                    depth: Int,
                    chosen: Vector[HistoricalPor],
                    used: Set[String]
                ): Option[Vector[HistoricalPor]] =
                    if depth == candidates.size then
                        if mirror.rootAfter(chosen.map(_.entry)) == Right(committed) then
                            Some(chosen)
                        else None
                    else
                        candidates(depth).iterator
                            .filterNot(c => used.contains(c.outpoint.toHex))
                            .map { c =>
                                budget -= 1
                                if budget < 0 then None
                                else search(depth + 1, chosen :+ c, used + c.outpoint.toHex)
                            }
                            .collectFirst { case Some(found) => found }
                search(0, Vector.empty, Set.empty) match {
                    case Some(found) => Right(found.map(_.entry))
                    case None if budget < 0 =>
                        Left(
                          s"search budget exhausted over ${payments.size} payment(s) with " +
                              s"${candidates.map(_.size).sum} candidate(s)"
                        )
                    case None =>
                        Left(
                          s"no assignment of ${payments.size} payment(s) to the peg-out requests " +
                              "in history reproduces the committed root"
                        )
                }
        }
    }

    /** The final safety net: the finished trie must equal the on-chain CPO singleton's root. */
    private def crossCheck(
        mirror: CpoTrieMirror,
        onChainRoot: Option[ByteString],
        log: String => Unit
    ): Either[String, Unit] = onChainRoot match {
        case Some(root) if root != mirror.root =>
            Left(
              s"reconstruction produced root ${mirror.root.toHex} over ${mirror.size} entries, " +
                  s"but the on-chain completed-peg-outs singleton holds ${root.toHex} — refusing " +
                  "to use a trie the chain disagrees with"
            )
        case Some(root) =>
            log(s"cpo reconstruction: root ${root.toHex} matches the on-chain CPO singleton")
            Right(())
        case None =>
            log(
              "cpo reconstruction: WARNING no on-chain root supplied — the reconstructed trie was " +
                  "NOT cross-checked against the CPO singleton"
            )
            Right(())
    }
}
