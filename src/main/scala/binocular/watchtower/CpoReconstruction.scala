package binocular.watchtower

import binocular.bitcoin.BitcoinHelpers
import binocular.oracle.reverse
import scalus.uplc.builtin.{ByteString, Data}

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap
import scala.collection.mutable
import scala.util.Try

/** Rebuild the completed-peg-outs trie from Cardano history alone.
  *
  * This is the cold-start / recovery path: it exists so a watchtower that has lost (or never had) a
  * local mirror can produce membership proofs again without asking anyone for the entry set. It is
  * the Scala mirror of heimdall's `src/cardano/cpo_trie.rs::reconstruct`, and it keeps that
  * implementation's invariants deliberately:
  *
  *   1. NO SILENT SKIPS at the TM address. An output there whose datum cannot be read is a hard
  *      error, because an unreadable `Confirmed` record would drop a whole movement's entries while
  *      the result still looked complete.
  *   1. PER-TM ROOT ASSERTION. After each confirmed TM the running root MUST equal the root that TM
  *      committed in its `"CPOR1"` output. A mismatch names the offending TM instead of surfacing
  *      later as an unexplained wrong root.
  *   1. ALL HINTS TRIED. Posting a TM record is permissionless, so several `Unconfirmed` records
  *      may claim the same Bitcoin txid with different hints. Every candidate is tried and accepted
  *      only if it reproduces the attested root; a hostile one simply fails.
  *   1. FINAL CROSS-CHECK. The finished trie is compared against the on-chain CPO singleton's root.
  *      Only this catches a replay that stopped early — the per-TM assertion has no later TM to
  *      fail against at the tip.
  *
  * The asymmetry between the two addresses is intentional. An unreadable output at the PEG-OUT
  * address is skipped: that address is permissionlessly payable, so erroring would let one junk
  * UTxO block every reconstruction forever, and a missing request cannot shrink the trie silently —
  * it makes some TM fail its root assertion by name.
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
      * @param onChainRoot
      *   the root the deployed CPO singleton holds. `Some` turns on the final safety net. `None`
      *   skips it and MUST be logged loudly — only for a bridge whose singleton is not deployed
      *   yet, and for tests.
      */
    final case class Config(
        tmAddress: String,
        pegOutAddress: String,
        fbtcPolicyHex: String,
        fbtcAssetNameHex: String,
        onChainRoot: Option[ByteString]
    )

    /** A confirmed Treasury Movement, as its on-chain `Confirmed` datum records it.
      *
      * `outputs` is EVERY output of the signed Bitcoin transaction, root commitment included — the
      * Confirm transition stores `allOutputs(signedBtcTx)` verbatim — so the committed root is
      * readable from Cardano state without re-parsing Bitcoin bytes.
      */
    final case class ConfirmedTm(
        btcTxid: ByteString,
        sweptInputs: Seq[ByteString],
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

    /** Reconstruct the mirror. `log` receives one progress line per phase. */
    def reconstruct(
        source: CpoHistorySource,
        cfg: Config,
        log: String => Unit = _ => ()
    ): Either[String, CpoTrieMirror] = {
        log(s"cpo reconstruction backend: ${source.backend}")
        for {
            tmOutputs <- source.addressHistory(cfg.tmAddress)
            _ = log(s"cpo reconstruction: ${tmOutputs.size} output(s) ever at the TM address")
            scanned <- scanTmAddress(tmOutputs, cfg.tmAddress)
            (confirmed, hints) = scanned
            ordered = chainOrder(confirmed)
            porOutputs <- source.addressHistory(cfg.pegOutAddress)
            history = pegOutHistory(porOutputs, cfg)
            _ = log(
              s"cpo reconstruction: ${ordered.size} confirmed TM(s), " +
                  s"${history.size} peg-out request(s) in history"
            )
            mirror <- replay(ordered, hints, history, log)
            _ <- crossCheck(mirror, cfg.onChainRoot, log)
        } yield mirror
    }

    /** Split the TM address's history into confirmed records and hint candidates.
      *
      * Returned hints are keyed by the btc txid RECOMPUTED from the record's own signed bytes,
      * never by a self-declared field, and every candidate for a txid is kept (see the object doc).
      */
    def scanTmAddress(
        outputs: Seq[ChainOutput],
        tmAddress: String
    ): Either[String, (Seq[ConfirmedTm], Map[String, Seq[Seq[ByteString]]])] = {
        val confirmed = Vector.newBuilder[ConfirmedTm]
        val hints = mutable.LinkedHashMap.empty[String, Vector[Seq[ByteString]]]
        var error: Option[String] = None
        val it = outputs.iterator
        while error.isEmpty && it.hasNext do {
            val out = it.next()
            out.inlineDatum match {
                case None =>
                    error = Some(
                      s"cannot resolve the datum of ${out.ref} at the TM address $tmAddress — " +
                          "refusing to reconstruct with an unexplained gap: if that output is a " +
                          "Confirmed TM record, skipping it yields a trie that silently omits a " +
                          "movement"
                    )
                case Some(datum) =>
                    parseConfirmed(datum) match {
                        case Some(tm) => confirmed += tm
                        case None     =>
                            // Not a Confirmed record. If it is an Unconfirmed one, harvest its
                            // hint; anything else is junk at a permissionlessly-payable address,
                            // which is NOT an error — its datum resolved, it just is not a TM.
                            parseUnconfirmedHint(datum).foreach { case (txid, hint) =>
                                hints.updateWith(txid.toHex) {
                                    case Some(seen) => Some(seen :+ hint)
                                    case None       => Some(Vector(hint))
                                }
                            }
                    }
            }
        }
        error.toLeft((confirmed.result(), hints.view.mapValues(_.toSeq).toMap))
    }

    /** Decode a `Confirmed` TM datum (Constr 1) defensively.
      *
      * Read positionally from raw `Data` rather than through `TmDatum`'s derived decoder: the TM
      * address is permissionlessly payable, so most outputs here are not TM records at all, and a
      * typed decode would just throw on each of them.
      */
    def parseConfirmed(datum: Data): Option[ConfirmedTm] =
        datum match {
            case Data.Constr(1, fields) =>
                val f = fields.asScala.toIndexedSeq
                if f.size < 3 then None
                else
                    try {
                        val txid = f(0).toByteString
                        val swept = f(1) match {
                            case Data.List(items) => items.asScala.toSeq.map(_.toByteString)
                            case _                => Seq.empty
                        }
                        val outs = f(2) match {
                            case Data.List(items) =>
                                items.asScala.toSeq.flatMap {
                                    case Data.Constr(0, of) =>
                                        val o = of.asScala.toIndexedSeq
                                        if o.size < 2 then None
                                        else Some(PegOutEntry(o(0).toByteString, o(1).toBigInt))
                                    case _ => None
                                }
                            case _ => Seq.empty
                        }
                        if txid.size == 32 then Some(ConfirmedTm(txid, swept, outs)) else None
                    } catch { case _: Exception => None }
            case _ => None
        }

    /** The rev-5.1 data-availability hint of an `Unconfirmed` TM datum (Constr 0, field 5), keyed
      * by the txid recomputed from field 0's signed Bitcoin bytes.
      *
      * Tolerates the OLD 5-field shape by returning an empty hint: such records confirm fine
      * on-chain, so real history contains them, and reconstruction must read the chain rather than
      * refuse it. A malformed entry (not 36 bytes) is dropped — the hint is unverified,
      * attacker-supplied data. Every failure path is safe because a hint is only ever ACCEPTED
      * after it reproduces the attested root.
      */
    def parseUnconfirmedHint(datum: Data): Option[(ByteString, Seq[ByteString])] =
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
                        val hint = f.lift(5) match {
                            case Some(Data.List(items)) =>
                                items.asScala.toSeq
                                    .flatMap(d => Try(d.toByteString).toOption)
                                    .filter(_.size == 36)
                            case _ => Seq.empty
                        }
                        Some((txid, hint))
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

    /** Order confirmed TM records by treasury linkage: *B* follows *A* iff *B* spends
      * `(A.txid, 0)`.
      *
      * Records that do not link into the main chain (a re-confirmation, a divergent lineage) are
      * appended in txid order, so the replay is deterministic and still sees them.
      */
    def chainOrder(confirmed: Seq[ConfirmedTm]): Seq[ConfirmedTm] = {
        // Dedupe on btc txid: the same TM can be confirmed into two records.
        val byTxid = TreeMap.from(
          confirmed.reverse.map(tm => tm.btcTxid.toHex -> tm)
        )
        val successor = mutable.Map.empty[String, String]
        val hasPredecessor = mutable.Set.empty[String]
        byTxid.foreach { case (txid, tm) =>
            tm.sweptInputs.foreach { input =>
                CpoTrieMirror.parseHint(input).foreach { case (prevTx, vout) =>
                    val prev = prevTx.toHex
                    if vout == 0 && byTxid.contains(prev) then {
                        if !successor.contains(prev) then successor.put(prev, txid)
                        hasPredecessor += txid
                    }
                }
            }
        }
        val ordered = Vector.newBuilder[ConfirmedTm]
        val placed = mutable.Set.empty[String]
        byTxid.keys.filterNot(hasPredecessor.contains).foreach { root =>
            @tailrec def walk(cur: Option[String]): Unit = cur match {
                case Some(txid) if placed.add(txid) =>
                    byTxid.get(txid).foreach(ordered += _)
                    walk(successor.get(txid))
                case _ => ()
            }
            walk(Some(root))
        }
        byTxid.foreach { case (txid, tm) => if !placed.contains(txid) then ordered += tm }
        ordered.result()
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
            CompletedPegOutsTrie.committedRoot(tm.outputs) match {
                // A pre-rev-5.1 TM has no commitment output. It also fulfilled nothing under the
                // trie regime, so it moves the root not at all. Backstopped by the final
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
            TreasuryMovementValidator.isRootCommitment(o.scriptPubKey)
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
