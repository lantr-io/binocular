package binocular.watchtower

import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF
import scalus.uplc.builtin.{Builtins, ByteString}

/** Off-chain twin of [[TreasuryMovementValidator.foldCompletedPegOuts]]: it turns a Treasury
  * Movement's Bitcoin outputs into completed-peg-outs trie entries, replays the trie, and produces
  * the [[PegOutTrieStep]] list the TM Confirm redeemer carries.
  *
  * Every rule here MUST agree byte for byte with the on-chain fold, so the marker predicate and the
  * POR-id slice are CALLED from [[TreasuryMovementValidator]] rather than re-implemented, and the
  * trie value is built with the same `integerToByteString(false, 8, amount)` encoding that
  * `peg-out.ak`'s Complete branch rebuilds from the request datum.
  *
  * Nothing here touches the network. The command layer supplies the parsed TM outputs.
  */
object CompletedPegOutsTrie {

    /** One `(payment, POR marker)` output pair of a TM, reduced to its trie entry.
      *
      * @param porId
      *   the trie key: the 32 bytes the marker's `OP_RETURN` push commits to.
      * @param value
      *   the trie value: `payment scriptPubKey ++ amount as 8 little-endian bytes`.
      */
    final case class PorPair(porId: ByteString, value: ByteString)

    /** The trie value for a payment output: `scriptPubKey ++ amount_le8`.
      *
      * `peg-out.ak` rebuilds this from `source_chain_destination_address` and
      * `bridged_tokens_locked - per_pegout_fee`, so any change here silently breaks peg-out
      * completion.
      */
    def trieValue(payment: PegOutEntry): ByteString =
        payment.scriptPubKey ++ Builtins.integerToByteString(false, 8, payment.amount)

    /** Reduce ALL outputs of one TM to its trie entries.
      *
      * `outputs` is the full parsed output list, output 0 first. Output 0 is the treasury change
      * and is dropped here — the on-chain fold does the same with `fulfilled.tail`. The remainder
      * must be `[payment, marker, payment, marker, ...]`.
      *
      * Returns Left with the on-chain failure reason for a TM the validator would reject, so the
      * caller can report an unconfirmable TM instead of submitting a doomed transaction.
      */
    def pairsOf(outputs: Seq[PegOutEntry]): Either[String, Vector[PorPair]] =
        if outputs.isEmpty then Left("TM has no outputs (no treasury change output)")
        else walk(outputs.drop(1), Vector.empty)

    @annotation.tailrec
    private def walk(
        rest: Seq[PegOutEntry],
        acc: Vector[PorPair]
    ): Either[String, Vector[PorPair]] =
        rest match {
            case Seq() => Right(acc)
            case Seq(payment, tail*) =>
                if TreasuryMovementValidator.isPorMarker(payment.scriptPubKey) then
                    Left("POR marker in a payment position")
                else
                    tail match {
                        case Seq() => Left("odd output count after the change output")
                        case Seq(marker, more*) =>
                            if !TreasuryMovementValidator.isPorMarker(marker.scriptPubKey) then
                                Left("payment output without a POR marker")
                            else
                                walk(
                                  more,
                                  acc :+ PorPair(
                                    TreasuryMovementValidator.porMarkerId(marker.scriptPubKey),
                                    trieValue(payment)
                                  )
                                )
                    }
        }

    /** Rebuild the trie content from the outputs of every already-Confirmed TM.
      *
      * Insertion ORDER IS IRRELEVANT: an MPF root is a function of the key/value SET, not of the
      * order the keys arrived in. So the caller may pass the Confirmed records in any order and
      * does not have to reconstruct the TM chain. The caller MUST still check the resulting root
      * against the on-chain trie datum — that check is what proves the replay saw every record.
      *
      * A POR id repeated across two records with the SAME value is the on-chain `AlreadyPresent`
      * tolerance and is inserted once. A POR id repeated with a DIFFERENT value cannot exist in a
      * confirmed chain (no step accepts it), so it is reported rather than silently resolved.
      */
    def replay(records: Seq[Seq[PegOutEntry]]): Either[String, OffChainMPF] = {
        // Keyed by hex, not by ByteString, so the de-duplication never depends on ByteString's
        // hashCode contract.
        val merged = scala.collection.mutable.LinkedHashMap.empty[String, PorPair]
        var error: Option[String] = None
        val recs = records.iterator
        while error.isEmpty && recs.hasNext do
            pairsOf(recs.next()) match {
                case Left(err) => error = Some(s"a Confirmed TM record is malformed: $err")
                case Right(pairs) =>
                    val it = pairs.iterator
                    while error.isEmpty && it.hasNext do {
                        val p = it.next()
                        merged.get(p.porId.toHex) match {
                            case Some(seen) if seen.value != p.value =>
                                error = Some(
                                  s"POR id ${p.porId.toHex} is recorded twice with different " +
                                      s"values (${seen.value.toHex} and ${p.value.toHex}) — the " +
                                      "Confirmed TM records cannot all be genuine"
                                )
                            case Some(_) => ()
                            case None    => merged.put(p.porId.toHex, p)
                        }
                    }
            }
        error.toLeft(
          merged.valuesIterator.foldLeft(OffChainMPF.empty)((t, p) => t.insert(p.porId, p.value))
        )
    }

    /** Build the per-pair [[PegOutTrieStep]] list for the TM being confirmed, and the trie the
      * Confirm tx must recreate.
      *
      * Proofs are generated INCREMENTALLY against the intermediate root each step sees, because the
      * on-chain fold applies the steps in order and each `insert` is proven against the root the
      * previous step produced.
      *
      *   - key absent -> `Insert(nonMembershipProof)`, and the trie advances.
      *   - key present with the SAME value -> `AlreadyPresent(membershipProof)`, root unchanged.
      *   - key present with a DIFFERENT value -> Left. No step accepts this, so the TM is
      *     permanently unconfirmable; heimdall's trie-dedup filter is what must prevent it.
      */
    def buildSteps(
        start: OffChainMPF,
        pairs: Seq[PorPair]
    ): Either[String, (List[PegOutTrieStep], OffChainMPF)] = {
        val steps = List.newBuilder[PegOutTrieStep]
        var tree = start
        var error: Option[String] = None
        val it = pairs.iterator
        while error.isEmpty && it.hasNext do {
            val p = it.next()
            tree.get(p.porId) match {
                case None =>
                    steps += PegOutTrieStep.Insert(tree.proveNonMembership(p.porId))
                    tree = tree.insert(p.porId, p.value)
                case Some(v) if v == p.value =>
                    steps += PegOutTrieStep.AlreadyPresent(tree.proveMembership(p.porId))
                case Some(v) =>
                    error = Some(
                      s"POR id ${p.porId.toHex} is already in the completed-peg-outs trie with a " +
                          s"different value (stored ${v.toHex}, this TM pays ${p.value.toHex}) — " +
                          "this TM can never be confirmed"
                    )
            }
        }
        error.toLeft((steps.result(), tree))
    }
}
