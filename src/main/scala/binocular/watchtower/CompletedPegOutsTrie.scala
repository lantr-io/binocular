package binocular.watchtower

import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF
import scalus.uplc.builtin.{Builtins, ByteString}

/** Off-chain helpers for the completed-peg-outs (CPO) trie.
  *
  * Since rev 5.1 the trie root is ATTESTED, not derived on-chain: every FROST-signed Treasury
  * Movement carries exactly one `"CPOR1"` OP_RETURN output holding the root that must hold after
  * it, and the TM Confirm transition copies that root into the CPO singleton
  * ([[TreasuryMovementValidator.committedRoot]]). So the confirm builder no longer produces MPF
  * proofs; it only has to read the same root the validator will read.
  *
  * [[committedRoot]] is that reader. It MUST accept exactly the outputs the on-chain rule accepts,
  * so the commitment predicate is CALLED from [[TreasuryMovementValidator]] rather than
  * re-implemented here.
  *
  * [[trieValue]] and [[trieFrom]] are the remaining trie mechanics: the entry encoding
  * `peg-out.ak`'s Complete branch rebuilds, and a set-to-root builder. Nothing in binocular writes
  * the trie today (heimdall owns it), but a membership-proof builder for peg-out Complete needs
  * both — it must rebuild the trie from resolved entries before it can prove anything against it.
  *
  * Nothing here touches the network. The command layer supplies the parsed TM outputs.
  */
object CompletedPegOutsTrie {

    /** The trie value for a payment output: `scriptPubKey ++ amount_le8`.
      *
      * `peg-out.ak` rebuilds this from `source_chain_destination_address` and
      * `bridged_tokens_locked - per_pegout_fee`, so any change here silently breaks peg-out
      * completion.
      */
    def trieValue(payment: PegOutEntry): ByteString =
        payment.scriptPubKey ++ Builtins.integerToByteString(false, 8, payment.amount)

    /** The completed-peg-outs root a TM attests, read from its parsed outputs.
      *
      * Same rule as the on-chain [[TreasuryMovementValidator.committedRoot]]: `outputs` is the FULL
      * output list (treasury change included), a commitment is a 39-byte `scriptPubKey` with the
      * `"CPOR1"` prefix, EXACTLY ONE must be present in any position, and the root is bytes [7,
      * 39).
      *
      * Returns Left with the reason a TM the validator would reject, so the caller can report an
      * unconfirmable TM instead of submitting a doomed transaction.
      */
    def committedRoot(outputs: Seq[PegOutEntry]): Either[String, ByteString] =
        outputs.map(_.scriptPubKey).filter(TreasuryMovementValidator.isRootCommitment) match {
            case Seq(spk) =>
                Right(
                  spk.slice(
                    TreasuryMovementValidator.RootCommitmentPrefixLength,
                    TreasuryMovementValidator.RootLength
                  )
                )
            case Seq() => Left("missing root commitment (no \"CPOR1\" OP_RETURN output)")
            case many  => Left(s"multiple root commitments (${many.size} \"CPOR1\" outputs)")
        }

    /** Build the trie holding exactly `entries`, keyed by POR id.
      *
      * Insertion ORDER IS IRRELEVANT: an MPF root is a function of the key/value SET, not of the
      * order the keys arrived in. So the caller may pass entries in any order and does not have to
      * reconstruct the TM chain.
      *
      * A POR id repeated with the SAME value is inserted once (a double-fulfillment records the
      * same completion twice). A POR id repeated with a DIFFERENT value is reported rather than
      * silently resolved: one of the two sources must be wrong, and picking either would produce a
      * root no TM ever committed.
      */
    def trieFrom(entries: Seq[(ByteString, ByteString)]): Either[String, OffChainMPF] = {
        // Keyed by hex, not by ByteString, so the de-duplication never depends on ByteString's
        // hashCode contract.
        val merged = scala.collection.mutable.LinkedHashMap.empty[String, (ByteString, ByteString)]
        var error: Option[String] = None
        val it = entries.iterator
        while error.isEmpty && it.hasNext do {
            val (key, value) = it.next()
            merged.get(key.toHex) match {
                case Some((_, seen)) if seen != value =>
                    error = Some(
                      s"POR id ${key.toHex} is recorded twice with different values " +
                          s"(${seen.toHex} and ${value.toHex})"
                    )
                case Some(_) => ()
                case None    => merged.put(key.toHex, (key, value))
            }
        }
        error.toLeft(
          merged.valuesIterator.foldLeft(OffChainMPF.empty)((t, kv) => t.insert(kv._1, kv._2))
        )
    }
}
