package binocular.watchtower

import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF
import scalus.uplc.builtin.{Builtins, ByteString}

/** Off-chain helpers for the completed-peg-outs (CPO) trie.
  *
  * The root is ATTESTED, not derived on-chain: every FROST-signed Treasury Movement carries exactly
  * one `"BTMR1"` two-root commitment output, whose second root is the CPO root that must hold after
  * it; TM Confirm copies it into the bridge-state singleton
  * ([[TreasuryMovementValidator.committedRoots]]). The off-chain reader of both roots is
  * [[SweptPegInsTrie.committedRoots]].
  *
  * [[trieValue]] and [[trieFrom]] are the trie mechanics: the entry encoding `peg-out.ak`'s
  * Complete branch rebuilds, and a set-to-root builder. Nothing in binocular writes the trie
  * (heimdall owns it), but a membership-proof builder for peg-out Complete needs both — it must
  * rebuild the trie from resolved entries before it can prove anything against it.
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

    /** Build the trie holding exactly `entries`, keyed by POR id.
      *
      * A POR id repeated with the SAME value is inserted once (a double-fulfillment records the
      * same completion twice). A POR id repeated with a DIFFERENT value is reported rather than
      * silently resolved. See [[MpfSetBuilder.trieFrom]] for the order-independence and duplicate
      * rules.
      */
    def trieFrom(entries: Seq[(ByteString, ByteString)]): Either[String, OffChainMPF] =
        MpfSetBuilder.trieFrom("POR id", entries)
}
