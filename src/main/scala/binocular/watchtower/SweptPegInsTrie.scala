package binocular.watchtower

import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry.ProofStep
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF
import scalus.uplc.builtin.ByteString

/** Off-chain helpers for the swept-peg-ins (SPI) trie.
  *
  * The SPI trie answers one question: did this deposit reach the treasury. Its entries come from
  * one place, the inputs of a confirmed Treasury Movement, so the whole trie is a pure function of
  * Bitcoin data and any observer can recompute it ([SPI-2]).
  *
  *   - [SPI-1] every input of a confirmed TM becomes a key, EXCEPT input 0. Input 0 is the treasury
  *     outpoint the TM spends, not a deposit.
  *   - [SPI-3] every entry a TM adds carries that TM's OWN input-0 outpoint as its value, so one
  *     TM's entries all share one value. The sweeping TM's txid cannot be the value: `spi_root`
  *     rides in that same transaction's commitment output, and a txid hashes every output, so a
  *     txid value would need a hash fixed point.
  *   - [SPI-4] a membership proof must be servable to any caller, because a depositor cannot build
  *     one without the whole trie. [CPI-9] peg-in completion verifies that proof on-chain.
  *
  * The root itself is ATTESTED, not derived on-chain: it rides in the TM's single `"BTMR1"`
  * commitment output, which [[TreasuryMovementValidator.committedRoots]] reads.
  *
  * Structured like [[CompletedPegOutsTrie]], and for the same reason: the entry encoding and the
  * set-to-root builder live together, because a membership-proof builder needs both — it must
  * rebuild the trie from resolved entries before it can prove anything against it.
  *
  * Nothing here touches the network. The caller supplies the raw TM bytes.
  */
object SweptPegInsTrie {

    /** The SPI entries a single confirmed TM adds: `(peg_in_utxo_id, sweeping_tm_input_0)`.
      *
      * `rawTm` is the segwit-serialized TM, the same bytes the `Unconfirmed` datum carries. Keys
      * are every input outpoint after the first, in input order, 36 bytes each ([SPI-1]). Every
      * value is the TM's own input-0 outpoint ([SPI-3]).
      *
      * Parsing is delegated to [[TreasuryMovementValidator.allInputOutpoints]], the same function
      * the validator uses, so off-chain and on-chain can never disagree about what the inputs are.
      *
      * A TM that sweeps nothing (treasury input only) yields no entries and leaves the root
      * unchanged. That is normal: such a TM still re-commits both roots.
      */
    def entriesOf(rawTm: ByteString): Seq[(ByteString, ByteString)] =
        TreasuryMovementValidator.allInputOutpoints(rawTm).asScala.toSeq match {
            case treasuryIn +: swept => swept.map(pegInUtxoId => (pegInUtxoId, treasuryIn))
            case _                   => Seq.empty
        }

    /** Build the trie holding exactly `entries`, keyed by peg-in UTxO id.
      *
      * Insertion ORDER IS IRRELEVANT: an MPF root is a function of the key/value SET, not of the
      * order the keys arrived in. So the caller may pass the entries of many TMs in any order and
      * does not have to reconstruct the TM chain.
      *
      * A peg-in UTxO id repeated with the SAME value is inserted once. A peg-in UTxO id repeated
      * with a DIFFERENT value means two TMs claim to have swept one deposit, so one of the two
      * sources must be wrong. That is reported, never resolved: picking either would produce a root
      * no TM ever committed.
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
                      s"peg-in UTxO id ${key.toHex} is recorded as swept twice with different " +
                          s"values (${seen.toHex} and ${value.toHex})"
                    )
                case Some(_) => ()
                case None    => merged.put(key.toHex, (key, value))
            }
        }
        error.toLeft(
          merged.valuesIterator.foldLeft(OffChainMPF.empty)((t, kv) => t.insert(kv._1, kv._2))
        )
    }

    /** An MPF membership proof that `pegInUtxoId` was swept, in the shape [CPI-9] verifies on-chain
      * with `mpf.has(peg_in_utxo_id, sweeping_tm_input_0, proof)` ([SPI-4]).
      *
      * Returns Left when the deposit is not in the trie, so a caller serving a depositor reports
      * "not swept yet" instead of raising. A non-membership proof is deliberately NOT offered here:
      * absence from the SPI trie is not evidence of anything, because the trie only grows.
      */
    def membershipProof(
        trie: OffChainMPF,
        pegInUtxoId: ByteString
    ): Either[String, PList[ProofStep]] =
        if trie.get(pegInUtxoId).isEmpty then
            Left(s"peg-in UTxO id ${pegInUtxoId.toHex} is not in the swept peg-ins trie")
        else Right(trie.proveMembership(pegInUtxoId))
}
