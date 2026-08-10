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

    /** Both roots a TM attests — `(spi_root, cpo_root)` — read from its parsed outputs.
      *
      * Same rule as the on-chain [[TreasuryMovementValidator.committedRoots]] ([CTM-26]): `outputs`
      * is the FULL output list (treasury change included), a commitment is a 71-byte `scriptPubKey`
      * with the `"BTMR1"` prefix, EXACTLY ONE must be present in any position, `spi_root` is bytes
      * [7, 39) and `cpo_root` bytes [39, 71).
      *
      * Returns Left with the reason for a TM the validator would reject, so the caller reports an
      * unconfirmable TM instead of submitting a doomed transaction.
      */
    def committedRoots(outputs: Seq[PegOutEntry]): Either[String, (ByteString, ByteString)] =
        outputs.map(_.scriptPubKey).filter(TreasuryMovementValidator.isTwoRootCommitment) match {
            case Seq(spk) =>
                Right(
                  (
                    spk.slice(
                      TreasuryMovementValidator.TwoRootCommitmentPrefixLength,
                      TreasuryMovementValidator.RootLength
                    ),
                    spk.slice(
                      TreasuryMovementValidator.CpoRootOffset,
                      TreasuryMovementValidator.RootLength
                    )
                  )
                )
            case Seq() => Left("missing two-root commitment (no \"BTMR1\" OP_RETURN output)")
            case many  => Left(s"multiple two-root commitments (${many.size} \"BTMR1\" outputs)")
        }

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
      * A peg-in UTxO id repeated with the SAME value is inserted once (two sources reporting one
      * sweep). A peg-in UTxO id repeated with a DIFFERENT value means two TMs claim to have swept
      * one deposit, so one of the two sources must be wrong; that is reported, never resolved. See
      * [[MpfSetBuilder.trieFrom]] for the order-independence and duplicate rules.
      */
    def trieFrom(entries: Seq[(ByteString, ByteString)]): Either[String, OffChainMPF] =
        MpfSetBuilder.trieFrom("swept peg-in UTxO id", entries)

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
