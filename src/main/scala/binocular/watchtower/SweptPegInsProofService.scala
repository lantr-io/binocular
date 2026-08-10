package binocular.watchtower

import binocular.bitcoin.{BitcoinHelpers, SimpleBitcoinRpc}
import binocular.oracle.reverse

import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry.ProofStep
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF
import scalus.uplc.builtin.ByteString

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration
import scala.util.Try
import scalus.utils.await

/** The swept-peg-ins membership proof server ([SPI-4]).
  *
  * Given a `peg_in_utxo_id` (a 36-byte Bitcoin outpoint), this service produces the MPF membership
  * proof that `peg_in.ak`'s [CPI-9] check verifies on-chain with
  * `mpf.has(spi_root, peg_in_utxo_id, sweeping_tm_input_0, proof)`, together with the proven value
  * (the sweeping TM's input-0 outpoint) the [CPI-9] redeemer must carry. [SPI-4] names binocular as
  * the proof server, and [OB-13] extends the same terms to the deposit-inclusion bundle: any caller
  * may ask, no trust is involved, because a wrong proof simply fails `mpf.has` on-chain.
  *
  * ==How the swept set is derived, and the confirmed/unconfirmed boundary ([SPI-6])==
  *
  * [SPI-6] requires deriving the swept set by walking the Bitcoin treasury chain and reconciling it
  * against the singleton's confirm history. The treasury is a linear spend chain: each TM spends
  * the previous TM's output 0, and the single `"BTMR1"` commitment output identifies a protocol TM.
  * Walking that chain and taking every input except input 0 ([SPI-1]) yields the swept set from
  * Bitcoin alone.
  *
  * Bitcoin alone is a SUPERSET, though: it reports what was swept, while `spi_root` only advances
  * at Confirm on Cardano. A proof served for an entry whose TM has not yet confirmed would fail
  * on-chain. The reconciliation here is structural: the walk STARTS at the singleton's
  * `treasury_utxo_id` — the head the last Confirm wrote — and follows input-0 ancestry BACKWARD. A
  * TM that is mined on Bitcoin but not yet confirmed on Cardano spends the head, so the backward
  * walk never visits it. The set this walk produces is exactly the set the current on-chain
  * `spi_root` attests, which the per-TM and final root checks then prove:
  *
  *   - after replaying each TM's entries, the running root must equal the `spi_root` that TM's own
  *     `"BTMR1"` output committed — a mismatch names the offending TM;
  *   - the final root must equal the singleton's attested `spi_root` — otherwise NOTHING is served,
  *     because every proof against a different root is useless on-chain.
  *
  * An entry that is absent from the confirmed set is answered with [[NotInConfirmedSet]], never
  * with a proof: the deposit was either never swept, or swept by a TM whose Confirm has not
  * happened yet. This service cannot tell those two apart (and does not need to — the caller
  * retries after the next Confirm).
  *
  * Nothing here trusts the Bitcoin node either: the fetched bytes are re-hashed against the
  * requested txid, and the resulting trie is checked against the quorum-attested root.
  */
object SweptPegInsProofService {

    /** One confirmed TM as the walk recovers it from Bitcoin.
      *
      * @param txidLE
      *   the TM's txid, internal (little-endian) byte order.
      * @param committedSpiRoot
      *   the `spi_root` the TM's own `"BTMR1"` commitment output attests (bytes [7, 39)).
      * @param entries
      *   the SPI entries this TM adds: `(peg_in_utxo_id -> its own input-0 outpoint)`, per
      *   [SPI-1]/[SPI-3].
      */
    final case class ConfirmedTm(
        txidLE: ByteString,
        committedSpiRoot: ByteString,
        entries: Seq[(ByteString, ByteString)]
    )

    /** Everything the [CPI-9] redeemer needs from the proof server: the key, the proven value
      * (`sweeping_tm_input_0`), the root the proof verifies against, and the proof itself.
      */
    final case class SpiMembershipProof(
        pegInUtxoId: ByteString,
        sweepingTmInput0: ByteString,
        spiRoot: ByteString,
        proof: PList[ProofStep]
    )

    sealed trait ServeError extends Product with Serializable {
        def message: String
    }

    /** The request itself is malformed — nothing was looked up. */
    final case class InvalidRequest(message: String) extends ServeError

    /** The treasury-chain walk or the replay failed. No proof can be served for ANY key until the
      * cause is fixed: a partial set would produce roots the chain never attested.
      */
    final case class WalkFailed(message: String) extends ServeError

    /** The set derived from Bitcoin does not reproduce the singleton's attested `spi_root`. The
      * service REFUSES to serve rather than guessing: a proof against a root the chain does not
      * hold is useless, and inventing entries to force a match would be forgery.
      */
    final case class RootMismatch(derivedRoot: ByteString, attestedRoot: ByteString)
        extends ServeError {
        def message: String =
            s"the swept set derived from the Bitcoin treasury chain yields spi_root " +
                s"${derivedRoot.toHex}, but the on-chain singleton attests ${attestedRoot.toHex} " +
                "— refusing to serve proofs the chain would reject"
    }

    /** THE [SPI-6] BOUNDARY: the deposit is not in the set the current on-chain `spi_root`
      * contains. It was either never swept, or swept by a TM that has not yet confirmed on Cardano
      * — in which case a proof would exist on a Bitcoin-only view but MUST NOT be served, because
      * it fails [CPI-9] until that TM's Confirm advances the root.
      */
    final case class NotInConfirmedSet(pegInUtxoId: ByteString, spiRoot: ByteString)
        extends ServeError {
        def message: String =
            s"peg-in UTxO id ${pegInUtxoId.toHex} is not in the CONFIRMED swept set (on-chain " +
                s"spi_root ${spiRoot.toHex}). Either the deposit was never swept, or its sweeping " +
                "TM has not yet confirmed on Cardano — retry after the next Confirm ([SPI-6])"
    }

    /** Walk-depth bound so a cyclic or absurd fetcher cannot loop forever. Reconstruction is an
      * operator-visible operation, so a bounded walk that reports failure beats one that hangs.
      */
    val MaxWalkDepth: Int = 100_000

    /** Walk the Bitcoin treasury chain BACKWARD from the singleton's confirmed head, returning the
      * confirmed TMs OLDEST FIRST.
      *
      * `headUtxoId` is the singleton's `treasury_utxo_id` (36 bytes, `btc_txid ‖ vout LE`).
      * `fetchRawTx` maps an internal (little-endian) txid to the raw transaction bytes; `None`
      * aborts the walk — a gap would silently drop every older TM's entries.
      *
      * Termination: the first ancestor WITHOUT a `"BTMR1"` commitment output is the genesis funding
      * transaction (or the bootstrap origin), not a TM. Its inputs are not swept deposits, so the
      * walk stops BEFORE harvesting it. A transaction with several commitment outputs is malformed
      * ([CTM-26] admits exactly one) and aborts the walk.
      */
    def walkConfirmedChain(
        headUtxoId: ByteString,
        fetchRawTx: ByteString => Option[ByteString],
        maxDepth: Int = MaxWalkDepth
    ): Either[String, Seq[ConfirmedTm]] = {
        if headUtxoId.size != 36 then
            return Left(
              s"the head outpoint must be 36 bytes (txid ++ vout LE), got ${headUtxoId.size}"
            )
        var cursorTxid = ByteString.fromArray(headUtxoId.bytes.take(32))
        var acc = List.empty[ConfirmedTm] // prepend while walking newest -> oldest = oldest first
        var depth = 0
        var done = false
        var error: Option[String] = None
        while !done && error.isEmpty do {
            if depth >= maxDepth then
                error = Some(
                  s"treasury chain walk exceeded $maxDepth transactions without reaching the " +
                      "genesis funding transaction"
                )
            else
                fetchRawTx(cursorTxid) match {
                    case None =>
                        error = Some(
                          s"Bitcoin tx ${cursorTxid.reverse.toHex} is not retrievable — cannot " +
                              "continue the treasury chain walk (a gap would drop every older " +
                              "TM's entries)"
                        )
                    case Some(raw) =>
                        // Never trust the fetcher: the bytes must hash to the txid asked for.
                        if Try(BitcoinHelpers.getTxHash(raw)).toOption.contains(cursorTxid) then {
                            val commitments = TreasuryMovementValidator
                                .allOutputs(raw)
                                .asScala
                                .toSeq
                                .map(_.scriptPubKey)
                                .filter(TreasuryMovementValidator.isTwoRootCommitment)
                            commitments match {
                                case Seq() =>
                                    // Not a TM: the genesis funding tx / bootstrap origin. Stop
                                    // WITHOUT harvesting — its inputs are not swept deposits.
                                    done = true
                                case Seq(spk) =>
                                    val spiRoot = spk.slice(
                                      TreasuryMovementValidator.TwoRootCommitmentPrefixLength,
                                      TreasuryMovementValidator.RootLength
                                    )
                                    val inputs = TreasuryMovementValidator
                                        .allInputOutpoints(raw)
                                        .asScala
                                        .toSeq
                                    inputs.headOption match {
                                        case None =>
                                            error = Some(
                                              s"TM ${cursorTxid.reverse.toHex} has no inputs"
                                            )
                                        case Some(input0) if input0.size != 36 =>
                                            error = Some(
                                              s"TM ${cursorTxid.reverse.toHex} has a malformed " +
                                                  s"input-0 outpoint (${input0.size} bytes)"
                                            )
                                        case Some(input0) =>
                                            acc = ConfirmedTm(
                                              cursorTxid,
                                              spiRoot,
                                              SweptPegInsTrie.entriesOf(raw)
                                            ) :: acc
                                            cursorTxid = ByteString.fromArray(input0.bytes.take(32))
                                            depth += 1
                                    }
                                case many =>
                                    error = Some(
                                      s"tx ${cursorTxid.reverse.toHex} carries ${many.size} " +
                                          "\"BTMR1\" commitment outputs — a well-formed TM has " +
                                          "exactly one ([CTM-26]); refusing to walk past it"
                                    )
                            }
                        } else
                            error = Some(
                              s"fetcher returned bytes that do not hash to the requested txid " +
                                  s"${cursorTxid.reverse.toHex}"
                            )
                }
        }
        error.toLeft(acc)
    }

    /** Derive the CONFIRMED swept-peg-ins trie: walk from the singleton's head, replay oldest
      * first, and reconcile against the attestations ([SPI-6]).
      *
      * Two checks turn the replay from trust into search-and-check:
      *   - after each TM, the running root must equal that TM's own committed `spi_root`;
      *   - the final root must equal `attestedSpiRoot` (the singleton's field), otherwise
      *     [[RootMismatch]] and nothing is served.
      */
    def confirmedTrie(
        attestedSpiRoot: ByteString,
        headUtxoId: ByteString,
        fetchRawTx: ByteString => Option[ByteString],
        maxDepth: Int = MaxWalkDepth
    ): Either[ServeError, OffChainMPF] =
        walkConfirmedChain(headUtxoId, fetchRawTx, maxDepth).left
            .map(WalkFailed.apply)
            .flatMap { tms =>
                var trie = OffChainMPF.empty
                // Keyed by hex, like MpfSetBuilder: the duplicate check must not depend on
                // ByteString's hashCode contract. A Bitcoin outpoint is spent once, so a duplicate
                // key with a different value means the walk (or the node) is lying.
                val seen = mutable.Map.empty[String, ByteString]
                var error: Option[ServeError] = None
                val it = tms.iterator
                while error.isEmpty && it.hasNext do {
                    val tm = it.next()
                    val entryIt = tm.entries.iterator
                    while error.isEmpty && entryIt.hasNext do {
                        val (key, value) = entryIt.next()
                        seen.get(key.toHex) match {
                            case Some(prev) if prev != value =>
                                error = Some(
                                  WalkFailed(
                                    s"deposit ${key.toHex} is claimed by two TMs with different " +
                                        s"input-0 outpoints (${prev.toHex} and ${value.toHex})"
                                  )
                                )
                            case Some(_) => ()
                            case None =>
                                seen.put(key.toHex, value)
                                trie = trie.insert(key, value)
                        }
                    }
                    if error.isEmpty && trie.rootHash != tm.committedSpiRoot then
                        error = Some(
                          WalkFailed(
                            s"replaying TM ${tm.txidLE.reverse.toHex} yields spi_root " +
                                s"${trie.rootHash.toHex}, but its \"BTMR1\" output commits " +
                                s"${tm.committedSpiRoot.toHex}"
                          )
                        )
                }
                error.toLeft(trie).flatMap { t =>
                    if t.rootHash != attestedSpiRoot then
                        Left(RootMismatch(t.rootHash, attestedSpiRoot))
                    else Right(t)
                }
            }

    /** Serve the [CPI-9] membership proof for one deposit outpoint, or say precisely why not.
      *
      * The [SPI-6] boundary is enforced HERE: the trie this serves from contains exactly the
      * entries the current on-chain `spi_root` attests (see [[confirmedTrie]]), so an entry swept
      * on Bitcoin by a not-yet-confirmed TM is answered with [[NotInConfirmedSet]], never with a
      * proof that would fail on-chain.
      */
    def serve(
        state: BridgeState,
        fetchRawTx: ByteString => Option[ByteString],
        pegInUtxoId: ByteString,
        maxDepth: Int = MaxWalkDepth
    ): Either[ServeError, SpiMembershipProof] =
        if pegInUtxoId.size != 36 then
            Left(
              InvalidRequest(
                s"peg_in_utxo_id must be 36 bytes (txid ++ vout LE), got ${pegInUtxoId.size}"
              )
            )
        else
            confirmedTrie(state.spiRoot, state.treasuryUtxoId, fetchRawTx, maxDepth).flatMap {
                trie =>
                    trie.get(pegInUtxoId) match {
                        case None =>
                            Left(NotInConfirmedSet(pegInUtxoId, state.spiRoot))
                        case Some(sweepingTmInput0) =>
                            Right(
                              SpiMembershipProof(
                                pegInUtxoId = pegInUtxoId,
                                sweepingTmInput0 = sweepingTmInput0,
                                spiRoot = trie.rootHash,
                                proof = trie.proveMembership(pegInUtxoId)
                              )
                            )
                    }
            }

    /** A tx fetcher backed by a Bitcoin node. Requires `txindex=1` on bitcoind, because the walk
      * retrieves arbitrary confirmed transactions by txid.
      */
    def rpcFetcher(
        rpc: SimpleBitcoinRpc,
        timeout: Duration
    )(using ExecutionContext): ByteString => Option[ByteString] =
        txidLE =>
            Try(rpc.getRawTransaction(txidLE.reverse.toHex).await(timeout)).toOption
                .map(info => ByteString.fromHex(info.hex))
}
