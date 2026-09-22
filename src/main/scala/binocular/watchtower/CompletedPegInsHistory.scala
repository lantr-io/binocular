package binocular.watchtower

import scalus.crypto.trie.MerklePatriciaForestry as MPF
import scalus.uplc.builtin.ByteString

import scala.collection.mutable
import scala.util.Try

/** Recover the completed set without assuming that every swept or spent request completed. Each
  * completion adds one entry. Historical singleton roots identify those transitions; the live
  * singleton root is the final authority. Values come from the reconciled swept trie. This uses the
  * existing history provider, not another HTTP client or a local database.
  */
object CompletedPegInsHistory {
    final case class Reconstructed(tree: MPF)

    def reconstruct(
        history: CpoHistorySource,
        pegInAddress: String,
        completedPegInsAddress: String,
        pegInPolicy: String,
        completedPegInsPolicy: String,
        completedPegInsAsset: String,
        expectedRoot: ByteString,
        swept: MPF
    ): Either[String, Reconstructed] = {
        if expectedRoot == MPF.empty.rootHash then return Right(Reconstructed(MPF.empty))
        for {
            requests <- history.addressHistory(pegInAddress).left.map(_.message)
            singletons <- history.addressHistory(completedPegInsAddress).left.map(_.message)
            // Missing/unreadable datums never imply completion. If they hide a required entry or
            // transition, replay cannot reach the live root and fails closed.
            candidates = requests
                .filter { out =>
                    val tokens = out.assets.filter(_._1.startsWith(pegInPolicy.toLowerCase))
                    tokens.size == 1 && tokens.values.head == 1
                }
                .flatMap(_.inlineDatum.flatMap(d => Try(d.to[PegInDatum]).toOption))
                .map(_.pegInUtxoId)
            roots = singletons
                .filter(_.quantityOf(completedPegInsPolicy, completedPegInsAsset) == 1)
                .flatMap(
                  _.inlineDatum.flatMap(d => Try(d.to[CompletedPegInsMerkleTreeDatum]).toOption)
                )
                .map(_.root)
            result <- replay(expectedRoot, roots, candidates, swept)
        } yield result
    }

    /** Unordered history may contain orphan branches or duplicate pages. Explore only transitions
      * to an observed root, keeping each root once. Work is O(roots * candidate deposits), not a
      * powerset search. Intended for the small demo history; callers may cache a root-matched
      * result in memory, but must never substitute a stale root after a reorg.
      */
    private[binocular] def replay(
        expectedRoot: ByteString,
        historicalRoots: Seq[ByteString],
        candidateIds: Seq[ByteString],
        swept: MPF
    ): Either[String, Reconstructed] = {
        val empty = Reconstructed(MPF.empty)
        val allowed = historicalRoots.toSet + expectedRoot
        val entries = candidateIds.distinct.filter(_.size == 36).flatMap { key =>
            swept.get(key).map(key -> _)
        }
        val pending = mutable.Queue(empty)
        val seen = mutable.Set(empty.tree.rootHash)
        while pending.nonEmpty do {
            val current = pending.dequeue()
            if current.tree.rootHash == expectedRoot then return Right(current)
            entries.foreach { case (key, value) =>
                if current.tree.get(key).isEmpty then {
                    val next = current.tree.insert(key, value)
                    if allowed(next.rootHash) && seen.add(next.rootHash) then
                        pending.enqueue(Reconstructed(next))
                }
            }
        }
        Left(
          s"Cannot reconstruct completed peg-ins at live root ${expectedRoot.toHex}: " +
              "request or singleton history is incomplete, or the swept trie is stale"
        )
    }
}
