package binocular.watchtower

import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF
import scalus.uplc.builtin.ByteString

/** The set-to-root builder shared by the bridge's off-chain MPF mirrors,
  * [[CompletedPegOutsTrie.trieFrom]] and [[SweptPegInsTrie.trieFrom]].
  *
  * Both mirrors face the same problem: entries are resolved from many Treasury Movements, in no
  * particular order, and the same key may be resolved more than once. One implementation serves
  * both, so the two roots can never disagree about how a duplicate key is handled — a divergence
  * there would show up only as a root no TM ever committed.
  */
object MpfSetBuilder {

    /** Build the trie holding exactly `entries`.
      *
      * Insertion ORDER IS IRRELEVANT: an MPF root is a function of the key/value SET, not of the
      * order the keys arrived in. So the caller may pass entries in any order and does not have to
      * reconstruct the TM chain.
      *
      * A key repeated with the SAME value is inserted once. A key repeated with a DIFFERENT value
      * is reported rather than silently resolved: one of the two sources must be wrong, and picking
      * either would produce a root no TM ever committed. `keyLabel` names the key in that report,
      * for example `"POR id"`.
      */
    def trieFrom(
        keyLabel: String,
        entries: Seq[(ByteString, ByteString)]
    ): Either[String, OffChainMPF] = {
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
                      s"$keyLabel ${key.toHex} is recorded twice with different values " +
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
