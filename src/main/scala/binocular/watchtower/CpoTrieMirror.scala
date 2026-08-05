package binocular.watchtower

import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry.ProofStep
import scalus.cardano.onchain.plutus.prelude.List as ScalusList
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}
import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF
import scalus.uplc.builtin.{Builtins, ByteString}
import scalus.uplc.builtin.Data.toData

import java.nio.file.{Files, Path, Paths, StandardCopyOption}

/** Atomic JSON state files, shared by the trie mirror and the pending-hint queue.
  *
  * Atomicity is not decoration here: both files are rewritten on every confirm, and a torn one is
  * read back at the next start as a truncated trie whose root matches nothing — the exact state
  * that halts sweeping. Temp file + `ATOMIC_MOVE` makes a reader see either the old file or the new
  * one.
  */
object JsonState {

    def write(target: Path, json: ujson.Value): Either[String, Unit] =
        try {
            val dir = target.getParent
            Files.createDirectories(dir)
            val tmp = Files.createTempFile(dir, s"${target.getFileName}-", ".tmp")
            Files.write(tmp, ujson.write(json, indent = 2).getBytes("UTF-8"))
            Files.move(
              tmp,
              target,
              StandardCopyOption.REPLACE_EXISTING,
              StandardCopyOption.ATOMIC_MOVE
            )
            Right(())
        } catch {
            case e: Exception => Left(s"writing $target: ${e.getMessage}")
        }

    def read(source: Path): Either[String, ujson.Value] =
        try Right(ujson.read(Files.readString(source)))
        catch { case e: Exception => Left(s"reading $source: ${e.getMessage}") }
}

/** The watchtower's local mirror of the completed-peg-outs (CPO) trie.
  *
  * The on-chain artifact is only a 32-byte root. To build the membership proof `peg-out.ak`'s
  * Complete branch needs, a completer must hold the whole key/value SET the root commits to. Nobody
  * publishes that set, so every completer reconstructs it from chain data and keeps it in step.
  *
  * ==Why a mirror and not a fresh reconstruction per sweep==
  * Reconstruction reads the full history of two addresses. Doing it once per confirm would make the
  * sweeper's cost grow with the age of the bridge. The mirror is reconstructed ONCE (cold start)
  * and then advanced by the entries of each confirmed TM.
  *
  * ==The invariant that makes it safe==
  * An MPF root is a function of the key/value SET, not of insertion order, so the mirror is correct
  * iff its set is correct. After every update the mirror's root MUST equal the root the confirmed
  * TM attested. If it does not, the mirror's set is wrong and every proof built from it would be
  * rejected on-chain — so the caller stops sweeping rather than submitting. Nothing here decides
  * that policy; [[applied]] simply reports the resulting root and lets the caller compare.
  *
  * The state file records the ENTRIES, not the trie structure: it is smaller, human-inspectable,
  * and verifiable — [[load]] rebuilds the trie and refuses a file whose recorded root does not
  * match what its own entries produce.
  */
final class CpoTrieMirror private (
    private val byKey: Map[String, (ByteString, ByteString)],
    val trie: OffChainMPF
) {

    /** The root this mirror currently holds — what an on-chain CPO singleton should carry. */
    def root: ByteString = trie.rootHash

    /** Number of completed peg-outs recorded. */
    def size: Int = byKey.size

    def contains(porId: ByteString): Boolean = byKey.contains(porId.toHex)

    /** The trie value recorded for `porId` (`dest_spk ++ amount_le8`), if any. */
    def valueOf(porId: ByteString): Option[ByteString] = byKey.get(porId.toHex).map(_._2)

    /** Every recorded entry, in no particular order — the backing map is unordered, and the trie
      * root is a function of the entry SET, so nothing downstream may depend on the order. It is
      * what the state file stores.
      */
    def entries: Seq[(ByteString, ByteString)] = byKey.valuesIterator.toSeq

    /** The membership proof `peg-out.ak` verifies with `mpf.has(root, por_id, value, proof)`.
      *
      * Left when the key is absent: an absent key means this POR is NOT in the paid set, so no
      * proof exists and no Complete transaction can be built for it.
      */
    def proveMembership(porId: ByteString): Either[String, ScalusList[ProofStep]] =
        if !contains(porId) then
            Left(
              s"POR id ${porId.toHex} is not in the mirror (${size} entries, root ${root.toHex})"
            )
        else
            try Right(trie.proveMembership(porId))
            catch {
                case e: Exception =>
                    Left(s"building the membership proof for ${porId.toHex}: ${e.getMessage}")
            }

    /** This mirror with `newEntries` added, or the reason they cannot be added.
      *
      * Re-adding an entry with the SAME value is a no-op (a TM record confirmed twice, a hint that
      * repeats an outpoint). Re-adding a POR id with a DIFFERENT value is an ERROR, never a
      * last-writer-wins: one of the two sources must be wrong and either choice yields a root no TM
      * ever committed.
      *
      * Pure — the receiver is unchanged, so a caller can test a candidate batch with [[rootAfter]]
      * before committing to it.
      */
    def applied(newEntries: Seq[(ByteString, ByteString)]): Either[String, CpoTrieMirror] = {
        var acc = byKey
        var t = trie
        var error: Option[String] = None
        val it = newEntries.iterator
        while error.isEmpty && it.hasNext do {
            val (key, value) = it.next()
            acc.get(key.toHex) match {
                case Some((_, seen)) if seen != value =>
                    error = Some(
                      s"POR id ${key.toHex} is recorded twice with different values " +
                          s"(${seen.toHex} and ${value.toHex})"
                    )
                case Some(_) => ()
                case None =>
                    acc = acc.updated(key.toHex, (key, value))
                    t = t.insert(key, value)
            }
        }
        error.toLeft(new CpoTrieMirror(acc, t))
    }

    /** The root this mirror would hold after `newEntries`, without changing it. */
    def rootAfter(newEntries: Seq[(ByteString, ByteString)]): Either[String, ByteString] =
        applied(newEntries).map(_.root)

    /** Write the mirror to `stateDir`, atomically (temp file + ATOMIC_MOVE).
      *
      * Atomicity matters because the sweeper writes after every confirm: a torn file would be read
      * back at the next start as a trie with a missing tail, whose root matches nothing.
      */
    def save(stateDir: Path): Either[String, Unit] =
        JsonState.write(
          CpoTrieMirror.stateFile(stateDir),
          ujson.Obj(
            "version" -> ujson.Num(CpoTrieMirror.StateVersion),
            "root" -> ujson.Str(root.toHex),
            "entries" -> ujson.Arr(
              entries.map { case (k, v) =>
                  ujson.Obj("por_id" -> ujson.Str(k.toHex), "value" -> ujson.Str(v.toHex))
              }*
            )
          )
        )

    override def toString: String = s"CpoTrieMirror(root=${root.toHex}, entries=$size)"
}

object CpoTrieMirror {

    /** State-file format version. Bump when the on-disk shape changes; [[load]] refuses anything
      * else rather than mis-reading an older file into a wrong-rooted trie.
      */
    val StateVersion = 1

    val StateFileName = "cpo-trie.json"

    def stateFile(stateDir: Path): Path = stateDir.resolve(StateFileName)

    val empty: CpoTrieMirror = new CpoTrieMirror(Map.empty, OffChainMPF.empty)

    /** Build a mirror holding exactly `entries` (order irrelevant; conflicts reported). */
    def fromEntries(entries: Seq[(ByteString, ByteString)]): Either[String, CpoTrieMirror] =
        empty.applied(entries)

    /** POR id = `sha2_256(serialise_data(OutputReference))` over the request UTxO's OWN outpoint.
      *
      * This is Aiken `bifrost/utils.hash_output_ref`, which `peg-out.ak` recomputes on-chain from
      * `peg_out_input.output_reference`. The `TxOutRef` Data encoding must therefore match the
      * Plutus V3 ledger form exactly (`Constr 0 [B txid, I index]`) — the same expression
      * [[PegInContract.assetName]] already relies on.
      */
    def porId(txHash: ByteString, outputIndex: Long): ByteString =
        Builtins.sha2_256(
          Builtins.serialiseData(TxOutRef(TxId(txHash), BigInt(outputIndex)).toData)
        )

    /** The 36-byte data-availability hint encoding of a Cardano outpoint: tx hash (32 bytes) ++
      * output index as 4 little-endian bytes. This is what a TM `Unconfirmed` datum's
      * `fulfilled_por_outpoints` carries.
      */
    def hintBytes(txHash: ByteString, outputIndex: Long): ByteString = {
        val idx = outputIndex & 0xffffffffL
        txHash ++ ByteString.fromArray(
          Array(idx, idx >> 8, idx >> 16, idx >> 24).map(b => (b & 0xff).toByte)
        )
    }

    /** Inverse of [[hintBytes]]. `None` on any length other than 36 — a hint is unverified,
      * attacker-placeable data, so a malformed entry is rejected rather than guessed at.
      */
    def parseHint(b: ByteString): Option[(ByteString, Long)] =
        if b.size != 36 then None
        else {
            val bytes = b.bytes
            val idx = (bytes(32) & 0xffL) | ((bytes(33) & 0xffL) << 8) |
                ((bytes(34) & 0xffL) << 16) | ((bytes(35) & 0xffL) << 24)
            Some((ByteString.fromArray(bytes.take(32)), idx))
        }

    /** Read the mirror from `stateDir`.
      *
      * `Right(None)` means no state file exists — a cold start, which the caller answers with
      * reconstruction. `Left` means a file exists but cannot be trusted: wrong version,
      * unparseable, or (the important one) a recorded root that its own entries do not reproduce.
      * Rebuilding the trie from the entries and comparing is what turns a silently corrupted file
      * into a loud failure instead of a stream of rejected completions.
      */
    def load(stateDir: Path): Either[String, Option[CpoTrieMirror]] = {
        val file = stateFile(stateDir)
        if !Files.isReadable(file) then Right(None)
        else
            try {
                val json = JsonState.read(file).fold(e => throw new RuntimeException(e), identity)
                val version = json.obj.get("version").map(_.num.toInt).getOrElse(-1)
                if version != StateVersion then
                    Left(
                      s"$file has state version $version, expected $StateVersion — delete it to " +
                          "force a fresh reconstruction"
                    )
                else {
                    val recorded = ByteString.fromHex(json("root").str)
                    val parsed = json("entries").arr.toSeq.map { e =>
                        (ByteString.fromHex(e("por_id").str), ByteString.fromHex(e("value").str))
                    }
                    fromEntries(parsed).flatMap { mirror =>
                        if mirror.root != recorded then
                            Left(
                              s"$file records root ${recorded.toHex} but its ${mirror.size} " +
                                  s"entries produce ${mirror.root.toHex} — the file is corrupt; " +
                                  "delete it to force a fresh reconstruction"
                            )
                        else Right(Some(mirror))
                    }
                }
            } catch {
                case e: Exception => Left(s"reading $file: ${e.getMessage}")
            }
    }

    /** Resolve the configured state directory, expanding a leading `~`. */
    def resolveStateDir(configured: String): Path = {
        val trimmed = Option(configured).map(_.trim).filter(_.nonEmpty).getOrElse(".binocular")
        val expanded =
            if trimmed == "~" then System.getProperty("user.home")
            else if trimmed.startsWith("~/") then System.getProperty("user.home") + trimmed.drop(1)
            else trimmed
        Paths.get(expanded).toAbsolutePath
    }
}
