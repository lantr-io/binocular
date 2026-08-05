package binocular

import binocular.watchtower.{CompletedPegOutsTrie, CpoTrieMirror, PegOutEntry}

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.{Builtins, ByteString}
import scalus.uplc.builtin.Data.toData
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}

import java.nio.file.Files

/** Tests for the watchtower's persistent completed-peg-outs mirror.
  *
  * The mirror is only useful if its root equals the root the chain committed, so everything here is
  * about the two ways that can break: an entry SET that differs from what the TMs recorded, and a
  * state file that is read back as something other than what was written.
  *
  * The proof itself is validated against the real Aiken validator in [[PegOutCompleteCekTest]].
  */
class CpoTrieMirrorTest extends AnyFunSuite {

    private def key(b: Int): ByteString = ByteString.fromArray(Array.fill[Byte](32)(b.toByte))
    private def spk(b: Int): ByteString = ByteString.fromHex("0014" + f"$b%02x" * 20)

    private def value(b: Int, sat: Long): ByteString =
        CompletedPegOutsTrie.trieValue(PegOutEntry(spk(b), BigInt(sat)))

    private def mirror(entries: Seq[(ByteString, ByteString)]): CpoTrieMirror =
        CpoTrieMirror.fromEntries(entries).fold(e => fail(e), identity)

    // --- entry set semantics --------------------------------------------------------------------

    test("the root depends on the entry SET, not on insertion order") {
        // This is what lets the sweeper fold a TM's entries in hint order without reconstructing the
        // order heimdall used.
        val entries = (1 to 6).map(i => key(i) -> value(i, 1000L + i))
        assert(mirror(entries).root == mirror(entries.reverse).root)
    }

    test("re-adding an entry with the same value is a no-op") {
        val m = mirror(Seq(key(1) -> value(1, 1000)))
        val again = m.applied(Seq(key(1) -> value(1, 1000))).fold(e => fail(e), identity)
        assert(again.root == m.root)
        assert(again.size == 1)
    }

    test("the same POR id with a different value is REPORTED, never resolved") {
        // Picking either value would produce a root no TM ever committed, which the sweeper would
        // then submit proofs against.
        val m = mirror(Seq(key(1) -> value(1, 1000)))
        val err =
            m.applied(Seq(key(1) -> value(1, 1001))).swap.getOrElse(fail("expected a conflict"))
        assert(err.contains(key(1).toHex))
    }

    test("applied does not mutate the receiver") {
        val m = mirror(Seq(key(1) -> value(1, 1000)))
        val before = m.root
        val after = m.applied(Seq(key(2) -> value(2, 2000))).fold(e => fail(e), identity)
        assert(m.root == before)
        assert(after.root != before)
        assert(m.rootAfter(Seq(key(2) -> value(2, 2000))) == Right(after.root))
    }

    test("a membership proof is refused for a key the mirror does not hold") {
        val m = mirror(Seq(key(1) -> value(1, 1000)))
        assert(m.proveMembership(key(9)).isLeft)
        assert(m.proveMembership(key(1)).isRight)
    }

    // --- encodings ------------------------------------------------------------------------------

    test("porId is sha2_256 of the serialised OutputReference") {
        val txHash = key(0x33)
        val expected = Builtins.sha2_256(
          Builtins.serialiseData(TxOutRef(TxId(txHash), BigInt(7)).toData)
        )
        assert(CpoTrieMirror.porId(txHash, 7) == expected)
    }

    test("hintBytes is tx hash ++ 4-byte little-endian index, and round-trips") {
        val txHash = key(0x44)
        val hint = CpoTrieMirror.hintBytes(txHash, 258)
        assert(hint.size == 36)
        assert(hint.slice(0, 32) == txHash)
        assert(hint.slice(32, 4) == ByteString.fromHex("02010000"))
        assert(CpoTrieMirror.parseHint(hint) == Some((txHash, 258L)))
    }

    test("a hint of the wrong length is rejected, not guessed at") {
        // Hints are unverified attacker-placeable datum bytes.
        assert(CpoTrieMirror.parseHint(key(1)) == None)
        assert(CpoTrieMirror.parseHint(ByteString.empty) == None)
    }

    // --- persistence ----------------------------------------------------------------------------

    private def tempDir() = Files.createTempDirectory("cpo-mirror-test")

    test("save then load round-trips the entry set and the root") {
        val dir = tempDir()
        val m = mirror((1 to 5).map(i => key(i) -> value(i, 1000L + i)))
        assert(m.save(dir) == Right(()))
        val loaded =
            CpoTrieMirror.load(dir).fold(e => fail(e), identity).getOrElse(fail("no state"))
        assert(loaded.root == m.root)
        assert(loaded.size == m.size)
        assert(loaded.entries.toSet == m.entries.toSet)
    }

    test("a missing state file is a cold start, not an error") {
        assert(CpoTrieMirror.load(tempDir()) == Right(None))
    }

    test("a state file whose entries do not produce its recorded root is REFUSED") {
        // The whole point of storing the root next to the entries: silent corruption becomes a loud
        // failure instead of a stream of on-chain script rejections.
        val dir = tempDir()
        mirror(Seq(key(1) -> value(1, 1000))).save(dir)
        val file = CpoTrieMirror.stateFile(dir)
        val json = ujson.read(Files.readString(file))
        json("root") = ujson.Str(key(0xff).toHex)
        Files.write(file, ujson.write(json).getBytes("UTF-8"))
        val err = CpoTrieMirror.load(dir).swap.getOrElse(fail("expected a corruption error"))
        assert(err.contains("corrupt"))
    }

    test("a state file from an unknown version is REFUSED") {
        val dir = tempDir()
        mirror(Seq(key(1) -> value(1, 1000))).save(dir)
        val file = CpoTrieMirror.stateFile(dir)
        val json = ujson.read(Files.readString(file))
        json("version") = ujson.Num(CpoTrieMirror.StateVersion + 1)
        Files.write(file, ujson.write(json).getBytes("UTF-8"))
        assert(CpoTrieMirror.load(dir).isLeft)
    }

    test("resolveStateDir expands a leading ~ and yields an absolute path") {
        val home = System.getProperty("user.home")
        assert(
          CpoTrieMirror.resolveStateDir("~/binocular-state").toString == s"$home/binocular-state"
        )
        assert(CpoTrieMirror.resolveStateDir("").isAbsolute)
    }
}
