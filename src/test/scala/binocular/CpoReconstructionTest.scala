package binocular

import binocular.bitcoin.BitcoinHelpers
import binocular.oracle.reverse
import binocular.watchtower.*

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.cardano.onchain.plutus.v3.PubKeyHash
import scalus.uplc.builtin.Builtins.integerToByteString
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData

/** Tests for the cold-start / recovery path: rebuilding the completed-peg-outs trie from Cardano
  * history alone.
  *
  * They mirror the invariants of heimdall's `cpo_trie.rs::reconstruct`, which is the reference
  * implementation of the same algorithm:
  *
  *   - no silent skips at the TM address,
  *   - the running root is asserted after every TM,
  *   - every published hint is tried, so a hostile one only costs time,
  *   - the finished trie is cross-checked against the on-chain singleton.
  *
  * The history source is a fake, so nothing here touches a network; [[BlockfrostCpoHistory]] only
  * decides where the same `ChainOutput` values come from.
  */
class CpoReconstructionTest extends AnyFunSuite {

    private val tmAddress = "addr_test_tm"
    private val pegOutAddress = "addr_test_pegout"
    private val fbtcPolicy = "a1" * 28
    private val fbtcAsset = "66534154" // "fSAT"

    private def filled(v: Int, n: Int): ByteString =
        ByteString.fromArray(Array.fill[Byte](n)(v.toByte))

    private def spk(b: Int): ByteString = ByteString.fromHex("0014" + f"$b%02x" * 20)

    /** A fake source: outputs keyed by address, in the order history would return them. */
    private final class FakeHistory(byAddress: Map[String, Seq[ChainOutput]])
        extends CpoHistorySource {
        override def backend: String = "fake"
        override def addressHistory(address: String): Either[HistoryError, Seq[ChainOutput]] =
            Right(byAddress.getOrElse(address, Seq.empty))
    }

    // --- Bitcoin TM fixtures --------------------------------------------------------------------

    /** `OP_RETURN OP_PUSHBYTES_37 "CPOR1" <root>` — the 39-byte commitment scriptPubKey. */
    private def commitment(root: ByteString): ByteString =
        ByteString.fromHex("6a2543504f5231") ++ root

    /** A 1-input segwit transaction with the given outputs. `inOutpoint` is the treasury input, so
      * a TM can be chained onto its predecessor's output 0.
      */
    private def rawTx(inOutpoint: ByteString, outs: Seq[(ByteString, BigInt)]): ByteString = {
        val outsHex = outs.map { case (s, amt) =>
            integerToByteString(false, 8, amt).toHex + f"${s.size}%02x" + s.toHex
        }.mkString
        ByteString.fromHex(
          "02000000" + "0001" + "01" + inOutpoint.toHex + "00" + "ffffffff" +
              f"${outs.size}%02x" + outsHex + "00" + "00000000"
        )
    }

    private val genesisOutpoint = ByteString.fromHex(("aa" * 32) + "00000000")

    /** Bitcoin outpoint of a TM's own treasury output 0 — what the next TM spends. */
    private def treasuryOutpointOf(raw: ByteString): ByteString =
        BitcoinHelpers.getTxHash(raw) ++ ByteString.fromHex("00000000")

    // --- Cardano fixtures -----------------------------------------------------------------------

    private def confirmedOutput(raw: ByteString, index: Long = 0): ChainOutput = {
        val datum: Data = (TmDatum.Confirmed(
          BitcoinHelpers.getTxHash(raw),
          TreasuryMovementValidator.allInputOutpoints(raw),
          TreasuryMovementValidator.allOutputs(raw),
          false,
          PubKeyHash(filled(0x0c, 28)),
          BigInt(0),
          BigInt(0),
          BigInt(0)
        ): TmDatum).toData
        ChainOutput(filled(0xc1 + index.toInt, 32), index, Some(datum), Map.empty)
    }

    private def unconfirmedOutput(
        raw: ByteString,
        hints: Seq[ByteString],
        seed: Int = 0xe0
    ): ChainOutput = {
        val datum: Data = (TmDatum.Unconfirmed(
          raw,
          PubKeyHash(filled(0x0c, 28)),
          BigInt(0),
          BigInt(0),
          BigInt(0),
          PList.from(hints.toList)
        ): TmDatum).toData
        ChainOutput(filled(seed, 32), 0, Some(datum), Map.empty)
    }

    private def porOutput(
        txSeed: Int,
        index: Long,
        dest: ByteString,
        locked: Long,
        fee: Long
    ): ChainOutput = {
        val datum: Data = PegOutDatum(
          ownerAuth = AuthorizationMethod.CardanoSignature(filled(0xd0, 28)),
          sourceChainDestinationAddress = dest,
          perPegoutFee = BigInt(fee),
          created = BigInt(1_700_000_000_000L)
        ).toData
        ChainOutput(
          filled(txSeed, 32),
          index,
          Some(datum),
          Map(fbtcPolicy + fbtcAsset -> BigInt(locked), "lovelace" -> BigInt(2_000_000))
        )
    }

    private def entryOf(por: ChainOutput, dest: ByteString, net: Long): (ByteString, ByteString) =
        CpoTrieMirror.porId(por.txHash, por.outputIndex) ->
            CompletedPegOutsTrie.trieValue(PegOutEntry(dest, BigInt(net)))

    private def hintOf(por: ChainOutput): ByteString =
        CpoTrieMirror.hintBytes(por.txHash, por.outputIndex)

    private def cfg(onChainRoot: Option[ByteString]) = CpoReconstruction.Config(
      tmAddress = tmAddress,
      pegOutAddress = pegOutAddress,
      fbtcPolicyHex = fbtcPolicy,
      fbtcAssetNameHex = fbtcAsset,
      onChainRoot = onChainRoot
    )

    private def mirrorOf(entries: Seq[(ByteString, ByteString)]): CpoTrieMirror =
        CpoTrieMirror.fromEntries(entries).fold(e => fail(e), identity)

    // --- a two-movement history -----------------------------------------------------------------

    // TM1 pays one request; TM2 chains from TM1's treasury output and pays another.
    private val por1 = porOutput(0x51, 0, spk(0x22), 100_000L, 1_000L)
    private val por2 = porOutput(0x52, 3, spk(0x33), 50_000L, 1_000L)
    private val entry1 = entryOf(por1, spk(0x22), 99_000L)
    private val entry2 = entryOf(por2, spk(0x33), 49_000L)
    private val root1 = mirrorOf(Seq(entry1)).root
    private val root2 = mirrorOf(Seq(entry1, entry2)).root

    private val tm1 = rawTx(
      genesisOutpoint,
      Seq(spk(0x11) -> BigInt(500_000), spk(0x22) -> BigInt(99_000), commitment(root1) -> BigInt(0))
    )
    private val tm2 = rawTx(
      treasuryOutpointOf(tm1),
      Seq(spk(0x11) -> BigInt(400_000), spk(0x33) -> BigInt(49_000), commitment(root2) -> BigInt(0))
    )

    private def history(tmOutputs: Seq[ChainOutput]) = new FakeHistory(
      Map(tmAddress -> tmOutputs, pegOutAddress -> Seq(por1, por2))
    )

    private val honestTmOutputs = Seq(
      confirmedOutput(tm1, 0),
      confirmedOutput(tm2, 1),
      unconfirmedOutput(tm1, Seq(hintOf(por1)), 0xe1),
      unconfirmedOutput(tm2, Seq(hintOf(por2)), 0xe2)
    )

    // --- tests ----------------------------------------------------------------------------------

    test("hints reconstruct the trie and the running root matches every TM") {
        val m = CpoReconstruction
            .reconstruct(history(honestTmOutputs), cfg(Some(root2)))
            .fold(e => fail(e.message), identity)
        assert(m.root == root2)
        assert(m.size == 2)
    }

    test("a missing hint falls back to matching the TM's payments") {
        // Payment outputs are (spk, net) pairs; each is matched against the requests in history and
        // the assignment is accepted only if it reproduces the attested root.
        val noHints = honestTmOutputs.take(2)
        val m = CpoReconstruction
            .reconstruct(history(noHints), cfg(Some(root2)))
            .fold(e => fail(e.message), identity)
        assert(m.root == root2)
    }

    test("a hostile hint for the same TM costs nothing when an honest one exists") {
        // Posting a TM record is permissionless, so anyone can publish a second record embedding the
        // same signed Bitcoin transaction with a garbage hint.
        val hostile = unconfirmedOutput(tm1, Seq(hintOf(por2)), 0xe3)
        val m = CpoReconstruction
            .reconstruct(history(honestTmOutputs :+ hostile), cfg(Some(root2)))
            .fold(e => fail(e.message), identity)
        assert(m.root == root2)
    }

    test("a garbled hint for every record still reconstructs via the fallback") {
        val garbled = Seq(
          confirmedOutput(tm1, 0),
          confirmedOutput(tm2, 1),
          unconfirmedOutput(tm1, Seq(ByteString.fromHex("dead")), 0xe1),
          unconfirmedOutput(tm2, Seq(hintOf(por1)), 0xe2) // points at the WRONG request
        )
        val m = CpoReconstruction
            .reconstruct(history(garbled), cfg(Some(root2)))
            .fold(e => fail(e.message), identity)
        assert(m.root == root2)
    }

    test("an output at the TM address whose datum cannot be READ is a HARD error") {
        // It could be a Confirmed record, and dropping one yields a trie that silently omits a whole
        // movement while still looking complete.
        val withGap = honestTmOutputs :+ ChainOutput(
          filled(0xff, 32),
          0,
          None,
          Map.empty,
          unresolvedDatum = Some("datum hash has no resolvable preimage on this backend")
        )
        val err = CpoReconstruction
            .reconstruct(history(withGap), cfg(Some(root2)))
            .swap
            .getOrElse(fail("expected a hard error"))
        assert(err.message.contains("cannot resolve the datum"))
        assert(!err.transient, "an unexplained gap is an integrity failure, not a retryable one")
    }

    test("an output at the TM address with NO datum at all is skipped") {
        // Both bridge addresses are permissionlessly payable, so anyone can pay ADA to the TM
        // address with no datum. Every TM record carries an inline datum, so such an output provably
        // is not one — treating it as a gap let a single junk payment block reconstruction forever.
        val withJunk = honestTmOutputs :+ ChainOutput(filled(0xff, 32), 0, None, Map.empty)
        val m = CpoReconstruction
            .reconstruct(history(withJunk), cfg(Some(root2)))
            .fold(e => fail(e.message), identity)
        assert(m.root == root2)
    }

    test("a peg-out request whose datum cannot be read is skipped, not fatal") {
        // The asymmetry with the TM address: a missing request cannot shrink the trie silently, it
        // makes some TM fail its root assertion by name. Erroring here would hand anyone a denial of
        // service against reconstruction.
        val unreadable = por1.copy(
          inlineDatum = None,
          unresolvedDatum = Some("datum hash has no resolvable preimage on this backend")
        )
        val src = new FakeHistory(
          Map(tmAddress -> honestTmOutputs, pegOutAddress -> Seq(unreadable, por1, por2))
        )
        val m = CpoReconstruction
            .reconstruct(src, cfg(Some(root2)))
            .fold(e => fail(e.message), identity)
        assert(m.root == root2)
    }

    test("a transient source failure is passed through as transient") {
        // The caller must be able to tell "the backend was busy" from "the chain disagrees": only
        // the second may latch a permanent halt.
        val flaky = new CpoHistorySource {
            override def backend: String = "flaky"
            override def addressHistory(a: String): Either[HistoryError, Seq[ChainOutput]] =
                Left(HistoryError.transient("HTTP 429"))
        }
        val err = CpoReconstruction
            .reconstruct(flaky, cfg(Some(root2)))
            .swap
            .getOrElse(fail("expected a failure"))
        assert(err.transient)
    }

    test("junk with a resolvable datum at the TM address is ignored, not an error") {
        val junk = ChainOutput(filled(0xfe, 32), 0, Some(Data.I(BigInt(1))), Map.empty)
        val m = CpoReconstruction
            .reconstruct(history(honestTmOutputs :+ junk), cfg(Some(root2)))
            .fold(e => fail(e.message), identity)
        assert(m.root == root2)
    }

    test("a TM whose payments match no request in history names that TM") {
        val orphanHistory = new FakeHistory(
          Map(tmAddress -> honestTmOutputs.take(2), pegOutAddress -> Seq(por1))
        )
        val err = CpoReconstruction
            .reconstruct(orphanHistory, cfg(Some(root2)))
            .swap
            .getOrElse(fail("expected an unreconstructable TM"))
        assert(err.message.contains(BitcoinHelpers.getTxHash(tm2).reverse.toHex))
    }

    test("a reconstructed root that the chain disagrees with is REFUSED") {
        val err = CpoReconstruction
            .reconstruct(history(honestTmOutputs), cfg(Some(filled(0x00, 32))))
            .swap
            .getOrElse(fail("expected a cross-check failure"))
        assert(err.message.contains("refusing to use a trie the chain disagrees with"))
    }

    test("no on-chain root supplied warns instead of cross-checking") {
        var warned = false
        val m = CpoReconstruction
            .reconstruct(
              history(honestTmOutputs),
              cfg(None),
              line => if line.contains("WARNING") then warned = true
            )
            .fold(e => fail(e.message), identity)
        assert(m.root == root2)
        assert(warned)
    }

    test("chainOrder follows the treasury linkage regardless of input order") {
        val a = CpoReconstruction
            .parseConfirmed(confirmedOutput(tm1).inlineDatum.get)
            .getOrElse(fail("tm1"))
        val b = CpoReconstruction
            .parseConfirmed(confirmedOutput(tm2).inlineDatum.get)
            .getOrElse(fail("tm2"))
        assert(CpoReconstruction.chainOrder(Seq(b, a)).map(_.btcTxid) == Seq(a.btcTxid, b.btcTxid))
        assert(CpoReconstruction.chainOrder(Seq(a, b)).map(_.btcTxid) == Seq(a.btcTxid, b.btcTxid))
    }

    test("a duplicate Confirmed record for the same TM is replayed once") {
        val duplicated = honestTmOutputs :+ confirmedOutput(tm2, 2)
        val m = CpoReconstruction
            .reconstruct(history(duplicated), cfg(Some(root2)))
            .fold(e => fail(e.message), identity)
        assert(m.size == 2)
    }

    test("an old 5-field Unconfirmed datum yields an empty hint rather than a failure") {
        // Records posted before the rev-5.1 field append confirm fine on-chain, so real history
        // contains them; reconstruction must read the chain, not refuse it.
        val old: Data = Data.Constr(
          0,
          PList.from(
            List[Data](
              Data.B(tm1),
              Data.B(filled(0x0c, 28)),
              Data.I(BigInt(0)),
              Data.I(BigInt(0)),
              Data.I(BigInt(0))
            )
          )
        )
        val parsed = CpoReconstruction.parseUnconfirmedHint(old).getOrElse(fail("expected a parse"))
        assert(parsed._1 == BitcoinHelpers.getTxHash(tm1))
        assert(parsed._2.isEmpty)
    }
}
