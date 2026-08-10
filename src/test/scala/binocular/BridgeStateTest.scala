package binocular

import binocular.watchtower.*

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.ByteString.hex
import scalus.uplc.builtin.Data.{to, toData}

/** Pins the rev-5.4 core types of the bridge-state singleton.
  *
  * Two things are pinned here, and both are consensus-visible.
  *
  *   - [[BridgeState]]'s POSITIONAL encoding. The datum is written by this Scalus validator and
  *     read by three Aiken validators, so the Constr tag and the field order are a cross-language
  *     contract. Spec §BridgeState, the singleton datum: index 0 `spi_root`, 1 `cpo_root`, 2
  *     `treasury_utxo_id`, 3 `treasury_amount`. [LIB-3] makes a new field append-only, so the arity
  *     is pinned too — an INSERT must break this test, not a validator.
  *   - The `"BTMR1"` two-root commitment reader. A TM now attests BOTH roots in one 71-byte
  *     OP_RETURN, and [CTM-26] requires exactly one such output per TM. The negative cases are the
  *     point: zero, two, the old 39-byte `"CPOR1"` output, and a 71-byte look-alike must all fail
  *     rather than yield a root the quorum never signed.
  */
class BridgeStateTest extends AnyFunSuite {

    private def filled(v: Int, n: Int): ByteString =
        ByteString.fromArray(Array.fill[Byte](n)(v.toByte))

    // --- BridgeState datum ----------------------------------------------------------------------

    private val spiRoot = filled(0x11, 32)
    private val cpoRoot = filled(0x22, 32)
    // btc_txid ++ 00000000: the treasury outpoint the next TM must spend as its input 0.
    private val treasuryUtxoId = filled(0x33, 32) ++ hex"00000000"
    private val treasuryAmount = BigInt(4_200_000_000L)

    private val state = BridgeState(spiRoot, cpoRoot, treasuryUtxoId, treasuryAmount)

    test(
      "BridgeState encodes as Constr 0 [spi_root, cpo_root, treasury_utxo_id, treasury_amount]"
    ) {
        assert(
          state.toData == Data.Constr(
            0,
            PList(Data.B(spiRoot), Data.B(cpoRoot), Data.B(treasuryUtxoId), Data.I(treasuryAmount))
          )
        )
    }

    test("BridgeState round-trips through Data") {
        assert(state.toData.to[BridgeState] == state)
    }

    test("BridgeState has exactly four fields, so an INSERT breaks the build ([LIB-3])") {
        state.toData match
            case Data.Constr(0, fields) => assert(fields.asScala.size == 4)
            case other                  => fail(s"expected Constr 0, got $other")
    }

    test("the two roots are 32 bytes and the treasury UTxO id is 36") {
        assert(state.spiRoot.size == 32)
        assert(state.cpoRoot.size == 32)
        assert(state.treasuryUtxoId.size == 36)
    }

    test("the bridge-state singleton NFT asset name is \"BSS\"") {
        assert(TreasuryMovementValidator.BridgeStateAssetName == ByteString.fromString("BSS"))
    }

    // --- "BTMR1" two-root commitment ------------------------------------------------------------

    /** `OP_RETURN OP_PUSHBYTES_69 "BTMR1"` — 7 bytes, then 64 bytes of roots: 71 in all. */
    private val Btmr1Prefix = hex"6a4542544d5231"

    private def commitment(spi: ByteString, cpo: ByteString): PegOutEntry =
        PegOutEntry(Btmr1Prefix ++ spi ++ cpo, 0)

    private def payment(spkByte: Int, sats: Long): PegOutEntry =
        PegOutEntry(ByteString.fromArray(Array.fill[Byte](22)(spkByte.toByte)), BigInt(sats))

    private val change = PegOutEntry(hex"0014aabbccddeeff00112233445566778899aabbcc", BigInt(999))

    /** The reader over the given outputs: `Right((spi, cpo))`, or `Left` for a rejection. */
    private def read(outs: Seq[PegOutEntry]): Either[String, (ByteString, ByteString)] =
        try Right(TreasuryMovementValidator.committedRoots(PList.from(outs)))
        catch { case t: Throwable => Left(Option(t.getMessage).getOrElse(t.toString)) }

    test("committedRoots reads both roots of the single 71-byte \"BTMR1\" output") {
        val outs = Seq(change, payment(0xaa, 2000L), commitment(spiRoot, cpoRoot))
        assert(read(outs) == Right((spiRoot, cpoRoot)))
    }

    test("committedRoots finds the commitment at any position, including output 0") {
        assert(
          read(Seq(commitment(spiRoot, cpoRoot), change, payment(0xaa, 1L))) ==
              Right((spiRoot, cpoRoot))
        )
    }

    test("committedRoots slices spi_root from [7,39) and cpo_root from [39,71)") {
        // Distinct roots, so a swapped or mis-sliced pair cannot pass.
        val spi = filled(0xa1, 32)
        val cpo = filled(0xb2, 32)
        val spk = commitment(spi, cpo).scriptPubKey
        assert(spk.size == 71)
        assert(
          read(Seq(change, commitment(spi, cpo))) == Right((spk.slice(7, 32), spk.slice(39, 32)))
        )
    }

    test("committedRoots rejects a TM with no commitment output") {
        assert(read(Seq(change, payment(0xaa, 2000L))).isLeft)
        assert(read(Seq.empty).isLeft)
    }

    test("committedRoots rejects two commitment outputs, even of the same roots") {
        assert(read(Seq(change, commitment(spiRoot, cpoRoot), commitment(cpoRoot, spiRoot))).isLeft)
        assert(read(Seq(change, commitment(spiRoot, cpoRoot), commitment(spiRoot, cpoRoot))).isLeft)
    }

    test("committedRoots rejects the old 39-byte \"CPOR1\" commitment") {
        val cpor1 = PegOutEntry(hex"6a2543504f5231" ++ cpoRoot, 0)
        assert(cpor1.scriptPubKey.size == 39)
        assert(read(Seq(change, cpor1)).isLeft)
    }

    test("committedRoots rejects a 71-byte output with a wrong prefix") {
        // "BTMR2": right length, right OP_RETURN push, wrong tag.
        val wrongTag = PegOutEntry(hex"6a4542544d5232" ++ spiRoot ++ cpoRoot, 0)
        assert(wrongTag.scriptPubKey.size == 71)
        assert(read(Seq(change, wrongTag)).isLeft)
        // ...and it does not stop the real one from being read.
        assert(
          read(Seq(change, wrongTag, commitment(spiRoot, cpoRoot))) == Right((spiRoot, cpoRoot))
        )
    }

    test("committedRoots rejects a right-prefix output of the wrong length") {
        // 70 bytes: without the length check the second slice would read past the payload.
        val short = PegOutEntry(commitment(spiRoot, cpoRoot).scriptPubKey.slice(0, 70), 0)
        assert(read(Seq(change, short)).isLeft)
    }

    test("committedRoots ignores a 71-byte payment script") {
        assert(read(Seq(change, PegOutEntry(filled(0x51, 71), BigInt(1)))).isLeft)
    }
}
