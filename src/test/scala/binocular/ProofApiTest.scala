package binocular

import binocular.server.ProofApi
import binocular.watchtower.{PegInProofBundle, SweptPegInsProofService}

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.hex

/** Unit tests for the pure request/response layer of the proof server: outpoint parsing, the JSON
  * shapes both transports (REST and CLI) emit, and the service-error-to-HTTP-status mapping.
  *
  * The status mapping is API surface the frontend depends on: 404 `not_in_confirmed_set` and 503
  * `oracle_lagging` are the two "not yet" states it polls through, 400 is a caller bug, and every
  * other 503 means the server itself cannot currently serve consistently.
  */
class ProofApiTest extends AnyFunSuite {

    private def filled(v: Int, n: Int): ByteString =
        ByteString.fromArray(Array.fill[Byte](n)(v.toByte))

    private val txidLE = filled(0xab, 32)
    private val outpoint36 = txidLE ++ hex"01000000"

    // --- outpoint parsing ------------------------------------------------------------------------

    test("parseBtcOutpoint accepts TXID:VOUT with the display (big-endian) txid") {
        val display = txidLE.bytes.reverse.map("%02x".format(_)).mkString
        assert(ProofApi.parseBtcOutpoint(s"$display:1") == Right(outpoint36))
    }

    test("parseBtcOutpoint accepts the 72-hex raw outpoint, case-insensitively") {
        assert(ProofApi.parseBtcOutpoint(outpoint36.toHex.toUpperCase) == Right(outpoint36))
    }

    test("parseBtcOutpoint rejects malformed arguments") {
        assert(ProofApi.parseBtcOutpoint("nonsense").isLeft)
        assert(ProofApi.parseBtcOutpoint("aabb:1").isLeft)
        assert(ProofApi.parseBtcOutpoint(s"${"aa" * 32}:-1").isLeft)
        assert(ProofApi.parseBtcOutpoint(s"${"aa" * 32}:4294967296").isLeft)
        assert(ProofApi.parseBtcOutpoint("zz" * 36).isLeft)
    }

    // --- SPI proof shape and mapping -------------------------------------------------------------

    test("spiProofJson carries the [CPI-9] redeemer fields") {
        val proof = SweptPegInsProofService.SpiMembershipProof(
          pegInUtxoId = outpoint36,
          sweepingTmInput0 = filled(0xcd, 32) ++ hex"00000000",
          spiRoot = filled(0x11, 32),
          proof = PList.Nil
        )
        val json = ujson.read(ProofApi.spiProofJson(proof))
        assert(json("peg_in_utxo_id").str == outpoint36.toHex)
        assert(json("sweeping_tm_input_0").str == proof.sweepingTmInput0.toHex)
        assert(json("spi_root").str == proof.spiRoot.toHex)
        // The empty proof list serialises as Plutus Data CBOR: 80 = empty array.
        assert(json("proof_cbor").str == "80")
    }

    test("spiError maps the [SPI-6] boundary to 404 and integrity refusals to 503") {
        val notIn = ProofApi.spiError(
          SweptPegInsProofService.NotInConfirmedSet(outpoint36, filled(0x11, 32))
        )
        assert(notIn.status == 404 && notIn.code == "not_in_confirmed_set")
        assert(notIn.message.contains("not yet confirmed"))

        assert(
          ProofApi.spiError(SweptPegInsProofService.InvalidRequest("bad")).status == 400
        )
        val walk = ProofApi.spiError(SweptPegInsProofService.WalkFailed("gap"))
        assert(walk.status == 503 && walk.code == "history_unavailable")
        val mismatch = ProofApi.spiError(
          SweptPegInsProofService.RootMismatch(filled(1, 32), filled(2, 32))
        )
        assert(mismatch.status == 503 && mismatch.code == "root_mismatch")
    }

    // --- deposit bundle shape and mapping --------------------------------------------------------

    test("depositBundleJson carries the four [OB-12] items plus the PegInDatum fields") {
        // The dual-key deposit fixture from bitcoin.ak (see PegInProofBundleTest).
        val rawTx = ByteString.fromHex(
          "020000000100000000000000000000000000000000000000000000000000000000000000000000000000ffffffff02a086010000000000225120bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb0000000000000000456a43424652ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddcccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc00000000"
        )
        val bundle = PegInProofBundle(
          rawTxHex = rawTx,
          blockHeader = filled(0, 80),
          txIndex = 2,
          txInBlockMerklePath = Seq(filled(0x21, 32), filled(0x22, 32)),
          mpfHeaderInclusionProof = PList.Nil,
          pegInVout = 0,
          pegInAmountSat = 100_000L,
          userSourceChainPubKey = filled(0xcc, 32)
        )
        val json = ujson.read(ProofApi.depositBundleJson(bundle, filled(0x0b, 32)))
        assert(json("raw_tx").str == rawTx.toHex)
        // The root the MPF proof was built against — parity with spi-proof's spi_root echo, so a
        // client can detect an oracle promotion race before building.
        assert(json("confirmed_blocks_root").str == ("0b" * 32))
        assert(json("block_header").str == filled(0, 80).toHex)
        assert(json("tx_index").num == 2)
        assert(
          json("tx_merkle_proof").arr.map(_.str) == Seq(filled(0x21, 32), filled(0x22, 32)).map(
            _.toHex
          )
        )
        assert(json("block_mpf_proof_cbor").str == "80")
        assert(json("peg_in_vout").num == 0)
        // Longs serialise as JSON strings (upickle: no silent precision loss in JS).
        assert(json("peg_in_amount_sat").str == "100000")
        assert(json("user_source_chain_pub_key").str == ("cc" * 32))
        // peg_in_utxo_id = txid(LE) of the raw tx ++ vout LE — the datum's key.
        assert(json("peg_in_utxo_id").str == bundle.pegInUtxoId.toHex)
    }

    test("depositError maps 'not yet' cases to 404 and the lagging oracle to 503") {
        import PegInProofBundle.*
        assert(ProofApi.depositError(BadOutpoint("x")).status == 400)
        assert(ProofApi.depositError(TxNotConfirmed("t")).status == 404)
        assert(ProofApi.depositError(TxNotInBlock("t", "b")).status == 404)
        assert(ProofApi.depositError(NoBfrOpReturn("t")).code == "not_a_deposit")
        assert(ProofApi.depositError(NoP2trOutput("t")).code == "not_a_deposit")
        assert(ProofApi.depositError(VoutNotDeposit("t", 7)).code == "not_a_deposit")
        val lagging = ProofApi.depositError(BlockNotConfirmedByOracle("t", "b"))
        assert(lagging.status == 503 && lagging.code == "oracle_lagging")
        assert(lagging.message.contains("retry"))
    }

    test("errorBody is the {error, message} JSON the frontend switches on") {
        val body = ujson.read(
          ProofApi.errorBody(ProofApi.ApiError(404, "not_in_confirmed_set", "why"))
        )
        assert(body("error").str == "not_in_confirmed_set")
        assert(body("message").str == "why")
    }
}
