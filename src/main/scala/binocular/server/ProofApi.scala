package binocular.server

import binocular.watchtower.{CpoTrieMirror, PegInProofBundle, SweptPegInsProofService}

import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Data.toData
import scalus.utils.Hex.hexToBytes

/** The pure request/response layer of the proof server: outpoint parsing, JSON shapes, and the
  * mapping from service errors to HTTP statuses. No I/O — [[ProofService]] resolves live chain
  * state and delegates here, and the CLI commands print the same JSON, so the HTTP API and the
  * manual commands can never drift apart.
  *
  * Error bodies are `{"error": <machine-readable code>, "message": <human text>}`. The codes are
  * API surface: the frontend polls `not_in_confirmed_set` and `oracle_lagging` ("not yet" states)
  * and treats 5xx codes as server trouble.
  */
object ProofApi {

    /** An API failure: the HTTP status to answer with, a stable machine-readable code, and the
      * human-readable reason.
      */
    final case class ApiError(status: Int, code: String, message: String)

    def errorBody(e: ApiError): String =
        ujson.write(ujson.Obj("error" -> e.code, "message" -> e.message))

    /** Parse a Bitcoin outpoint request argument into its 36-byte `txid(LE) ++ vout(LE)` form — the
      * `peg_in_utxo_id` encoding. Accepts the display form `TXID:VOUT` (txid big-endian, as
      * explorers and bitcoind print it) or 72 hex chars of the raw 36-byte outpoint.
      */
    def parseBtcOutpoint(s: String): Either[String, ByteString] = {
        val t = s.trim.toLowerCase
        def isHex(x: String) = x.nonEmpty && x.forall(c => "0123456789abcdef".contains(c))
        if t.contains(":") then
            t.split(":") match {
                case Array(txid, voutStr) if txid.length == 64 && isHex(txid) =>
                    voutStr.toLongOption
                        .filter(v => v >= 0 && v <= 0xffffffffL)
                        .toRight(s"invalid vout: $voutStr")
                        .map { vout =>
                            CpoTrieMirror.hintBytes(
                              ByteString.fromArray(txid.hexToBytes.reverse),
                              vout
                            )
                        }
                case _ => Left(s"expected TXID:VOUT with a 64-hex txid, got: $t")
            }
        else if t.length == 72 && isHex(t) then Right(ByteString.fromHex(t))
        else
            Left(
              "expected a Bitcoin outpoint as TXID:VOUT (display txid) or 72 hex chars " +
                  s"(raw txid-LE ++ vout-LE), got: $t"
            )
    }

    def invalidOutpoint(reason: String): ApiError =
        ApiError(400, "invalid_request", s"invalid outpoint: $reason")

    // --- swept-peg-ins membership proof ([SPI-4], [CPI-9]) ---------------------------------------

    /** The [CPI-9] redeemer's fields: the proven value and the membership proof as Plutus `Data`
      * CBOR hex.
      */
    def spiProofJson(proof: SweptPegInsProofService.SpiMembershipProof): String =
        ujson.write(
          ujson.Obj(
            "peg_in_utxo_id" -> proof.pegInUtxoId.toHex,
            "sweeping_tm_input_0" -> proof.sweepingTmInput0.toHex,
            "spi_root" -> proof.spiRoot.toHex,
            "proof_cbor" -> ByteString.fromArray(proof.proof.toData.toCbor).toHex
          ),
          indent = 2
        )

    /** Map a proof-service refusal to an HTTP answer.
      *
      *   - `NotInConfirmedSet` is 404 with a "retry after the next Confirm" message: THE [SPI-6]
      *     boundary, a normal state the frontend polls through, not a fault.
      *   - `WalkFailed` / `RootMismatch` are 503: this server currently cannot serve ANY proof
      *     consistently, and saying so beats serving one the chain would reject.
      */
    def spiError(e: SweptPegInsProofService.ServeError): ApiError = e match {
        case SweptPegInsProofService.InvalidRequest(msg) =>
            ApiError(400, "invalid_request", msg)
        case e: SweptPegInsProofService.NotInConfirmedSet =>
            ApiError(404, "not_in_confirmed_set", e.message)
        case e: SweptPegInsProofService.WalkFailed =>
            ApiError(503, "history_unavailable", e.message)
        case e: SweptPegInsProofService.RootMismatch =>
            ApiError(503, "root_mismatch", e.message)
    }

    // --- deposit-inclusion bundle ([OB-12]) ------------------------------------------------------

    /** The four `PegInRequest` mint redeemer items plus the `PegInDatum` convenience fields
      * ([OF-8]: the frontend builds the request from this instead of assembling proofs itself).
      */
    def depositBundleJson(bundle: PegInProofBundle): String =
        ujson.write(
          ujson.Obj(
            "peg_in_utxo_id" -> bundle.pegInUtxoId.toHex,
            // PegInDatum.source_chain_peg_in_raw_tx (witness-stripped, hashes to the txid).
            "raw_tx" -> bundle.rawTxHex.toHex,
            // PegInDatum.source_chain_peg_in_raw_tx_index.
            "tx_index" -> bundle.txIndex,
            // PegInRequest.block_header, 80 bytes.
            "block_header" -> bundle.blockHeader.toHex,
            // PegInRequest.tx_in_block_header_inclusion_proof.
            "tx_merkle_proof" -> ujson.Arr.from(bundle.txInBlockMerklePath.map(_.toHex)),
            // PegInRequest.block_header_in_source_chain_inclusion_proof, as Data CBOR.
            "block_mpf_proof_cbor" ->
                ByteString.fromArray(bundle.mpfHeaderInclusionProof.toData.toCbor).toHex,
            // Convenience fields for the rest of the PegInDatum. The amount is a JSON STRING —
            // upickle's Long encoding — which is also the right call for money: a satoshi amount
            // is safe in a double today, but a string can never silently lose precision in JS.
            "peg_in_vout" -> bundle.pegInVout,
            "peg_in_amount_sat" -> bundle.pegInAmountSat,
            "user_source_chain_pub_key" -> bundle.userSourceChainPubKey.toHex
          ),
          indent = 2
        )

    /** Map a bundle-production failure to an HTTP answer.
      *
      *   - 404s are "no servable bundle exists for this outpoint (yet)": not confirmed, not in a
      *     block, or not a well-formed deposit. `deposit_binding_ok` would reject the same ones at
      *     mint.
      *   - `BlockNotConfirmedByOracle` is 503 `oracle_lagging`: the deposit is fine, the oracle has
      *     not caught up — the one case that resolves by itself, so the frontend retries it.
      */
    def depositError(e: PegInProofBundle.ProduceError): ApiError = e match {
        case PegInProofBundle.BadOutpoint(msg) =>
            ApiError(400, "invalid_request", msg)
        case PegInProofBundle.TxNotConfirmed(txId) =>
            ApiError(404, "tx_not_confirmed", s"tx $txId is not in a Bitcoin block yet")
        case PegInProofBundle.TxNotInBlock(txId, blockHash) =>
            ApiError(404, "tx_not_in_block", s"tx $txId is not in its claimed block $blockHash")
        case PegInProofBundle.NoBfrOpReturn(txId) =>
            ApiError(
              404,
              "not_a_deposit",
              s"tx $txId carries no dual-key BFR beacon at vout 1 — not a bridge deposit"
            )
        case PegInProofBundle.NoP2trOutput(txId) =>
            ApiError(404, "not_a_deposit", s"tx $txId has no P2TR deposit output")
        case PegInProofBundle.VoutNotDeposit(txId, vout) =>
            ApiError(
              404,
              "not_a_deposit",
              s"vout $vout of tx $txId is not the P2TR deposit output"
            )
        case PegInProofBundle.BlockNotConfirmedByOracle(txId, blockHash) =>
            ApiError(
              503,
              "oracle_lagging",
              s"block $blockHash holding tx $txId is not yet in the oracle's " +
                  "confirmed_blocks_root — retry once the oracle catches up"
            )
    }
}
