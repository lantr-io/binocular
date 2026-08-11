package binocular.server

import sttp.model.StatusCode
import sttp.shared.Identity
import sttp.tapir.*
import sttp.tapir.server.interceptor.cors.CORSInterceptor
import sttp.tapir.server.netty.sync.{NettySyncServer, NettySyncServerOptions}
import sttp.tapir.swagger.bundle.SwaggerInterpreter

/** The proof-serving REST API ([SPI-4], [OB-13]): the thin Tapir transport over [[ProofService]].
  *
  * Two endpoints, one resource each, keyed by the deposit outpoint (`TXID:VOUT` display form or
  * 72-hex raw):
  *
  *   - `GET /api/v1/spi-proof/{outpoint}` — the [CPI-9] swept-peg-ins membership proof;
  *   - `GET /api/v1/deposit-proof/{outpoint}` — the [OB-12] deposit-inclusion bundle.
  *
  * Success bodies are the JSON shapes [[ProofApi]] defines (shared with the CLI commands); errors
  * are `{"error", "message"}` with the status [[ProofApi]] maps. Swagger UI at `/docs`.
  *
  * CORS is wide open by design: the API is trustless ([SPI-4] — every served proof is verified
  * on-chain, a wrong one just fails), so there is nothing to protect, and the ft-bifrost-frontend
  * calls it straight from the browser ([OF-3], [OF-8]).
  */
class ProofServer(
    spiProofFor: String => Either[ProofApi.ApiError, String],
    depositProofFor: String => Either[ProofApi.ApiError, String]
) {

    private def proofEndpoint(name: String, description: String) =
        endpoint.get
            .in("api" / "v1" / name / path[String]("outpoint"))
            .out(stringJsonBody)
            .errorOut(statusCode.and(stringJsonBody))
            .description(description)

    private val spiProof =
        proofEndpoint(
          "spi-proof",
          "The [CPI-9] swept-peg-ins membership proof: the sweeping TM's input-0 outpoint and " +
              "the MPF proof (Plutus Data CBOR) against the singleton's attested spi_root. " +
              "404 not_in_confirmed_set until the sweeping TM confirms on Cardano ([SPI-6])."
        ).handle(answer(spiProofFor))

    private val depositProof =
        proofEndpoint(
          "deposit-proof",
          "The [OB-12] deposit-inclusion bundle: raw deposit tx, 80-byte block header, tx " +
              "merkle proof with index, and the MPF proof of the block against the oracle's " +
              "confirmed_blocks_root — exactly the PegInRequest mint redeemer."
        ).handle(answer(depositProofFor))

    private def answer(
        serve: String => Either[ProofApi.ApiError, String]
    ): String => Either[(StatusCode, String), String] =
        outpoint => serve(outpoint).left.map(e => (StatusCode(e.status), ProofApi.errorBody(e)))

    private val apiEndpoints = List(spiProof, depositProof)

    private val swaggerEndpoints = SwaggerInterpreter()
        .fromEndpoints[Identity](
          apiEndpoints.map(_.endpoint),
          "Binocular Proof Server",
          binocular.BuildInfo.version
        )

    private def server(port: Int): NettySyncServer =
        NettySyncServer(
          NettySyncServerOptions.customiseInterceptors
              .corsInterceptor(CORSInterceptor.default[Identity])
              .options
        )
            .host("0.0.0.0")
            .port(port)
            .addEndpoints(apiEndpoints ++ swaggerEndpoints)

    /** Bind and block forever. Both run modes want exactly this: the standalone `serve-proofs`
      * process blocks its main thread, and the watchtower embeds the server as a supervised
      * [[binocular.watchtower.Watchtower.Worker]], whose contract is a call that never returns.
      */
    def startAndWait(port: Int): Unit = server(port).startAndWait()
}

object ProofServer {

    /** The production wiring: both endpoints served by one [[ProofService]]. The
      * function-per-endpoint constructor exists so the transport is testable with stubs.
      */
    def apply(service: ProofService): ProofServer =
        new ProofServer(service.spiProof, service.depositProof)
}
