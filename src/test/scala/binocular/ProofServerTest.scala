package binocular

import binocular.server.{ProofApi, ProofServer}

import org.scalatest.funsuite.AnyFunSuite

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import scala.util.Random

/** Smoke test of the REST transport: the Netty server binds, routes both endpoints, emits JSON with
  * the mapped status codes, and answers browser (CORS) requests — everything the frontend needs
  * from the wire that the pure [[ProofApiTest]] cannot see.
  *
  * The handlers are stubs; the real proof logic has its own suites. The server runs on a daemon
  * thread for the remainder of the test JVM (netty-sync's blocking `startAndWait` has no stop
  * handle by design — in production the process supervisor owns its lifecycle).
  */
class ProofServerTest extends AnyFunSuite {

    private val goodOutpoint = "aa" * 36
    private val boundaryOutpoint = "bb" * 36

    private val server = new ProofServer(
      spiProofFor = {
          case `goodOutpoint` => Right("""{"spi_root":"11"}""")
          case `boundaryOutpoint` =>
              Left(ProofApi.ApiError(404, "not_in_confirmed_set", "retry after the next Confirm"))
          case other => Left(ProofApi.invalidOutpoint(other))
      },
      depositProofFor = {
          case `goodOutpoint` => Right("""{"block_header":"22"}""")
          case other          => Left(ProofApi.ApiError(503, "oracle_lagging", "retry"))
      }
    )

    private val port = {
        // A fixed port collides across repeated local runs; a random high port practically never.
        val p = 20000 + Random.nextInt(20000)
        val t = new Thread(() => server.startAndWait(p), "proof-server-test")
        t.setDaemon(true)
        t.start()
        p
    }

    private val client = HttpClient.newHttpClient()

    private def get(path: String, headers: (String, String)*): HttpResponse[String] = {
        val base = HttpRequest.newBuilder().uri(URI.create(s"http://localhost:$port$path")).GET()
        val req = headers.foldLeft(base) { case (b, (k, v)) => b.header(k, v) }.build()
        // The server binds asynchronously; poll briefly instead of sleeping a fixed amount.
        var last: Option[HttpResponse[String]] = None
        var attempts = 0
        while last.isEmpty && attempts < 50 do
            try last = Some(client.send(req, HttpResponse.BodyHandlers.ofString()))
            catch { case _: java.net.ConnectException => attempts += 1; Thread.sleep(100) }
        last.getOrElse(fail(s"server did not come up on port $port"))
    }

    test("a served proof is 200 application/json") {
        val resp = get(s"/api/v1/spi-proof/$goodOutpoint")
        assert(resp.statusCode() == 200)
        assert(resp.headers().firstValue("Content-Type").orElse("").startsWith("application/json"))
        assert(ujson.read(resp.body())("spi_root").str == "11")
    }

    test("the [SPI-6] boundary answers 404 with the machine-readable code") {
        val resp = get(s"/api/v1/spi-proof/$boundaryOutpoint")
        assert(resp.statusCode() == 404)
        val body = ujson.read(resp.body())
        assert(body("error").str == "not_in_confirmed_set")
        assert(body("message").str.contains("Confirm"))
    }

    test("a malformed outpoint answers 400") {
        assert(get("/api/v1/spi-proof/nonsense").statusCode() == 400)
    }

    test("the deposit endpoint routes independently and maps 503") {
        assert(get(s"/api/v1/deposit-proof/$goodOutpoint").statusCode() == 200)
        val lagging = get("/api/v1/deposit-proof/other")
        assert(lagging.statusCode() == 503)
        assert(ujson.read(lagging.body())("error").str == "oracle_lagging")
    }

    test("browser requests get a CORS answer ([OF-3]/[OF-8]: called from the frontend)") {
        val resp = get(s"/api/v1/spi-proof/$goodOutpoint", "Origin" -> "http://localhost:3000")
        assert(resp.headers().firstValue("Access-Control-Allow-Origin").orElse("") == "*")
    }
}
