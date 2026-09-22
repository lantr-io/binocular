package binocular

import binocular.bitcoin.*
import binocular.cli.DaemonExecution
import com.sun.net.httpserver.HttpServer
import java.net.InetSocketAddress
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.transaction.TransactionOutPoint
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.*
import scala.util.chaining.*
import scodec.bits.ByteVector

class BitcoinWalletRpcTest extends AnyFunSuite {
    private given ExecutionContext = DaemonExecution.ec
    private def await[A](f: Future[A]): A = Await.result(f, 5.seconds)
    private val wallet =
        Bip86Wallet.fromMnemonic("abandon " * 11 + "about", BitcoinNetwork.Testnet4).toOption.get
    private val txid = "12" * 32
    private val block = "34" * 32
    private val point =
        TransactionOutPoint(DoubleSha256DigestBE(ByteVector.fromValidHex(txid)), UInt32(2))
    private val spk = wallet.funding.scriptPubKey

    private def withRpc(
        reply: ujson.Value => (Int, ujson.Value)
    )(body: SimpleBitcoinRpc => Unit): Unit = {
        val server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
        val failure = new java.util.concurrent.atomic.AtomicReference[Throwable]()
        server.createContext(
          "/",
          exchange => {
              val request = ujson.read(new String(exchange.getRequestBody.readAllBytes(), "UTF-8"))
              val (status, response) =
                  try reply(request)
                  catch {
                      case scala.util.control.NonFatal(e) =>
                          failure.set(e)
                          (500, ujson.Obj("error" -> "test handler failed"))
                  }
              val bytes = response.render().getBytes("UTF-8")
              exchange.sendResponseHeaders(status, bytes.length)
              try exchange.getResponseBody.write(bytes)
              finally exchange.close()
          }
        )
        server.start()
        try
            body(
              new SimpleBitcoinRpc(
                BitcoinNodeConfig(url = s"http://127.0.0.1:${server.getAddress.getPort}")
              )
            )
        finally {
            server.stop(0)
            Option(failure.get()).foreach(throw _)
        }
    }
    private def ok(request: ujson.Value, result: ujson.Value) =
        (200, ujson.Obj("result" -> result, "error" -> ujson.Null, "id" -> request("id")))
    private def error(request: ujson.Value, status: Int, code: Int) =
        (
          status,
          ujson.Obj(
            "result" -> ujson.Null,
            "error" -> ujson.Obj("code" -> code, "message" -> "test RPC failure"),
            "id" -> request("id")
          )
        )
    private def scan(
        success: Boolean = true,
        amount: ujson.Value = ujson.Num(0.000005),
        coinbase: Option[Boolean] = None
    ) =
        ujson.Obj(
          "success" -> success,
          "txouts" -> 1000,
          "height" -> 900,
          "bestblock" -> block,
          "unspents" -> ujson.Arr(
            ujson
                .Obj(
                  "txid" -> txid,
                  "vout" -> 2,
                  "scriptPubKey" -> spk.toHex,
                  "desc" -> s"addr(${wallet.funding.address})",
                  "amount" -> amount,
                  "height" -> 899
                )
                .pipe(o => { coinbase.foreach(c => o("coinbase") = c); o })
          ),
          "total_amount" -> amount
        )

    test("scans only the fixed address without importing a descriptor or opening a wallet") {
        withRpc { req =>
            assert(req("method").str == "scantxoutset")
            assert(
              req("params") == ujson.Arr("start", ujson.Arr(s"addr(${wallet.funding.address})"))
            )
            ok(req, scan())
        } { rpc =>
            val result = await(rpc.scanAddress(wallet.funding.address))
            assert(result.height == 900 && result.bestBlockHash == block)
            assert(result.outputs.map(_.outpoint) == Vector(point))
            assert(result.outputs.head.amountSat == 500)
            assert(result.outputs.head.scriptPubKey == spk)
            assert(result.outputs.head.height == 899)
        }
    }

    test("a scan reads the coinbase flag, and treats a node that omits it as non-coinbase") {
        withRpc(req => ok(req, scan(coinbase = Some(true)))) { rpc =>
            assert(await(rpc.scanAddress(wallet.funding.address)).outputs.head.coinbase)
        }
        withRpc(req => ok(req, scan())) { rpc =>
            assert(!await(rpc.scanAddress(wallet.funding.address)).outputs.head.coinbase)
        }
    }

    test("aborted scan, wrong script and fractional satoshi fail instead of returning a balance") {
        val wrongScript = scan()
        wrongScript("unspents")(0)("scriptPubKey") = "5120" + "ff" * 32
        for result <- Seq(scan(false), wrongScript, scan(amount = ujson.Num(0.000000001))) do
            withRpc(req => ok(req, result)) { rpc =>
                intercept[Exception] { await(rpc.scanAddress(wallet.funding.address)) }
            }
    }

    test("estimatesmartfee and getmempoolinfo convert BTC/kvB upward to whole sat/kvB") {
        withRpc { req =>
            if req("method").str == "estimatesmartfee" then {
                assert(req("params") == ujson.Arr(3, "conservative"))
                ok(req, ujson.Obj("feerate" -> ujson.Num(0.00000500001)))
            } else {
                assert(req("method").str == "getmempoolinfo")
                ok(req, ujson.Obj("mempoolminfee" -> ujson.Num(0.000005)))
            }
        } { rpc =>
            assert(await(rpc.estimateSmartFeeSatPerKvb(3, "conservative")).contains(501))
            assert(await(rpc.getMempoolMinFeeSatPerKvb()) == 500)
        }
    }

    test("a missing or zero estimate is None; a negative, malformed or huge one fails") {
        for result <- Seq(
              ujson.Obj("errors" -> ujson.Arr("Insufficient data")),
              ujson.Obj("feerate" -> 0)
            )
        do
            withRpc(req => ok(req, result)) { rpc =>
                assert(await(rpc.estimateSmartFeeSatPerKvb(6, "economical")).isEmpty)
            }
        for result <- Seq(
              ujson.Obj("feerate" -> -1),
              ujson.Obj("feerate" -> "bad"),
              ujson.Obj("feerate" -> 1e30)
            )
        do
            withRpc(req => ok(req, result)) { rpc =>
                intercept[Exception](await(rpc.estimateSmartFeeSatPerKvb(6, "economical")))
            }
    }

    test("RPC errors from HTTP 200 and 500 retain their code and message") {
        for status <- Seq(200, 500) do
            withRpc { req =>
                assert(req("method").str == "getrawtransaction")
                val (responseStatus, response) = error(req, status, -28)
                if status == 200 then response.obj.remove("result")
                (responseStatus, response)
            } { rpc =>
                val failure = intercept[BitcoinRpcError] { await(rpc.getRawTransaction(txid)) }
                assert(failure.code == -28)
                assert(failure.detail == "test RPC failure")
            }
    }

    test("wrong response IDs and malformed replies fail active RPC calls") {
        withRpc { req =>
            val (status, response) = ok(req, scan())
            response("id") = -999
            (status, response)
        } { rpc => intercept[Exception] { await(rpc.scanAddress(wallet.funding.address)) } }
        withRpc(_ => (200, ujson.Obj("error" -> "missing"))) { rpc =>
            intercept[Exception] { await(rpc.getRawTransaction(txid)) }
        }
    }
}
