package binocular

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.*
import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.net.http.HttpRequest.BodyPublishers
import java.net.http.HttpResponse.BodyHandlers
import java.time.Duration as JavaDuration
import upickle.default.*
import java.util.Base64

/** Lightweight Bitcoin RPC client using Java 11+ HTTP client
  *
  * Does not depend on bitcoin-s and works with:
  *   - API-key-in-URL services (GetBlock, QuickNode)
  *   - Local Bitcoin Core nodes with username/password
  *
  * This avoids the cookie file lookup issues in bitcoin-s.
  */
class SimpleBitcoinRpc(config: BitcoinNodeConfig)(using ec: ExecutionContext) {

    private val httpClient = HttpClient
        .newBuilder()
        .connectTimeout(JavaDuration.ofSeconds(30))
        .build()

    private var requestId = 0
    private val requestTimeout = JavaDuration.ofSeconds(60)

    case class RpcRequest(
        jsonrpc: String = "2.0",
        id: Int,
        method: String,
        params: ujson.Value = ujson.Arr()
    ) derives ReadWriter

    case class RpcResponse(
        result: ujson.Value,
        error: Option[ujson.Value],
        id: Int
    ) derives ReadWriter

    /** Make a Bitcoin RPC call */
    private def call(method: String, params: ujson.Value = ujson.Arr()): Future[ujson.Value] =
        Future {
            requestId += 1
            val request = RpcRequest(id = requestId, method = method, params = params)
            val requestJson = write(request)

            val startTime = System.currentTimeMillis()

            // Build HTTP request with timeout
            val httpRequestBuilder = HttpRequest
                .newBuilder()
                .uri(URI.create(config.url))
                .header("Content-Type", "application/json")
                .timeout(requestTimeout)
                .POST(BodyPublishers.ofString(requestJson))

            // Add Basic Auth if username/password provided (for local nodes)
            if config.username.nonEmpty && config.password.nonEmpty then {
                val auth = Base64.getEncoder.encodeToString(
                  s"${config.username}:${config.password}".getBytes
                )
                httpRequestBuilder.header("Authorization", s"Basic $auth")
            }

            val httpRequest = httpRequestBuilder.build()

            // Send request with diagnostic logging
            try {
                val response = httpClient.send(httpRequest, BodyHandlers.ofString())
                val elapsed = System.currentTimeMillis() - startTime

                if elapsed > 5000 then {
                    System.err.println(s"⚠️  Slow RPC call: $method took ${elapsed}ms")
                }

                if response.statusCode() != 200 then {
                    throw new RuntimeException(
                      s"HTTP ${response.statusCode()}: ${response.body()}"
                    )
                }

                // Parse response
                val rpcResponse = read[RpcResponse](response.body())

                rpcResponse.error match {
                    case Some(error) =>
                        throw new RuntimeException(s"RPC error: $error")
                    case None =>
                        rpcResponse.result
                }
            } catch {
                case e: java.net.http.HttpTimeoutException =>
                    val elapsed = System.currentTimeMillis() - startTime
                    throw new RuntimeException(
                      s"RPC timeout after ${elapsed}ms calling $method with params $params",
                      e
                    )
                case e: java.net.ConnectException =>
                    throw new RuntimeException(
                      s"Connection failed to ${config.url}: ${e.getMessage}",
                      e
                    )
                case e: Exception =>
                    val elapsed = System.currentTimeMillis() - startTime
                    throw new RuntimeException(
                      s"RPC call failed after ${elapsed}ms: ${e.getMessage}",
                      e
                    )
            }
        }

    /** Get block hash by height */
    def getBlockHash(height: Int): Future[String] = {
        call("getblockhash", ujson.Arr(height)).map { result =>
            result.str
        }
    }

    /** Get raw block header hex by hash (verbose=false) - returns 80 bytes as hex */
    def getBlockHeaderRaw(hash: String): Future[String] = {
        call("getblockheader", ujson.Arr(hash, false)).map(_.str)
    }

    /** Get block header by hash (verbose=true) */
    def getBlockHeader(hash: String): Future[BlockHeaderInfo] = {
        call("getblockheader", ujson.Arr(hash, true)).map { result =>
            // Validate required fields are present
            val requiredFields = Seq(
              "hash",
              "height",
              "version",
              "merkleroot",
              "time",
              "nonce",
              "bits",
              "difficulty"
            )
            requiredFields.foreach { field =>
                if !result.obj.contains(field) then {
                    throw new RuntimeException(
                      s"Missing required field '$field' in getblockheader response for hash $hash"
                    )
                }
            }

            BlockHeaderInfo(
              hash = result("hash").str,
              height = result("height").num.toInt,
              version = result("version").num.toInt,
              merkleroot = result("merkleroot").str,
              time = result("time").num.toLong,
              nonce = result("nonce").num.toLong,
              bits = result("bits").str,
              difficulty = result("difficulty").num,
              previousblockhash = result.obj.get("previousblockhash").map(_.str)
            )
        }
    }

    /** Get block with full transactions (verbosity=2) */
    def getBlock(hash: String): Future[BlockInfo] = {
        call("getblock", ujson.Arr(hash, 2)).map { result =>
            // Validate required fields are present
            val requiredFields = Seq(
              "hash",
              "height",
              "version",
              "merkleroot",
              "time",
              "nonce",
              "bits",
              "difficulty",
              "tx"
            )
            requiredFields.foreach { field =>
                if !result.obj.contains(field) then {
                    throw new RuntimeException(
                      s"Missing required field '$field' in getblock response for hash $hash"
                    )
                }
            }

            val txs = result("tx").arr.map { tx =>
                TransactionInfo(
                  txid = tx("txid").str,
                  hex = tx("hex").str
                )
            }.toSeq

            BlockInfo(
              hash = result("hash").str,
              height = result("height").num.toInt,
              version = result("version").num.toInt,
              merkleroot = result("merkleroot").str,
              time = result("time").num.toLong,
              nonce = result("nonce").num.toLong,
              bits = result("bits").str,
              difficulty = result("difficulty").num,
              previousblockhash = result.obj.get("previousblockhash").map(_.str),
              tx = txs
            )
        }
    }

    /** Get best block hash (tip of the chain) */
    def getBestBlockHash(): Future[String] = {
        call("getbestblockhash").map(_.str)
    }

    /** Get blockchain info */
    def getBlockchainInfo(): Future[BlockchainInfo] = {
        call("getblockchaininfo").map { result =>
            BlockchainInfo(
              chain = result("chain").str,
              blocks = result("blocks").num.toInt,
              headers = result("headers").num.toInt,
              bestblockhash = result("bestblockhash").str
            )
        }
    }

    /** Get raw transaction by txid (verbose=true) */
    def getRawTransaction(txid: String): Future[RawTransactionInfo] = {
        call("getrawtransaction", ujson.Arr(txid, true)).map { result =>
            RawTransactionInfo(
              txid = result("txid").str,
              hash = result("hash").str,
              hex = result("hex").str,
              blockhash = result.obj.get("blockhash").map(_.str),
              confirmations = result.obj.get("confirmations").map(_.num.toInt).getOrElse(0)
            )
        }
    }
}

/** Block header information */
case class BlockHeaderInfo(
    hash: String,
    height: Int,
    version: Int,
    merkleroot: String,
    time: Long,
    nonce: Long,
    bits: String,
    difficulty: Double,
    previousblockhash: Option[String]
)

/** Transaction information */
case class TransactionInfo(
    txid: String,
    hex: String
)

/** Full block with transactions */
case class BlockInfo(
    hash: String,
    height: Int,
    version: Int,
    merkleroot: String,
    time: Long,
    nonce: Long,
    bits: String,
    difficulty: Double,
    previousblockhash: Option[String],
    tx: Seq[TransactionInfo]
)

/** Blockchain info */
case class BlockchainInfo(
    chain: String,
    blocks: Int,
    headers: Int,
    bestblockhash: String
)

/** Raw transaction information */
case class RawTransactionInfo(
    txid: String,
    hash: String,
    hex: String,
    blockhash: Option[String],
    confirmations: Int
)
