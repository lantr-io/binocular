package binocular.watchtower

import binocular.oracle.{CardanoConfig, CardanoNetwork}
import scalus.cardano.node.BlockfrostProvider
import scalus.uplc.builtin.{ByteString, Data}

/** Why a chain-history read failed, and whether trying again can fix it.
  *
  * The distinction is load-bearing rather than cosmetic. [[PorSweeper]] latches a permanent halt on
  * a failure it cannot explain, because continuing would mean submitting membership proofs the
  * ledger rejects. Latching on a rate-limit response instead would disable peg-out cleanup for the
  * rest of the process's life over one HTTP 429 — so a source MUST say which kind of failure it
  * hit, and only a `transient = false` failure is allowed to latch.
  */
final case class HistoryError(message: String, transient: Boolean) {
    override def toString: String = if transient then s"$message (transient)" else message
}

object HistoryError {
    def permanent(message: String): HistoryError = HistoryError(message, transient = false)
    def transient(message: String): HistoryError = HistoryError(message, transient = true)
}

/** One output that has EVER existed at a bridge address — spent or unspent.
  *
  * Reconstruction is built on history, not on current state: the Confirm transition spends the
  * `Unconfirmed` TM record that carries the data-availability hint, and completion spends the
  * peg-out request whose datum defines the trie entry. Both are gone from the UTxO set by the time
  * anyone needs to read them, and both remain in transaction history forever.
  *
  * @param inlineDatum
  *   the resolved inline datum, when there is one.
  * @param unresolvedDatum
  *   set when the output HAS a datum that could not be read (a datum hash the backend did not
  *   resolve, or inline bytes that failed to decode), carrying the reason.
  *
  * `inlineDatum = None, unresolvedDatum = None` therefore means the output provably carries NO
  * datum. Reconstruction relies on the three-way distinction: a TM record always carries an inline
  * datum, so a datum-less output at the TM address cannot be one and is safely skipped, while an
  * unreadable one might be a `Confirmed` record and must stop everything.
  *
  * @param assets
  *   quantity by Blockfrost `unit` (`lovelace`, or `policyHex ++ assetNameHex`).
  */
final case class ChainOutput(
    txHash: ByteString,
    outputIndex: Long,
    inlineDatum: Option[Data],
    assets: Map[String, BigInt],
    unresolvedDatum: Option[String] = None
) {
    def ref: String = s"${txHash.toHex}#$outputIndex"

    /** Quantity of `(policyHex, assetNameHex)`; 0 when absent. An empty asset name is legal. */
    def quantityOf(policyHex: String, assetNameHex: String): BigInt =
        assets.getOrElse((policyHex + assetNameHex).toLowerCase, BigInt(0))
}

/** Where [[CpoReconstruction]] reads chain history from.
  *
  * One method, deliberately: the whole algorithm needs nothing but "every output ever created at
  * this address, with its datum". That is servable by a plain Blockfrost-compatible API (address
  * transactions + per-tx UTxOs), by Kupo, or by a test fake — and the algorithm is identical for
  * all of them, which is what keeps the reconstruction path testable without a network.
  */
trait CpoHistorySource {

    /** Every output ever created at `addressBech32`, spent ones included. */
    def addressHistory(addressBech32: String): Either[HistoryError, Seq[ChainOutput]]

    /** Human-readable backend name for the log line that opens a reconstruction. */
    def backend: String
}

/** A [[CpoHistorySource]] over a Blockfrost-compatible HTTP API.
  *
  * Two endpoints, both in the subset Dolos and Yaci-store serve:
  *   - `GET /addresses/{addr}/transactions` — every transaction that touched the address;
  *   - `GET /txs/{hash}/utxos` — that transaction's outputs, with `inline_datum`.
  *
  * Kupo is NOT required (design rev 5.2): a watchtower, a demo box, or a non-SPO completer runs on
  * Blockfrost alone. The cost is one `/txs/{hash}/utxos` call per transaction at the two bridge
  * addresses, paid once at cold start.
  *
  * Outputs are filtered to `addressBech32` here, so a transaction that merely SPENDS a bridge
  * output contributes nothing — its own outputs live at other addresses.
  *
  * ==Retries==
  * A cold-start reconstruction issues one request per transaction at two addresses, which is
  * exactly the shape that trips a hosted Blockfrost project's rate limit. Retryable statuses (429
  * and 5xx) and transport exceptions are retried with exponential backoff before the call gives up,
  * and what it gives up with is a TRANSIENT [[HistoryError]] so the caller waits rather than
  * latching.
  */
final class BlockfrostCpoHistory(
    baseUrl: String,
    projectId: String,
    pageSize: Int = 100,
    maxAttempts: Int = 5,
    baseBackoffMs: Long = 500L,
    sleep: Long => Unit = ms => Thread.sleep(ms)
) extends CpoHistorySource {

    override def backend: String = s"blockfrost ($baseUrl)"

    private val client = java.net.http.HttpClient.newHttpClient()

    /** One HTTP GET, retried while the failure looks retryable.
      *
      * Backoff is exponential from `baseBackoffMs` (0.5s, 1s, 2s, 4s by default). It is a plain
      * sleep because reconstruction already runs on the confirm loop's thread and has nothing else
      * to do meanwhile.
      */
    private def get(path: String): Either[HistoryError, ujson.Value] = {
        def attempt(n: Int): Either[HistoryError, ujson.Value] = {
            val outcome =
                try {
                    val req = java.net.http.HttpRequest
                        .newBuilder()
                        .uri(java.net.URI.create(s"$baseUrl$path"))
                        .header("project_id", projectId)
                        .GET()
                        .build()
                    val resp = client.send(req, java.net.http.HttpResponse.BodyHandlers.ofString())
                    resp.statusCode() match {
                        // 404 on an address history means "no transactions", which is a legitimate
                        // empty answer (a freshly deployed bridge). Every other non-200 is an error:
                        // silently treating it as empty would reconstruct a confidently short trie.
                        case 200 => Right(ujson.read(resp.body()))
                        case 404 => Right(ujson.Arr())
                        case code if BlockfrostCpoHistory.isRetryable(code) =>
                            Left(
                              HistoryError.transient(
                                s"GET $path returned HTTP $code: ${resp.body().take(200)}"
                              )
                            )
                        case code =>
                            // 400/403/... are configuration or protocol faults (a bad project id, an
                            // address this backend refuses). Retrying cannot fix them.
                            Left(
                              HistoryError.permanent(
                                s"GET $path returned HTTP $code: ${resp.body().take(200)}"
                              )
                            )
                    }
                } catch {
                    // Every transport-level failure is transient by nature: a socket timeout, a
                    // reset connection, DNS during a blip. None of them says anything about the
                    // chain.
                    case e: Exception =>
                        Left(HistoryError.transient(s"GET $path failed: ${e.getMessage}"))
                }
            outcome match {
                case Left(err) if err.transient && n < maxAttempts =>
                    sleep(baseBackoffMs * (1L << (n - 1)))
                    attempt(n + 1)
                case Left(err) if err.transient =>
                    Left(
                      HistoryError.transient(
                        s"${err.message} — gave up after $maxAttempts attempts"
                      )
                    )
                case other => other
            }
        }
        attempt(1)
    }

    override def addressHistory(addressBech32: String): Either[HistoryError, Seq[ChainOutput]] = {
        def txPage(page: Int): Either[HistoryError, Seq[String]] =
            get(s"/addresses/$addressBech32/transactions?count=$pageSize&page=$page&order=asc")
                .map(_.arr.toSeq.map(_("tx_hash").str))

        def allTxs(page: Int, acc: Vector[String]): Either[HistoryError, Vector[String]] =
            txPage(page) match {
                case Left(err)                             => Left(err)
                case Right(items) if items.size < pageSize => Right(acc ++ items)
                case Right(items)                          => allTxs(page + 1, acc ++ items)
            }

        allTxs(1, Vector.empty).flatMap { txHashes =>
            // distinct: a transaction that both spends from and pays to the address is listed once
            // by Blockfrost, but chain reorg pagination can repeat one across page boundaries.
            val results = txHashes.distinct.map(h => outputsOf(h, addressBech32))
            results
                .collectFirst { case Left(err) => Left(err) }
                .getOrElse(Right(results.collect { case Right(outs) => outs }.flatten))
        }
    }

    private def outputsOf(
        txHash: String,
        addressBech32: String
    ): Either[HistoryError, Seq[ChainOutput]] =
        get(s"/txs/$txHash/utxos").map { json =>
            val outputs = json.obj.get("outputs").map(_.arr.toSeq).getOrElse(Seq.empty)
            outputs
                .filter(_("address").str == addressBech32)
                .map(o => BlockfrostCpoHistory.parseOutput(txHash, o))
        }
}

object BlockfrostCpoHistory {

    /** HTTP statuses worth trying again: rate limiting and any server-side fault. */
    def isRetryable(status: Int): Boolean = status == 429 || status == 408 || status >= 500

    /** Decode one Blockfrost output JSON object into a [[ChainOutput]].
      *
      * Never fails. A datum that is present but unreadable becomes `unresolvedDatum`, and the
      * ALGORITHM decides what that means: fatal at the TM address (it might be a `Confirmed`
      * record), skippable at the permissionlessly-payable peg-out address. Deciding here instead
      * would force both addresses to share one policy.
      */
    def parseOutput(txHash: String, o: ujson.Value): ChainOutput = {
        val index = o("output_index").num.toLong
        val (inline, unresolved) = o.obj.get("inline_datum") match {
            case Some(ujson.Str(hex)) =>
                try (Some(Data.fromCbor(ByteString.fromHex(hex))), None)
                catch {
                    case e: Exception =>
                        (None, Some(s"inline datum did not decode: ${e.getMessage}"))
                }
            case _ =>
                // A `data_hash` with no `inline_datum` is a datum the backend did not witness the
                // preimage of. The output HAS a datum; we just cannot read it.
                o.obj.get("data_hash") match {
                    case Some(ujson.Str(h)) =>
                        (None, Some(s"datum hash $h has no resolvable preimage on this backend"))
                    case _ => (None, None)
                }
        }
        val assets = o.obj
            .get("amount")
            .map(_.arr.toSeq)
            .getOrElse(Seq.empty)
            .map(a => a("unit").str.toLowerCase -> BigInt(a("quantity").str))
            .toMap
        ChainOutput(
          txHash = ByteString.fromHex(txHash),
          outputIndex = index,
          inlineDatum = inline,
          assets = assets,
          unresolvedDatum = unresolved
        )
    }

    /** The Blockfrost-compatible base URL for a [[CardanoConfig]], or the reason there is none.
      *
      * The `yaci` backend is Blockfrost-compatible (yaci-store serves the same routes), so it is
      * supported here too — the reconstruction path deliberately requires nothing beyond the shared
      * subset.
      */
    def baseUrlFor(cardano: CardanoConfig): Either[String, (String, String)] =
        cardano.backend.toLowerCase match {
            case "blockfrost" =>
                if cardano.blockfrostProjectId.isEmpty || cardano.blockfrostProjectId == "changeme"
                then Left("cardano.blockfrost-project-id is not set")
                else
                    val url = cardano.cardanoNetwork match {
                        case CardanoNetwork.Mainnet => BlockfrostProvider.mainnetUrl
                        case CardanoNetwork.Preprod => BlockfrostProvider.preprodUrl
                        case _                      => BlockfrostProvider.previewUrl
                    }
                    Right((url, cardano.blockfrostProjectId))
            case "yaci" => Right((cardano.yaciStoreUrl, ""))
            case other  => Left(s"backend '$other' has no Blockfrost-compatible history API")
        }

    def fromConfig(cardano: CardanoConfig): Either[String, CpoHistorySource] =
        baseUrlFor(cardano).map((url, id) => new BlockfrostCpoHistory(url, id))
}
