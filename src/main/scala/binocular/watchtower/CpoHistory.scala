package binocular.watchtower

import binocular.oracle.{CardanoConfig, CardanoNetwork}
import scalus.cardano.node.BlockfrostProvider
import scalus.uplc.builtin.{ByteString, Data}

/** One output that has EVER existed at a bridge address — spent or unspent.
  *
  * Reconstruction is built on history, not on current state: the Confirm transition spends the
  * `Unconfirmed` TM record that carries the data-availability hint, and completion spends the
  * peg-out request whose datum defines the trie entry. Both are gone from the UTxO set by the time
  * anyone needs to read them, and both remain in transaction history forever.
  *
  * @param assets
  *   quantity by Blockfrost `unit` (`lovelace`, or `policyHex ++ assetNameHex`).
  */
final case class ChainOutput(
    txHash: ByteString,
    outputIndex: Long,
    inlineDatum: Option[Data],
    assets: Map[String, BigInt]
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
    def addressHistory(addressBech32: String): Either[String, Seq[ChainOutput]]

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
  */
final class BlockfrostCpoHistory(baseUrl: String, projectId: String, pageSize: Int = 100)
    extends CpoHistorySource {

    override def backend: String = s"blockfrost ($baseUrl)"

    private val client = java.net.http.HttpClient.newHttpClient()

    private def get(path: String): Either[String, ujson.Value] =
        try {
            val req = java.net.http.HttpRequest
                .newBuilder()
                .uri(java.net.URI.create(s"$baseUrl$path"))
                .header("project_id", projectId)
                .GET()
                .build()
            val resp = client.send(req, java.net.http.HttpResponse.BodyHandlers.ofString())
            resp.statusCode() match {
                // 404 on an address history means "no transactions", which is a legitimate empty
                // answer (a freshly deployed bridge). Every other non-200 is an error: silently
                // treating it as empty would reconstruct a confidently short trie.
                case 200 => Right(ujson.read(resp.body()))
                case 404 => Right(ujson.Arr())
                case code =>
                    Left(s"GET $path returned HTTP $code: ${resp.body().take(200)}")
            }
        } catch {
            case e: Exception => Left(s"GET $path failed: ${e.getMessage}")
        }

    override def addressHistory(addressBech32: String): Either[String, Seq[ChainOutput]] = {
        def txPage(page: Int): Either[String, Seq[String]] =
            get(s"/addresses/$addressBech32/transactions?count=$pageSize&page=$page&order=asc")
                .map(_.arr.toSeq.map(_("tx_hash").str))

        def allTxs(page: Int, acc: Vector[String]): Either[String, Vector[String]] =
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
    ): Either[String, Seq[ChainOutput]] =
        get(s"/txs/$txHash/utxos").flatMap { json =>
            val outputs = json.obj.get("outputs").map(_.arr.toSeq).getOrElse(Seq.empty)
            val mine = outputs.filter(_("address").str == addressBech32)
            val decoded = mine.map(o => BlockfrostCpoHistory.parseOutput(txHash, o))
            decoded
                .collectFirst { case Left(err) => Left(s"tx $txHash: $err") }
                .getOrElse(Right(decoded.collect { case Right(out) => out }))
        }
}

object BlockfrostCpoHistory {

    /** Decode one Blockfrost output JSON object into a [[ChainOutput]].
      *
      * A PRESENT-but-undecodable `inline_datum` is an error, not a silent `None`: the caller treats
      * a missing datum at the TM address as a hard failure precisely so a Confirmed record can
      * never be dropped unnoticed, and turning a decode failure into `None` would route around
      * that.
      */
    def parseOutput(txHash: String, o: ujson.Value): Either[String, ChainOutput] = {
        val datum = o.obj.get("inline_datum") match {
            case Some(ujson.Str(hex)) =>
                try Right(Some(Data.fromCbor(ByteString.fromHex(hex))))
                catch {
                    case e: Exception =>
                        Left(
                          s"output ${o("output_index").num.toInt}: inline datum: ${e.getMessage}"
                        )
                }
            case _ => Right(None)
        }
        datum.map { d =>
            val assets = o.obj
                .get("amount")
                .map(_.arr.toSeq)
                .getOrElse(Seq.empty)
                .map(a => a("unit").str.toLowerCase -> BigInt(a("quantity").str))
                .toMap
            ChainOutput(
              txHash = ByteString.fromHex(txHash),
              outputIndex = o("output_index").num.toLong,
              inlineDatum = d,
              assets = assets
            )
        }
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
