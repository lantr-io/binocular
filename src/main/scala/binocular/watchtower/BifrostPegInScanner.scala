package binocular.watchtower

import binocular.*
import binocular.bitcoin.*

import org.apache.pekko.actor.ActorSystem

import java.time.Instant
import java.time.ZoneOffset
import java.time.format.DateTimeFormatter
import scala.concurrent.{ExecutionContext, Future}

/** Scans recent Bitcoin blocks for Bifrost peg-in transactions.
  *
  * A peg-in is identified by the ONE-KEY deposit beacon `bitcoin.ak::beacon_payload` requires: an
  * OP_RETURN output with the payload `"BFR" ‖ Q_auth(32)` = 35 bytes.
  *
  * scriptPubKey hex prefix: `6a23424652` — 6a = OP_RETURN, 23 = PUSH 35 bytes, 424652 = "BFR" (0x42
  * 0x46 0x52), then Q_auth: the depositor's Taproot OUTPUT key, whose BIP-322 signature [CPI-3]
  * verifies at completion and which the deposit's refund leaf also commits. One key, both roles.
  * The retired dual-key form (PUSH67, "BFR" + D + Q_auth) is NOT scanned: the validator rejects it,
  * so a deposit carrying it can never mint.
  */
object BifrostPegInScanner {

    // OP_RETURN PUSH35 "BFR" — mirrors PegInProofBundle.BfrOpReturnPrefix.
    private val PegInPrefix = "6a23424652"

    // Whole beacon script: the 5-byte prefix + the 32-byte key = 37 bytes = 74 hex chars. Checked
    // alongside the prefix so this scanner and PegInProofBundle recognize exactly the same set of
    // outputs — a prefix match alone would also accept an over-long OP_RETURN the validator reads
    // only the first 32 payload bytes of.
    private val PegInScriptHexLength = 74

    private val DateFmt =
        DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss").withZone(ZoneOffset.UTC)

    case class OutputStatus(vout: VoutInfo, unspent: Boolean)

    case class PegIn(
        blockHeight: Int,
        blockTime: Long,
        txid: String,
        depositorKey: String, // Q_auth: the 32 bytes after "BFR", as hex
        outputs: Seq[OutputStatus] // non-OP_RETURN outputs with spent/unspent status
    )

    def scanBlocks(rpc: SimpleBitcoinRpc, numBlocks: Int)(using
        ec: ExecutionContext
    ): Future[Seq[PegIn]] =
        for
            info <- rpc.getBlockchainInfo()
            tipHeight = info.blocks
            startHeight = tipHeight - numBlocks + 1
            _ = println(
              s"Scanning blocks $startHeight to $tipHeight ($numBlocks blocks) on ${info.chain}"
            )
            results <- scanRange(rpc, startHeight, tipHeight)
        yield results

    private def scanRange(
        rpc: SimpleBitcoinRpc,
        from: Int,
        to: Int
    )(using ec: ExecutionContext): Future[Seq[PegIn]] =
        Future
            .sequence((from to to).map { height =>
                for
                    hash <- rpc.getBlockHash(height)
                    block <- rpc.getBlock(hash)
                    // Find transactions with a BFR OP_RETURN output
                    candidates = block.tx.flatMap { tx =>
                        tx.vouts.collectFirst {
                            case vout
                                if vout.scriptPubKey.startsWith(PegInPrefix) &&
                                    vout.scriptPubKey.length == PegInScriptHexLength =>
                                val depositorKey = vout.scriptPubKey.drop(PegInPrefix.length)
                                (tx, depositorKey)
                        }
                    }
                    // For each candidate, check spent status of non-OP_RETURN outputs
                    pegIns <- Future.sequence(candidates.map { (tx, depositorKey) =>
                        val valueOuts = tx.vouts.filter(!_.scriptPubKey.startsWith("6a"))
                        Future
                            .sequence(valueOuts.map { vout =>
                                // includeMempool = true matches gettxout's default (the prior
                                // 2-arg form): a mempool spend already marks the deposit taken.
                                rpc.isTxOutUnspent(tx.txid, vout.index, includeMempool = true)
                                    .map(OutputStatus(vout, _))
                            })
                            .map { statuses =>
                                PegIn(height, block.time, tx.txid, depositorKey, statuses)
                            }
                    })
                    _ = print(s"\rScanned block $height / $to (${block.tx.length} txs)  ")
                yield pegIns
            })
            .map(_.flatten.toSeq)

    def main(args: Array[String]): Unit =
        val configPath = args.headOption
        val numBlocks = args.lift(1).map(_.toInt).getOrElse(200)
        val config = BinocularConfig.load(configPath).bitcoinNode
        given system: ActorSystem = ActorSystem("bifrost-peg-in-scanner")
        given ec: ExecutionContext = system.dispatcher

        val rpc = new SimpleBitcoinRpc(config)

        scanBlocks(rpc, numBlocks)
            .map { pegIns =>
                println()
                if pegIns.isEmpty then
                    println(s"No Bifrost peg-in transactions found in the last $numBlocks blocks.")
                else
                    println(s"Found ${pegIns.length} Bifrost peg-in transaction(s):\n")
                    pegIns.foreach { p =>
                        val date = DateFmt.format(Instant.ofEpochSecond(p.blockTime))
                        println(s"block=${p.blockHeight}  date=$date  txid=${p.txid}")
                        p.outputs.foreach { os =>
                            val status = if os.unspent then "UNSPENT" else "SPENT"
                            println(
                              f"  vout=${os.vout.index}  ${os.vout.valueBtc}%.8f BTC  $status"
                            )
                        }
                        println(s"  depositor Q_auth: ${p.depositorKey}")
                        println()
                    }
            }
            .recover { case e =>
                println(s"\nScan failed: ${e.getMessage}")
            }
            .onComplete { _ =>
                system.terminate()
            }
}
