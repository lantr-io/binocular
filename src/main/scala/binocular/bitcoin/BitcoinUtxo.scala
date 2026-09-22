package binocular.bitcoin

import org.bitcoins.core.protocol.transaction.TransactionOutPoint
import scodec.bits.ByteVector

/** One unspent output of a `scantxoutset` scan. The scan reads the UTXO set, so every output here
  * is confirmed; `coinbase` is what decides whether it is also mature.
  */
final case class ScannedBitcoinOutput(
    outpoint: TransactionOutPoint,
    amountSat: Long,
    scriptPubKey: ByteVector,
    height: Int,
    coinbase: Boolean
)

final case class BitcoinUtxoScan(
    height: Int,
    bestBlockHash: String,
    outputs: Vector[ScannedBitcoinOutput]
)

final case class BitcoinRpcError(code: Int, detail: String)
    extends RuntimeException(s"Bitcoin RPC $code: $detail")
