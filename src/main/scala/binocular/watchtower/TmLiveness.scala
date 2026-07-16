package binocular.watchtower

import binocular.bitcoin.BitcoinRpc

import scalus.uplc.builtin.ByteString
import scalus.utils.await

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration

/** Decides whether a Treasury Movement whose signed BTC tx is unknown to the node (bitcoind -5:
  * neither in the mempool nor, with txindex=1, on chain) is permanently dead or merely not
  * broadcast/mined yet.
  *
  * A TM is only marked dead on **on-chain evidence**: one of its input outpoints is an unspent
  * output in neither the confirmed-only view nor the mempool-inclusive view of `gettxout`. That
  * means the input was spent by a competing confirmed tx (or never existed), so the signed TM can
  * never be mined. Softer states stay retryable:
  *   - all inputs unspent → the relay simply hasn't broadcast the TM yet (a confirm/relay race)
  *   - input created by a mempool-only parent → confirmed view says null, mempool view says unspent
  *   - input conflicted by a mempool-only double-spend → confirmed view still says unspent
  *   - any RPC failure during the check → unknown, retry next poll
  */
object TmLiveness {

    /** bitcoind's RPC_INVALID_ADDRESS_OR_KEY (-5) message for a txid it knows nothing about. */
    private val TxUnknownMarker = "No such mempool or blockchain transaction"

    /** True iff `t` (or any cause in its chain) is bitcoind's tx-unknown error. Transport failures
      * (timeouts, connection refused) don't match and must be retried, never skipped.
      */
    def isTxUnknown(t: Throwable): Boolean = {
        Iterator
            .iterate(t)(_.getCause)
            .takeWhile(_ != null)
            .take(10) // cause chains are short; guard against cycles
            .exists(e => e.getMessage != null && e.getMessage.contains(TxUnknownMarker))
    }

    /** Parse a 36-byte Bitcoin outpoint (32-byte txid in internal/LE order ++ 4-byte LE vout) into
      * the human/RPC form (display/BE txid, vout). The vout is parsed unsigned: values ≥ 0x80000000
      * (e.g. the coinbase marker 0xffffffff) are valid outpoint encodings and must not throw — a
      * crafted TM datum would otherwise poison the whole confirm batch.
      */
    def parseOutpoint(op: ByteString): (String, Int) = {
        val hex = op.toHex // 72 hex chars
        val txid = hex.substring(0, 64).grouped(2).toList.reverse.mkString // LE -> display (BE)
        val vout =
            Integer.parseUnsignedInt(hex.substring(64, 72).grouped(2).toList.reverse.mkString, 16)
        (txid, vout)
    }

    /** First input outpoint proving the TM can never be mined (see class doc), as a
      * `displaytxid:vout` string. None = every input still spendable or unverifiable → transient.
      *
      * An outpoint that is unspent in NEITHER gettxout view is only dead if its funding tx is
      * actually known to the node: an input created by a parent tx that has not been broadcast yet
      * (a chained TM whose parent the relay hasn't sent) is also absent from both views, but is
      * merely early, not dead. Any classification error (including a parse failure on a malformed
      * outpoint) counts as alive → retried next poll, never permanently skipped.
      */
    def firstDeadInput(
        rpc: BitcoinRpc,
        outpoints: Seq[ByteString],
        timeout: Duration
    )(using ExecutionContext): Option[String] = {
        outpoints.view.flatMap { op =>
            try {
                val (txid, vout) = parseOutpoint(op)
                val dead =
                    !rpc.isTxOutUnspent(txid, vout, includeMempool = false).await(timeout)
                        && !rpc
                            .isTxOutUnspent(txid, vout, includeMempool = true)
                            .await(timeout)
                        // Distinguish "spent" from "parent not broadcast yet": only a known
                        // funding tx whose output is gone proves a double-spend. getRawTransaction
                        // throws -5 for an unknown parent → transient (caught below).
                        && rpc.getRawTransaction(txid).await(timeout) != null
                if dead then Some(s"$txid:$vout") else None
            } catch {
                case _: Throwable => None // can't verify → treat as alive, retry later
            }
        }.headOption
    }
}
