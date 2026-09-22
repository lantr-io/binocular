package binocular.traffic

import binocular.bitcoin.{BitcoinRpc, PegInDeposit}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutPoint}
import org.bitcoins.crypto.DoubleSha256DigestBE
import scala.collection.mutable
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scodec.bits.ByteVector

/** One of our deposits, as the deposit transaction states it. */
final case class OurDeposit(
    outpoint: TransactionOutPoint,
    amountSat: Long,
    scriptPubKey: ByteVector
)

/** [TRF-107]: our deposits are every transaction reachable from the funding UTXOs through inputs,
  * as long as each step carries our beacon at vout 1. Change from every deposit returns to the
  * funding address and the next deposit spends it, so the chain of inputs reaches them all; a
  * payout or a faucet payment carries no beacon and ends the walk.
  */
final class DepositWalk(rpc: BitcoinRpc, qAuth: ByteVector, timeout: Duration) {
    private val beacon = PegInDeposit.BeaconPrefix ++ qAuth
    // A raw transaction never changes, so this is a cache of ledger facts, not worker state.
    // ponytail: grows by one entry per transaction ever walked; fine for a demo wallet's lifetime.
    private val known = mutable.Map.empty[DoubleSha256DigestBE, Transaction]

    private def transaction(txid: DoubleSha256DigestBE): Transaction =
        known.getOrElseUpdate(
          txid,
          Transaction.fromHex(Await.result(rpc.getRawTransaction(txid.hex), timeout).hex)
        )
    private def isOurs(tx: Transaction): Boolean =
        tx.outputs.lift(1).exists(_.scriptPubKey.asmBytes == beacon)

    /** The scriptPubKey an outpoint of a walked transaction pays. */
    def script(outpoint: TransactionOutPoint): ByteVector =
        transaction(outpoint.txIdBE).outputs(outpoint.vout.toInt).scriptPubKey.asmBytes

    /** Deposits behind the given funding outpoints, nearest first. */
    def deposits(funding: Seq[TransactionOutPoint]): Seq[OurDeposit] = {
        val found = mutable.LinkedHashMap.empty[DoubleSha256DigestBE, Transaction]
        val queue = mutable.Queue.from(funding.map(_.txIdBE).distinct)
        val seen = mutable.Set.empty[DoubleSha256DigestBE]
        while queue.nonEmpty do {
            val txid = queue.dequeue()
            if seen.add(txid) then {
                val tx = transaction(txid)
                if isOurs(tx) then {
                    found(txid) = tx
                    queue.enqueueAll(tx.inputs.map(_.previousOutput.txIdBE))
                }
            }
        }
        found.values.toSeq.map { tx =>
            val deposit = tx.outputs(PegInDeposit.DepositVout)
            OurDeposit(
              TransactionOutPoint(tx.txId, UInt32(PegInDeposit.DepositVout)),
              deposit.value.satoshis.toLong,
              deposit.scriptPubKey.asmBytes
            )
        }
    }
}
