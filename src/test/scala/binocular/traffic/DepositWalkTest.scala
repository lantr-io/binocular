package binocular.traffic

import binocular.MockBitcoinRpc
import binocular.bitcoin.*
import binocular.cli.DaemonExecution
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.script.{EmptyScriptSignature, ScriptPubKey}
import org.bitcoins.core.protocol.transaction.*
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.*
import scodec.bits.ByteVector

/** [TRF-107] and [TRF-123]: our deposits are found by walking the funding UTXOs backwards. */
class DepositWalkTest extends AnyFunSuite {
    private given ExecutionContext = DaemonExecution.ec
    private val wallet =
        Bip86Wallet.fromMnemonic("abandon " * 11 + "about", BitcoinNetwork.Testnet4).toOption.get
    private val funding = ScriptPubKey.fromAsmBytes(wallet.funding.scriptPubKey)
    private val qAuth = wallet.identity.outputKey
    private val other = ScriptPubKey.fromAsmBytes(wallet.identity.scriptPubKey)
    private def beacon(key: ByteVector) =
        TransactionOutput(
          Satoshis.zero,
          ScriptPubKey.fromAsmBytes(PegInDeposit.BeaconPrefix ++ key)
        )
    private def out(sat: Long, spk: ScriptPubKey = funding) = TransactionOutput(Satoshis(sat), spk)
    private def tx(inputs: Seq[TransactionOutPoint], outputs: TransactionOutput*): Transaction =
        BaseTransaction(
          Int32.two,
          inputs.map(TransactionInput(_, EmptyScriptSignature, UInt32.zero)).toVector,
          outputs.toVector,
          UInt32.zero
        )
    private def at(t: Transaction, vout: Int) = TransactionOutPoint(t.txId, UInt32(vout))

    // Faucet -> D1 -> (D1 change + payout P0) -> D2; funding now holds D2 change and payout P1.
    private val faucet = tx(Seq.empty, out(500000))
    private val d1 = tx(Seq(at(faucet, 0)), out(30000, other), beacon(qAuth), out(469000))
    private val p0 = tx(Seq.empty, out(1, other), out(29000))
    private val d2 = tx(Seq(at(d1, 2), at(p0, 1)), out(40000, other), beacon(qAuth), out(457000))
    private val p1 = tx(Seq.empty, out(1, other), out(28000))
    // Someone else's deposit that happened to pay us change: the beacon is not ours.
    private val foreign =
        tx(Seq(at(faucet, 0)), out(30000, other), beacon(ByteVector.fill(32)(7)), out(1000))
    private val all = Seq(faucet, d1, p0, d2, p1, foreign)

    private class Rpc extends MockBitcoinRpc {
        var calls = 0
        override def getRawTransaction(txid: String) = {
            calls += 1
            val found = all.find(_.txIdBE.hex == txid).getOrElse(fail(s"unknown txid $txid"))
            Future.successful(RawTransactionInfo(txid, txid, found.hex, None, 3))
        }
    }

    test("the walk reaches every deposit through the change chain and stops at foreign outputs") {
        val rpc = new Rpc
        val walk = new DepositWalk(rpc, qAuth, 5.seconds)
        val found = walk.deposits(Seq(at(d2, 2), at(p1, 1), at(foreign, 2)))
        assert(found.map(_.outpoint) == Seq(at(d2, 0), at(d1, 0)))
        assert(found.map(_.amountSat) == Seq(40000, 30000))
        assert(found.forall(_.scriptPubKey == other.asmBytes))
        // D2, P1, foreign, D1, P0, faucet: each read once; foreign's inputs are never read.
        assert(rpc.calls == 6)
    }

    test("a raw transaction is read once for the life of the walk") {
        val rpc = new Rpc
        val walk = new DepositWalk(rpc, qAuth, 5.seconds)
        walk.deposits(Seq(at(d2, 2)))
        val before = rpc.calls
        assert(
          walk.deposits(Seq(at(d2, 2), at(p1, 1))).map(_.outpoint) == Seq(at(d2, 0), at(d1, 0))
        )
        assert(rpc.calls == before + 1, "only the new payout is read")
    }
}
