package binocular.traffic

import binocular.MockBitcoinRpc
import binocular.bitcoin.*
import binocular.cli.DaemonExecution
import binocular.server.ProofApi.ApiError
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.transaction.TransactionOutPoint
import org.bitcoins.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.*
import scodec.bits.ByteVector

class LiveTrafficTest extends AnyFunSuite {
    private given ExecutionContext = DaemonExecution.ec
    private val wallet =
        Bip86Wallet.fromMnemonic("abandon " * 11 + "about", BitcoinNetwork.Testnet4).toOption.get
    private val tree = PeginTreeParams(wallet.funding.outputKey, wallet.identity.outputKey, 2, 6)
    private def outPoint(fill: Int, vout: Int) =
        TransactionOutPoint(DoubleSha256Digest(ByteVector.fill(32)(fill.toByte)), UInt32(vout))
    private val point = outPoint(1, 0)
    private def coin(sat: Long, height: Int = 90, coinbase: Boolean = false) =
        ScannedBitcoinOutput(point, sat, wallet.funding.scriptPubKey, height, coinbase)
    private class Rpc extends MockBitcoinRpc {
        override def estimateSmartFeeSatPerKvb(target: Int, mode: String) =
            Future.successful(Some(2000L))
        override def getMempoolMinFeeSatPerKvb() = Future.successful(1000L)
        override def sendRawTransaction(hex: String) = fail("building must not broadcast")
    }
    private def deposit(rpc: MockBitcoinRpc, coins: Seq[ScannedBitcoinOutput]) =
        Await.result(LiveTraffic.depositTransaction(rpc, wallet, tree, 30000, coins), 5.seconds)

    test("deposit uses node fees and the fixed funding key, and only builds") {
        val tx = deposit(new Rpc, Seq(coin(200000)))
        assert(tx.outputs.head.value.satoshis.toLong == 30000)
        assert(tx.outputs.last.scriptPubKey.asmBytes == wallet.funding.scriptPubKey)
        // 2000 sat/kvB is 2 sat/vB; bitcoin-s sizes on a witness one byte larger than signed.
        val fee = 200000 - tx.outputs.map(_.value.satoshis.toLong).sum
        assert(fee >= tx.vsize * 2 && fee <= (tx.vsize + 1) * 2, s"fee $fee")
    }

    test("spendable coins exclude an immature coinbase and a mempool-spent output") {
        val spent = outPoint(2, 1)
        val rpc = new MockBitcoinRpc {
            override def isTxOutUnspent(txid: String, vout: Int, includeMempool: Boolean) = {
                assert(includeMempool, "an unconfirmed deposit spends the funding coins")
                Future.successful(txid != spent.txIdBE.hex)
            }
        }
        def spendable(outputs: ScannedBitcoinOutput*) =
            Await.result(
              LiveTraffic.spendable(rpc, BitcoinUtxoScan(100, "11" * 32, outputs.toVector)),
              5.seconds
            )
        val mature = coin(200000, height = 1, coinbase = true) // 100 confirmations
        val immature = coin(200000, height = 2, coinbase = true) // 99: one short of maturity
        val inMempool = coin(200000).copy(outpoint = spent)
        assert(spendable(mature, immature, inMempool, coin(5)) == Seq(mature, coin(5)))
    }

    private class FeeRpc(estimate: Future[Option[Long]], floor: Future[Long])
        extends MockBitcoinRpc {
        override def estimateSmartFeeSatPerKvb(target: Int, mode: String) = {
            assert(target == 6 && mode == "economical", "[TRF-72]")
            estimate
        }
        override def getMempoolMinFeeSatPerKvb() = floor
    }
    private val failed = Future.failed(new RuntimeException("node unavailable"))
    private def rate(estimate: Future[Option[Long]], floor: Future[Long]): Long =
        Await.result(LiveTraffic.feeRateSatPerKvb(new FeeRpc(estimate, floor)), 5.seconds)

    test("fee rate is the node's estimate, else 1000 sat/kvB, never below the mempool floor") {
        assert(rate(Future.successful(Some(500)), Future.successful(100)) == 500)
        assert(rate(Future.successful(Some(500)), Future.successful(2000)) == 2000)
        for missing <- Seq(Future.successful(None), Future.successful(Some(0L)), failed) do
            assert(rate(missing, Future.successful(100)) == 1000)
        assert(rate(failed, Future.successful(2000)) == 2000)
        assert(rate(Future.successful(Some(500)), failed) == 500)
        assert(rate(failed, failed) == 1000)
    }

    test("a deposit whose fee exceeds the 10000 sat cap is not built") {
        val rpc = new Rpc {
            override def estimateSmartFeeSatPerKvb(target: Int, mode: String) =
                Future.successful(Some(100000L)) // 100 sat/vB on ~200 vB
        }
        intercept[Exception](deposit(rpc, Seq(coin(200000))))
    }

    test("a failed dependency names its cause") {
        val failure = intercept[IllegalStateException](LiveTraffic.checked(Left("Blockfrost 503")))
        assert(failure.getMessage.contains("Blockfrost 503"))
        assert(LiveTraffic.checked(Right(7)) == 7)
    }

    test("deposit proof polling waits only for tx confirmation or oracle lag") {
        assert(LiveTraffic.retryableDepositProof(ApiError(404, "tx_not_confirmed", "wait")))
        assert(LiveTraffic.retryableDepositProof(ApiError(503, "oracle_lagging", "wait")))
        assert(!LiveTraffic.retryableDepositProof(ApiError(404, "not_a_deposit", "stop")))
        assert(!LiveTraffic.retryableDepositProof(ApiError(503, "backend_error", "stop")))
    }

    test("an action is described by what it does to which deposit") {
        val d = DepositView(outPoint(0xab, 0), 30000, 5, true, false, None, true)
        val ref = scalus.cardano.ledger.TransactionInput(
          scalus.cardano.ledger.TransactionHash.fromHex("cd" * 32),
          2
        )
        assert(LiveTraffic.describe(Action.Refund(d)) == s"refund ${"ab" * 32}:0 (30000 sat)")
        assert(
          LiveTraffic.describe(Action.Complete(d, ref)) ==
              s"complete ${"ab" * 32}:0 via ${"cd" * 32}#2"
        )
        assert(LiveTraffic.describe(Action.Request(d)) == s"request ${"ab" * 32}:0")
        assert(LiveTraffic.describe(Action.Deposit(12345)) == "deposit 12345 sat")
        assert(LiveTraffic.describe(Action.PegOut(12345)) == "peg out 12345 sat")
    }

    test("the peg-in UTXO id is the txid in wire order followed by the vout") {
        val displayTxid = (0 until 32).map(i => f"$i%02x").mkString
        val outpoint = TransactionOutPoint(DoubleSha256DigestBE.fromHex(displayTxid), UInt32(1))
        assert(
          LiveTraffic.pegInUtxoId(outpoint) ==
              scalus.uplc.builtin.ByteString.fromArray(
                scalus.uplc.builtin.ByteString.fromHex(displayTxid).bytes.reverse
              ) ++
              scalus.uplc.builtin.ByteString.fromHex("01000000")
        )
    }
}
