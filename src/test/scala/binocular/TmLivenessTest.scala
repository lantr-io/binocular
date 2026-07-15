package binocular

import binocular.bitcoin.BitcoinRpc
import binocular.watchtower.TmLiveness

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString

import scala.concurrent.duration.*
import scala.concurrent.{ExecutionContext, Future}

class TmLivenessTest extends AnyFunSuite {

    given ExecutionContext = ExecutionContext.global

    private val timeout = 5.seconds

    // display txid = ff..00 pattern reversed; build a 36-byte outpoint from display txid + vout
    private def outpoint(displayTxid: String, vout: Int): ByteString = {
        val txidLE = displayTxid.grouped(2).toList.reverse.mkString
        val voutLE =
            Array(vout & 0xff, (vout >> 8) & 0xff, (vout >> 16) & 0xff, (vout >> 24) & 0xff)
                .map(b => f"$b%02x")
                .mkString
        ByteString.fromHex(txidLE + voutLE)
    }

    private val txidA = "aa" * 32
    private val txidB = "bb" * 32

    /** Rpc stub where only isTxOutUnspent matters; everything else is unimplemented. */
    private class StubRpc(unspent: (String, Int, Boolean) => Future[Boolean]) extends BitcoinRpc {
        override def isTxOutUnspent(
            txid: String,
            vout: Int,
            includeMempool: Boolean
        ): Future[Boolean] =
            unspent(txid, vout, includeMempool)
        def getBlockHash(height: Int) = Future.failed(new UnsupportedOperationException)
        def getBlockHeader(hash: String) = Future.failed(new UnsupportedOperationException)
        def getBlock(hash: String) = Future.failed(new UnsupportedOperationException)
        def getBlockchainInfo() = Future.failed(new UnsupportedOperationException)
        def getRawTransaction(txid: String) = Future.failed(new UnsupportedOperationException)
        def sendRawTransaction(hexString: String) = Future.failed(new UnsupportedOperationException)
    }

    test("parseOutpoint decodes LE txid and LE vout into display form") {
        val (txid, vout) = TmLiveness.parseOutpoint(outpoint(txidA, 258))
        assert(txid == txidA)
        assert(vout == 258)
    }

    test("isTxUnknown matches bitcoind -5 anywhere in the cause chain") {
        val inner = new RuntimeException(
          """HTTP 500: {"result":null,"error":{"code":-5,"message":"No such mempool or blockchain transaction. Use gettransaction for wallet transactions."},"id":3}"""
        )
        val wrapped = new RuntimeException("RPC call failed after 3ms: " + inner.getMessage, inner)
        assert(TmLiveness.isTxUnknown(wrapped))
        assert(TmLiveness.isTxUnknown(inner))
    }

    test("isTxUnknown rejects transport errors and null messages") {
        assert(
          !TmLiveness.isTxUnknown(new RuntimeException("Connection failed to http://x: refused"))
        )
        assert(!TmLiveness.isTxUnknown(new RuntimeException(null: String)))
    }

    test("firstDeadInput: all inputs unspent -> None (not yet broadcast, retry)") {
        val rpc = new StubRpc((_, _, _) => Future.successful(true))
        assert(TmLiveness.firstDeadInput(rpc, Seq(outpoint(txidA, 0)), timeout).isEmpty)
    }

    test("firstDeadInput: input spent in both confirmed and mempool views -> dead") {
        val rpc = new StubRpc((_, _, _) => Future.successful(false))
        val result = TmLiveness.firstDeadInput(
          rpc,
          Seq(outpoint(txidA, 0), outpoint(txidB, 1)),
          timeout
        )
        assert(result.contains(s"$txidA:0"))
    }

    test("firstDeadInput: parent tx only in mempool (confirmed=null, mempool=unspent) -> retry") {
        val rpc = new StubRpc((_, _, includeMempool) => Future.successful(includeMempool))
        assert(TmLiveness.firstDeadInput(rpc, Seq(outpoint(txidA, 0)), timeout).isEmpty)
    }

    test("firstDeadInput: mempool double-spend but confirmed view unspent -> retry") {
        val rpc = new StubRpc((_, _, includeMempool) => Future.successful(!includeMempool))
        assert(TmLiveness.firstDeadInput(rpc, Seq(outpoint(txidA, 0)), timeout).isEmpty)
    }

    test("firstDeadInput: RPC failure during the check -> None (unknown, retry)") {
        val rpc = new StubRpc((_, _, _) => Future.failed(new RuntimeException("node down")))
        assert(TmLiveness.firstDeadInput(rpc, Seq(outpoint(txidA, 0)), timeout).isEmpty)
    }

    test("firstDeadInput: reports the first dead input among live ones") {
        val rpc = new StubRpc((txid, _, _) => Future.successful(txid != txidB))
        val result = TmLiveness.firstDeadInput(
          rpc,
          Seq(outpoint(txidA, 0), outpoint(txidB, 7)),
          timeout
        )
        assert(result.contains(s"$txidB:7"))
    }
}
