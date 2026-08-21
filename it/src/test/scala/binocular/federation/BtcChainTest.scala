package binocular.federation

import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite

/** The Bitcoin half of the scenario, on its own.
  *
  * Worth its own suite because it needs no Cardano devnet: when the full federation run fails at
  * "the deposit never confirmed", this says in seconds whether bitcoind, the nix resolution and the
  * wallet plumbing are the cause or are fine.
  */
class BtcChainTest extends AnyFunSuite with BeforeAndAfterAll {

    private val node = new RegtestBitcoind()
    private lazy val chain = new BtcChain(node)

    override def beforeAll(): Unit = node.start()
    override def afterAll(): Unit = node.stop()

    test("mining advances the tip") {
        val before = chain.tipHeight
        chain.mine(3)
        assert(chain.tipHeight == before + 3)
    }

    test("mineAndRelay calls the relay hook with the new tip") {
        var relayedTo = List.empty[Int]
        val relaying = new BtcChain(node, onBlocksMined = h => relayedTo ::= h)
        relaying.mineAndRelay(2)
        assert(relayedTo.size == 1, "the hook fires once per mineAndRelay, not once per block")
        assert(relayedTo.head == chain.tipHeight)
    }

    test("a matured wallet can send, and the send lands in the mempool then confirms") {
        chain.ensureSpendableFunds()
        assert(chain.balanceBtc > 0, "coinbase should have matured after 101 blocks")

        val dest = chain.newAddress
        val txid = chain.sendTo(dest, BigDecimal("0.001"))
        assert(chain.mempoolContains(txid), s"$txid should be in the mempool before mining")
        assert(chain.confirmations(txid) == 0)

        chain.mine(1)
        assert(!chain.mempoolContains(txid), "mining should clear it from the mempool")
        assert(chain.confirmations(txid) >= 1)
        // The raw tx is what a deposit proof is built from, so it must be retrievable by txid —
        // this is what -txindex=1 buys, and a node without it fails only much later.
        assert(chain.rawTx(txid).nonEmpty)
    }
}
