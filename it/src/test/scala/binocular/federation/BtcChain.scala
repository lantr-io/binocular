package binocular.federation

/** The Bitcoin side of the scenario, as verbs.
  *
  * A scenario step is "mine three blocks and let the oracle catch up", not "call generatetoaddress,
  * then fetch each header, then submit an oracle update". [[mineAndRelay]] is that one verb; the
  * relay half is supplied by whoever owns the oracle, so this class stays a thin, testable shell
  * over bitcoind and cannot drift into scenario logic.
  *
  * @param node
  *   the running regtest daemon
  * @param onBlocksMined
  *   called after each mine with the new tip height — the oracle relay hook. Default no-op so the
  *   Bitcoin half can be tested, and used, before an oracle exists.
  */
final class BtcChain(val node: RegtestBitcoind, onBlocksMined: Int => Unit = _ => ()) {

    /** Coinbase maturity: a freshly mined output is spendable only 100 blocks later. */
    val CoinbaseMaturity = 101

    private var minerAddress: Option[String] = None

    /** The wallet address blocks are mined to; created once so the balance accumulates. */
    def miner: String = minerAddress.getOrElse {
        val addr = node.walletCli("getnewaddress")
        minerAddress = Some(addr)
        addr
    }

    def tipHeight: Int = node.cli("getblockcount").toInt

    /** Mine `n` blocks. Does NOT relay to the oracle — use [[mineAndRelay]] for that. */
    def mine(n: Int): Unit = {
        node.cli("generatetoaddress", n.toString, miner)
        ()
    }

    /** Mine `n` blocks and let the oracle follow them. The scenario's normal verb: an unrelayed
      * block is invisible to every bridge participant, so mining without relaying is the setup
      * step, not the operation.
      */
    def mineAndRelay(n: Int): Unit = {
        mine(n)
        onBlocksMined(tipHeight)
    }

    /** Mine until the wallet has spendable coins. Idempotent: cheap when already funded. */
    def ensureSpendableFunds(): Unit =
        if balanceBtc < 1.0 then mine(CoinbaseMaturity)

    def balanceBtc: BigDecimal = BigDecimal(node.walletCli("getbalance"))

    def newAddress: String = node.walletCli("getnewaddress")

    /** Send BTC from the miner wallet and return the txid. Leaves it in the mempool. */
    def sendTo(address: String, btc: BigDecimal): String =
        node.walletCli("sendtoaddress", address, btc.toString)

    def mempool: Seq[String] = {
        val raw = node.cli("getrawmempool")
        // Minimal JSON array of quoted hex strings; a parser would be a dependency for nothing.
        "[0-9a-f]{64}".r.findAllIn(raw).toSeq
    }

    def mempoolContains(txid: String): Boolean = mempool.contains(txid.toLowerCase)

    /** Confirmations of `txid`, or 0 when it is unconfirmed or unknown. */
    def confirmations(txid: String): Int = {
        val raw = node.walletCli("gettransaction", txid)
        "\"confirmations\"\\s*:\\s*(-?\\d+)".r.findFirstMatchIn(raw).map(_.group(1).toInt).getOrElse(0)
    }

    def rawTx(txid: String): String = node.cli("getrawtransaction", txid)
}
