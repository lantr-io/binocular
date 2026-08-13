package binocular.federation

import binocular.bitcoin.SimpleBitcoinRpc
import binocular.bitcoin.BitcoinNodeConfig

import scala.concurrent.ExecutionContext

/** A bitcoind regtest node, run as a subprocess for the duration of a suite.
  *
  * Lifted out of `BinocularRegtestIntegrationTest`, where it was an inner class, so the federation
  * suite does not carry a second copy that drifts from it. The RPC port is allocated rather than
  * fixed at 18543, so a leftover daemon or a second suite in the same sbt session no longer
  * collides on bind.
  *
  * `bitcoind` and `bitcoin-cli` come from this project's flake and are expected ON PATH: run sbt
  * from inside `nix develop`. That is a precondition, not something to work around — wrapping each
  * call in `nix develop --command` costs 5.4 s of shell entry per invocation, and a scenario makes
  * dozens of them. [[start]] checks the precondition once and says exactly that when it fails.
  *
  * @param wallet
  *   the wallet name to create; separate wallets keep a depositor's coins away from the miner's.
  */
final class RegtestBitcoind(val wallet: String = "test") {

    val rpcPort: Int = Ports.free()
    private val rpcUser = "test"
    private val rpcPassword = "test"
    private val dataDir = os.temp.dir(prefix = "binocular-regtest-")
    private var subProcess: Option[os.SubProcess] = None

    val rpcUrl: String = s"http://127.0.0.1:$rpcPort"

    /** Fails with the fix rather than with "No such file or directory" from inside a spawn.
      *
      * The old inner class used `assume(...)` here, which CANCELLED the suite - and a cancelled
      * integration test is indistinguishable from a passing one in the summary, which is how that
      * suite came to be skipped on every run without anyone noticing.
      */
    private def requireOnPath(bin: String): Unit =
        require(
          os.proc("sh", "-c", s"command -v $bin").call(check = false).exitCode == 0,
          s"`$bin` is not on PATH. It comes from binocular's flake - run sbt from inside " +
              "`nix develop`."
        )

    def start(): Unit = {
        requireOnPath("bitcoind")
        requireOnPath("bitcoin-cli")
        println(s"[bitcoind] starting regtest, dataDir=$dataDir, rpcPort=$rpcPort")
        val proc = os
            .proc(
              "bitcoind",
              "-regtest",
              s"-datadir=$dataDir",
              s"-rpcport=$rpcPort",
              s"-rpcuser=$rpcUser",
              s"-rpcpassword=$rpcPassword",
              "-listen=0",
              "-txindex=1",
              "-server=1",
              "-fallbackfee=0.0001",
              "-daemon=0"
            )
            .spawn(stdout = os.root / "dev" / "null", stderr = os.root / "dev" / "null")
        subProcess = Some(proc)
        waitForReady()
        cli("createwallet", wallet)
    }

    def stop(): Unit = {
        subProcess.foreach { p =>
            if p.isAlive() then {
                p.destroy()
                if !p.waitFor(5000) then p.destroyForcibly()
            }
        }
        subProcess = None
    }

    private def waitForReady(): Unit = {
        val maxAttempts = 60
        var attempts = 0
        while attempts < maxAttempts do {
            try
                if cli("getblockchaininfo").contains("\"chain\"") then {
                    println(s"[bitcoind] ready after $attempts attempt(s)")
                    return
                }
            catch { case _: Exception => () }
            Thread.sleep(500)
            attempts += 1
        }
        throw new RuntimeException(s"bitcoind not ready after ${maxAttempts * 500}ms")
    }

    /** `bitcoin-cli` against this node, on the base (no-wallet) endpoint. */
    def cli(args: String*): String =
        os.proc(
          "bitcoin-cli",
          "-regtest",
          s"-rpcport=$rpcPort",
          s"-rpcuser=$rpcUser",
          s"-rpcpassword=$rpcPassword",
          args
        ).call(timeout = 60000, stderr = os.root / "dev" / "null")
            .out
            .text()
            .trim

    /** `bitcoin-cli -rpcwallet=<wallet>` — the wallet endpoint, for balances and sends. */
    def walletCli(args: String*): String =
        cli((s"-rpcwallet=$wallet" +: args)*)

    def createRpc()(using ExecutionContext): SimpleBitcoinRpc =
        new SimpleBitcoinRpc(
          BitcoinNodeConfig(
            url = rpcUrl,
            username = rpcUser,
            password = rpcPassword,
            network = "regtest"
          )
        )

    def nodeConfig: BitcoinNodeConfig =
        BitcoinNodeConfig(
          url = rpcUrl,
          username = rpcUser,
          password = rpcPassword,
          network = "regtest"
        )
}
