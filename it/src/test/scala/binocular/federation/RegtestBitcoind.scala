package binocular.federation

import binocular.bitcoin.SimpleBitcoinRpc
import binocular.bitcoin.BitcoinNodeConfig

import scala.concurrent.ExecutionContext

/** A bitcoind regtest node, run as a subprocess for the duration of a suite.
  *
  * Lifted out of `BinocularRegtestIntegrationTest`, where it was an inner class, so the federation
  * suite does not carry a second copy that drifts from it.
  *
  * Two changes came with the lift. The RPC port is allocated rather than fixed at 18543 — two
  * suites in one sbt session, or one leftover daemon, otherwise collide on bind. And `bitcoind` is
  * resolved through [[NixTool]], because it comes from binocular's flake and is not on the PATH
  * sbt inherits unless the developer launched sbt from inside `nix develop`.
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

    /** binocular's flake, which packages bitcoind. `it`'s baseDirectory is the binocular root. */
    private def flakeDir: os.Path = os.pwd

    def start(): Unit = {
        println(s"[bitcoind] starting regtest, dataDir=$dataDir, rpcPort=$rpcPort")
        val proc = os
            .proc(
              NixTool.cmd(
                "bitcoind",
                Seq(
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
                ),
                flakeDir
              )
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
          NixTool.cmd(
            "bitcoin-cli",
            Seq(
              "-regtest",
              s"-rpcport=$rpcPort",
              s"-rpcuser=$rpcUser",
              s"-rpcpassword=$rpcPassword"
            ) ++ args,
            flakeDir
          )
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
