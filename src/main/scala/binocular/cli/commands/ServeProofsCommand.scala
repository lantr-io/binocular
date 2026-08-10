package binocular.cli.commands

import binocular.BinocularConfig
import binocular.cli.{Command, Console}
import binocular.server.{ProofServer, ProofService}

import scala.concurrent.ExecutionContext
import scala.util.boundary
import boundary.break

/** Run the proof-serving REST API ([SPI-4], [OB-13]) as its own process.
  *
  * The same server the watchtower embeds (see [[WatchtowerCommand]]); this command exists so a
  * proof server can run without confirming or relaying anything — serving is trustless, so anyone
  * may run one. Needs Cardano access and a Bitcoin node (for the [OB-12] bundle); no wallet.
  *
  * With `--dry-run` the command resolves both proof sources once (config, singleton, TM history,
  * oracle, confirmed-blocks MPF) and exits without binding the port.
  */
case class ServeProofsCommand(
    port: Option[Int] = None,
    dryRun: Boolean = false
) extends Command {

    override def execute(config: BinocularConfig): Int = boundary {
        given ec: ExecutionContext = ExecutionContext.global
        val bindPort = port.getOrElse(config.bridge.proofServerPort)

        Console.header("Binocular Proof Server")
        val service = ProofService.fromConfig(config, msg => Console.info("proofs", msg)) match {
            case Right(s)  => s
            case Left(err) => Console.error(err); break(1)
        }

        if dryRun then {
            Console.warn("Dry-run mode — resolving both proof sources, not binding")
            service.dryRunCheck() match {
                case Right(()) =>
                    Console.success("Both proof sources resolve; server would bind on " + bindPort)
                    0
                case Left(err) =>
                    // A pre-migration deployment (no BSS singleton) or a lagging backend is
                    // reported, not fatal to a dry-run: the server binds regardless in real runs
                    // and answers 503 until the state exists.
                    Console.warn(s"Proof sources do not fully resolve yet: ${err.message}")
                    0
            }
        } else {
            Console.info(
              "Endpoints",
              "/api/v1/spi-proof/{outpoint}, /api/v1/deposit-proof/{outpoint}, /docs"
            )
            Console.info("Port", bindPort.toString)
            ProofServer(service).startAndWait(bindPort)
            0
        }
    }
}
