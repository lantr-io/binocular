package binocular.cli.commands

import binocular.BinocularConfig
import binocular.cli.{Command, Console}
import binocular.server.ProofService

import scala.concurrent.ExecutionContext

/** Serve the [CPI-9] swept-peg-ins membership proof for one deposit outpoint ([SPI-4]), once, on
  * the command line.
  *
  * Thin wrapper over [[binocular.server.ProofService]] — the same resolution, walk, [SPI-6]
  * reconciliation, and JSON shape the REST endpoint `GET /api/v1/spi-proof/{outpoint}` serves, so
  * the manual command and the API cannot drift apart. The raw TM bytes come from the spent
  * `Unconfirmed` records at the TM address via the scalus provider; no Bitcoin node is needed.
  */
case class SpiProofCommand(
    outpoint: String
) extends Command {

    override def execute(config: BinocularConfig): Int = {
        given ec: ExecutionContext = ExecutionContext.global
        ProofService.fromConfig(config, msg => Console.info("proofs", msg)) match {
            case Left(err) =>
                Console.error(err)
                1
            case Right(service) =>
                service.spiProof(outpoint) match {
                    case Right(json) =>
                        println(json)
                        0
                    case Left(err) =>
                        Console.error(s"${err.code}: ${err.message}")
                        1
                }
        }
    }
}
