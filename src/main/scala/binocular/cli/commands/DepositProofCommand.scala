package binocular.cli.commands

import binocular.BinocularConfig
import binocular.cli.{Command, Console}
import binocular.server.ProofService

import scala.concurrent.ExecutionContext

/** Serve the [OB-12] deposit-inclusion bundle for one Bitcoin deposit outpoint, once, on the
  * command line.
  *
  * Thin wrapper over [[binocular.server.ProofService]] — the same oracle MPF rebuild, bundle
  * production, and JSON shape the REST endpoint `GET /api/v1/deposit-proof/{outpoint}` serves
  * ([OB-13]), so the manual command and the API cannot drift apart.
  */
case class DepositProofCommand(
    outpoint: String
) extends Command {

    override def execute(config: BinocularConfig): Int = {
        given ec: ExecutionContext = ExecutionContext.global
        ProofService.fromConfig(config, msg => Console.info("proofs", msg)) match {
            case Left(err) =>
                Console.error(err)
                1
            case Right(service) =>
                service.depositProof(outpoint) match {
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
