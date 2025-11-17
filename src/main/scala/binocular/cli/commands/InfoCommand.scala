package binocular.cli.commands

import binocular.{CardanoConfig, OracleConfig}
import binocular.cli.Command

/** Display oracle information (script address, network, backend) */
case class InfoCommand() extends Command {

    override def execute(): Int = {
        println("Binocular Bitcoin Oracle")
        println("=" * 60)

        // Load configuration
        val oracleConfig = OracleConfig.load()
        val cardanoConfig = CardanoConfig.load()

        (oracleConfig, cardanoConfig) match {
            case (Right(oracle), Right(cardano)) =>
                println(s"Network:        ${oracle.network}")
                println(s"Script Address: ${oracle.scriptAddress}")
                println(s"Backend:        ${cardano.backend}")
                println(s"Start Height:   ${oracle.startHeight.getOrElse("not set")}")
                0

            case (Left(err), _) =>
                System.err.println(s"Error loading oracle config: $err")
                1

            case (_, Left(err)) =>
                System.err.println(s"Error loading cardano config: $err")
                1
        }
    }
}
