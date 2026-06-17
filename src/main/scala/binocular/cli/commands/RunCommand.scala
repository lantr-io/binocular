package binocular.cli.commands

import binocular.cli.{Command, Console, HonestPlanner, OracleDaemon}
import binocular.*
import scala.concurrent.ExecutionContext

case class RunCommand(dryRun: Boolean = false) extends Command {
    override def execute(config: BinocularConfig): Int = {
        Console.header("Binocular Oracle Daemon")
        if dryRun then Console.warn("Dry-run mode — will compute one update and exit")
        println()
        given ec: ExecutionContext = ExecutionContext.global
        new OracleDaemon(new HonestPlanner(config.oracle.maxHeadersPerTx), dryRun).run(config)
    }
}
