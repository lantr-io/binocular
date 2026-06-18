package binocular.cli.commands

import binocular.*
import binocular.attack.RoguePlanner
import binocular.cli.{Command, Console, OracleDaemon}
import scala.concurrent.ExecutionContext

case class AttackCommand(
    parent: String,
    rogueSprint: Int,
    blockSpacing: Long,
    dryRun: Boolean
) extends Command {
    override def execute(config: BinocularConfig): Int = {
        Console.header("Binocular ADVERSARIAL Oracle Daemon (Eve)")
        Console.warn(
          s"Mining rogue blocks: parent=$parent, sprint=$rogueSprint, spacing=${blockSpacing}s"
        )
        if dryRun then Console.warn("Dry-run mode — will mine one update and exit")
        println()
        given ec: ExecutionContext = ExecutionContext.global
        val planner = new RoguePlanner(parent, rogueSprint, BigInt(blockSpacing))
        new OracleDaemon(planner, dryRun).run(config)
    }
}
