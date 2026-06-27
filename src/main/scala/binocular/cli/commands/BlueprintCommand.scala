package binocular.cli.commands

import binocular.*
import binocular.blueprint.BlueprintGenerator
import binocular.cli.Command

import java.nio.file.Paths

/** Generate the frozen, fully-applied blueprint for the loaded config and write it to
  * `src/main/resources/blueprints/binocular-blueprint-scalus-<v>-scala-<v>.json` (merging with any
  * existing entries). Run once per live network config to populate all deployment-keyed entries.
  */
case class BlueprintCommand() extends Command {

    override def execute(config: BinocularConfig): Int = {
        val dir = Paths.get("src/main/resources/blueprints")
        val path = BlueprintGenerator.writeMerged(
          dir,
          config,
          BuildInfo.scalusVersion,
          BuildInfo.scalaVersion
        )
        println(s"Wrote blueprint: $path")
        0
    }
}
