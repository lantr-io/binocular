package binocular.cli.commands

import binocular.*
import binocular.cli.Command

/** Print CIP-57 Blueprint JSON to stdout */
case class BlueprintCommand() extends Command {

    override def execute(config: BinocularConfig): Int = {
        println(BitcoinContract.blueprint.toJson())
        0
    }
}
