package binocular.cli

import binocular.BinocularConfig
import binocular.bitcoin.BitcoinNodeConfig
import binocular.cli.commands.WatchtowerCommand
import binocular.notify.{NoopNotifier, Notifier}
import binocular.oracle.{CardanoConfig, OracleConfig, WalletConfig}
import binocular.traffic.TrafficConfig
import org.scalatest.funsuite.AnyFunSuite

class TrafficWatchtowerTest extends AnyFunSuite {
    private val config =
        BinocularConfig(BitcoinNodeConfig(), CardanoConfig(), WalletConfig(), OracleConfig())

    test("watchtower registers traffic only when enabled; registration does not access networks") {
        val command = WatchtowerCommand(dryRun = true)
        val normal = command.workers(config, NoopNotifier).map(_.label)
        val enabled = command
            .workers(config.copy(traffic = TrafficConfig(enabled = true)), NoopNotifier)
            .map(_.label)
        assert(!normal.contains("traffic"))
        assert(enabled == normal :+ "traffic")
    }

    test("dry-run reports invalid traffic locally without throwing or starting a trip") {
        var errors = Vector.empty[String]
        val notifier = new Notifier {
            def error(source: String, message: String): Unit = { errors :+= source }
            def success(source: String, message: String): Unit = fail("must not complete a trip")
            def newBlock(
                tipHeight: BigInt,
                confirmedHeight: BigInt,
                confirmedHash: String,
                confirmedTimeIso: String,
                headersAdded: Int,
                treeBlocks: Int,
                confirmedBlocks: Int
            ): Unit = ()
        }
        val enabled = config.copy(traffic = TrafficConfig(enabled = true))
        WatchtowerCommand(dryRun = true)
            .workers(enabled, notifier)
            .find(_.label == "traffic")
            .get
            .run()
        assert(errors == Vector("traffic"))
    }
}
