package binocular.traffic

import binocular.BinocularConfig
import binocular.cli.{Console, DaemonExecution}
import binocular.notify.Notifier
import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal

/** The tick loop of [TRF-110] and [TRF-118]: a failed tick is logged and notified, and the next
  * tick starts from the ledgers again. There is no stopped state; a construction failure (bad
  * configuration, unreachable node) is retried every tick too, so a fixed node needs no restart.
  */
object TrafficDaemon {
    def run(config: BinocularConfig, notifier: Notifier, dryRun: Boolean): Unit = {
        given ExecutionContext = DaemonExecution.ec
        var traffic: Option[LiveTraffic] = None
        while true do {
            try {
                val live = traffic.getOrElse {
                    val started = new LiveTraffic(config, notifier)
                    started.check()
                    traffic = Some(started)
                    started
                }
                if dryRun then {
                    live.tick(dryRun = true)
                    Console.log("Traffic: dry run complete; no transactions built")
                    return
                }
                live.tick()
            } catch {
                case NonFatal(e) =>
                    val message = s"Traffic tick failed: $e"
                    Console.logError(message)
                    notifier.error("traffic", message)
                    if dryRun then return
            }
            Thread.sleep(TrafficPlan.TickSeconds * 1000)
        }
    }
}
