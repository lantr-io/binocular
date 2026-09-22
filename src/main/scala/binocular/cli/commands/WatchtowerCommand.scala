package binocular.cli.commands

import binocular.*
import binocular.cli.{Command, Console}
import binocular.notify.Notifier
import binocular.traffic.TrafficDaemon
import binocular.watchtower.Watchtower
import binocular.watchtower.Watchtower.Worker

/** Run the three Binocular daemons — oracle sync, TM relay, and TM confirm — together in a single
  * process, each on its own labeled thread (`[oracle]` / `[relay]` / `[confirm]` in the log).
  *
  * Each worker is the existing standalone command loop, reused as-is. The loops are not
  * coordinated: they rely on their own retry logic to recover from transient conflicts (such as two
  * loops briefly selecting the same wallet UTxO). A crash in one loop is contained by its
  * supervisor and does not stop the others.
  *
  * With `--dry-run` each loop runs a single pass (connectivity / config check) and the command
  * returns.
  */
case class WatchtowerCommand(dryRun: Boolean = false) extends Command {

    override def execute(config: BinocularConfig): Int = {
        Console.header("Binocular Watchtower")
        if dryRun then Console.warn("Dry-run mode — one pass per daemon, then exit")
        Console.info(
          "Daemons",
          "oracle sync, TM relay, TM confirm" +
              (if config.bridge.proofServer then
                   s", proof server (:${config.bridge.proofServerPort})"
               else "") +
              (if config.traffic.enabled then ", demo traffic" else "")
        )
        println()

        // One shared notifier for all three loops, so error debounce and the new-block height
        // dedup are coordinated across them (and there is a single background post thread).
        val notifier = Notifier.fromConfig(config.notifications)

        val registered = workers(config, notifier)
        if dryRun then
            Watchtower.runOnce(registered, timeoutMs = config.oracle.transactionTimeout * 1000L)
        else
            Watchtower.runSupervised(registered, retryDelayMs = config.oracle.retryInterval * 1000L)
        0
    }

    private[cli] def workers(config: BinocularConfig, notifier: Notifier): List[Worker] = {

        // The proof-serving REST API ([SPI-4]/[OB-13]) rides in the same process as a fourth
        // supervised worker: `startAndWait` blocks like the other loops, and a crash restarts the
        // process the same way. In dry-run it resolves both proof sources once without binding
        // (the same check `serve-proofs --dry-run` runs).
        val proofWorker =
            if config.bridge.proofServer then
                List(
                  Worker("proofs", () => { ServeProofsCommand(None, dryRun).execute(config); () })
                )
            else Nil

        List(
          Worker("oracle", () => { RunCommand(dryRun, Some(notifier)).execute(config); () }),
          Worker("relay", () => { RelayCommand(dryRun, Some(notifier)).execute(config); () }),
          Worker(
            "confirm",
            () => { ConfirmTmtxCommand(dryRun, Some(notifier)).execute(config); () }
          )
        ) ++ proofWorker ++ Option
            .when(config.traffic.enabled)(
              Worker("traffic", () => TrafficDaemon.run(config, notifier, dryRun))
            )
            .toList
    }
}
