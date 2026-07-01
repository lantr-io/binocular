package binocular.cli.commands

import binocular.*
import binocular.cli.{Command, Console}
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
        Console.info("Daemons", "oracle sync, TM relay, TM confirm")
        println()

        val workers = List(
          Worker("oracle", () => { RunCommand(dryRun).execute(config); () }),
          Worker("relay", () => { RelayCommand(dryRun).execute(config); () }),
          Worker("confirm", () => { ConfirmTmtxCommand(dryRun).execute(config); () })
        )

        if dryRun then
            Watchtower.runOnce(workers, timeoutMs = config.oracle.transactionTimeout * 1000L)
        else Watchtower.runSupervised(workers, retryDelayMs = config.oracle.retryInterval * 1000L)
        0
    }
}
