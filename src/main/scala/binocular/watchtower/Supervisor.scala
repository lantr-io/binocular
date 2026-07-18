package binocular.watchtower

import binocular.cli.Console
import scala.util.control.NonFatal

/** Keeps a single watchtower worker alive: runs it, and if it returns or throws, restarts it after
  * `retryDelayMs` while still running. One `Supervisor` drives one thread; the orchestrator spawns
  * a thread per supervised loop so a crash in one leaves the others running.
  *
  * `sleep` is injectable so the restart timing can be unit-tested without real delays.
  */
class Supervisor(
    name: String,
    retryDelayMs: Long,
    sleep: Long => Unit = Thread.sleep(_)
) {
    @volatile private var running = true

    /** Signal the supervise loop to exit after the current attempt. */
    def stop(): Unit = running = false

    def isRunning: Boolean = running

    /** Run `work` under supervision until [[stop]] is called. Swallows and retries transient
      * failures, but re-raises an [[UnrecoverableWorkerError]]: retrying cannot fix it, so the
      * orchestrator must stop the whole process instead of looping every retry interval.
      */
    def supervise(work: () => Unit): Unit = {
        var first = true
        while running do {
            if !first then sleep(retryDelayMs)
            first = false
            try work()
            catch {
                // Unrecoverable (e.g. a deep reorg orphaning confirmed history) — do NOT retry;
                // propagate so runSupervised stops the process with a do-not-restart exit code.
                case e: UnrecoverableWorkerError => throw e
                case NonFatal(e) =>
                    Console.logError(
                      s"$name loop crashed: ${e.getMessage} — restarting in ${retryDelayMs}ms"
                    )
            }
        }
    }
}
