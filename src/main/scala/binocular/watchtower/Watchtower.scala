package binocular.watchtower

import binocular.cli.Console

/** Runs several long-lived daemon loops in one process, each on its own labeled thread.
  *
  * The watchtower runs the oracle sync, TM relay, and TM confirm loops together. They are NOT
  * coordinated: each is the existing standalone command loop, and they rely on their own retry
  * logic to recover from transient conflicts (e.g. two loops briefly selecting the same wallet
  * UTxO). A crash in one loop is contained by its [[Supervisor]] and does not stop the others.
  */
object Watchtower {

    /** A named unit of work. `run` is a full daemon loop (it normally never returns). */
    case class Worker(label: String, run: () => Unit)

    private def thread(w: Worker, body: Runnable): Thread = {
        val t = new Thread(body, s"watchtower-${w.label}")
        t
    }

    /** Production mode: one supervised, restart-on-crash thread per worker; block until all end
      * (which, for daemon loops, is never — the call runs for the life of the process).
      *
      * The [[Supervisor]] restarts a loop that returns or throws a `NonFatal` error. But a *fatal*
      * throwable — an `Error` such as `OutOfMemoryError`/`StackOverflowError` — escapes both the
      * loop's own `catch Exception` and the Supervisor's `catch NonFatal`, ending that thread while
      * the others keep running. That leaves the process half-dead yet still "active", so systemd
      * never restarts it. So: if any worker's supervised loop EVER ends (returns, or a fatal
      * throwable escapes), invoke `onWorkerExit` — by default log and exit the process, so
      * systemd's `Restart=always` brings every loop back on a fresh JVM. `onWorkerExit` is
      * injectable for tests (a fresh JVM is the only reliable recovery from OOM/stack overflow
      * anyway).
      */
    def runSupervised(
        workers: List[Worker],
        retryDelayMs: Long,
        onWorkerExit: String => Unit = Watchtower.exitProcess
    ): Unit = {
        val threads = workers.map { w =>
            val t = thread(
              w,
              () => {
                  Console.setLabel(Some(w.label))
                  try new Supervisor(w.label, retryDelayMs).supervise(w.run)
                  catch {
                      case t: Throwable =>
                          Console.logError(s"${w.label} loop crashed fatally: $t")
                  }
                  // Reaching here means the supervised loop is gone (a daemon loop must never end).
                  onWorkerExit(w.label)
              }
            )
            t.start()
            t
        }
        threads.foreach(_.join())
    }

    /** Default `onWorkerExit`: log which loop died and exit non-zero so the service manager
      * restarts the whole process (all loops) on a fresh JVM.
      */
    def exitProcess(label: String): Unit = {
        Console.logError(
          s"watchtower loop '$label' terminated — exiting so systemd restarts all loops"
        )
        System.exit(1)
    }

    /** Dry-run mode: run each worker once, concurrently, and return once they finish or `timeoutMs`
      * elapses. A worker that blocks (e.g. an already-synced oracle) does not hold up the call.
      */
    def runOnce(workers: List[Worker], timeoutMs: Long): Unit = {
        val threads = workers.map { w =>
            val t = thread(
              w,
              () => {
                  Console.setLabel(Some(w.label))
                  w.run()
              }
            )
            t.setDaemon(true) // don't keep the JVM alive if a worker is still blocking
            t.start()
            t
        }
        threads.foreach(_.join(timeoutMs))
    }
}
