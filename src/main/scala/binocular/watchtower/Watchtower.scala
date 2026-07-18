package binocular.watchtower

import binocular.cli.Console

/** Marker for a worker failure that no amount of retrying can fix — only manual intervention can
  * (e.g. a deep Bitcoin reorg that orphaned the oracle's confirmed history, requiring a re-init).
  *
  * The [[Supervisor]] must NOT restart a worker that fails this way: restarting would just re-hit
  * the identical state every retry interval (the 5-second log spam this is meant to end). Instead
  * the [[Watchtower]] orchestrator stops the WHOLE process with
  * [[Watchtower.UnrecoverableExitCode]] — a code the systemd unit lists in
  * `RestartPreventExitStatus`, so the service stays down until an operator re-inits the oracle and
  * restarts it.
  */
trait UnrecoverableWorkerError extends Throwable

/** Runs several long-lived daemon loops in one process, each on its own labeled thread.
  *
  * The watchtower runs the oracle sync, TM relay, and TM confirm loops together. They are NOT
  * coordinated: each is the existing standalone command loop, and they rely on their own retry
  * logic to recover from transient conflicts (e.g. two loops briefly selecting the same wallet
  * UTxO). A crash in one loop is contained by its [[Supervisor]] and does not stop the others.
  */
object Watchtower {

    /** Process exit code signalling an unrecoverable worker state (manual re-init required). The
      * systemd unit lists this in `RestartPreventExitStatus`, so `Restart=always` does NOT bring
      * the service back — otherwise it would just re-detect the same condition on every restart.
      */
    val UnrecoverableExitCode = 3

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
      *
      * A worker that throws an [[UnrecoverableWorkerError]] is different: retrying cannot help, so
      * the [[Supervisor]] re-raises it here and we stop the WHOLE process via `onUnrecoverable` —
      * by default [[exitUnrecoverable]], which exits with [[UnrecoverableExitCode]] so systemd (via
      * `RestartPreventExitStatus`) leaves the service stopped for manual re-init instead of
      * restarting it into the same failure.
      */
    def runSupervised(
        workers: List[Worker],
        retryDelayMs: Long,
        onWorkerExit: String => Unit = Watchtower.exitProcess,
        onUnrecoverable: String => Unit = Watchtower.exitUnrecoverable
    ): Unit = {
        val threads = workers.map { w =>
            val t = thread(
              w,
              () => {
                  Console.setLabel(Some(w.label))
                  try {
                      new Supervisor(w.label, retryDelayMs).supervise(w.run)
                      // Reaching here means the supervised loop returned (a daemon loop must never
                      // end) — restart the whole process on a fresh JVM.
                      onWorkerExit(w.label)
                  } catch {
                      case _: UnrecoverableWorkerError =>
                          onUnrecoverable(w.label)
                      case t: Throwable =>
                          Console.logError(s"${w.label} loop crashed fatally: $t")
                          onWorkerExit(w.label)
                  }
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

    /** `onUnrecoverable` default: a worker hit a state manual intervention alone can fix. Log it
      * and exit with [[UnrecoverableExitCode]] so the service manager (via
      * `RestartPreventExitStatus`) leaves the watchtower stopped rather than restarting it into the
      * same failure.
      */
    def exitUnrecoverable(label: String): Unit = {
        Console.logError(
          s"watchtower loop '$label' hit an unrecoverable state — manual re-init required. " +
              s"Exiting $UnrecoverableExitCode so the service stays stopped (no auto-restart)."
        )
        System.exit(UnrecoverableExitCode)
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
