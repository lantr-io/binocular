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
      */
    def runSupervised(workers: List[Worker], retryDelayMs: Long): Unit = {
        val threads = workers.map { w =>
            val t = thread(
              w,
              () => {
                  Console.setLabel(Some(w.label))
                  new Supervisor(w.label, retryDelayMs).supervise(w.run)
              }
            )
            t.start()
            t
        }
        threads.foreach(_.join())
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
