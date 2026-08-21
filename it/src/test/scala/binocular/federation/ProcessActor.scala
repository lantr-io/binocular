package binocular.federation

import scala.concurrent.duration.FiniteDuration
import scala.util.matching.Regex

/** One external process in the scenario, with its output captured to a file.
  *
  * The federation scenario runs five processes at once. When an assertion fails, the question is
  * always "which one stopped doing its job, and what did it say about it" — so every actor's output
  * goes to its own file that outlives the process, and the failure dumps read from there. Streaming
  * to the test's stdout instead would interleave three SPOs' logs into an unreadable braid.
  *
  * @param name
  *   labels the log file and every error message
  * @param cmd
  *   argv, passed to os-lib unchanged
  * @param env
  *   extra environment on top of the inherited one
  * @param logDir
  *   directory for `<name>.log`; created if absent
  */
final class ProcessActor(
    val name: String,
    cmd: Seq[String],
    env: Map[String, String],
    logDir: os.Path
) {

    val logFile: os.Path = logDir / s"$name.log"

    private var proc: Option[os.SubProcess] = None

    def start(): Unit = {
        require(proc.isEmpty, s"$name: already started")
        os.makeDir.all(logDir)
        os.write.over(logFile, "")
        // stdout and stderr into ONE file: heimdall logs through tracing (stderr) but panics and
        // some CLI output land on stdout, and reading them apart loses the ordering that says
        // which came first.
        proc = Some(
          os.proc(cmd)
              .spawn(
                stdout = logFile,
                stderr = os.ProcessOutput.Readlines(line => os.write.append(logFile, line + "\n")),
                env = env
              )
        )
    }

    /** Kill the process if it is still alive. Safe to call twice, and safe when it already died —
      * teardown runs from `finally` blocks that cannot know which.
      */
    def stop(): Unit = {
        proc.foreach { p =>
            if p.isAlive() then {
                p.destroy()
                if !p.waitFor(5000) then p.destroyForcibly()
            }
        }
        proc = None
    }

    def isAlive: Boolean = proc.exists(_.isAlive())

    /** The last `n` lines of this actor's log, for a failure dump. */
    def tailLog(n: Int = 50): String =
        if !os.exists(logFile) then s"[$name] (no log file at $logFile)"
        else {
            val lines = os.read.lines(logFile)
            val tail = lines.takeRight(n)
            s"[$name] last ${tail.size} of ${lines.size} line(s) from $logFile:\n" +
                tail.map("  " + _).mkString("\n")
        }

    /** Block until a line matching `pattern` appears in the log, and return that line.
      *
      * The failure names the pattern AND shows what was logged instead: "the DKG never completed"
      * is not actionable, "waited for `group_key =`, got a Blockfrost 404 loop" is.
      */
    def awaitLogLine(pattern: Regex, timeout: FiniteDuration): String = {
        val deadline = System.currentTimeMillis() + timeout.toMillis
        while System.currentTimeMillis() < deadline do {
            if os.exists(logFile) then
                os.read.lines(logFile).find(pattern.findFirstIn(_).isDefined) match {
                    case Some(line) => return line
                    case None       => ()
                }
            Thread.sleep(200)
        }
        throw new RuntimeException(
          s"[$name] no log line matching /${pattern.regex}/ within $timeout" +
              (if isAlive then "" else " (the process is no longer running)") +
              s"\n${tailLog(50)}"
        )
    }
}

object ProcessActor {
    def apply(
        name: String,
        cmd: Seq[String],
        env: Map[String, String],
        logDir: os.Path
    ): ProcessActor = new ProcessActor(name, cmd, env, logDir)
}
