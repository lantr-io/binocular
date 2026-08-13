package binocular.federation

import org.scalatest.funsuite.AnyFunSuite

import scala.concurrent.duration.*

/** The federation suite's process plumbing, tested on its own.
  *
  * Everything here is infrastructure the real scenario cannot fail informatively without: a
  * heimdall binary that is not there, a port already taken, or a log line that never arrives all
  * surface as "the TM never appeared" five minutes into a run otherwise. Testing them here means
  * a broken workstation is diagnosed in seconds rather than at the end of the slow suite.
  */
class ScaffoldingTest extends AnyFunSuite {

    test("Ports.free hands out distinct, bindable ports") {
        val ports = List.fill(5)(Ports.free())
        assert(ports.distinct.size == 5, s"ports repeated: $ports")
        // Each one must still be bindable — `free` has to release what it reserves, or the
        // heimdall instance we hand it to fails to listen.
        ports.foreach { p =>
            val s = new java.net.ServerSocket(p)
            try assert(s.getLocalPort == p)
            finally s.close()
        }
    }

    test("ProcessActor captures output and awaits a log line") {
        val dir = os.temp.dir(prefix = "actor-test-")
        val actor =
            ProcessActor("echo-actor", Seq("sh", "-c", "echo hello-marker; sleep 30"), Map.empty, dir)
        actor.start()
        try {
            val line = actor.awaitLogLine("hello-(\\w+)".r, 10.seconds)
            assert(line.contains("hello-marker"))
            assert(os.exists(actor.logFile))
        } finally actor.stop()
    }

    test("awaitLogLine fails with the pattern and the captured tail") {
        val dir = os.temp.dir(prefix = "actor-test-")
        val actor = ProcessActor("noisy", Seq("sh", "-c", "echo wrong-line; sleep 30"), Map.empty, dir)
        actor.start()
        try {
            val err = intercept[RuntimeException](actor.awaitLogLine("never-appears".r, 2.seconds))
            assert(err.getMessage.contains("never-appears"), "must name the pattern")
            assert(err.getMessage.contains("wrong-line"), "must show what WAS logged")
        } finally actor.stop()
    }

    test("ProcessActor.stop is idempotent and survives an already-dead process") {
        val dir = os.temp.dir(prefix = "actor-test-")
        val actor = ProcessActor("shortlived", Seq("sh", "-c", "echo bye"), Map.empty, dir)
        actor.start()
        actor.awaitLogLine("bye".r, 10.seconds)
        actor.stop()
        actor.stop()
    }

    test("HeimdallBuild resolves a runnable binary") {
        val bin = HeimdallBuild.binary()
        val res = os.proc(bin, "--help").call(check = false)
        assert(res.exitCode == 0, s"$bin --help failed: ${res.err.text()}")
        assert(res.out.text().contains("Bifrost Bridge SPO program"))
    }

    test("HeimdallBuild also resolves the depositor binary") {
        val bin = HeimdallBuild.depositor()
        val res = os.proc(bin, "--help").call(check = false)
        assert(res.exitCode == 0, s"$bin --help failed: ${res.err.text()}")
    }
}
