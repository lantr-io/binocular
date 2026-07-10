package binocular

import binocular.cli.DaemonExecution
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.*

class DaemonExecutionTest extends AnyFunSuite {

    test("daemon execution context runs work on virtual threads (avoids pool starvation)") {
        given ExecutionContext = DaemonExecution.ec
        val isVirtual = Await.result(Future(Thread.currentThread().isVirtual), 5.seconds)
        assert(isVirtual, "daemon EC must use virtual threads so blocking I/O never starves a pool")
    }

    test("daemon execution context does not starve under many concurrent blocking tasks") {
        given ExecutionContext = DaemonExecution.ec
        // Far more concurrent BLOCKING tasks than any bounded pool would have threads for. A
        // ForkJoinPool sized to #cores would deadlock/queue; virtual threads each get a carrier.
        val tasks = (1 to 200).map(_ => Future { Thread.sleep(100); 1 })
        val total = Await.result(Future.sequence(tasks), 10.seconds).sum
        assert(total == 200)
    }
}
