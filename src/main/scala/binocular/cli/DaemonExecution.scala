package binocular.cli

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext

/** Execution context for the long-lived watchtower daemons (oracle / relay / confirm).
  *
  * Uses a virtual-thread-per-task executor (JDK 21+; we run 25) instead of
  * `ExecutionContext.global`. The daemons submit BLOCKING I/O — `SimpleBitcoinRpc` and the
  * Blockfrost provider both run `Future { httpClient.send(...) }` — and `global` is a
  * `ForkJoinPool` sized to ~availableProcessors (2 on the prod box). Enough concurrent blocking
  * calls pin every pool thread, so submitted futures stop making progress and the loops wedge
  * indefinitely (observed: the relay and confirm loops went silent for ~23h while the oracle loop
  * survived).
  *
  * Virtual threads never starve this way: each blocking call runs on its own virtual thread, so a
  * slow or stuck HTTP call parks one cheap virtual thread rather than consuming a scarce pool
  * carrier. Blocking is exactly what virtual threads are for.
  */
object DaemonExecution {
    val ec: ExecutionContext =
        ExecutionContext.fromExecutorService(Executors.newVirtualThreadPerTaskExecutor())
}
