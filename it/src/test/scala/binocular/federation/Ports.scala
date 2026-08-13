package binocular.federation

import java.net.ServerSocket

/** Ephemeral-port allocation for the SPO HTTP listeners.
  *
  * heimdall's peer network takes its listen port from the roster's `bifrost_url`, so the port has
  * to be decided BEFORE the SPO is registered on chain — the test cannot let the process pick one.
  * Hardcoding 18500-18502 (as the local dkz demo does) makes two concurrent runs, or one leftover
  * process, fail with a bind error deep inside a five-minute scenario.
  *
  * Bind-then-release has a race window: nothing stops another process taking the port between the
  * close here and heimdall's bind. It is the standard trade-off, and the alternative — holding the
  * socket open — would make heimdall's own bind fail instead. `SO_REUSEADDR` keeps the port from
  * lingering in TIME_WAIT.
  */
object Ports {

    def free(): Int = {
        val socket = new ServerSocket(0)
        try {
            socket.setReuseAddress(true)
            socket.getLocalPort
        } finally socket.close()
    }
}
