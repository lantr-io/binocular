package binocular.cli

/** Base trait for all CLI commands
  *
  * Each command implements this trait and provides its execution logic. This allows for better
  * separation of concerns, easier testing, and cleaner maintainability.
  */
trait Command {

    /** Execute the command
      *
      * @return
      *   Exit code (0 for success, non-zero for error)
      */
    def execute(): Int
}

/** Helper utilities for commands */
object CommandHelpers {

    /** Parse UTxO reference string (TX_HASH:OUTPUT_INDEX)
      *
      * @param utxo
      *   UTxO reference string
      * @return
      *   Either error message or (txHash, outputIndex)
      */
    def parseUtxo(utxo: String): Either[String, (String, Int)] = {
        val parts = utxo.split(":")
        if parts.length != 2 then {
            Left(s"Invalid UTxO format. Expected: <TX_HASH>:<OUTPUT_INDEX>")
        } else {
            parts(1).toIntOption match {
                case Some(index) => Right((parts(0), index))
                case None        => Left(s"Invalid output index: ${parts(1)}")
            }
        }
    }

    /** Print error and exit
      *
      * @param message
      *   Error message
      * @param exitCode
      *   Exit code (default: 1)
      */
    def exitWithError(message: String, exitCode: Int = 1): Nothing = {
        System.err.println(s"Error: $message")
        System.exit(exitCode)
        throw new RuntimeException() // Never reached
    }
}
