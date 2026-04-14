package binocular

import scalus.compiler.Options
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.{ByteString, Data}

/** Unique alwaysOk V3 minting/spending script for TMTx UTxOs.
  *
  * The script always succeeds. A random 32-byte salt is embedded in the compiled UPLC as an outer
  * lambda application. `optimizeUplc = false` prevents the optimizer from beta-reducing away the
  * salt constant, so the script hash differs from bare `PlutusV3.alwaysOk`. On-chain the CEK
  * machine performs the single beta-reduction, which is negligible for an alwaysOk script.
  */
object TmtxScript {

    /** Random 32-byte salt — included in the compiled UPLC to produce a distinct script hash. */
    private val salt: ByteString =
        ByteString.fromHex("a7f3e82b1c49d056f7a3b9c124d8e05f6a2b7c9d3e4f0a1b2c3d4e5f6a7b8c90")

    /** PlutusV3 script: always succeeds.
      *
      * Compiled with `Options.release` (error tagging on) and `optimizeUplc = false` so the salt
      * ByteString survives as a UPLC constant and the script hash is distinct.
      */
    lazy val mintingScript: PlutusV3[Data => Unit] =
        PlutusV3
            .compile((s: ByteString) => (_: Data) => ())(using
              Options.release.copy(optimizeUplc = false)
            )
            .apply(salt)
}
