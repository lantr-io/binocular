package binocular

import binocular.cli.CliApp

/** Main entry point for Binocular CLI
  *
  * This is a thin wrapper around CliApp that provides the @main annotation. All CLI logic lives in
  * the binocular.cli package.
  */
@main def main(args: String*): Unit = {
    // Suppress verbose Scalus provider logs (uses scribe, not logback)
    scribe.Logger("scalus").withMinimumLevel(scribe.Level.Warn).replace()

    val exitCode = CliApp.run(args)
    if exitCode != 0 then throw new RuntimeException(s"Exit code: $exitCode")
}
