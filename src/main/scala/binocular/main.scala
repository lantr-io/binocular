package binocular

import binocular.cli.CliApp

/** Main entry point for Binocular CLI
  *
  * This is a thin wrapper around CliApp that provides the @main annotation.
  * All CLI logic lives in the binocular.cli package.
  */
@main def main(args: String*): Unit = {
    val exitCode = CliApp.run(args)
    System.exit(exitCode)
}
