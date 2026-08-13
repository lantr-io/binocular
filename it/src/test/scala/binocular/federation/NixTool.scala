package binocular.federation

/** Resolves external tools that live in a nix flake rather than on PATH.
  *
  * Two of this suite's dependencies are packaged that way: `bitcoind` comes from binocular's own
  * flake, and `cargo` from heimdall's. Neither is on the PATH sbt inherits unless the developer
  * happened to launch sbt from inside `nix develop`, and "happened to" is not a foundation for a
  * suite that otherwise runs itself.
  *
  * So: use the tool directly when it is already reachable, and otherwise run it through
  * `nix develop --command`, which executes one command in the flake's dev shell and exits.
  * Entering the shell costs a second or two per invocation — worth paying to keep `sbt it/test`
  * working from any shell.
  */
object NixTool {

    private def onPath(bin: String): Boolean =
        os.proc("sh", "-c", s"command -v $bin").call(check = false).exitCode == 0

    private def nixAvailable: Boolean = onPath("nix")

    /** Argv prefix that makes `bin` runnable, given the flake that packages it.
      *
      * Empty when the tool is already on PATH. Throws — rather than returning something that
      * fails later inside a process spawn — because a missing toolchain is a workstation problem
      * the developer must fix, and the message has to say which tool and which flake.
      */
    def prefixFor(bin: String, flakeDir: os.Path): Seq[String] =
        if onPath(bin) then Nil
        else if os.exists(flakeDir / "flake.nix") && nixAvailable then
            Seq("nix", "develop", flakeDir.toString, "--command")
        else
            throw new IllegalStateException(
              s"`$bin` is not on PATH and there is no usable nix flake at $flakeDir. " +
                  s"Enter that project's `nix develop` before running this suite, or install $bin."
            )

    /** `prefixFor` applied to a whole command line. */
    def cmd(bin: String, args: Seq[String], flakeDir: os.Path): Seq[String] =
        prefixFor(bin, flakeDir) ++ (bin +: args)
}
