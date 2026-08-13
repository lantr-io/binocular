package binocular.federation

/** Locates the heimdall binaries the federation scenario spawns.
  *
  * heimdall is a sibling Rust repository, not a dependency this build can resolve, so the suite
  * has to find and build it. Two paths, in order:
  *
  *   1. `HEIMDALL_BIN` / `HEIMDALL_DEPOSITOR_BIN` — a prebuilt binary. For CI, and for iterating
  *      on the Scala side without paying for cargo.
  *   2. `cargo build --release` in the sibling checkout, once per JVM.
  *
  * The build is memoized rather than skipped-if-present on purpose: a stale binary against a
  * moved checkout is the failure this suite is least equipped to diagnose, since it shows up as a
  * protocol disagreement (a different Config field, a different Taproot tree) rather than as a
  * build error. cargo's own incremental check makes the repeat cost a second or two.
  *
  * A missing toolchain FAILS rather than cancelling the suite. A silently skipped integration
  * test is indistinguishable from a passing one on the next run, which is how a suite rots.
  */
object HeimdallBuild {

    /** Sibling checkout: `$HEIMDALL_REPO`, else `~/projects/lantr/heimdall`.
      *
      * Not derived from the working directory: sbt sets `it`'s `baseDirectory` to the binocular
      * root, but this repo is also vendored as a git submodule inside ft-bifrost-bridge, where
      * the sibling is somewhere else entirely. An explicit default that the env can override
      * beats a relative walk that silently resolves to a submodule checkout nobody commits to.
      */
    private lazy val repo: os.Path = {
        val configured = sys.env.get("HEIMDALL_REPO").map(_.trim).filter(_.nonEmpty)
        val path = configured
            .map(os.Path(_))
            .getOrElse(os.home / "projects" / "lantr" / "heimdall")
        require(
          os.exists(path / "Cargo.toml"),
          s"no heimdall checkout at $path (no Cargo.toml). Set HEIMDALL_REPO to the clone, or " +
              "HEIMDALL_BIN and HEIMDALL_DEPOSITOR_BIN to prebuilt binaries."
        )
        path
    }

    /** Runs `cargo build --release` once per JVM and yields the target directory.
      *
      * heimdall's Rust toolchain comes from its own flake, so `cargo` is normally absent from the
      * environment sbt inherits; [[NixTool]] wraps the build when it has to.
      */
    private lazy val built: os.Path = {
        println(s"[heimdall] cargo build --release in $repo (first run takes minutes)")
        val res = os
            .proc(
              NixTool.cmd(
                "cargo",
                Seq("build", "--release", "--bin", "heimdall", "--bin", "depositor"),
                repo
              )
            )
            .call(cwd = repo, check = false, stdout = os.Inherit, stderr = os.Inherit)
        require(
          res.exitCode == 0,
          s"cargo build --release failed in $repo (exit ${res.exitCode}). Run it by hand — " +
              "heimdall needs `nix develop` for its toolchain — or set HEIMDALL_BIN and " +
              "HEIMDALL_DEPOSITOR_BIN to prebuilt binaries."
        )
        repo / "target" / "release"
    }

    private def resolve(envVar: String, binName: String): os.Path =
        sys.env.get(envVar).map(_.trim).filter(_.nonEmpty) match {
            case Some(p) =>
                val path = os.Path(p, os.pwd)
                require(os.exists(path), s"$envVar points at $path, which does not exist")
                path
            case None =>
                val path = built / binName
                require(os.exists(path), s"cargo build produced no $binName at $path")
                path
        }

    /** The SPO daemon: `heimdall demo`, `heimdall register-spo`, and the rest. */
    def binary(): os.Path = resolve("HEIMDALL_BIN", "heimdall")

    /** The depositor tool. A separate binary, and deliberately not part of the SPO control plane
      * — it is the one actor that cannot read the bridge Config, so every address-deciding value
      * is an explicit argument (WI-084).
      */
    def depositor(): os.Path = resolve("HEIMDALL_DEPOSITOR_BIN", "depositor")
}
