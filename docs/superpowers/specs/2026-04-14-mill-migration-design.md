# Mill Migration Design

**Date:** 2026-04-14
**Status:** Approved

## Overview

Migrate the binocular build system from sbt 1.12.9 to mill 0.12.x. The build uses a single
`build.mill` file with three flat top-level module objects — a direct translation of the existing
`build.sbt`. No YAML configuration: the project's requirements (compiler plugin, assembly, BuildInfo,
dependency exclusions, custom JVM options) exceed what mill's YAML DSL supports.

## Module Structure

```
build.mill                  ← all 3 modules in one file
.mill-version               ← pinned mill version (e.g. 0.12.10), used by millw
millw                       ← wrapper script, auto-downloads mill
it/
  src/test/...              ← unchanged
example/
  src/main/...              ← unchanged
src/
  main/...                  ← unchanged
  test/...                  ← unchanged
```

Three flat objects in `build.mill`:

```scala
import mill._, scalalib._, scalalib.scalafmt.ScalafmtModule
import mill.contrib.buildinfo.BuildInfo

object binocular extends ScalaModule with ScalafmtModule with BuildInfo { ... }
object it        extends ScalaModule with ScalafmtModule { ... }
object example   extends ScalaModule with ScalafmtModule { ... }
```

`it` depends on `binocular` via `def moduleDeps = Seq(binocular)`. The sbt build also declares
`binocular % "compile->compile;test->test"` — verify during implementation whether `it`'s tests
actually import anything from `binocular`'s test sources; if so, reference `binocular.test` in
`it.test`'s `moduleDeps`. It sets `def forkWorkingDir = millSourcePath / os.up` so tests run from
the project root (matching sbt's `Test / baseDirectory` override).

## Plugin Mapping

| sbt concern | mill equivalent |
|---|---|
| `sbt-scalafmt` | `ScalafmtModule` (built-in contrib) |
| `sbt-assembly` | `assembly` task built into `JavaModule`; override `assemblyRules` |
| `sbt-buildinfo` | `mill.contrib.buildinfo.BuildInfo` (built-in contrib) |
| Scalus compiler plugin | `def scalacPluginMvnDeps` |
| Snapshot resolver | `def repositoriesTask` with Coursier `MavenRepository` |
| JVM opts at run | `def forkArgs` on the module |
| JVM opts at test | `def forkArgs` on the `test` inner object |
| bitcoin-s exclusions | `.exclude("org", "artifact")` on the `mvn"..."` dep |
| `Test / parallelExecution := false` | No action — mill is sequential within a module by default |
| `Test / baseDirectory` (it) | `def forkWorkingDir = millSourcePath / os.up` |
| `addCommandAlias("scalafmtAll", ...)` | `./millw __.reformat` |
| `addCommandAlias("scalafmtCheckAll", ...)` | `./millw __.checkFormat` |

## Key Configuration Details

### Scala version and scalus plugin

```scala
def scalaVersion = "3.3.7"

def scalacPluginMvnDeps = Seq(
  mvn"org.scalus:scalus-plugin_3.3.7:$scalusVersion"
)
```

### Snapshot resolver (Sonatype Central Snapshots)

```scala
def repositoriesTask = T.task {
  super.repositoriesTask() ++ Seq(
    coursier.maven.MavenRepository(
      "https://central.sonatype.com/repository/maven-snapshots/"
    )
  )
}
```

### Assembly rules

The sbt assembly merge strategy maps to mill `assemblyRules`:

```scala
import mill.scalalib.Assembly._
def assemblyRules = Seq(
  Rule.ExcludePattern("META-INF/.*"),
  Rule.ExcludePattern("module-info.class"),
  Rule.Append("reference.conf"),
  Rule.Append("application.conf"),
  // unmatched files: mill default is "take first" — no rule needed
)
def mainClass = Some("binocular.main")
```

### JVM options

```scala
// On binocular module (run)
def forkArgs = Seq("--sun-misc-unsafe-memory-access=allow")

// On test inner object
def forkArgs = Seq("-Xmx2g", "--sun-misc-unsafe-memory-access=allow")
```

### ManualTest exclusion

sbt filtered `binocular.ManualTest`-tagged tests automatically via `Tests.Argument`. Mill has no
equivalent default-arg mechanism for ScalaTest. The exclusion is applied explicitly:

- **CI**: `./millw binocular.test -- -l binocular.ManualTest`
- **Local**: same command, or omit to run all tests including manual ones

### BuildInfo

```scala
// define a top-level version val in build.mill (binocular has no PublishModule)
val binocularVersion = "0.1.0-SNAPSHOT"

def buildInfoPackageName = "binocular"
def buildInfoMembers = Seq(
  BuildInfo.Value("name", artifactName()),
  BuildInfo.Value("version", binocularVersion),
)
```

### bitcoin-s dependency exclusions

```scala
mvn"org.bitcoin-s:bitcoin-s-bitcoind-rpc_2.13:1.9.11"
  .exclude("com.lihaoyi", "upickle_2.13")
  .exclude("com.lihaoyi", "ujson_2.13")
  .exclude("com.lihaoyi", "upack_2.13")
  .exclude("com.lihaoyi", "upickle-core_2.13")
  .exclude("com.lihaoyi", "upickle-implicits_2.13")
  .exclude("com.lihaoyi", "geny_2.13")
```

## millw and Version Pinning

- `millw` wrapper script checked into repo root (standard mill wrapper)
- `.mill-version` file contains the pinned version (e.g. `0.12.10`)
- Developers use `./millw <command>` instead of `mill <command>`
- Memory note: use `./millw` like `sbtn` — prefer it over installing mill globally

## flake.nix

Both `sbt` and `mill` remain in `flake.nix` (sbt kept for reference/fallback during migration):

```nix
sbt  = pkgs.sbt.override  { jre = jdk; };
mill = pkgs.mill.override { jre = jdk; };
```

Both are added to `packages`.

## CI Changes

File: `.github/workflows/ci.yml`

1. **Install**: change `apps: sbt` to `apps: mill` in `coursier/setup-action`
2. **Cache**: replace sbt paths with mill/coursier paths
   ```yaml
   path: |
     ~/.mill
     ~/.cache/coursier
   key: ${{ runner.os }}-mill-${{ hashFiles('build.mill', '.mill-version') }}
   restore-keys: |
     ${{ runner.os }}-mill-
   ```
3. **Test command**:
   ```yaml
   run: ./millw __.checkFormat && ./millw binocular.test -- -l binocular.ManualTest
   ```
4. **`build.mill` format**: not enforced in CI (build file is small, touched rarely)

Everything else in CI is unchanged: Discord webhook, artifact upload on failure, `fetch-depth: 0`.

## What Is Removed

- `project/build.properties` (sbt version)
- `project/plugins.sbt` (sbt plugins)
- `project/metals.sbt` (Metals sbt support — Metals supports mill natively)
- `build.sbt`

sbt is kept in `flake.nix` but all sbt build files are deleted.

## Out of Scope

- Integration tests (`it/`) are not run in CI — unchanged from current behaviour
- `build.mill` format is not enforced in CI
- No YAML configuration (project complexity exceeds YAML DSL capabilities)
