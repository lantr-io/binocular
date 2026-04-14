# Mill Migration Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Migrate the binocular build from sbt 1.12.9 to mill 1.1.5 while keeping all three modules (binocular, it, example) compiling and tests passing.

**Architecture:** Single `build.mill` with three flat `SbtModule` objects (preserving `src/main/scala` / `src/test/scala` layout). Mill 1.1.5 is pinned via `.mill-version` and downloaded on demand by the `millw` bootstrap script. CI switches from `coursier/setup-action sbt` to `mill`.

**Tech Stack:** Mill 1.1.5, Scala 3.3.7, Scalus compiler plugin, mill-contrib-buildinfo, ScalaTest, GitHub Actions

---

## File Map

| Action | Path | Purpose |
|--------|------|---------|
| Create | `millw` | Bootstrap script — downloads mill 1.1.5 if absent |
| Create | `.mill-version` | Pins mill to `1.1.5` |
| Create | `build.mill` | Full build definition (3 modules) |
| Modify | `flake.nix` | Add `mill` alongside `sbt` |
| Modify | `.github/workflows/ci.yml` | Switch from sbt to mill |
| Delete | `build.sbt` | Replaced by `build.mill` |
| Delete | `project/build.properties` | sbt version pin |
| Delete | `project/plugins.sbt` | sbt plugins |
| Delete | `project/metals.sbt` | Metals sbt support |
| Delete | `project/project/metals.sbt` | Nested Metals sbt support |

---

## Task 1: Add millw bootstrap script and version pin

**Files:**
- Create: `millw`
- Create: `.mill-version`

- [ ] **Step 1: Download the mill 1.1.5 bootstrap script**

```bash
curl -L https://repo1.maven.org/maven2/com/lihaoyi/mill-dist/1.1.5/mill-dist-1.1.5-mill.sh \
  -o /Users/nau/projects/lantr/binocular/millw
chmod +x /Users/nau/projects/lantr/binocular/millw
```

- [ ] **Step 2: Create the version pin file**

```bash
echo "1.1.5" > /Users/nau/projects/lantr/binocular/.mill-version
```

- [ ] **Step 3: Verify millw runs**

```bash
cd /Users/nau/projects/lantr/binocular && ./millw version
```

Expected: prints `1.1.5`

- [ ] **Step 4: Commit**

```bash
cd /Users/nau/projects/lantr/binocular
git add millw .mill-version
git commit -m "build: add mill 1.1.5 bootstrap script and version pin"
```

---

## Task 2: Write build.mill

**Files:**
- Create: `build.mill`

- [ ] **Step 1: Write build.mill**

Create `/Users/nau/projects/lantr/binocular/build.mill` with the following content:

```scala
//| mvnDeps: ["com.lihaoyi::mill-contrib-buildinfo:$MILL_VERSION"]
package build
import mill.*, scalalib.*, scalalib.SbtModule
import mill.contrib.buildinfo.BuildInfo
import mill.scalalib.Assembly.*

val scalusVersion = "0.16.0+206-a6df70ab-SNAPSHOT"
val binocularVersion = "0.1.0-SNAPSHOT"

object binocular extends SbtModule with BuildInfo {

  def scalaVersion = "3.3.7"

  def scalacOptions = Seq(
    "-deprecation",
    "-feature",
    "-Wunused:imports",
  )

  def scalacPluginMvnDeps = Seq(
    mvn"org.scalus:scalus-plugin_3.3.7:$scalusVersion"
  )

  def repositoriesTask = T.task {
    super.repositoriesTask() ++ Seq(
      coursier.maven.MavenRepository(
        "https://central.sonatype.com/repository/maven-snapshots/"
      )
    )
  }

  def mvnDeps = Seq(
    mvn"org.scalus::scalus:$scalusVersion",
    mvn"com.lihaoyi::upickle:4.4.3",
    mvn"com.typesafe:config:1.4.6",
    mvn"ch.qos.logback:logback-classic:1.5.32",
    mvn"com.typesafe.scala-logging::scala-logging:3.9.6",
    mvn"com.github.pureconfig::pureconfig-core:0.17.10",
    mvn"org.bouncycastle:bcprov-jdk18on:1.83",
    mvn"org.bitcoin-s:bitcoin-s-bitcoind-rpc_2.13:1.9.11"
      .exclude("com.lihaoyi" -> "upickle_2.13")
      .exclude("com.lihaoyi" -> "ujson_2.13")
      .exclude("com.lihaoyi" -> "upack_2.13")
      .exclude("com.lihaoyi" -> "upickle-core_2.13")
      .exclude("com.lihaoyi" -> "upickle-implicits_2.13")
      .exclude("com.lihaoyi" -> "geny_2.13"),
    mvn"org.scalus::scalus-testkit:$scalusVersion",
    mvn"com.monovore::decline:2.6.0",
  )

  def forkArgs = Seq("--sun-misc-unsafe-memory-access=allow")

  def mainClass = Some("binocular.main")

  def assemblyRules = Seq(
    Rule.ExcludePattern("META-INF/.*"),
    Rule.ExcludePattern("module-info.class"),
    Rule.Append("reference.conf"),
    Rule.Append("application.conf"),
  )

  def buildInfoPackageName = "binocular"
  def buildInfoMembers = Seq(
    BuildInfo.Value("name", "binocular"),
    BuildInfo.Value("version", binocularVersion),
  )

  object test extends SbtTests with TestModule.ScalaTest {
    def mvnDeps = Seq(
      mvn"com.lihaoyi::os-lib:0.11.8",
      mvn"com.lihaoyi::pprint:0.9.6",
      mvn"org.scalatest::scalatest:3.2.19",
      mvn"org.scalatestplus::scalacheck-1-18:3.2.19.0",
      mvn"org.scalacheck::scalacheck:1.19.0",
    )
    def forkArgs = Seq("-Xmx2g", "--sun-misc-unsafe-memory-access=allow")
  }
}

object it extends SbtModule {

  def scalaVersion = "3.3.7"

  def scalacOptions = Seq(
    "-deprecation",
    "-feature",
    "-Wunused:imports",
  )

  def scalacPluginMvnDeps = Seq(
    mvn"org.scalus:scalus-plugin_3.3.7:$scalusVersion"
  )

  def repositoriesTask = T.task {
    super.repositoriesTask() ++ Seq(
      coursier.maven.MavenRepository(
        "https://central.sonatype.com/repository/maven-snapshots/"
      )
    )
  }

  def moduleDeps = Seq(binocular)

  object test extends SbtTests with TestModule.ScalaTest {
    def mvnDeps = Seq(
      mvn"com.lihaoyi::os-lib:0.11.8",
      mvn"org.scalatest::scalatest:3.2.19",
    )
    def forkArgs = Seq("-Xmx2g", "--sun-misc-unsafe-memory-access=allow")
    def forkWorkingDir = T { T.workspace }
    def forkEnv = T {
      super.forkEnv() ++ Map("TESTCONTAINERS_RYUK_DISABLED" -> "true")
    }
  }
}

object example extends SbtModule {

  def scalaVersion = "3.3.7"

  def scalacOptions = Seq(
    "-deprecation",
    "-feature",
    "-Wunused:imports",
  )

  def scalacPluginMvnDeps = Seq(
    mvn"org.scalus:scalus-plugin_3.3.7:$scalusVersion"
  )

  def repositoriesTask = T.task {
    super.repositoriesTask() ++ Seq(
      coursier.maven.MavenRepository(
        "https://central.sonatype.com/repository/maven-snapshots/"
      )
    )
  }

  def moduleDeps = Seq(binocular)

  def mainClass = Some("binocular.example.bitcoinDependentLock")
}
```

- [ ] **Step 2: Verify mill can parse and resolve the build**

```bash
cd /Users/nau/projects/lantr/binocular && ./millw resolve __
```

Expected: lists all available tasks without errors. If you see a compile error in `build.mill`, fix it before proceeding. Common issues:
- `repositories` vs `repositoriesTask` — if `repositories` gives a type error use `repositoriesTask = T.task { super.repositoriesTask() ++ ... }`
- `SbtModule` import — ensure `import mill.*, scalalib.*, scalalib.SbtModule` is present

- [ ] **Step 3: Commit**

```bash
cd /Users/nau/projects/lantr/binocular
git add build.mill
git commit -m "build: add mill build.mill replacing build.sbt"
```

---

## Task 3: Verify binocular compiles

**Files:** (none new)

- [ ] **Step 1: Compile binocular main sources**

```bash
cd /Users/nau/projects/lantr/binocular && ./millw binocular.compile
```

Expected: `[1/1] binocular.compile` then `Done`. First run will be slow (downloads deps, runs Scalus compiler plugin). Subsequent runs are cached.

If compilation fails:
- **Dep resolution error** (`not found: org.scalus:scalus-plugin_3.3.7`)  — check `repositoriesTask` is returning the snapshot URL
- **Scalus plugin error** (`No such plugin`) — verify `scalacPluginMvnDeps` uses single `:` not `::` for `scalus-plugin_3.3.7`
- **Cross-version conflict** — check that `mvn"..."` with explicit `_2.13` suffix uses single `:` not `::`

- [ ] **Step 2: Compile example**

```bash
cd /Users/nau/projects/lantr/binocular && ./millw example.compile
```

Expected: `Done` with no errors.

- [ ] **Step 3: Compile it**

```bash
cd /Users/nau/projects/lantr/binocular && ./millw it.compile
```

Expected: `Done` with no errors. (No main sources in `it/`, only test sources — compiling `it.compile` may compile nothing; that's fine.)

---

## Task 4: Verify binocular tests pass

**Files:** (none new)

- [ ] **Step 1: Run binocular unit tests (excluding ManualTest)**

```bash
cd /Users/nau/projects/lantr/binocular && ./millw binocular.test -- -l binocular.ManualTest
```

Expected: all tests pass. If a test fails that was passing under sbt, investigate before continuing.

If test compilation fails due to missing test resources:
- Check `src/test/resources/` is picked up automatically by `SbtTests` (it should be — SbtTests uses `src/test/resources/`)

- [ ] **Step 2: Verify format check works**

```bash
cd /Users/nau/projects/lantr/binocular && ./millw mill.scalalib.scalafmt/checkFormatAll
```

Expected: exits 0 (all files already formatted). If any file fails, run `./millw mill.scalalib.scalafmt/` to reformat, then re-check.

- [ ] **Step 3: Commit**

```bash
cd /Users/nau/projects/lantr/binocular
git commit --allow-empty -m "build: mill binocular compile+test verified"
```

(Empty commit just to mark the checkpoint — skip if you already have uncommitted fixes to stage.)

---

## Task 5: Update flake.nix

**Files:**
- Modify: `flake.nix`

- [ ] **Step 1: Add mill to flake.nix**

Edit `/Users/nau/projects/lantr/binocular/flake.nix`. Change the `let` block and `packages` list:

```nix
let
  pkgs = import nixpkgs { inherit system; };
  jdk = pkgs.openjdk25;
  sbt = pkgs.sbt.override { jre = jdk; };
  mill = pkgs.mill.override { jre = jdk; };   # add this line
  visualvm = pkgs.visualvm.override { jdk = jdk; };
in
```

And in `packages`:

```nix
packages = with pkgs; [
  git
  jdk
  sbt
  mill        # add this line
  visualvm
  nixpkgs-fmt
  nodejs
  texliveFull
  pandoc
  bitcoind
];
```

Note: if `pkgs.mill.override { jre = jdk; }` fails with a nix error (attribute `jre` not found), use `pkgs.mill` directly without the override — `millw` will use the `java` on PATH from the nix shell regardless.

- [ ] **Step 2: Commit**

```bash
cd /Users/nau/projects/lantr/binocular
git add flake.nix
git commit -m "build: add mill to flake.nix dev shell"
```

---

## Task 6: Update CI

**Files:**
- Modify: `.github/workflows/ci.yml`

- [ ] **Step 1: Rewrite the CI workflow**

Replace the contents of `/Users/nau/projects/lantr/binocular/.github/workflows/ci.yml` with:

```yaml
name: CI

on:
  push:
    branches: ["**"]

permissions:
  contents: read

jobs:
  test:
    runs-on: ubuntu-latest
    timeout-minutes: 30

    steps:
      - name: Checkout code
        uses: actions/checkout@v4
        with:
          fetch-depth: 0

      - name: Setup Java
        uses: actions/setup-java@v4
        with:
          distribution: temurin
          java-version: 21

      - name: Cache Mill
        uses: actions/cache@v4
        with:
          path: |
            ~/.mill
            ~/.cache/coursier
          key: ${{ runner.os }}-mill-${{ hashFiles('build.mill', '.mill-version') }}
          restore-keys: |
            ${{ runner.os }}-mill-

      - name: Check formatting and run tests
        run: ./millw mill.scalalib.scalafmt/checkFormatAll && ./millw binocular.test -- -l binocular.ManualTest

      - name: List contract debug files
        if: failure()
        run: |
          echo "Current directory: $(pwd)"
          echo "Looking for .contract-debug:"
          find . -name ".contract-debug" -type d 2>/dev/null || echo "Not found with find"
          ls -la .contract-debug/ 2>/dev/null || echo ".contract-debug/ not found in current dir"
          ls -la ${{ github.workspace }}/.contract-debug/ 2>/dev/null || echo "Not found in workspace"

      - name: Upload contract debug artifacts
        uses: actions/upload-artifact@v4
        if: failure()
        with:
          name: contract-debug-${{ github.sha }}
          path: ${{ github.workspace }}/.contract-debug/
          retention-days: 30
          if-no-files-found: warn
          include-hidden-files: true

      - name: Report build status
        uses: sarisia/actions-status-discord@v1
        if: always()
        with:
          webhook: ${{ secrets.DISCORD_WEBHOOK }}
          title: "[${{ github.event.repository.name }}] ${{ job.status == 'success' && '✅' || '❌' }} - ${{ github.actor }} on ${{ github.ref_name }}"
          description: "[${{ github.event.head_commit.message }}](${{ github.event.head_commit.url }})"
          url: ${{ github.server_url }}/${{ github.repository }}/actions/runs/${{ github.run_id }}
          username: "GitHub Actions"
          avatar_url: "https://github.githubassets.com/images/modules/logos_page/GitHub-Mark.png"
          nodetail: true
          notimestamp: true
```

Key changes from the old CI:
- `coursier/setup-action` → `actions/setup-java@v4` (mill downloads itself via `millw`)
- Cache paths: `~/.sbt`, `~/.ivy2` → `~/.mill`, `~/.cache/coursier`
- Cache key: `hashFiles('build.sbt', 'project/**')` → `hashFiles('build.mill', '.mill-version')`
- Test command: `sbt "scalafmtCheckAll;scalafmtSbtCheck;test"` → `./millw mill.scalalib.scalafmt/checkFormatAll && ./millw binocular.test -- -l binocular.ManualTest`

- [ ] **Step 2: Commit**

```bash
cd /Users/nau/projects/lantr/binocular
git add .github/workflows/ci.yml
git commit -m "ci: switch from sbt to mill"
```

---

## Task 7: Remove sbt build files

**Files:**
- Delete: `build.sbt`, `project/build.properties`, `project/plugins.sbt`, `project/metals.sbt`, `project/project/metals.sbt`

- [ ] **Step 1: Delete sbt-specific files**

```bash
cd /Users/nau/projects/lantr/binocular
git rm build.sbt project/build.properties project/plugins.sbt project/metals.sbt project/project/metals.sbt
```

If any of those files don't exist in git (e.g. `project/project/metals.sbt`), that's fine — just skip the ones that fail.

- [ ] **Step 2: Verify build still works after deletion**

```bash
cd /Users/nau/projects/lantr/binocular && ./millw binocular.compile
```

Expected: compiles cleanly (mill is unaffected by the absence of sbt files).

- [ ] **Step 3: Commit**

```bash
cd /Users/nau/projects/lantr/binocular
git commit -m "build: remove sbt build files"
```

---

## Task 8: Final verification

**Files:** (none)

- [ ] **Step 1: Clean build — recompile everything from scratch**

```bash
cd /Users/nau/projects/lantr/binocular && rm -rf out/ && ./millw binocular.compile example.compile
```

Expected: all three modules compile from a cold cache.

- [ ] **Step 2: Run all unit tests**

```bash
cd /Users/nau/projects/lantr/binocular && ./millw binocular.test -- -l binocular.ManualTest
```

Expected: all tests pass.

- [ ] **Step 3: Check format**

```bash
cd /Users/nau/projects/lantr/binocular && ./millw mill.scalalib.scalafmt/checkFormatAll
```

Expected: exits 0.

- [ ] **Step 4: Verify assembly jar is buildable**

```bash
cd /Users/nau/projects/lantr/binocular && ./millw binocular.assembly
```

Expected: produces `out/binocular/assembly.dest/out.jar`. Verify main class runs:

```bash
java -jar out/binocular/assembly.dest/out.jar --help 2>&1 | head -5
```

Expected: prints binocular CLI help (not a `ClassNotFoundException`).

- [ ] **Step 5: Update memory — replace sbtn with millw**

The project memory says "always use sbtn". Update the workflow for mill: use `./millw` instead.

- [ ] **Step 6: Final commit (if any fixes were made)**

```bash
cd /Users/nau/projects/lantr/binocular
git add -p  # stage any fixups from verification
git commit -m "build: mill migration final fixes"  # only if there are changes
```

---

## Troubleshooting Reference

### `repositories` vs `repositoriesTask`
Mill 1.x uses `repositoriesTask` (a `Task[Seq[Repository]]`). If you see a type error, ensure the override is:
```scala
def repositoriesTask = T.task {
  super.repositoriesTask() ++ Seq(coursier.maven.MavenRepository("..."))
}
```

### Scalus plugin not found
The snapshot resolver must be active. The plugin is `org.scalus:scalus-plugin_3.3.7` (explicit Scala version suffix, single `:` in mvn"..." — no `::` cross-version).

### `exclude` syntax
If `.exclude("org" -> "artifact")` gives a type error, try `.exclude("org", "artifact")` (two separate strings).

### `T.workspace` in `forkWorkingDir`
In mill 1.x, if `T.workspace` is not accessible inside `T { }`, use:
```scala
def forkWorkingDir = T { millSourcePath / os.up }
```
(`millSourcePath` for `it.test` is `<root>/it`, so `os.up` goes to `<root>`.)

### `SbtTests` not found
In mill 1.x, `SbtTests` is defined in `SbtModule`. Ensure `import mill.*, scalalib.*, scalalib.SbtModule` is present. If `SbtTests` still isn't resolved, use `ScalaTests` instead and override `def sources = T.sources(millSourcePath / "src" / "test" / "scala")`.
