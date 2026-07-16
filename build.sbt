val scalusVersion = "0.18.2"

// Common settings for all projects
ThisBuild / scalaVersion := "3.3.8"
ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-Wunused:imports",
)
ThisBuild / resolvers += Resolver.sonatypeCentralSnapshots

Global / onChangedBuildSource := ReloadOnSourceChanges

// Command aliases to compile/format all projects (including it and example)
// which can't be aggregated due to circular dependsOn
addCommandAlias("compileAll", ";compile;it/compile;example/compile")
addCommandAlias(
  "scalafmtAll",
  ";scalafmt;Test/scalafmt;it/scalafmt;it/Test/scalafmt;example/scalafmt"
)
addCommandAlias(
  "scalafmtCheckAll",
  ";scalafmtCheck;Test/scalafmtCheck;it/scalafmtCheck;it/Test/scalafmtCheck;example/scalafmtCheck"
)

// Copies freshly generated CIP-57 blueprints over the PINNED copies in src/main/resources.
// Re-pinning is a deliberate, git-visible act: the diff shows exactly which compiledCode/hash
// changed, and committing it is the decision that the next deployment uses those scripts.
lazy val blueprintCopy = taskKey[Unit](
  "Copy generated CIP-57 blueprints from resourceManaged over the pinned copies in src/main/resources"
)

// `blueprint / skip := true` suppresses generation everywhere (including direct invocation), so
// re-pinning flips it off for one session, regenerates, and copies.
addCommandAlias(
  "blueprintPin",
  ";set binocular / blueprint / skip := false; binocular / blueprint; binocular / blueprintCopy"
)

// Root project (binocular)
lazy val binocular = (project in file("."))
    .enablePlugins(BuildInfoPlugin, ScalusBlueprintPlugin)
    .settings(
      name := "binocular",
      // Blueprints are PINNED (src/main/resources/META-INF/scalus/blueprints, committed): the
      // runtime always loads the pin, so validator edits never silently move a deployed script
      // hash — development runs freshly compiled code only in tests. Regenerate deliberately
      // with `sbt blueprintPin` when preparing a redeploy.
      blueprint / skip := true,
      blueprintCopy := {
          val pinDir = (Compile / resourceDirectory).value / "META-INF" / "scalus" / "blueprints"
          IO.createDirectory(pinDir)
          val genDir = (Compile / resourceManaged).value / "META-INF" / "scalus" / "blueprints"
          val files = Option(genDir.listFiles()).getOrElse(Array.empty[File])
              .filter(_.getName.endsWith(".json"))
          require(files.nonEmpty, s"no generated blueprints in $genDir — run the blueprint task first")
          files.foreach(f => IO.copyFile(f, pinDir / f.getName))
          streams.value.log.info(
            s"Pinned ${files.length} blueprint(s) to $pinDir — review the git diff before committing"
          )
      },
      run / fork := true,
      run / connectInput := true,
      run / javaOptions += "--sun-misc-unsafe-memory-access=allow",
      Test / parallelExecution := false,
      Test / testOptions += Tests
          .Argument(TestFrameworks.ScalaTest, "-l", "binocular.ManualTest"),
      Test / javaOptions ++= Seq("-Xmx2g", "--sun-misc-unsafe-memory-access=allow"),
      addCompilerPlugin("org.scalus" % "scalus-plugin_3.3.8" % scalusVersion),
      libraryDependencies ++= coreDependencies ++ testDependencies,
      // Assembly configuration
      assembly / assemblyMergeStrategy := {
          case PathList("META-INF", "MANIFEST.MF") => MergeStrategy.discard
          // PINNED CIP-57 blueprints — the runtime loads scripts from these; keep them ahead of
          // the blanket META-INF discard below. `deduplicate` (not `first`): with generation
          // skipped there is exactly one copy; a second differing copy on the classpath means
          // the pin is being shadowed and the build must fail loudly. The scalus jars' own
          // `blueprint-modules` compiler manifests are build-time-only — discard.
          case PathList("META-INF", "scalus", "blueprints", xs @ _*) =>
              MergeStrategy.deduplicate
          case PathList("META-INF", xs @ _*) => MergeStrategy.discard
          case PathList("module-info.class")       => MergeStrategy.discard
          case x if x.endsWith(".proto")           => MergeStrategy.first
          case x if x.contains("bouncycastle")     => MergeStrategy.first
          case "reference.conf"                    => MergeStrategy.concat
          case "application.conf"                  => MergeStrategy.concat
          case _                                   => MergeStrategy.first
      },
      assembly / mainClass := Some("binocular.main"),
      assembly / assemblyJarName := "binocular.jar",
      // BuildInfo
      buildInfoKeys := Seq[BuildInfoKey](
        name,
        version,
        scalaVersion,
        "scalusVersion" -> scalusVersion
      ),
      buildInfoPackage := "binocular"
    )

// Integration test project
lazy val it = (project in file("it"))
    .dependsOn(binocular, binocular % "compile->compile;test->test")
    .settings(
      name := "binocular-it",
      Test / parallelExecution := false,
      Test / fork := true,
      Test / baseDirectory := (binocular / baseDirectory).value,
      Test / javaOptions ++= Seq("-Xmx2g", "--sun-misc-unsafe-memory-access=allow"),
      Test / envVars += ("TESTCONTAINERS_RYUK_DISABLED" -> "true"),
      addCompilerPlugin("org.scalus" % "scalus-plugin_3.3.8" % scalusVersion),
      libraryDependencies ++= integrationTestDependencies,
      // Use test resources from root project
      Test / resourceDirectory := (binocular / Test / resourceDirectory).value
    )

// Define example as a subproject
lazy val example = (project in file("example"))
    .dependsOn(binocular)
    .settings(
      name := "binocular-example",
      addCompilerPlugin("org.scalus" % "scalus-plugin_3.3.8" % scalusVersion),
      Compile / mainClass := Some("binocular.example.bitcoinDependentLock")
    )

// Time tolerance check is always enforced in the validator (no TestMode bypass)
// Tests use time advancement within the 1-hour tolerance (e.g., +25 min for promotion)

// Core dependencies
lazy val coreDependencies = Seq(
  "org.scalus" %% "scalus" % scalusVersion,
  "com.lihaoyi" %% "upickle" % "4.4.3",
  "com.typesafe" % "config" % "1.4.6", // Configuration library
  "ch.qos.logback" % "logback-classic" % "1.5.32",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.6",
  "com.github.pureconfig" %% "pureconfig-core" % "0.17.10",
  "org.bouncycastle" % "bcprov-jdk18on" % "1.83",
  ("org.bitcoin-s" % "bitcoin-s-bitcoind-rpc_2.13" % "1.9.11").excludeAll(
    ExclusionRule(organization = "com.lihaoyi", name = "upickle_2.13"),
    ExclusionRule(organization = "com.lihaoyi", name = "ujson_2.13"),
    ExclusionRule(organization = "com.lihaoyi", name = "upack_2.13"),
    ExclusionRule(organization = "com.lihaoyi", name = "upickle-core_2.13"),
    ExclusionRule(organization = "com.lihaoyi", name = "upickle-implicits_2.13"),
    ExclusionRule(organization = "com.lihaoyi", name = "geny_2.13")
  ),
  "org.scalus" %% "scalus-testkit" % scalusVersion,
  "com.monovore" %% "decline" % "2.6.0"
)

// Unit test dependencies
lazy val testDependencies = Seq(
  "com.lihaoyi" %% "os-lib" % "0.11.8" % Test,
  "com.lihaoyi" %% "pprint" % "0.9.6" % Test,
  "org.scalatest" %% "scalatest" % "3.2.19" % Test,
  "org.scalatestplus" %% "scalacheck-1-18" % "3.2.19.0" % Test,
  "org.scalacheck" %% "scalacheck" % "1.19.0" % Test
)

// Integration test dependencies
lazy val integrationTestDependencies = Seq(
  "com.lihaoyi" %% "os-lib" % "0.11.8" % Test,
  "org.scalatest" %% "scalatest" % "3.2.19" % Test
)
