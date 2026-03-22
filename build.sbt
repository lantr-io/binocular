val scalusVersion = "0.16.0+37-11f23b8d-SNAPSHOT"

// Common settings for all projects
ThisBuild / scalaVersion := "3.3.7"
ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-Wunused:imports",
)
ThisBuild / resolvers += Resolver.sonatypeCentralSnapshots

Global / onChangedBuildSource := ReloadOnSourceChanges

// Root project (binocular)
lazy val binocular = (project in file("."))
    .enablePlugins(BuildInfoPlugin)
    .settings(
      name := "binocular",
      run / fork := true,
      run / connectInput := true,
      Test / parallelExecution := false,
      Test / javaOptions ++= Seq("-Xmx2g"),
      addCompilerPlugin("org.scalus" % "scalus-plugin_3.3.7" % scalusVersion),
      libraryDependencies ++= coreDependencies ++ testDependencies,
      // Assembly configuration
      assembly / assemblyMergeStrategy := {
          case PathList("META-INF", "MANIFEST.MF") => MergeStrategy.discard
          case PathList("META-INF", xs @ _*)       => MergeStrategy.discard
          case PathList("module-info.class")       => MergeStrategy.discard
          case x if x.endsWith(".proto")           => MergeStrategy.first
          case x if x.contains("bouncycastle")     => MergeStrategy.first
          case "reference.conf"                    => MergeStrategy.concat
          case "application.conf"                  => MergeStrategy.concat
          case _                                   => MergeStrategy.first
      },
      assembly / mainClass := Some("binocular.main"),
      // BuildInfo
      buildInfoKeys := Seq[BuildInfoKey](name, version),
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
      Test / javaOptions ++= Seq("-Xmx2g"),
      Test / envVars += ("TESTCONTAINERS_RYUK_DISABLED" -> "true"),
      addCompilerPlugin("org.scalus" % "scalus-plugin_3.3.7" % scalusVersion),
      libraryDependencies ++= integrationTestDependencies,
      // Use test resources from root project
      Test / resourceDirectory := (binocular / Test / resourceDirectory).value
    )

// Define example as a subproject
lazy val example = (project in file("example"))
    .dependsOn(binocular)
    .settings(
      name := "binocular-example",
      addCompilerPlugin("org.scalus" % "scalus-plugin_3.3.7" % scalusVersion),
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
