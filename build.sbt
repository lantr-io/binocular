scalaVersion := "3.3.7"

//val scalusVersion = "0.13.0"
//val scalusVersion = "0.13.0+207-58f4bcc1+20251113-0824-SNAPSHOT"
val scalusVersion = "0.13.0+411-8f5422a9-SNAPSHOT"

scalacOptions ++= Seq("-deprecation", "-feature")

testFrameworks += new TestFramework("munit.Framework")

Test / parallelExecution := false

Test / javaOptions ++= Seq("-Xmx2g")

Global / onChangedBuildSource := ReloadOnSourceChanges

resolvers += Resolver.sonatypeCentralSnapshots

addCompilerPlugin("org.scalus" %% "scalus-plugin" % scalusVersion)

// TestConfig is now a static file: src/main/scala/binocular/TestConfig.scala
// This ensures deterministic compilation when the project is used as a dependency

// Define example as a subproject
lazy val example = (project in file("example"))
    .dependsOn(LocalProject("binocular"))
    .settings(
      name := "binocular-example",
      scalaVersion := "3.3.7",
      resolvers += Resolver.sonatypeCentralSnapshots,
      addCompilerPlugin("org.scalus" %% "scalus-plugin" % scalusVersion),
      Compile / mainClass := Some("binocular.example.bitcoinDependentLock")
    )

libraryDependencies ++= Seq(
  "org.scalus" %% "scalus" % scalusVersion,
  "org.scalus" %% "scalus-bloxbean-cardano-client-lib" % scalusVersion,
  "com.lihaoyi" %% "upickle" % "4.3.2",
  "com.lihaoyi" %% "os-lib" % "0.11.3" % Test,
  "com.typesafe" % "config" % "1.4.3", // Configuration library
  "org.slf4j" % "slf4j-simple" % "2.0.17",
  "org.bouncycastle" % "bcprov-jdk18on" % "1.82",
  ("org.bitcoin-s" % "bitcoin-s-bitcoind-rpc_2.13" % "1.9.11").excludeAll(
    ExclusionRule(organization = "com.lihaoyi", name = "upickle_2.13"),
    ExclusionRule(organization = "com.lihaoyi", name = "ujson_2.13"),
    ExclusionRule(organization = "com.lihaoyi", name = "upack_2.13"),
    ExclusionRule(organization = "com.lihaoyi", name = "upickle-core_2.13"),
    ExclusionRule(organization = "com.lihaoyi", name = "upickle-implicits_2.13"),
    ExclusionRule(organization = "com.lihaoyi", name = "geny_2.13")
  ),
  // Exclude eddsa to avoid conflict with version bundled in cardano-client-lib
  ("com.bloxbean.cardano" % "cardano-client-lib" % "0.7.0").excludeAll(
    ExclusionRule(organization = "net.i2p.crypto", name = "eddsa")
  ),
  "com.bloxbean.cardano" % "cardano-client-backend-blockfrost" % "0.7.0",
  "com.bloxbean.cardano" % "cardano-client-backend-koios" % "0.7.0",
  "com.bloxbean.cardano" % "cardano-client-backend-ogmios" % "0.7.0",
  "com.bloxbean.cardano" % "cardano-client-quicktx" % "0.7.0" % Test,
  "com.monovore" %% "decline" % "2.5.0",
  "org.scalameta" %% "munit" % "1.2.0" % Test,
  "org.scalameta" %% "munit-scalacheck" % "1.2.0" % Test,
  "org.scalacheck" %% "scalacheck" % "1.19.0" % Test,
  // Testcontainers for integration testing
  "com.dimafeng" %% "testcontainers-scala-core" % "0.41.5" % Test,
  "com.dimafeng" %% "testcontainers-scala-munit" % "0.41.5" % Test,
  // Yaci DevKit for Cardano local devnet
  "com.bloxbean.cardano" % "yaci-cardano-test" % "0.1.0" % Test
)

// Assembly configuration
assembly / assemblyMergeStrategy := {
    case PathList("META-INF", "MANIFEST.MF")                => MergeStrategy.discard
    case PathList("META-INF", xs @ _*)                      => MergeStrategy.discard
    case PathList("module-info.class")                      => MergeStrategy.discard
    case PathList("net", "i2p", "crypto", "eddsa", xs @ _*) => MergeStrategy.first
    case x if x.endsWith(".proto")                          => MergeStrategy.first
    case x if x.contains("bouncycastle")                    => MergeStrategy.first
    case "reference.conf"                                   => MergeStrategy.concat
    case "application.conf"                                 => MergeStrategy.concat
    case _                                                  => MergeStrategy.first
}

// Specify main class for assembly
assembly / mainClass := Some("binocular.main")
