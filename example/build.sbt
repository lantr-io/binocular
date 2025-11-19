lazy val example = (project in file("."))
    .dependsOn(RootProject(file("..")))
    .settings(
      name := "binocular-example",
      version := "0.1.0-SNAPSHOT",
      scalaVersion := "3.3.7",

      // Main class for the example application
      Compile / mainClass := Some("binocular.example.bitcoinDependentLock")
    )
