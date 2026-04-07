addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.6")
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "2.3.1")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.13.1")

// Some sbt 2.x plugins still pull in Scala 2.13 transitives (scala-xml, scala-collection-compat)
// while others bring the Scala 3 versions, causing cross-version conflicts in the meta-build.
// Disable the conflict warning until upstream plugins are fully migrated.
conflictWarning := ConflictWarning.disable
