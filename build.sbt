ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.0"

lazy val root = (project in file("."))
  .settings(
    name := "advent-of-code-2021",
    libraryDependencies += "co.fs2" %% "fs2-core" % "3.2.2",
    libraryDependencies += "co.fs2" %% "fs2-io" % "3.2.2",
  )
