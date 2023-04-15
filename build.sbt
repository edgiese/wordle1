ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.3"

lazy val root = (project in file("."))
  .settings(
    name := "wordle1",
    idePackagePrefix := Some("com.edgiese.wordle1")
  )

libraryDependencies += "org.scalatest" %% "scalatest-flatspec" % "3.2.15" % "test"