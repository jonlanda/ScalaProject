import sbtassembly.AssemblyPlugin.autoImport._

ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.13.14"

lazy val root = (project in file("."))
  .settings(
    name := "CasinoApp",
    libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0",
    assembly / mainClass := Some("BlackjackUI")
  )