val scala3Version = "3.2.2"

// ThisBuild / name := "Scala machines"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := scala3Version
ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-explain",
  "-language:implicitConversions",
  "-language:postfixOps"
)

ThisBuild / libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test
ThisBuild / libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.15.4" % Test

lazy val machineLibrary = RootProject(
  uri("https://github.com/hmc-cs111-spring2023/machine-library.git#v0.3.0")
)

lazy val root = project
  .in(file("."))
  .dependsOn(machineLibrary)
