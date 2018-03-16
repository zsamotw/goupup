import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.tomaszwiech",
      scalaVersion := "2.12.3",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "goupup",
    //libraryDependencies += scalaTest % Test
    libraryDependencies += "org.mockito" % "mockito-core" % "2.16.0",
    libraryDependencies += "org.scalatest" % "scalatest_2.12" % "3.0.5" % "test"
  )
