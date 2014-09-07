name := "form-binder"

version := "0.3.0"

scalaVersion := "2.11.2"

crossScalaVersions := Seq("2.11.2", "2.10.4")

organizationName := "form-binder"

organization := "com.github.tminglei"

libraryDependencies ++= Seq(
  "org.json4s" %% "json4s-jackson" % "3.2.10" % "provided",
  "org.scalatest" %% "scalatest" % "2.2.2" % "test"
)