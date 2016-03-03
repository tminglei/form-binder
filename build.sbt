name := "form-binder"

version := "0.12.1"

scalaVersion := "2.11.7"

crossScalaVersions := Seq("2.11.7", "2.10.6")

organizationName := "form-binder"

organization := "com.github.tminglei"

libraryDependencies ++= Seq(
  "io.spray" %% "spray-json" % "1.3.2",
  "org.slf4j" % "slf4j-api" % "1.7.12",
  "org.slf4j" % "slf4j-simple" % "1.7.12" % "provided",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test"
)

/////////////////  for publish/release  ///////////////////////////////
publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.toUpperCase.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

makePomConfiguration ~= { _.copy(configurations = Some(Seq(Compile, Runtime, Optional))) }

pomExtra := (
  <url>https://github.com/tminglei/form-binder</url>
  <licenses>
    <license>
      <name>BSD-style</name>
      <url>http://www.opensource.org/licenses/bsd-license.php</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>git@github.com:tminglei/form-binder.git</url>
    <connection>scm:git:git@github.com:tminglei/form-binder.git</connection>
  </scm>
  <developers>
    <developer>
      <id>tminglei</id>
      <name>Minglei Tu</name>
      <timezone>+8</timezone>
    </developer>
  </developers>
)