name := "form-binder"

version := "0.9.0"

scalaVersion := "2.11.6"

crossScalaVersions := Seq("2.11.6", "2.10.5")

organizationName := "form-binder"

organization := "com.github.tminglei"

libraryDependencies ++= Seq(
  "org.json4s" %% "json4s-jackson" % "3.2.10" % "provided",
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