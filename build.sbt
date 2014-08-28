name := "form-binder"

version := "0.0.1"

scalaVersion := "2.11.2"

crossScalaVersions := Seq("2.11.2", "2.10.4")

organizationName := "Infowise, Inc"

organization := "com.infowise"

libraryDependencies ++= Seq(
  "org.scalatra" %% "scalatra"        % "2.3.0" % "provided",
  "org.scalatra" %% "scalatra-json"   % "2.3.0" % "provided",
  "org.json4s"   %% "json4s-jackson"  % "3.2.10" % "provided",
  "org.scalatra" %% "scalatra-scalatest" % "2.3.0" % "test",
  "org.eclipse.jetty" % "jetty-webapp" % "9.2.1.v20140609" % "container",
  "org.eclipse.jetty" % "jetty-plus" % "9.2.1.v20140609" % "container",
  "org.eclipse.jetty.orbit" % "javax.servlet" % "3.0.0.v201112011016" % "container;provided;test"
    artifacts (Artifact("javax.servlet", "jar", "jar"))
)

jetty(port = 9000)