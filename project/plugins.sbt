// Comment to get more information during initialization
logLevel := Level.Warn

// The Typesafe repository 
resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

addSbtPlugin("com.earldouglas" % "xsbt-web-plugin" % "1.0.0-M5")

// Add sbt idea plugin
addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.6.0")

// Add sbt eclipse plugin
addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.4.0")
