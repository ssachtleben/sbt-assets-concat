sbtPlugin := true

organization := "com.ssachtleben"

name := """sbt-assets-concat"""

version := "1.0.0"

scalaVersion := "2.10.4"

scalacOptions += "-feature"

autoScalaLibrary := false

crossPaths := false

publishArtifact in(Compile, packageDoc) := false

resolvers ++= Seq(
  "Typesafe Releases Repository" at "http://repo.typesafe.com/typesafe/releases/",
  Resolver.url("sbt snapshot plugins", url("http://repo.scala-sbt.org/scalasbt/sbt-plugin-snapshots"))(Resolver.ivyStylePatterns),
  Resolver.sonatypeRepo("snapshots"),
  "Typesafe Snapshots Repository" at "http://repo.typesafe.com/typesafe/snapshots/",
  Resolver.mavenLocal
)

addSbtPlugin("com.typesafe.sbt" % "sbt-js-engine" % "1.0.0")

addSbtPlugin("com.typesafe.sbt" % "sbt-web" % "1.1.1")