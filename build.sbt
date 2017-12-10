organization := "org.shade"

name := "common"

version := "2.0.0-SNAPSHOT"

scalaVersion := "2.12.4"

crossScalaVersions := Seq("2.11.11", "2.12.4")

scalacOptions ++= Seq("-deprecation")

resolvers ++= Seq(
  Resolver.mavenLocal,
  Opts.resolver.sonatypeReleases,
  Opts.resolver.sonatypeSnapshots
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.4" % "test" withSources() withJavadoc()
)

