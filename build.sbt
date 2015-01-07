import bintray.Keys._

name := "json-schema-parser"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.10.4"

libraryDependencies ++= Seq(
  "io.argonaut" %% "argonaut" % "6.0.4",
  "org.scalatest" % "scalatest_2.10" % "2.2.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.0" % "test"
)

bintraySettings

packageLabels in bintray := Seq("json-schema", "parser")

publishMavenStyle := false

repository in bintray := "vox-ivy"

bintrayOrganization in bintray := Some("voxsupplychain")