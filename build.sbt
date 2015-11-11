import bintray.Keys._

name := "json-schema-parser"

organization := "com.voxsupplychain"

scalaVersion := "2.10.5"

libraryDependencies ++= Seq(
  "io.argonaut" %% "argonaut" % "6.1",
  "org.scalatest" % "scalatest_2.10" % "2.2.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.0" % "test"
)

bintraySettings

releaseSettings

packageLabels in bintray := Seq("json-schema", "parser")

publishMavenStyle := false

licenses += "Apache-2.0" -> new URL("http://www.apache.org/licenses/LICENSE-2.0")

repository in bintray := "ivy-public"

bintrayOrganization in bintray := Some("voxsupplychain")

