import bintray.Keys._

name := "json-schema-parser"

organization := "com.voxsupplychain"

scalaVersion := "2.12.4"

crossScalaVersions := Seq("2.10.5", "2.11.11", "2.12.4")

libraryDependencies ++= {
  val scalaz = "7.2.14"
  val argonaut = "6.2.1"
  Seq(
    "io.argonaut" %% "argonaut" % argonaut,
    "io.argonaut" %% "argonaut-scalaz" % argonaut,
    "org.scalaz" %% "scalaz-core" % scalaz,
    "org.scalacheck" %% "scalacheck" % "1.13.4",
    "org.scalatest" %% "scalatest" % "3.0.1"
  )
}

bintraySettings

releaseSettings

packageLabels in bintray := Seq("json-schema", "parser")

publishMavenStyle := false

licenses += "Apache-2.0" -> new URL("http://www.apache.org/licenses/LICENSE-2.0")

repository in bintray := "ivy-public"

bintrayOrganization in bintray := Some("voxsupplychain")

