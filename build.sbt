
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

publishMavenStyle := true

publishTo := {
  val nexus = "https://my.artifact.repo.net/"
  if (isSnapshot.value)
    Some("snapshots"  at "https://nexus.tundra-shared.com/repository/maven-snapshots/")
  else
    Some("releases" at "https://nexus.tundra-shared.com/repository/maven-releases/")
}

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

