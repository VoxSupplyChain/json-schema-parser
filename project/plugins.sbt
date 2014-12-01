resolvers ++= Seq(
  "Era7 maven releases"  at "http://releases.era7.com.s3.amazonaws.com"
  , "SBT release"  at "http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases/"
)

addSbtPlugin("ohnosequences" % "sbt-s3-resolver" % "0.10.1")

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.6.0")

addSbtPlugin("com.github.gseitz" % "sbt-release" % "0.8.3")
