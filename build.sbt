name := "scala-oj"

version := "1.0"

scalaVersion := "2.11.7"

scalacOptions in ThisBuild ++= Seq("-deprecation", "-feature")

libraryDependencies ++= Seq(
  "org.scalanlp" %% "breeze" % "0.12",
  "org.scalanlp" %% "breeze-natives" % "0.12",
  "org.scalanlp" %% "breeze-viz" % "0.12",
  "org.scalaz" %% "scalaz-core" % "7.2.1"
)

resolvers ++= Seq(
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)
