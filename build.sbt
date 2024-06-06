name := "diverse-proteins"
version := "0.1"
scalaVersion := "3.4.2"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies ++= Seq(
  "io.jhdf" % "jhdf" % "0.7.0",
  "org.scalatest" %% "scalatest" % "3.2.18" % Test,
)
