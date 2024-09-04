name := "diverse-proteins"
version := "0.1"
scalaVersion := "3.5.0"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies ++= Seq(
  "io.jhdf" % "jhdf" % "0.7.0",
  "org.scalatest" %% "scalatest" % "3.2.19" % Test,
)
