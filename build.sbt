name := "CGC"

version := "0.1"

scalaVersion := "2.12.7"
lazy val akkaVersion = "2.5.17"
lazy val jamodVersion = "1.2"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  "com.typesafe.akka" %% "akka-testkit" % akkaVersion,
  "org.scalatest" %% "scalatest" % "3.0.5" % "test",
  "net.wimpi" % "jamod" % jamodVersion)
