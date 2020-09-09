name := "Hangman"

version := "0.1"

scalaVersion := "2.13.3"
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "latest.integration" % "test",
  "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0"
)
