lazy val root = (project in file(".")).
  settings(
    organization := "castle8080",
    version := "0.1.0",
    scalaVersion := "2.11.7",
    name := "FPInScala"
  )


libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.12.5",
  "org.scalatest" %% "scalatest" % "2.2.5"
)
