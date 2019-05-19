addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)

inScope(Global)(
  Seq(scalaVersion := "2.12.8")
)

name := "waves-exchange"

libraryDependencies ++= Seq(
  "io.estatico" %% "newtype" % "0.4.2",
  "org.typelevel" %% "cats-core" % "2.0.0-M1",
  "com.typesafe.play" %% "play-json" % "2.7.3",
  "org.scalacheck" %% "scalacheck" % "1.14.0" % Test,
  "org.scalatest" %% "scalatest" % "3.1.0-SNAP9" % Test
)
