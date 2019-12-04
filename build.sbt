inScope(Global)(
  Seq(
    scalaVersion := "2.13.1",
    scalacOptions += "-Ymacro-annotations"
  ))

name := "waves-exchange"

libraryDependencies ++= Seq(
  "io.estatico" %% "newtype" % "0.4.3",
  "org.typelevel" %% "cats-core" % "2.0.0",
  "org.typelevel" %% "alleycats-core" % "2.0.0",
  "dev.zio" %% "zio" % "1.0.0-RC17",
  "dev.zio" %% "zio-nio" % "0.4.0",
  "com.typesafe.play" %% "play-json" % "2.8.0",
  "org.scalacheck" %% "scalacheck" % "1.14.1" % Test,
  "org.scalatest" %% "scalatest" % "3.1.0" % Test
)
