name := "advent2018"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "1.5.0",
  "org.typelevel" %% "cats-effect" % "1.1.0",
  "io.monix" %% "monix" % "3.0.0-RC2"
)

scalacOptions += "-language:higherKinds"

addCompilerPlugin("com.github.cb372" %% "scala-typed-holes" % "0.0.3")
