name := "slothql-dev"

version := "0.1"

scalaVersion := "2.11.12"


resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.6")

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "1.1.0"
)