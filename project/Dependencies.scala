import sbt._

object Dependencies {

  object Scala {
    lazy val reflect = Def.setting("org.scala-lang" % "scala-reflect" % Keys.scalaVersion.value)
  }

  object Plugin {
    lazy val `kind-projector`   = "org.typelevel"        %% "kind-projector"   % Version.kindProjector cross CrossVersion.full
    lazy val `macro-paradise`   = "org.scalamacros"       % "paradise"         % Version.macroParadise cross CrossVersion.full
    lazy val `organize-imports` = "com.github.liancheng" %% "organize-imports" % Version.organizeImports
  }

  lazy val shapeless     = "com.chuusai"   %% "shapeless"   % Version.shapeless
  lazy val `cats-core`   = "org.typelevel" %% "cats-core"   % Version.cats
  lazy val `cats-free`   = "org.typelevel" %% "cats-free"   % Version.cats
  lazy val `cats-effect` = "org.typelevel" %% "cats-effect" % Version.catsEffect

  lazy val `fs2-core` = "co.fs2" %% "fs2-core"             % Version.fs2
  lazy val `fs2-re`   = "co.fs2" %% "fs2-reactive-streams" % Version.fs2

  lazy val `neo4j-driver` = "org.neo4j.driver" % "neo4j-java-driver" % Version.neo4jDriver
  lazy val `natchez`      = "org.tpolecat"    %% "natchez-core"      % Version.natchez

  object Test {
    lazy val scalatest = "org.scalatest" %% "scalatest" % Version.scalatest % sbt.Test

    lazy val `natchez-jaeger` = "org.tpolecat" %% "natchez-jaeger" % Version.natchez % sbt.Test

    lazy val `slf4j-simple` = "org.slf4j" % "slf4j-simple" % Version.`slf4j-simple` % sbt.Test
  }

  object Version {
    lazy val shapeless     = "2.3.9"
    lazy val kindProjector = "0.13.2"
    lazy val macroParadise = "2.1.1"

    lazy val cats       = "2.7.0"
    lazy val catsEffect = "3.3.4"
    lazy val fs2        = "3.2.0"

    lazy val neo4jDriver = "4.3.0"
    lazy val natchez     = "0.3.3"

    lazy val organizeImports = "0.5.0"

    lazy val scalatest = "3.2.11"

    lazy val `slf4j-simple` = "1.7.36"
  }
}
