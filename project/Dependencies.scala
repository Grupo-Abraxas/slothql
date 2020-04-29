import sbt._

object Dependencies {

  object Scala {
    lazy val reflect = Def.setting{ "org.scala-lang" % "scala-reflect" % Keys.scalaVersion.value }
  }

  lazy val shapeless         = "com.chuusai"    %% "shapeless"      % Version.shapeless
  lazy val `cats-core`       = "org.typelevel"  %% "cats-core"      % Version.cats
  lazy val `cats-free`       = "org.typelevel"  %% "cats-free"      % Version.cats
  lazy val `cats-effect`     = "org.typelevel"  %% "cats-effect"    % Version.catsEffect
  lazy val `kind-projector`  = "org.spire-math" %% "kind-projector" % Version.kindProjector
  lazy val `droste-core`     = "io.higherkindness" %% "droste-core" % Version.droste
  lazy val `fs2-core`        = "co.fs2"         %% "fs2-core"       % Version.fs2

  lazy val `neo4j-driver` = "org.neo4j.driver" % "neo4j-java-driver" % Version.neo4jDriver

  object Test {
    lazy val scalatest = "org.scalatest" %% "scalatest" % Version.scalatest % sbt.Test
  }


  object Version {
    lazy val shapeless = "2.3.3"
    lazy val kindProjector = "0.9.10"

    lazy val cats       = "2.1.0"
    lazy val catsEffect = "2.1.1"
    lazy val droste     = "0.6.0"
    lazy val fs2        = "2.3.0"

    lazy val neo4jDriver = "1.7.2"

    lazy val scalatest = "3.0.5"
  }
}
