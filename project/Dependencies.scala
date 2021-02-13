import sbt._

object Dependencies {

  object Scala {
    lazy val reflect = Def.setting{ "org.scala-lang" % "scala-reflect" % Keys.scalaVersion.value }
  }

  object Plugin {
    lazy val `kind-projector` = "org.typelevel"  %% "kind-projector" % Version.kindProjector cross CrossVersion.full
    lazy val `macro-paradise` = "org.scalamacros" % "paradise"       % Version.macroParadise cross CrossVersion.full
  }

  lazy val shapeless         = "com.chuusai"    %% "shapeless"      % Version.shapeless
  lazy val `cats-core`       = "org.typelevel"  %% "cats-core"      % Version.cats
  lazy val `cats-free`       = "org.typelevel"  %% "cats-free"      % Version.cats
  lazy val `cats-effect`     = "org.typelevel"  %% "cats-effect"    % Version.catsEffect
  lazy val `fs2-core`        = "co.fs2"         %% "fs2-core"       % Version.fs2

  lazy val `neo4j-driver` = "org.neo4j.driver" % "neo4j-java-driver" % Version.neo4jDriver

  lazy val `opentracing-scala`  = "com.arkondata" %% "opentracing-scala"  % Version.opentracingScala
  lazy val `opentracing-effect` = "com.arkondata" %% "opentracing-effect" % Version.opentracingScala
  lazy val `opentracing-fs2`    = "com.arkondata" %% "opentracing-fs2"    % Version.opentracingScala

  object Test {
    lazy val scalatest = "org.scalatest" %% "scalatest" % Version.scalatest % sbt.Test
  }


  object Version {
    lazy val shapeless = "2.3.3"
    lazy val kindProjector = "0.11.0"
    lazy val macroParadise = "2.1.1"

    lazy val cats       = "2.1.1"
    lazy val catsEffect = "2.1.3"
    lazy val fs2        = "2.4.2"

    lazy val neo4jDriver = "4.2.0"

    lazy val opentracingScala = "0.2.0"

    lazy val scalatest = "3.1.2"
  }
}
