import sbt._

object Dependencies {

  lazy val shapeless         = "com.chuusai"    %% "shapeless"      % Version.shapeless
  lazy val `cats-core`       = "org.typelevel"  %% "cats-core"      % Version.cats
  lazy val `cats-free`       = "org.typelevel"  %% "cats-free"      % Version.cats
  lazy val `cats-effect`     = "org.typelevel"  %% "cats-effect"    % Version.catsEffect
  lazy val `kind-projector`  = "org.spire-math" %% "kind-projector" % Version.kindProjector
  lazy val `droste-core`     = "io.higherkindness" %% "droste-core" % Version.droste

  lazy val `neo4j-driver` = "org.neo4j.driver" % "neo4j-java-driver" % Version.neo4jDriver

  object Test {
    lazy val scalatest = "org.scalatest" %% "scalatest" % Version.scalatest % sbt.Test
  }


  object Version {
    lazy val shapeless = "2.3.3"
    lazy val kindProjector = "0.9.6"

    lazy val cats = "1.6.0"
    lazy val catsEffect = "1.2.0"
    lazy val droste = "0.6.0"

    lazy val neo4jDriver = "1.7.2"

    lazy val scalatest = "3.0.5"
  }
}
