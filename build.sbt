import com.typesafe.sbt.SbtGit.GitKeys
import sbtrelease.ReleaseStateTransformations._
import sbtrelease._

enablePlugins(GitVersioning)

ThisBuild / scalaVersion := "2.13.8"

ThisBuild / isSnapshot := false
ThisBuild / git.baseVersion := "0.2-dev"
ThisBuild / git.gitHeadCommit := GitKeys.gitReader.value.withGit(
  _.asInstanceOf[com.typesafe.sbt.git.JGit].headCommit.map(_.abbreviate(8).name)
)

releaseVersion := { ver =>
  sys.env
    .get("CALCULATED_NEXT_VERSION")
    .orElse(Some(ver))
    .flatMap(Version(_))
    .map(_.string)
    .getOrElse(versionFormatError(ver))
}

releaseNextVersion := { ver =>
  sys.env
    .get("CALCULATED_NEXT_VERSION")
    .orElse(Some(ver))
    .flatMap(Version(_))
    .map(_.bump(releaseVersionBump.value).asSnapshot.string)
    .getOrElse(versionFormatError(ver))
}

releaseTagName := {
  sys.env
    .get("CALCULATED_NEXT_VERSION")
    .orElse(Some(version.value))
    .flatMap(Version(_))
    .map(ver => s"v${ver.string}")
    .getOrElse(versionFormatError("not version found"))
}

releaseProcess := Seq[ReleaseStep](
  inquireVersions,
  setReleaseVersion,
  tagRelease,
  publishArtifacts,
  setNextVersion,
  pushChanges
)

ThisBuild / organization := "com.arkondata"

ThisBuild / homepage := Some(url("https://github.com/Grupo-Abraxas/slothql"))
ThisBuild / scmInfo := Some(ScmInfo(homepage.value.get, "git@github.com:Grupo-Abraxas/slothql.git"))
ThisBuild / developers := List(
  Developer("fehu", "Dmitry K", "kdn.kovalev@gmail.com", url("https://github.com/fehu"))
)
ThisBuild / licenses += ("MIT", url("https://opensource.org/licenses/MIT"))

lazy val root = (project in file("."))
  .settings(
    docSettings,
    inThisBuild(
      List(
        git.gitUncommittedChanges := (git.gitUncommittedChanges.value || isSnapshot.value),
        Compile / scalacOptions ++= Seq("-unchecked", "-feature", "-deprecation", "-Wunused:imports"),
        addCompilerPlugin(Dependencies.Plugin.`kind-projector`)
      ) ++ versionWithGit
    ),
    crossScalaVersions := Nil,
    publish / skip := true,
    name := "slothql"
  )
  .aggregate(cypher, apoc, opentracingNeo4j)

lazy val cypher = (project in file("cypher"))
  .settings(
    docSettings,
    name := "slothql-cypher",
    Compile / scalacOptions ++= Seq("-Wunused:imports"),
    libraryDependencies ++= Seq(
      Dependencies.Scala.reflect.value,
      Dependencies.shapeless,
      Dependencies.`cats-core`,
      Dependencies.`cats-free`,
      Dependencies.`cats-effect`,
      Dependencies.`fs2-core`,
      Dependencies.`fs2-re`,
      Dependencies.`neo4j-driver`,
      Dependencies.Test.scalatest,
      Dependencies.Test.`slf4j-simple`
    ),
    console / initialCommands :=
      """
        |import org.neo4j.driver.{ AuthTokens, GraphDatabase }
        |import com.arkondata.slothql.cypher.syntax._
        |import com.arkondata.slothql.cypher.CypherFragment
        |import com.arkondata.slothql.neo4j.Neo4jCypherTransactor
      """.stripMargin
  )

lazy val apoc = (project in file("cypher-apoc"))
  .settings(
    docSettings,
    Compile / scalacOptions ++= Seq("-Wunused:imports"),
    name := "slothql-cypher-apoc"
  )
  .dependsOn(cypher % "compile -> compile; test -> test")

lazy val opentracingNeo4j = (project in file("opentracing-neo4j"))
  .settings(
    docSettings,
    name := "slothql-opentracing-neo4j",
    Compile / scalacOptions ++= Seq("-Ymacro-annotations", "-Wunused:imports"),
    libraryDependencies ++= Seq(Dependencies.`natchez`, Dependencies.Test.`natchez-jaeger`)
  )
  .dependsOn(cypher % "compile -> compile; test -> test")

// // // Scaladoc // // //

lazy val isGraphvizPresent = {
  import scala.sys.process._
  try "dot -V".! == 0
  catch { case _: Throwable => false }
}

lazy val docSettings = Seq(
  Compile / doc / scalacOptions ++= {
    val default  = Seq("-implicits")
    val diagrams = if (isGraphvizPresent) Seq("-diagrams", "-diagrams-debug") else Seq()
    default ++ diagrams
  }
)

// Publishing

inThisBuild(
  Seq(
    resolvers += Resolver.sonatypeRepo("releases"),
    resolvers += "arkondata--sbt-dev".at(
      "https://arkondata-744752950324.d.codeartifact.us-east-1.amazonaws.com/maven/sbt-dev"
    ),
    publishTo := Some(
      "arkondata--sbt-dev".at("https://arkondata-744752950324.d.codeartifact.us-east-1.amazonaws.com/maven/sbt-dev")
    ),
    credentials += Credentials(
      "arkondata/sbt-dev",
      "arkondata-744752950324.d.codeartifact.us-east-1.amazonaws.com",
      "aws",
      sys.env.getOrElse("CODEARTIFACT_AUTH_TOKEN", "")
    ),
    Compile / scalacOptions ++= Seq(
      "-feature",
      "-unchecked",
      "-deprecation",
      "-Wunused:imports",
      "-P:semanticdb:exclude:Macros.scala"
    ),
    addCompilerPlugin(Dependencies.Plugin.`better-monadic-for`),
    addCompilerPlugin(Dependencies.Plugin.`kind-projector`),
    Test / parallelExecution := true
  ) ++ Versioning.settings
)

// Scalafix dependencies
//ThisBuild / scalafixDependencies += Dependencies.ScalaFix.organizeImports

inThisBuild(
  List(
    scalaVersion := "2.13.2",
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision
  )
)

//    semanticdbEnabled := true,
//    semanticdbVersion := scalafixSemanticdb.revision
addCommandAlias("ff", "Test/scalafix;Test/scalafmt;scalafix;scalafmt")


