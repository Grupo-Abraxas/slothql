import com.typesafe.sbt.SbtGit.GitKeys

enablePlugins(GitVersioning)

lazy val scala213 = "2.13.2"

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.arkondata",
      scalaVersion := scala213,
      git.baseVersion := "0.2",
      git.gitHeadCommit := GitKeys.gitReader.value.withGit(
        _.asInstanceOf[com.typesafe.sbt.git.JGit]
          .headCommit.map(_.abbreviate(8).name)
      ),

      scalacOptions in Compile ++= Seq("-unchecked", "-feature", "-deprecation"),
      resolvers += Resolver.sonatypeRepo("releases"),
      resolvers += "Artifactory Realm" at "https://artifactory.arkondata.com/artifactory/sbt-dev",
      addCompilerPlugin(Dependencies.Plugin.`kind-projector`)
    ) ++ versionWithGit),

    crossScalaVersions := Nil,
    name := "slothql"
  )
  .settings(ammSettings: _*)
  .aggregate(cypher, apoc, opentracingNeo4j)

lazy val cypher = (project in file("cypher"))
  .settings(
    name := "slothql-cypher",
    libraryDependencies ++= Seq(
      Dependencies.Scala.reflect.value,
      Dependencies.shapeless,
      Dependencies.`cats-core`,
      Dependencies.`cats-free`,
      Dependencies.`cats-effect`,
      Dependencies.`fs2-core`,
      Dependencies.`neo4j-driver`,
      Dependencies.Test.scalatest
    ),
    initialCommands in console :=
      """
        |import org.neo4j.driver.v1.{ AuthTokens, GraphDatabase }
        |import com.arkondata.slothql.cypher.syntax._
        |import com.arkondata.slothql.cypher.CypherFragment
        |import com.arkondata.slothql.neo4j.Neo4jCypherTransactor
      """.stripMargin
  ).settings(ammSettings: _*)

lazy val apoc = (project in file("cypher-apoc"))
  .settings(
    name := "slothql-cypher-apoc"
  ).dependsOn(cypher % "compile -> compile; test -> test")

lazy val opentracingNeo4j = (project in file("opentracing-neo4j"))
  .settings(
    name := "slothql-opentracing-neo4j",
    scalacOptions in Compile += "-Ymacro-annotations",
    libraryDependencies ++= Seq(
      Dependencies.`opentracing-effect`,
      Dependencies.`opentracing-fs2`
    )
  ).dependsOn(cypher)


// // // Repository // // //

publishTo in ThisBuild := Some("Artifactory Realm" at "https://artifactory.arkondata.com/artifactory/sbt-dev")
credentials in ThisBuild += Credentials("Artifactory Realm", "artifactory.arkondata.com",
                                        sys.env.getOrElse("ARTIFACTORY_USER", ""),
                                        sys.env.getOrElse("ARTIFACTORY_PASSWORD", ""))

// Fix `java.net.ProtocolException: Unexpected status line: 0` when publishing to artifactory
ThisBuild / updateOptions := updateOptions.value.withGigahorse(false)

// Ammonite
lazy val ammoniteVersion = "1.6.5"
lazy val ammSettings = Seq(
  ammVersion := ammoniteVersion,
  ammHome := Some(((baseDirectory in ThisBuild).value / ".amm").getAbsolutePath) 
)
