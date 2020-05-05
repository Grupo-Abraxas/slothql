import com.typesafe.sbt.SbtGit.GitKeys

enablePlugins(GitVersioning)

lazy val scala212 = "2.12.11"
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
      crossScalaVersions := scala212 :: scala213 :: Nil,

      scalacOptions in Compile ++= Seq("-unchecked", "-feature", "-deprecation"),
      scalacOptions in Compile ++= (scalaBinaryVersion.value match {
        case "2.13" => Nil
        case _      => "-Ypartial-unification" :: Nil
      }),
      resolvers += Resolver.sonatypeRepo("releases"),
      addCompilerPlugin(Dependencies.`kind-projector` cross CrossVersion.full)
    ) ++ versionWithGit),

    crossScalaVersions := Nil,
    name := "slothql"
  )
  .settings(ammSettings: _*)
  .aggregate(cypher, apoc)


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


// // // Repository // // //

publishTo in ThisBuild := Some("Artifactory Realm" at "https://artifactory.arkondata.com/artifactory/sbt-dev")
credentials in ThisBuild += Credentials("Artifactory Realm", "artifactory.arkondata.com",
                                        sys.env.getOrElse("ARTIFACTORY_USER", ""),
                                        sys.env.getOrElse("ARTIFACTORY_PASSWORD", ""))


// Ammonite
lazy val ammoniteVersion = "1.6.5"
lazy val ammSettings = Seq(
  ammVersion := ammoniteVersion,
  ammHome := Some(((baseDirectory in ThisBuild).value / ".amm").getAbsolutePath) 
)
