import com.typesafe.sbt.SbtGit.GitKeys

enablePlugins(GitVersioning)

lazy val scala211 = "2.11.12"
lazy val scala212 = "2.12.8"

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.abraxas",
      scalaVersion := scala212,
      git.gitHeadCommit := GitKeys.gitReader.value.withGit(
        _.asInstanceOf[com.typesafe.sbt.git.JGit]
          .headCommit.map(_.abbreviate(8).name)
      ),
      crossScalaVersions := scala211 :: scala212 :: Nil,

      scalacOptions in Compile ++= Seq("-unchecked", "-feature"),
      scalacOptions in Compile += "-Ypartial-unification",

      resolvers += Resolver.sonatypeRepo("releases"),
      addCompilerPlugin(Dependencies.`kind-projector`)
    ) ++ versionWithGit),

    crossScalaVersions := Nil,
    name := "slothql"
  )
  .settings(ammSettings: _*)
  .aggregate(cypher, cypherApoc, arrows, arrowsShow)


lazy val cypher = (project in file("cypher"))
  .settings(
    name := "slothql-cypher",
    libraryDependencies ++= Seq(
      Dependencies.shapeless,
      Dependencies.`cats-core`,
      Dependencies.`cats-free`,
      Dependencies.`cats-effect`,
      Dependencies.`neo4j-driver`,
      Dependencies.Test.scalatest
    ),
    initialCommands in console :=
      """
        |import org.neo4j.driver.v1.{ AuthTokens, GraphDatabase }
        |import com.abraxas.slothql.cypher.syntax._
        |import com.abraxas.slothql.cypher.CypherFragment
        |import com.abraxas.slothql.neo4j.Neo4jCypherTransactor
      """.stripMargin
  ).settings(ammSettings: _*)

lazy val cypherApoc = (project in file("cypher-apoc"))
  .settings(
    name := "slothql-cypher-apoc"
  ).dependsOn(cypher % "compile->compile;test->test")

lazy val arrows = (project in file("arrows"))
  .settings(
    name := "slothql-arrows",
    libraryDependencies ++= Seq(
      Dependencies.shapeless,
      Dependencies.`cats-core`,
      Dependencies.Test.scalatest
    ),
    initialCommands in console :=
      """
        |import com.abraxas.slothql.arrow._
      """.stripMargin
  ).settings(ammSettings: _*)


lazy val arrowsShow = (project in file("arrows-show"))
  .settings(
    name := "slothql-arrows-show",
    libraryDependencies += Dependencies.`droste-core`,
    initialCommands in console := (initialCommands in (arrows, console)).value +
      """
        |import com.abraxas.slothql.arrow.show._
      """.stripMargin
  )
  .dependsOn(arrows % "compile->compile;test->test")

// // // Repository // // //

publishTo in ThisBuild := Some("Artifactory Realm" at "https://artifactory.arkondata.com/artifactory/sbt-dev")
credentials in ThisBuild += Credentials("Artifactory Realm", "artifactory.arkondata.com", sys.env("ARTIFACTORY_USER"), sys.env("ARTIFACTORY_PASSWORD"))


// Ammonite
lazy val ammoniteVersion = "1.6.5"
lazy val ammSettings = Seq(
  ammVersion := ammoniteVersion,
  ammHome := Some(((baseDirectory in ThisBuild).value / ".amm").getAbsolutePath) 
)
