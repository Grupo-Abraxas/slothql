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
  .aggregate(macros, cypher, arrows)


lazy val macros = (project in file("macros"))
  .settings(
    name := "slothql-macros",
    libraryDependencies += scalaOrganization.value % "scala-reflect" % scalaVersion.value,
    resolvers += "Artifactory Realm" at "http://artifactory.arkondata.com/artifactory/sbt-dev",
    libraryDependencies ++= Seq(
      "io.jaegertracing" % "jaeger-client" % "0.33.1",
      "com.github.fehu" %% "opentracing-akka" % "0.1.0",
      "org.slf4j" % "slf4j-simple" % "1.7.26",
      Dependencies.shapeless
    )
  )

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
    ammVersion := "1.6.5",
    initialCommands in console :=
      """
        |import org.neo4j.driver.v1.{ AuthTokens, GraphDatabase }
        |import com.abraxas.slothql.cypher.syntax._
        |import com.abraxas.slothql.neo4j.Neo4jCypherTransactor
      """.stripMargin

  )
  .dependsOn(macros)

lazy val arrows = (project in file("arrows"))
  .settings(
    name := "slothql-arrows",
    libraryDependencies ++= Seq(
      Dependencies.shapeless,
      Dependencies.`cats-core`,
      Dependencies.Test.scalatest
    ),
    ammVersion := "1.6.5"
  )
  .dependsOn(macros)


// // // Repository // // //

publishTo in ThisBuild := Some("Artifactory Realm" at "http://artifactory.arkondata.com/artifactory/sbt-dev")
credentials in ThisBuild += Credentials("Artifactory Realm", "artifactory.arkondata.com", sys.env("ARTIFACTORY_USER"), sys.env("ARTIFACTORY_PASSWORD"))


// Ammonite
ammHome in ThisBuild := Some((baseDirectory.value / ".amm").getAbsolutePath)
