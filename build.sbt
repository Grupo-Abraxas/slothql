import com.typesafe.sbt.SbtGit.GitKeys

enablePlugins(GitVersioning)

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.abraxas",
      scalaVersion := "2.11.12",
      git.gitHeadCommit := GitKeys.gitReader.value.withGit(
        _.asInstanceOf[com.typesafe.sbt.git.JGit]
          .headCommit.map(_.abbreviate(8).name)
      ),

      scalacOptions in Compile ++= Seq("-unchecked", "-feature"),
      scalacOptions in Compile += "-Ypartial-unification",

      resolvers += Resolver.sonatypeRepo("releases"),
      addCompilerPlugin(Dependencies.`kind-projector`)
    ) ++ versionWithGit),

    name := "slothql"
  )
  .aggregate(cypher, arrows)


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
        |import com.abraxas.slothql.neo4j.Neo4jCypherTransactor
      """.stripMargin

  )


lazy val `arrows-macros` = (project in file("arrows-macros"))
  .settings(
    name := "slothql-arrows-macros",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      Dependencies.shapeless
    )
  )

lazy val arrows = (project in file("arrows"))
  .settings(
    name := "slothql-arrows",
    libraryDependencies ++= Seq(
      Dependencies.`cats-core`
    )
  )
  .dependsOn(`arrows-macros`)
  .aggregate(`arrows-macros`)


// // // Repository // // //

publishTo in ThisBuild := Some("Artifactory Realm" at "http://artifactory.arkondata.com/artifactory/sbt-dev")
credentials += Credentials("Artifactory Realm", "artifactory.arkondata.com", sys.env("ARTIFACTORY_USER"), sys.env("ARTIFACTORY_PASSWORD"))


// Ammonite
ammHome in ThisBuild := Some((baseDirectory.value / ".amm").getAbsolutePath)
