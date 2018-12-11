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
      )
    ) ++ versionWithGit),

    name := "slothql-dev-mapper",

    scalacOptions in Compile ++= Seq("-unchecked", "-feature"),
    scalacOptions in Compile += "-Ypartial-unification",

    resolvers += Resolver.sonatypeRepo("releases"),
    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.6"),

    libraryDependencies ++= Seq(
      "org.typelevel"   %% "cats-core"          % catsVersion,
      "org.typelevel"   %% "cats-free"          % catsVersion,
      "org.typelevel"   %% "cats-effect"        % "0.10.1",
      "org.neo4j.driver" % "neo4j-java-driver"  % "1.6.1",
      "org.scalatest"   %% "scalatest"          % "3.0.5"       % Test
    )
  ).
  dependsOn(macros).
  aggregate(macros)


lazy val macros = (project in file("macros"))
  .settings(
    name := "slothql-mapper-macros-dev",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "com.chuusai"   %% "shapeless"     % "2.3.3"
    )
  )

// // // Dependencies // // //

lazy val catsVersion = "1.1.0"

// // // Repository // // //

publishTo in ThisBuild := Some("Artifactory Realm" at "http://artifactory.arkondata.com/artifactory/sbt-dev")
credentials += Credentials("Artifactory Realm", "artifactory.arkondata.com", sys.env("ARTIFACTORY_USER"), sys.env("ARTIFACTORY_PASSWORD"))

// // // REPL // // //

initialCommands in console :=
  """
    |import org.neo4j.driver.v1.{ AuthTokens, GraphDatabase }
    |import com.abraxas.slothql.cypher.syntax._
    |import com.abraxas.slothql.neo4j.Neo4jCypherTransactor
  """.stripMargin


// Ammonite
ammHome := Some((baseDirectory.value / ".amm").getAbsolutePath)
