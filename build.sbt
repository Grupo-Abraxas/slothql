name := "slothql-dev"

version := "0.1"

scalaVersion := "2.11.12"

scalacOptions in Compile ++= Seq("-unchecked", "-feature")

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.6")

libraryDependencies ++= Seq(
  "com.chuusai"     %% "shapeless"          % "2.3.3",
  "org.typelevel"   %% "cats-core"          % "1.1.0",
  "org.typelevel"   %% "cats-effect"        % "0.10.1",
  "org.neo4j.driver" % "neo4j-java-driver"  % "1.6.1"
)

// // // REPL // // //

initialCommands in console :=
  """
    |import org.neo4j.driver.v1.{ AuthTokens, GraphDatabase }
    |import com.abraxas.slothql.cypher.syntax._
    |import com.abraxas.slothql.neo4j.CypherTransactor;
  """.stripMargin


// Ammonite
ammHome := Some((baseDirectory.value / ".amm").getAbsolutePath)
