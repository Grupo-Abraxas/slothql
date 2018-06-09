import scala.sys.process.Process

organization := "com.abraxas"

name := "slothql-dev"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.12"

scalacOptions in Compile ++= Seq("-unchecked", "-feature")
scalacOptions in Compile += "-Ypartial-unification"

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.6")


lazy val catsVersion = "1.1.0"

libraryDependencies ++= Seq(
  "com.chuusai"     %% "shapeless"          % "2.3.3",
  "org.typelevel"   %% "cats-core"          % catsVersion,
  "org.typelevel"   %% "cats-free"          % catsVersion,
  "org.typelevel"   %% "cats-effect"        % "0.10.1",
  "org.neo4j.driver" % "neo4j-java-driver"  % "1.6.1"
)

// // // REPL // // //

initialCommands in console :=
  """
    |import org.neo4j.driver.v1.{ AuthTokens, GraphDatabase }
    |import com.abraxas.slothql.cypher.syntax._
    |import com.abraxas.slothql.neo4j.Neo4jCypherTransactor
  """.stripMargin


// Ammonite
ammHome := Some((baseDirectory.value / ".amm").getAbsolutePath)


// Misc: publish-local | version := git HEAD hash

lazy val gitHeadShortHash = settingKey[String]("Short hash of current HEAD.")
gitHeadShortHash := Process("git rev-parse --short HEAD").lineStream.head

lazy val gitHeadBranch = settingKey[String]("Branch of current HEAD.")
gitHeadBranch := Process("git rev-parse --abbrev-ref HEAD").lineStream.head

lazy val publishLocalHashVersionPrefix = settingKey[String]("")
publishLocalHashVersionPrefix := "SNAPSHOT-"

lazy val currentHashVersion = taskKey[String]("Current commit-hash version.")
currentHashVersion := Def.task {
  s"${publishLocalHashVersionPrefix.value}${gitHeadBranch.value}-${gitHeadShortHash.value}"
}.value

lazy val publishLocalHashVersion = taskKey[Unit]("Publish locally setting `gitHeadShortHash` as version.")
publishLocalHashVersion := Def.task {
  val extracted = Project extract state.value
  import extracted._
  val v = currentHashVersion.value
  runTask(
    publishLocal in Compile,
    appendWithSession(Seq(version := v), state.value)
  )._2
}.value
