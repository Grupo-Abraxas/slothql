package com.abraxas.slothql

import cats.data.NonEmptyList
import shapeless._

import com.abraxas.slothql.cypher.CypherFragment._
import com.abraxas.slothql.cypher.syntax._
import com.abraxas.slothql.neo4j.Neo4jCypherTransactor

object Test1 extends App {
  val driver = Connection.driver
  val tx = Neo4jCypherTransactor(driver.session())

  val pattern = Pattern.Node(alias = Some("n"), labels = Nil, map = Map())
  val query = Query.Clause(
    Clause.Match(
      NonEmptyList(pattern, Nil),
      optional = false,
      where = None
    ),
    Query.Return(Return.Expr(Expr.Var[Any]("n"), as = None))
  )

  println("query = " + query.known.toCypher)

  val io = tx.readIO(query)
  val result = io.unsafeRunSync()

  println("result = " + result)

  driver.close()
  sys.exit()
}

object Test2 extends App {
  val driver = Connection.driver
  val tx = Neo4jCypherTransactor(driver.session())

  val pattern = Pattern.Node(alias = Some("n"), labels = Nil, map = Map())
  val query = Query.Clause(
    Clause.Match(
      NonEmptyList(pattern, Nil),
      optional = false,
      where = None
    ),
    Query.Return(Return.All)
  )

  println("query = " + query.known.toCypher)

  val io = tx.readIO(query)
  val result = io.unsafeRunSync()

  println("result = " + result)

  driver.close()
  sys.exit()
}


object Test3 extends App {
  val driver = Connection.driver
  val tx = Neo4jCypherTransactor(driver.session())

  val pattern = Pattern.Node(alias = Some("n"), labels = List("User"), map = Map())
  val n = Expr.Var[Map[String, Any]]("n")
  val query = Query.Clause(
    Clause.Match(
      NonEmptyList(pattern, Nil),
      optional = false,
      where = None
    ),
    Query.Return(
      Return.List(
        Expr.Key[String](n, "email"),
        Expr.Key[String](n, "name")
      ).known
    )
  )

  println("query = " + query.known.toCypher)

  val io = tx.readIO(query)
  val result: Seq[(String, String)] = io.unsafeRunSync()

  println("result = " + result)

  driver.close()
  sys.exit()
}

object Test4 extends App {
  val driver = Connection.driver
  val tx = Neo4jCypherTransactor(driver.session())

  val q1 = Match { case u@Vertex("User") => u.id }
  def q2(id: Long) = Match {
    case (g@Vertex("Group")) -> _ -> u if u.id === lit(id) => g
  }

  println("q1 = " + q1.known.toCypher)
  val query = for {
    groupId <- tx.read(q1)
    _ = println(s"groupId = $groupId")
    user <- tx.read(q2(groupId))
    _ = println(s"user = $user")
  } yield (groupId, user)

  val io = tx.runRead(query)
  val result: Seq[(Long, Map[String, Any])] = io.unsafeRunSync()

  println("result = " + result)

  //  q1 = MATCH (`u`:`User`) RETURN `id`(`u`)
  //  groupId = 0
  //  user = Map(name -> Root Group, id -> g1)
  //  user = Map(name -> Sub Group, id -> g2)
  //  result = Vector((0,Map(name -> Root Group, id -> g1)), (0,Map(name -> Sub Group, id -> g2)))

  driver.close()
  sys.exit()
}
