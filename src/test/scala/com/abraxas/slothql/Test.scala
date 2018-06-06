package com.abraxas.slothql

import cats.data.NonEmptyList
import shapeless._

import com.abraxas.slothql.cypher.CypherFragment._
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

  val io = tx.read(query)
  val result = io.unsafeRunSync()

  println("result = " + result)

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

  val io = tx.read(query)
  val result = io.unsafeRunSync()

  println("result = " + result)

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

  val io = tx.read(query)
  val result: Seq[(String, String)] = io.unsafeRunSync()

  println("result = " + result)

  sys.exit()
}
