package com.abraxas.slothql

import cats.data.NonEmptyList
import shapeless._

import com.abraxas.slothql.cypher.CypherFragment.Expr.MapExpr0
import com.abraxas.slothql.cypher.CypherFragment._
import com.abraxas.slothql.neo4j.CypherTransactor

object Test1 extends App {
  val driver = Connection.driver
  val tx = CypherTransactor.Default(driver.session())

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

  import com.abraxas.slothql.neo4j.CypherTransactor.RecordReader._

  val io = tx.read(query)
  val result = io.unsafeRunSync()

  println("result = " + result)

  sys.exit()
}

object Test2 extends App {
  val driver = Connection.driver
  val tx = CypherTransactor.Default(driver.session())

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

  import com.abraxas.slothql.neo4j.CypherTransactor.RecordReader._

  val io = tx.read(query)
  val result = io.unsafeRunSync()

  println("result = " + result)

  sys.exit()
}


object Test3 extends App {
  val driver = Connection.driver
  val tx = CypherTransactor.Default(driver.session())

  val pattern = Pattern.Node(alias = Some("n"), labels = List("User"), map = Map())
  val n = Expr.Var[MapExpr0]("n")
  val query = Query.Clause(
    Clause.Match(
      NonEmptyList(pattern, Nil),
      optional = false,
      where = None
    ),
    Query.Return(
      Return.List(
        Return.Expr(Expr.Key[String](n, "email"), as = None),
        Return.Expr(Expr.Key[String](n, "name"), as = None) :: HNil
      ).known
    )
  )

  println("query = " + query.known.toCypher)

  import com.abraxas.slothql.neo4j.CypherTransactor.RecordReader._
  import com.abraxas.slothql.neo4j.CypherTransactor.ValueReader._

  val io = tx.read(query)
  val result: Seq[(String, String)] = io.unsafeRunSync()

  println("result = " + result)

  sys.exit()
}
