package com.abraxas.slothql

import cats.data.NonEmptyList
import cats.instances.vector._
import cats.syntax.traverse._
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
    userId <- tx.read(q1)
    _ = println(s"userId = $userId")
    q2i = q2(userId)
    _ = println("q2 = " + q2i.known.toCypher)
    group <- tx.read(q2i)
    _ = println(s"group = $group")
  } yield (userId, group)

  val io = tx.runRead(query)
  val result: Seq[(Long, Map[String, Any])] = io.unsafeRunSync()

  println("result = " + result)

  //  q1 = MATCH (`u`:`User`) RETURN `id`(`u`)
  //  userId = 0
  //  q2 = MATCH (`g`:`Group`) -[]-> () -[]-> (`u`) WHERE `id`(`u`) = 0 RETURN `g`
  //  group = Map(name -> Root Group, id -> g1)
  //  group = Map(name -> Sub Group, id -> g2)
  //  result = Vector((0,Map(name -> Root Group, id -> g1)), (0,Map(name -> Sub Group, id -> g2)))

  driver.close()
  sys.exit()
}

object Test5 extends App {
  val driver = Connection.driver
  val tx = Neo4jCypherTransactor(driver.session())

  val q1 = Match { case u@Vertex("User") => u.id }
  def q2(id: Long) = Match {
    case (g@Vertex("Group")) -> _ -> u if u.id === lit(id) => g
  }

  println("q1 = " + q1.known.toCypher)
  val query = for {
    userId <- tx.read(q1)
    _ = println(s"userId = $userId")
    q2i = q2(userId)
    _ = println("q2 = " + q2i.known.toCypher)
    groups <- tx.read(q2i).gather
    _ = println(s"groups = $groups")
  } yield (userId, groups)

  val io = tx.runRead(query)
  val result: Seq[(Long, Seq[Map[String, Any]])] = io.unsafeRunSync()

  println("result = " + result)

  //  q1 = MATCH (`u`:`User`) RETURN `id`(`u`)
  //  userId = 0
  //  q2 = MATCH (`g`:`Group`) -[]-> () -[]-> (`u`) WHERE `id`(`u`) = 0 RETURN `g`
  //  groups = Vector(Map(name -> Root Group, id -> g1), Map(name -> Sub Group, id -> g2))
  //  result = Vector((0,Vector(Map(name -> Root Group, id -> g1), Map(name -> Sub Group, id -> g2))))

  driver.close()
  sys.exit()
}

object Test6 extends App {
  val driver = Connection.driver
  val tx = Neo4jCypherTransactor(driver.session())

  val q1 = Match { case g@Vertex("Group") => g.id }
  def q2(id: Long) = Match {
    case (u@Vertex("User")) `<-` _ `<-` g if g.id === lit(id) => u
  }

  println("q1 = " + q1.known.toCypher)
  val query = for {
    groupIds <- tx.read(q1).gather
    _ = println(s"groupIds = $groupIds")
    q2is = groupIds.map(q2)
    _ = println("q2s = " + q2is.zipWithIndex.map{ case (q, i) => s"\t$i. ${q.known.toCypher}" }.mkString("\n", "\n", ""))
    users <- q2is.traverse[tx.ReadTx, Map[String, Any]](tx.read(_)) // TODO: type params are required ========================== <<<<<<<<<<<<<<
    _ = println(s"users = $users")
  } yield (groupIds, users)

  val io = tx.runRead(query)
  val result: Seq[(Seq[Long], Seq[Map[String, Any]])] = io.unsafeRunSync()

  println("result = " + result)

  //  q1 = MATCH (`g`:`Group`) RETURN `id`(`g`)
  //  groupIds = Vector(2, 3)
  //  q2s =
  //    0. MATCH (`u`:`User`) <-[]- () <-[]- (`g`) WHERE `id`(`g`) = 2 RETURN `u`
  //    1. MATCH (`u`:`User`) <-[]- () <-[]- (`g`) WHERE `id`(`g`) = 3 RETURN `u`
  //  users = Vector(Map(name -> John, email -> john@example.com, confirmed -> true, age -> 28, id -> u1), Map(name -> John, email -> john@example.com, confirmed -> true, age -> 28, id -> u1))
  //  result = Vector((Vector(2, 3),Vector(Map(name -> John, email -> john@example.com, confirmed -> true, age -> 28, id -> u1), Map(name -> John, email -> john@example.com, confirmed -> true, age -> 28, id -> u1))))


  driver.close()
  sys.exit()
}
