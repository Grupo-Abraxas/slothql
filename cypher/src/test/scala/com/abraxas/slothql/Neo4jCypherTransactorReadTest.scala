package com.abraxas.slothql

import cats.data.NonEmptyList
import cats.instances.vector._
import cats.syntax.traverse._
import org.scalactic.source.Position
import org.scalatest.{ Assertion, BeforeAndAfterAll, Matchers, WordSpec }

import com.abraxas.slothql.cypher.CypherFragment.{ Clause, Expr, Pattern, Query, Return }
import com.abraxas.slothql.cypher.syntax._
import com.abraxas.slothql.neo4j.Neo4jCypherTransactor

// DB should contain `populate-1.cypher`
class Neo4jCypherTransactorReadTest extends WordSpec with Matchers with BeforeAndAfterAll {
  val tx = new Neo4jCypherTransactor(Connection.driver)

  private def test[R](read: tx.txBuilder.ReadTx[Vector, R], expected: Seq[R])(implicit pos: Position): Assertion =
    tx.runRead(read).unsafeRunSync() should contain theSameElementsAs expected

  private lazy val allVertices = Seq(
    Map("name" -> "John", "email" -> "john@example.com", "confirmed" -> true, "age" -> 28, "id" -> "u1"),
    Map("name" -> "Root Group", "id" -> "g1"),
    Map("name" -> "Sub Group", "id" -> "g2"),
    Map(), // Members 1
    Map()  // Members 2
  )

  "Neo4jCypherTransactor" should {
    "execute single query (1)" in {
      val pattern = Pattern.Node(alias = Some("n"), labels = Nil, map = Map())
      val query = Query.Clause(
        Clause.Match(
          NonEmptyList(pattern, Nil),
          optional = false,
          where = None
        ),
        Query.Return(Return.Expr(Expr.Var[Any]("n"), as = None))
      )
      test[Any](tx.read(query), allVertices)
    }
    "execute single query (2)" in {
      val pattern = Pattern.Node(alias = Some("n"), labels = Nil, map = Map())
      val query = Query.Clause(
        Clause.Match(
          NonEmptyList(pattern, Nil),
          optional = false,
          where = None
        ),
        Query.Return(Return.Wildcard)
      )
      test[Any](tx.read(query), allVertices)
    }
    "execute single query (3)" in {
      val pattern = Pattern.Node(alias = Some("n"), labels = List("User"), map = Map())
      val n = Expr.Var[Map[String, Any]]("n")
      val query = Query.Clause(
        Clause.Match(
          NonEmptyList(pattern, Nil),
          optional = false,
          where = None
        ),
        Query.Return(
          Return.Tuple(
            Expr.MapKey[String](n, "email"),
            Expr.MapKey[String](n, "name")
          ).known
        )
      )
      test[(String, String)](tx.read(query), Seq(
        "john@example.com" -> "John"
      ))
    }

    "execute single query (4)" in {
      val query = Match{ case x < Edge("foo") - Vertex("User", "id" := "u1") => collect(x.prop[String]("name")) }
      test[List[String]](tx.read(query), Seq(Nil))
    }

    "read tuple results" in {
      val query = tx.read[Vector](Match {
        case u@Vertex("User") =>
          u.labels -> (
            u.prop[String]("id"),
            (u.prop[String]("name"), u.prop[Int]("age")),
            u.prop[Boolean]("confirmed")
          )
      })
      test[(List[String], (String, (String, Int), Boolean))](query, Seq(
        List("User") -> ("u1", ("John", 28), true)
      ))
    }

    "assign paths to variables" in {
      val query = tx.read[Vector](Match {
        case ps ::= (Vertex("Group") - _ *:(0 - _, _) > Vertex("User")) =>
          (
            ps.length,
            ps.nodes.map(n => dict("labels" -> n.labels, "id" -> n.prop[String]("id"))),
            ps.edges.map(_.tpe)
          )
      })
      test[(Long, List[Map[String, Any]], List[String])](query, Seq(
        ( 2,
          List(
            Map("id" -> "g1",   "labels" -> List("Group")),
            Map("id" -> None,   "labels" -> List("Members")),
            Map("id" -> "u1",   "labels" -> List("User"))
          ),
          List("members", "Admin")
        ),
        ( 3,
          List(
            Map("id" -> "g2",   "labels" -> List("Group")),
            Map("id" -> "g1",   "labels" -> List("Group")),
            Map("id" -> None,   "labels" -> List("Members")),
            Map("id" -> "u1",   "labels" -> List("User"))
          ),
          List("parent", "members", "Admin")
        ),
        ( 2,
          List(
            Map("id" -> "g2",   "labels" -> List("Group")),
            Map("id" -> None,   "labels" -> List("Members")),
            Map("id" -> "u1",   "labels" -> List("User"))
          ),
          List("members", "Edit")
        )
      ))
    }

    object ChainAndGatherTest {
      val userQuery = Match { case u@Vertex("User") => u.prop[String]("id") }
      def groupDepQuery(id: String) = Match {
        case (g@Vertex("Group")) -> _ -> u if u.prop[String]("id") === lit(id) => g.props orderBy g.prop[String]("id")
      }

      val groupQuery = Match { case g@Vertex("Group") => g.prop[String]("id") }
      def userDepQuery(id: String) = Match {
        case (u@Vertex("User")) `<-` _ `<-` g if g.prop[String]("id") === lit(id) => u.props orderBy u.prop[String]("id")
      }
    }

    "chain dependent queries in transaction (1)" in {
      val query = for {
        userId <- tx.read[Vector](ChainAndGatherTest.userQuery)
        q2i = ChainAndGatherTest.groupDepQuery(userId)
        group <- tx.read(q2i)
      } yield (userId, group)

      test[(String, Map[String, Any])](query, Seq(
        "u1" -> Map("name" -> "Root Group", "id" -> "g1"),
        "u1" -> Map("name" -> "Sub Group",  "id" -> "g2")
      ))
    }

    "`gather` query results to Vector (1)" in {
      val query = for {
        userId <- tx.read(ChainAndGatherTest.userQuery)
        q2i = ChainAndGatherTest.groupDepQuery(userId)
        groups <- tx.read(q2i).gather
      } yield (userId, groups)

      test[(String, Vector[Map[String, Any]])](query, Seq(
        "u1" -> Vector(
          Map("name" -> "Root Group", "id" -> "g1"),
          Map("name" -> "Sub Group",  "id" -> "g2")
        )
      ))
    }

    "`gather` query results to Vector (2)" in {
      val query = for {
        groupIds <- tx.read[Vector](ChainAndGatherTest.groupQuery).gather
        q2is = groupIds.map(ChainAndGatherTest.userDepQuery)
        users <- q2is.traverse(tx.read[Vector](_))
      } yield (groupIds, users)

      test[(Vector[String], Vector[Map[String, Any]])](query, Seq(
        Vector("g1", "g2") -> Vector(
          Map("name" -> "John", "email" -> "john@example.com", "confirmed" -> true, "age" -> 28, "id" -> "u1"),
          Map("name" -> "John", "email" -> "john@example.com", "confirmed" -> true, "age" -> 28, "id" -> "u1")
        )
      ))
    }

    "`unwind` gathered results" in {
      val query = for {
        groupIds <- tx.read[Vector](ChainAndGatherTest.groupQuery).gather
        q2is = groupIds.map(ChainAndGatherTest.userDepQuery)
        users <- q2is.traverse(tx.read[Vector](_))
        user <- tx.Read.unwind(users.map(_("name").asInstanceOf[String]))
      } yield user

      test[String](query, Seq(
        "John",
        "John"
      ))
    }

    "`unwind` and `gather` empty sequence" in {
      val q0 = tx.Read.nothing[Vector, Int]
      val query = for {
        x <- q0.gather
      } yield (x, "foo")

      test[(Vector[Int], String)](query, Seq(
        Vector.empty -> "foo"
      ))
    }

    "`filter` results" in {
      val q = Match { case g@Vertex("Group") => g.prop[String]("name") }
      val query = for {
        name <- tx.read(q)
        if name contains "Root"
      } yield name

      test[String](query, Seq(
        "Root Group"
      ))
    }

    "`filterOpt` results" in {
      val q = Match { case g@Vertex("Group") => g.prop[String]("name") }
      val query = tx.read(q).filterOpt {
        case name if name contains "Root" => Some(name)
        case _ => None
      }

      test[String](query, Seq(
        "Root Group"
      ))
    }

    "filter results with another read transaction" in {
      val idsTx              = tx read[Vector] Match { case g@Vertex("Group")      => g.prop[String]("id")                        }
      def predTx(id: String) = tx read[Vector] Match { case g@Vertex("id" := `id`) => g.prop[String]("name") contains lit("Root") }
      val query = idsTx.filtering(predTx)

      test[String](query, Seq(
        "g1"
      ))
    }

    "build cartesian product of two queries' results" in {
      val q1 = tx.read[Vector](Match { case g@Vertex("Group") => g.prop[String]("name") })
      val q2 = tx.read[Vector](unwind(lit(List(1, 2))) { i => i }.result)
      val q = q1 x q2

      test[(String, Int)](q, Seq(
        "Root Group" -> 1,
        "Root Group" -> 2,
        "Sub Group" -> 1,
        "Sub Group" -> 2
      ))
    }

    "`zip` results of two queries" in {
      val q1 = tx.read[Vector](Match {
        case g@Vertex("Group") =>
          g.prop[String]("name")
            .orderBy(g.prop[String]("name"))
      })
      val q2 = tx.read[Vector](unwind(lit(List(1, 2))) { i => i }.result)
      val q = q1 zip q2

      test[(String, Int)](q, Seq(
        "Root Group" -> 1,
        "Sub Group"  -> 2
      ))
    }

    "`zip3` results of three queries" in {
      val q1 = tx.read[Vector](Match {
        case g@Vertex("Group") =>
          g.prop[String]("name")
            .orderBy(g.prop[String]("name"))
      })
      val q2 = tx.read[Vector](unwind(lit(List(1, 2))) { i => i }.result)
      val q3 = tx.read[Vector](unwind(lit(Vector("A", "B"))) { i => i }.result)
      val q = tx.Read.zip3(q1, q2, q3)

      test[(String, Int, String)](q, Seq(
        ("Root Group", 1, "A"),
        ("Sub Group",  2, "B")
      ))
    }
  }

  override protected def afterAll(): Unit = {
    Connection.driver.close()
    super.afterAll()
  }
}
