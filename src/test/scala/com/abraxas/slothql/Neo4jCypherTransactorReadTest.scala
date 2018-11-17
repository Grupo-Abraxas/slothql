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
  val driver = Connection.driver
  val tx = Neo4jCypherTransactor(driver.session())

  private def test[R](read: tx.txBuilder.ReadTx[R], expected: Seq[R])(implicit pos: Position): Assertion =
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
        Query.Return(Return.All)
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
          Return.List(
            Expr.Key[String](n, "email"),
            Expr.Key[String](n, "name")
          ).known
        )
      )
      test[(String, String)](tx.read(query), Seq(
        "john@example.com" -> "John"
      ))
    }

    "chain dependent queries in transaction (1)" in {
      val q1 = Match { case u@Vertex("User") => u.id }
      def q2(id: Long) = Match {
        case (g@Vertex("Group")) -> _ -> u if u.id === lit(id) => g
      }
      val query = for {
        userId <- tx.read(q1)
        q2i = q2(userId)
        group <- tx.read(q2i)
      } yield (userId, group)

      test[(Long, Map[String, Any])](query, Seq(
        0L -> Map("name" -> "Root Group", "id" -> "g1"),
        0L -> Map("name" -> "Sub Group",  "id" -> "g2")
      ))
    }

    "`gather` query results (1)" in {
      val q1 = Match { case u@Vertex("User") => u.id }
      def q2(id: Long) = Match {
        case (g@Vertex("Group")) -> _ -> u if u.id === lit(id) => g
      }
      val query = for {
        userId <- tx.read(q1)
        q2i = q2(userId)
        groups <- tx.read(q2i).gather
      } yield (userId, groups)

      test[(Long, Vector[Map[String, Any]])](query, Seq(
        0L -> Vector(
          Map("name" -> "Root Group", "id" -> "g1"),
          Map("name" -> "Sub Group",  "id" -> "g2")
        )
      ))
    }

    "`gather` query results (2)" in {
      val q1 = Match { case g@Vertex("Group") => g.id }
      def q2(id: Long) = Match {
        case (u@Vertex("User")) `<-` _ `<-` g if g.id === lit(id) => u
      }
      val query = for {
        groupIds <- tx.read(q1).gather
        q2is = groupIds.map(q2)
        users <- q2is.traverse(tx.read(_))
      } yield (groupIds, users)

      test[(Vector[Long], Vector[Map[String, Any]])](query, Seq(
        Vector(2L, 3L) -> Vector(
          Map("name" -> "John", "email" -> "john@example.com", "confirmed" -> true, "age" -> 28, "id" -> "u1"),
          Map("name" -> "John", "email" -> "john@example.com", "confirmed" -> true, "age" -> 28, "id" -> "u1")
        )
      ))
    }

    "`unwind` gathered results" in {
      val q1 = Match { case g@Vertex("Group") => g.id }
      def q2(id: Long) = Match {
        case (u@Vertex("User")) `<-` _ `<-` g if g.id === lit(id) => u
      }
      val query = for {
        groupIds <- tx.read(q1).gather
        q2is = groupIds.map(q2)
        users <- q2is.traverse(tx.read(_))
        user <- tx.Read.unwind(users.map(_("name").asInstanceOf[String]))
      } yield user

      test[String](query, Seq(
        "John",
        "John"
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
  }

  override protected def afterAll(): Unit = {
    driver.close()
    super.afterAll()
  }
}
