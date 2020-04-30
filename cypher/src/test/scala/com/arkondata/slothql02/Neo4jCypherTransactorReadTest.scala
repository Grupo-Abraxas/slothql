package com.arkondata.slothql02

import scala.concurrent.ExecutionContext

import cats.data.NonEmptyList
import cats.effect.IO
import cats.instances.list._
import cats.syntax.traverse._
import org.scalactic.source.Position
import org.scalatest.{ Assertion, BeforeAndAfterAll }
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import com.arkondata.slothql02.cypher.CypherFragment.{ Clause, Expr, Pattern, Query, Return }
import com.arkondata.slothql02.cypher.GraphElem
import com.arkondata.slothql02.cypher.syntax._
import com.arkondata.slothql02.neo4j.Neo4jCypherTransactor

// DB should contain `populate-1.cypher`
class Neo4jCypherTransactorReadTest extends AnyWordSpec with Matchers with BeforeAndAfterAll {
  implicit val cs = IO.contextShift(ExecutionContext.global)

  val tx = Neo4jCypherTransactor[IO](Connection.driver)
  import tx.readers._
  import tx.ops._

  private def test[R](read: tx.Tx[R], expected: Seq[R])(implicit pos: Position): Assertion =
    tx.runRead(read).compile.toList.unsafeRunSync() should contain theSameElementsAs expected

  private def resetId(node: GraphElem.Node): GraphElem.Node = node.copy(id = 0)

  private lazy val allVertices = Seq(
    GraphElem.Node(0, List("User"),
      Map("name" -> "John", "email" -> "john@example.com", "confirmed" -> true, "age" -> 28, "id" -> "u1")
    ),
    GraphElem.Node(0, List("Group"),
      Map("name" -> "Root Group", "id" -> "g1")
    ),
    GraphElem.Node(0, List("Group"),
      Map("name" -> "Sub Group", "id" -> "g2")
    ),
    GraphElem.Node(0, List("Members"), Map()),
    GraphElem.Node(0, List("Members"), Map())
  )

  "Neo4jCypherTransactor" should {
    "execute single query (1)" in {
      val n = Expr.Alias[GraphElem.Node]("n")
      val pattern = Pattern.Node(alias = Some(n), labels = Nil, props = Map())
      val query = Query.Clause(
        Clause.Match(
          NonEmptyList(pattern, Nil),
          optional = false,
          where = None
        ),
        Query.Return(Return.Expr(n, as = None))
      )
      test[GraphElem.Node](tx.query(query).map(resetId), allVertices)
    }

    "execute single query (2)" in {
      val pattern = Pattern.Node(alias = Some(Expr.Alias("n")), labels = Nil, props = Map())
      val query = Query.Clause(
        Clause.Match(
          NonEmptyList(pattern, Nil),
          optional = false,
          where = None
        ),
        Query.Return(Return.Wildcard)
      )
      test[GraphElem.Node](tx.query(query).map(v => resetId(v.asInstanceOf[GraphElem.Node])), allVertices)
    }

    "execute single query (3)" in {
      val n = Expr.Alias("n")
      val pattern = Pattern.Node(alias = Some(n), labels = List("User"), props = Map())
      val query = Query.Clause(
        Clause.Match(
          NonEmptyList(pattern, Nil),
          optional = false,
          where = None
        ),
        Query.Return(
          Return.Tuple(
            Return.Expr(Expr.MapKey[String](n, "email"), as = None) ::
            Return.Expr(Expr.MapKey[String](n, "name"),  as = None) ::
            Nil
          )
        )
      )
      test[(String, String)](tx.query(query), Seq(
        "john@example.com" -> "John"
      ))
    }

    "execute single query (4)" in {
      val query = Match{ case x < Rel("foo") - Node("User", "id" := "u1") => collect(x.prop[String]("name")) }
      test[List[String]](tx.query(query), Seq(Nil))
    }


    "assign paths to variables" in {
      val query = tx.query(Match {
        case ps ::= (Node("Group") - Rel(0 ** _) > Node("User")) =>
          (
            ps.length,
            ps.nodes.map(n => dict[Any]("labels" -> n.labels, "id" -> n.prop[String]("id"))),
            ps.relationships.map(_.tpe)
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
      val userQuery = Match { case u@Node("User") => u.prop[String]("id") }
      def groupDepQuery(id: String) = Match {
        case (g@Node("Group")) -_> _ -_> u if u.prop[String]("id") === lit(id) => g.props orderBy g.prop[String]("id")
      }

      val groupQuery = Match { case g@Node("Group") => g.prop[String]("id") }
      def userDepQuery(id: String) = Match {
        case (u@Node("User")) < _ - _ < _ - g if g.prop[String]("id") === lit(id) => u.props orderBy u.prop[String]("id")
      }

      def groupByIdQuery(id: String) = Match {
        case g@Node("Group", "id" := `id`) => g.props orderBy g.prop[String]("id")
      }
    }

    "chain dependent queries in transaction (1)" in {
      val query = for {
        userId <- tx.query(ChainAndGatherTest.userQuery)
        q2i = ChainAndGatherTest.groupDepQuery(userId)
        group <- tx.query(q2i)
      } yield (userId, group)

      test[(String, Map[String, Any])](query, Seq(
        "u1" -> Map("name" -> "Root Group", "id" -> "g1"),
        "u1" -> Map("name" -> "Sub Group",  "id" -> "g2")
      ))
    }

    "`gather` query results to fs2 Stream" in {
      val query = for {
        userId <- tx.query(ChainAndGatherTest.userQuery)
        q2i = ChainAndGatherTest.groupDepQuery(userId)
        groups <- tx.query(q2i).gather
      } yield (userId, groups)

      val io = for {
        l <- tx.runRead(query).compile.toList
        (ids, gs) = l.unzip
        groups <- gs.flatTraverse(_.compile.toList)
      } yield (ids, groups)

      val (userIds, groups) = io.unsafeRunSync()
      userIds shouldBe List("u1")
      groups should contain theSameElementsAs Seq(
        Map("name" -> "Root Group", "id" -> "g1"),
        Map("name" -> "Sub Group",  "id" -> "g2")
      )
    }

    "`gather` query results to Vector" in {
      val query = for {
        userId <- tx.query(ChainAndGatherTest.userQuery)
        q2i = ChainAndGatherTest.groupDepQuery(userId)
        groups <- tx.query(q2i).gatherStream(_.toVector)
      } yield (userId, groups)

      test[(String, Vector[Map[String, Any]])](query, Seq(
        "u1" -> Vector(
          Map("name" -> "Root Group", "id" -> "g1"),
          Map("name" -> "Sub Group",  "id" -> "g2")
        )
      ))
    }

    "`unwind` stream" in {
      val ids = Seq("g1", "g2")
      val query = for {
        id <- tx.unwind(fs2.Stream.emits(ids))
        group <- tx.query(ChainAndGatherTest.groupByIdQuery(id))
      } yield group("name").asInstanceOf[String]

      test[String](query, Seq(
        "Root Group",
        "Sub Group"
      ))
    }

    "`unwind` gathered results" in {
      val query = for {
        groupIds <- tx.query(ChainAndGatherTest.groupQuery).gather
        q2is = groupIds.map(ChainAndGatherTest.userDepQuery)
        users <- q2is.traverseTx(tx.query(_))
        user <- tx.unwind(users.map(_("name").asInstanceOf[String]))
      } yield user

      test[String](query, Seq(
        "John",
        "John"
      ))
    }

    "`unwind` and `gather` empty sequence" in {
      val q0 = tx.nothing[Int]
      val query = for {
        x0 <- q0.gather
        x <- tx.liftF(x0.compile.toVector)
      } yield (x, "foo")

      test[(Vector[Int], String)](query, Seq(
        Vector.empty -> "foo"
      ))
    }


    "`traverseTx` streams" in {
      val query = for {
        groupIds <- tx.query(ChainAndGatherTest.groupQuery).gather
        q2is = groupIds.map(ChainAndGatherTest.userDepQuery)
        users <- q2is.traverseTx(tx.query(_))
        groupsV <- tx.liftF(groupIds.compile.toVector)
        usersV <- tx.liftF(users.compile.toVector)
      } yield (groupsV, usersV)

      test[(Vector[String], Vector[Map[String, Any]])](query, Seq(
        Vector("g1", "g2") -> Vector(
          Map("name" -> "John", "email" -> "john@example.com", "confirmed" -> true, "age" -> 28, "id" -> "u1"),
          Map("name" -> "John", "email" -> "john@example.com", "confirmed" -> true, "age" -> 28, "id" -> "u1")
        )
      ))
    }

    "`flatTraverseTx` streams" in {
      val query = for {
        groupIds <- tx.query(ChainAndGatherTest.groupQuery).gather
        q2is = groupIds.map(ChainAndGatherTest.userDepQuery)
        users <- q2is.flatTraverseTx(tx.query(_))
      } yield users

      test[Map[String, Any]](query, Seq(
        Map("name" -> "John", "email" -> "john@example.com", "confirmed" -> true, "age" -> 28, "id" -> "u1"),
        Map("name" -> "John", "email" -> "john@example.com", "confirmed" -> true, "age" -> 28, "id" -> "u1")
      ))
    }

    "`filter` results" in {
      val q = Match { case g@Node("Group") => g.prop[String]("name") }
      val query = for {
        name <- tx.query(q)
        if name contains "Root"
      } yield name

      test[String](query, Seq(
        "Root Group"
      ))
    }

    "`mapOpt` results" in {
      val q = Match { case g@Node("Group") => g.prop[String]("name") }
      val query = tx.query(q).mapOpt {
        case name if name contains "Root" => Some(name)
        case _ => None
      }

      test[String](query, Seq(
        "Root Group"
      ))
    }

    "filter results with another read transaction" in {
      val idsTx              = tx query Match { case g@Node("Group")      => g.prop[String]("id")                        }
      def predTx(id: String) = tx query Match { case g@Node("id" := `id`) => g.prop[String]("name") contains lit("Root") }
      val query = idsTx.filtering(predTx)

      test[String](query, Seq(
        "g1"
      ))
    }

    "build cartesian product of two queries' results" in {
      val q1 = tx.query(Match { case g@Node("Group") => g.prop[String]("name") })
      val q2 = tx.query(Unwind(lit(List(1, 2))) { i => i })
      val q = q1 x q2

      test[(String, Int)](q, Seq(
        "Root Group" -> 1,
        "Root Group" -> 2,
        "Sub Group" -> 1,
        "Sub Group" -> 2
      ))
    }

    "`zip` results of two queries" in {
      val q1 = tx.query(Match {
        case g@Node("Group") =>
          g.prop[String]("name")
            .orderBy(g.prop[String]("name"))
      })
      val q2 = tx.query(Unwind(lit(List(1, 2))) { i => i })
      val q = q1 zip q2

      test[(String, Int)](q, Seq(
        "Root Group" -> 1,
        "Sub Group"  -> 2
      ))
    }

    "`zip3` results of three queries" in {
      val q1 = tx.query(Match {
        case g@Node("Group") =>
          g.prop[String]("name")
            .orderBy(g.prop[String]("name"))
      })
      val q2 = tx.query(Unwind(lit(List(1, 2))) { i => i })
      val q3 = tx.query(Unwind(lit(List("A", "B"))) { i => i })
      val q = tx.zip3(q1, q2, q3)

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
