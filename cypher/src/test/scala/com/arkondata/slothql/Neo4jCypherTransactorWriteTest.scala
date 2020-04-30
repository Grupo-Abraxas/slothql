package com.arkondata.slothql

import cats.instances.vector._
import org.scalactic.source.Position
import org.scalatest.{ Assertion, Matchers, WordSpec }

import com.arkondata.slothql.cypher.syntax._
import com.arkondata.slothql.neo4j.Neo4jCypherTransactor

class Neo4jCypherTransactorWriteTest extends WordSpec with Matchers {
  val tx = new Neo4jCypherTransactor(Connection.driver)

  private def test[R](write: tx.txBuilder.Tx[Vector, R], expected: Seq[R])(implicit pos: Position): Assertion =
    tx.runWrite(write).unsafeRunSync() should contain theSameElementsAs expected

  "Neo4jCypherTransactor" should {
    "execute query with CREATE clause" in {
      test(
        tx.query(
          Create { case (foo@Vertex("Foo", "name" := "foo")) -Edge("baz") > (bar@Vertex("Bar", "Baz", "name" := "bar")) =>
            (foo.labels, foo.props, bar.labels, bar.props)
          }
        ),
        Seq(
          (List("Foo"), Map("name" -> "foo"), List("Bar", "Baz"), Map("name" -> "bar"))
        )
      )
    }

    "create nodes given properties as parameter" in {
      import scala.collection.JavaConverters._
      import Neo4jCypherTransactor.SupportedParam.Unsafe.anyIsSupported
      test(
        tx.query(parameterized { props: Param[Map[String, Any]] =>
          Create { case t@Vertex("Test", `props`) => t.props }
        })
          .withParams(props = Map("a" -> 1, "b" -> List("x", "y", "z").asJava)),
        Seq(Map("a" -> 1, "b" -> List("x", "y", "z")))
      )
    }

    "execute query with SET clause, returning nothing" in test(
      tx.query(
        Create{ case Vertex("Q") =>
          withWildcard() {
            Match { case q@Vertex("Q") =>
              SetProp(q.set("n") = lit(10)) {
                returnNothing
              }
            }
          }
        }
      ),
      Seq()
    )
  }
}
