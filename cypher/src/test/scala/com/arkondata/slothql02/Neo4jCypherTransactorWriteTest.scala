/*
package com.arkondata.slothql02

import cats.instances.vector._
import org.scalactic.source.Position
import org.scalatest.{ Assertion, Matchers, WordSpec }

import com.arkondata.slothql02.cypher.syntax._
import com.arkondata.slothql02.neo4j.Neo4jCypherTransactor

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
*/
