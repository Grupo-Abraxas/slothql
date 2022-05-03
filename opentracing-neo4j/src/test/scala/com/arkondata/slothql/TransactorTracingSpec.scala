package com.arkondata.slothql

import cats.effect.IO
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import com.arkondata.slothql.cypher.GraphElem
import com.arkondata.slothql.cypher.syntax._
import com.arkondata.slothql.neo4j.TransactorTracing
import com.arkondata.slothql.test.Neo4jUsingTest
import com.arkondata.slothql.test.tags.RequiresNeo4j

@RequiresNeo4j
class TransactorTracingSpec extends AnyWordSpec with Matchers with Neo4jUsingTest with TracingSpec {
  implicit lazy val (tracingTx, _) = TransactorTracing[IO](Connection.driver).unsafeRunSync()
  import tracingTx.syntax._
  import tracingTx.readers._

  "run read" in {
    val tx = entryPointApp.use { implicit span =>
      val stream = for {
        _   <- tracingTx.runWrite(create("01"))
        _   <- tracingTx.runWrite(create("02"))
        _   <- tracingTx.runWrite(create("03"))
        res <- tracingTx.runRead(read)
      } yield res

      stream.compile.toList
        .map(_.flatMap(_.props.get("id").map(_.asInstanceOf[String])))
        .map {
          _ should contain theSameElementsAs Seq("01", "02", "03")
        }
        .guarantee(tracingTx.runWrite(delete).compile.drain)
    }

    tx.unsafeRunSync()
  }

  def create(id: String): Tx[GraphElem.Node] = query(parameterized { () =>
    Create { case n @ Node("NodeTrace") =>
      Update(n.set.id := lit(id)) {
        n
      }
    }
  })
    .withParams()

  def read: Tx[GraphElem.Node] = query(parameterized { () =>
    Match { case n @ Node("NodeTrace") =>
      n
    }
  })
    .withParams()

  def delete: Tx[Boolean] = query(parameterized { () =>
    Match { case n @ Node("NodeTrace") =>
      Delete(n)(lit(true))
    }
  })
    .withParams()

}
