package com.arkondata.slothql

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext
import scala.util.Random

import cats.effect.IO
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import com.arkondata.slothql.cypher.CypherStatement
import com.arkondata.slothql.cypher.syntax._
import com.arkondata.slothql.neo4j.Neo4jCypherTransactor

class ParameterizedQueryTest extends AnyWordSpec with Matchers with BeforeAndAfterAll {
  implicit val cs = IO.contextShift(ExecutionContext.global)

  val tx = Neo4jCypherTransactor[IO](Connection.driver)
  import tx.readers._
  import tx.ops._

  override protected def afterAll(): Unit = {
    Connection.driver.close()
    super.afterAll()
  }

  lazy val query1 = parameterized {
    (x: Param[Long], y: Param[String], z: Param[List[String]]) =>
      Unwind(z) { i =>
        Match.optional { case v@Node("i" := `i`) =>
          `return`(i, v.prop[String]("foo"), y)
            // TODO: .limit(x)
        }
      }
  }


  "Parameterized Query API" should {
    "build cypher parameterized statements" in {
      val x = Random.nextInt(100).toLong
      val y = Random.alphanumeric.take(10).mkString
      val z = List.fill(5){ Random.alphanumeric.take(5).mkString }

      query1.prepared.withParams(x = x, y = y, z = z)

      // TODO: LIMIT $`x`
      // TODO: "x" -> Long.box(x)
      query1.prepared.withParams(x = x, y = y, z = z) shouldBe CypherStatement.Prepared(
        "UNWIND $`z` AS `i0` OPTIONAL MATCH (`v0`{ `i`: `i0` }) RETURN `i0`, `v0`.`foo`, $`y`",
        Map("y" -> y, "z" -> z.asJava)
      )
    }

    "support reading parameterized queries" in {
      val x = 3 + Random.nextInt(100).toLong
      val y = Random.alphanumeric.take(10).mkString
      val z = (1 to 3).map(_.toString).toList

      val readTx = tx
        .query(query1)
        .withParams(x = x, y = y, z = z)

      val res = tx.runRead(readTx).compile.toList.unsafeRunSync()
      res shouldBe z.map((_, "null", y))
    }
  }


}
