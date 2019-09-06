package com.abraxas.slothql

import scala.util.Random

import cats.instances.list._
import org.scalatest.{ BeforeAndAfterAll, Matchers, WordSpec }

import com.abraxas.slothql.cypher.CypherFragment.Statement
import com.abraxas.slothql.cypher.syntax._
import com.abraxas.slothql.neo4j.Neo4jCypherTransactor

class ParameterizedQueryTest extends WordSpec with Matchers with BeforeAndAfterAll {
  val tx = new Neo4jCypherTransactor(Connection.driver)

  override protected def afterAll(): Unit = {
    Connection.driver.close()
    super.afterAll()
  }

  lazy val query1 = parameterized {
    (x: Param[Long], y: Param[String], z: Param[Seq[String]]) =>
      unwind(z) { i =>
        Match.optional { case v@Vertex("i" := `i`) =>
          (i, v.prop[String]("foo"), y)
            .limit(x)
        }
      }
  }


  "Parameterized Query API" should {
    "build cypher parameterized statements" in {
      val x = Random.nextInt(100).toLong
      val y = Random.alphanumeric.take(10).mkString
      val z = List.fill(5){ Random.alphanumeric.take(5).mkString }

      query1.prepared(x = x, y = y, z = z) shouldBe Statement(
        "UNWIND $`z` AS `i` OPTIONAL MATCH (`v`{ `i`: `i` }) RETURN `i`, `v`.`foo`, $`y` LIMIT $`x`",
        Map("x" -> x, "y" -> y, "z" -> z)
      )
    }

    "support reading parameterized queries" in {
      val x = 3 + Random.nextInt(100).toLong
      val y = Random.alphanumeric.take(10).mkString
      val z = (1 to 3).map(_.toString)

      val readTx = tx
        .query[List](query1)
        .withParams(x = x, y = y, z = z)

      val res = tx.runRead(readTx).unsafeRunSync()
      res shouldBe z.map((_, "null", y))
    }
  }


}
