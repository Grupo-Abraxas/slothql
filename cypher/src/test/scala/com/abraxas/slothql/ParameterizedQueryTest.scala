package com.abraxas.slothql

import scala.util.Random

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
    (x: Param[Int], y: Param[String]) =>
      Match.optional { case v@Vertex("x" := `x`) =>
        (v.prop[String]("foo"), y)
      }
  }


  "Parameterized Query API" should {
    "build cypher parameterized statements" in {
      val x = Random.nextInt()
      val y = Random.alphanumeric.take(10).mkString

      query1(x = x, y = y) shouldBe Statement(
        "OPTIONAL MATCH (`v`{ `x`: $`x` }) RETURN `v`.`foo`, $`y`",
        Map("x" -> x, "y" -> y)
      )
    }

    "support reading parameterized queries" in {
      val x = Random.nextInt()
      val y = Random.alphanumeric.take(10).mkString

      val readTx = tx
        .read(query1)
        .withParams(x = x, y = y)

      val res = tx.runRead(readTx).unsafeRunSync()
      res shouldBe Vector(("null", y))
    }
  }


}
