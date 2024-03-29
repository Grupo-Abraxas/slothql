package com.arkondata.slothql

import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters._
import scala.util.Random

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import com.arkondata.slothql.cypher.CypherStatement
import com.arkondata.slothql.cypher.CypherStatement.LiftedValue
import com.arkondata.slothql.cypher.syntax._
import com.arkondata.slothql.test.Neo4jUsingTest
import com.arkondata.slothql.test.tags.RequiresNeo4j

@RequiresNeo4j
class ParameterizedQueryTest extends AnyWordSpec with Matchers with Neo4jUsingTest {
  import tx.readers._

  lazy val query1 = parameterized { (x: Param[Long], y: Param[String], z: Param[List[String]]) =>
    Unwind(z) { i =>
      Match.optional { case v @ Node("i" := `i`) =>
        `return`(i, v.prop[String]("foo"), y)
      // TODO: .limit(x)
      }
    }
  }

  "Parameterized Query API" should {
    "build cypher parameterized statements" in {
      val x = Random.nextInt(100).toLong
      val y = Random.alphanumeric.take(10).mkString
      val z = List.fill(5)(Random.alphanumeric.take(5).mkString)

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

      val res = tx.runRead(readTx, 10.seconds).compile.toList.unsafeRunSync()
      res shouldBe z.map((_, "null", y))
    }

    "support creating node with properties from values map input" in {
      val query = parameterized { props: Param[LiftedMap] =>
        Create { case n @ Node("Test", `props`) =>
          n.id
        }
      }
      val props0: Map[String, LiftedValue] = Map("x" -> lit(10), "s" -> lit("abc"))
      query.prepared.withParams(props = props0) shouldBe CypherStatement.Prepared(
        "CREATE (`n0`:`Test`$`props`) RETURN `id`(`n0`)",
        Map(
          "props" -> Map("x" -> Int.box(10), "s" -> "abc").asJava
        )
      )
    }
  }

}
