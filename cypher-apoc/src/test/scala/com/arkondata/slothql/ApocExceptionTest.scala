package com.arkondata.slothql

import cats.syntax.monadError._
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import com.arkondata.slothql.apoc.ApocException
import com.arkondata.slothql.cypher.syntax._
import com.arkondata.slothql.test.Neo4jUsingTest
import com.arkondata.slothql.test.tags.{ RequiresApoc, RequiresNeo4j }

@RequiresNeo4j
@RequiresApoc
class ApocExceptionTest extends AnyWordSpec with Matchers with Neo4jUsingTest with EitherValues {
  import tx.readers._

  "Exception raised by `apoc.*` function" should {
    "Be extracted/adapted from neo4j exception" in {
      val message = "Test Assertion Failed"
      val query   = APOC.assert(lit(false), lit(message))(lit(true))
      val io = tx
        .runRead(tx.query(query))
        .compile
        .drain
        .adaptError(ApocException.adapt)
      val result = io.attempt.unsafeRunSync()

      result.left.value shouldBe an[ApocException]
      result.left.value.getMessage shouldBe message
    }
  }

}
