package com.arkondata.slothql.cypher.syntax

import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import com.arkondata.slothql.cypher.{ CypherFragment, CypherStatement }

trait CypherSyntaxBaseSpec extends AnyWordSpec with Matchers {
  protected def cypherGen: CypherStatement.Gen = CypherStatement.Gen.Default()

  protected def test[A](
    query: CypherFragment.Query[A],
    expectedTemplate: String,
    expectedParams: Map[String, CypherStatement.LiftValue[_]] = Map()
  ): Test[A] =
    new Test[A](query, expectedTemplate, expectedParams)

  protected class Test[T](
    query: CypherFragment.Query[T],
    expectedTemplate: String,
    expectedParams: Map[String, CypherStatement.LiftValue[_]]
  ) {

    def returns[R](implicit correct: T =:= R): Assertion = {
      val (CypherStatement.Complete(template, params), _) = query.toCypherF(cypherGen)
      withClue(s"complete template: $template\n") {
        template shouldBe expectedTemplate
        params shouldBe expectedParams
      }
    }
  }

  protected def assert[A](a: A): IsOps[A] = isOps.asInstanceOf[IsOps[A]]

  protected class IsOps[T] {
    def is[R](implicit correct: T <:< R): Assertion = succeed
  }
  private lazy val isOps = new IsOps[Any]
}
