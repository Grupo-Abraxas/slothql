package com.abraxas.slothql.newcypher.syntax

import org.scalatest.{ Assertion, Matchers, WordSpec }

import com.abraxas.slothql.newcypher.{ CypherFragment, CypherStatement }

trait CypherSyntaxBaseSpec extends WordSpec with Matchers {
  protected def cypherGen: CypherStatement.Gen = new CypherSyntaxBaseSpec.StubIdGen

  protected def test[A](query: CypherFragment.Query[A], expectedTemplate: String, expectedParams: Map[String, CypherStatement.LiftValue[_]] = Map()): Test[A] =
    new Test[A](query, expectedTemplate, expectedParams)
  protected class Test[T](query: CypherFragment.Query[T], expectedTemplate: String, expectedParams: Map[String, CypherStatement.LiftValue[_]]) {
    def returns[R](implicit correct: T =:= R): Assertion = {
      val (CypherStatement.Complete(template, params), _) = query.toCypherF(cypherGen)
      template shouldBe expectedTemplate
      params   shouldBe expectedParams
    }
  }

}

object CypherSyntaxBaseSpec {
  class StubIdGen extends CypherStatement.Gen {
    def nextAlias(prefix: String): (String, CypherStatement.Gen) = (prefix, this)
    def nextParam(prefix: String): (String, CypherStatement.Gen) = (prefix, this)
  }
}