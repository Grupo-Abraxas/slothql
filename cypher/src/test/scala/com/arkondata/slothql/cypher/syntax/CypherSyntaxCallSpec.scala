package com.arkondata.slothql.cypher.syntax

import com.arkondata.slothql.cypher.GraphElem

/** Calling procedures. */
class CypherSyntaxCallSpec extends CypherSyntaxBaseSpec {

  "Cypher syntax" should {
    "support yielding procedure calls" in
      test(
        Call("test", lit(1)).yielding("foo") { (bar: Expr[GraphElem.Node]) =>
          bar.prop[String]("x")
        },
        "CALL `test`(1) YIELD `foo` AS `bar0` " +
        "RETURN `bar0`.`x`"
      ).returns[String]
  }

}
