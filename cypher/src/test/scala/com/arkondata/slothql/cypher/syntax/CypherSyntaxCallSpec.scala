package com.arkondata.slothql.cypher.syntax

import com.arkondata.slothql.cypher.CypherFragment.Expr.Alias
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

    "support void procedure calls" in
    test(
      Call("test", lit(1)).void {
        lit(1)
      },
      "CALL `test`(1) " +
      "RETURN 1"
    ).returns[Int]

    "support query calls" in
    test(
      Match { case (n @ Node("Node")) =>
        Call {
          Match { case (m @ Node("Node0")) - Rel("Rel") > `n` =>
            `return`(m as "out", lit(true) as "out1", lit(1L) as "out2")
          } union
          Match { case (m @ Node("Node0")) - Rel("Rel0") > `n` =>
            `return`(m as "out", lit(true) as "out1", lit(1L) as "out2")
          }
        }.yielding("out0", "out1", "out2") { (out1: Node, out2: Alias[Boolean], out3: Alias[Long]) =>
          `return`(out1, out2, out3)
        }
      },
      s"""|MATCH (`n0`:`Node`) CALL { 
          |MATCH (`m0`:`Node0`) -[:`Rel`]-> (`n0`) 
          |RETURN `m0` AS `out`, true AS `out1`, 1 AS `out2`
          | UNION 
          |MATCH (`m1`:`Node0`) -[:`Rel0`]-> (`n0`) 
          |RETURN `m1` AS `out`, true AS `out1`, 1 AS `out2` 
          |} 
          |RETURN `out00`, `out10`, `out20`
          |""".stripMargin.replace("\n", "")
    ).returns[(GraphElem.Node, Boolean, Long)]
  }

}
