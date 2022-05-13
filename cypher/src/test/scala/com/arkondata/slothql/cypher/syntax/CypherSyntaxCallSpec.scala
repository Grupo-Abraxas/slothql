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
          With.references(n) {
            Match { case (m @ Node("Node0")) - Rel("Rel") > `n` =>
              `return`(m as "out0", lit(true) as "out1", lit(1L) as "out2")
            }
          } union
          With.references(n) {
            Match { case (m @ Node("Node0")) - Rel("Rel0") > `n` =>
              `return`(m as "out0", lit(true) as "out1", lit(1L) as "out2")
            }
          }
        }.yielding("out0", "out1", "out2") { (out1: Node, out2: Alias[Boolean], out3: Alias[Long]) =>
          With(out1, out2, out3) { (out1, out2, out3) =>
            `return`(out1, out2, out3)
          }
        }
      },
      s"""|MATCH (`n0`:`Node`) CALL { 
          |WITH `n0`
          | MATCH (`m0`:`Node0`) -[:`Rel`]-> (`n0`) 
          |RETURN `m0` AS `out0`, true AS `out1`, 1 AS `out2`
          | UNION 
          |WITH `n0`
          | MATCH (`m1`:`Node0`) -[:`Rel0`]-> (`n0`) 
          |RETURN `m1` AS `out0`, true AS `out1`, 1 AS `out2` 
          |} 
          |WITH `out0` AS `out10`, `out1` AS `out20`, `out2` AS `out30`
          | RETURN `out10`, `out20`, `out30`
          |""".stripMargin.replace("\n", "")
    ).returns[(GraphElem.Node, Boolean, Long)]
  }

}
