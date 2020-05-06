package com.arkondata.slothql.cypher.syntax

import com.arkondata.slothql.cypher.GraphElem

/** Writing.
 *  - Writing clauses: CREATE, DELETE, SET
 *  - Returning `Nothing`
 */
class CypherSyntaxWriteSpec extends CypherSyntaxBaseSpec {

  "Cypher syntax" should {

    "support creating paths in graph" in test(
      Match { case n@Node("id" := "abc") =>
      Create { case `n` -Rel("foo", "flag" := true)> (bar@Node("Bar", "id" := neo4j.randomUUID)) =>
        bar.prop[String]("id")
      }},
      "MATCH (`n0`{ `id`: \"abc\" }) " +
      "CREATE (`n0`) -[:`foo`{ `flag`: true }]-> (`bar0`:`Bar`{ `id`: `randomUUID`() }) " +
      "RETURN `bar0`.`id`"
    ).returns[String]

    "support setting node properties" in
      test(
        Match { case n@Node("id" := "123") =>
        Update(n.set.foo = lit("qwerty"), n.set.bar = lit(765)) {
          n.id
        }},
        "MATCH (`n0`{ `id`: \"123\" }) " +
        "SET `n0`.`foo` = \"qwerty\", `n0`.`bar` = 765 " +
        "RETURN `id`(`n0`)"
      ).returns[Long]

    "support setting relationship properties" in
      test(
        Match { case (a@Node("A")) -(e@Rel("E"))> (b@Node("B")) =>
        Update(e.set.test = a.prop[Int]("x") + b.prop[Int]("y")) {
          e
        }},
        "MATCH (`a0`:`A`) -[`e0`:`E`]-> (`b0`:`B`) " +
        "SET `e0`.`test` = `a0`.`x` + `b0`.`y` " +
        "RETURN `e0`"
      ).returns[GraphElem.Rel]

  }

}
