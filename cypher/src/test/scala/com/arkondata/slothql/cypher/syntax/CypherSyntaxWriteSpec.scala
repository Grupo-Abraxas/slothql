package com.arkondata.slothql.cypher.syntax

import com.arkondata.slothql.cypher.GraphElem

/** Writing.
  *  - Writing clauses: CREATE, DELETE, SET
  *  - Returning `Nothing`
  */
class CypherSyntaxWriteSpec extends CypherSyntaxBaseSpec {

  "Cypher syntax" should {

    "support creating paths in graph" in test(
      Match { case n @ Node("id" := "abc") =>
        Create { case `n` - Rel("foo", "flag" := true) > (bar @ Node("Bar", "id" := neo4j.randomUUID)) =>
          bar.prop[String]("id")
        }
      },
      "MATCH (`n0`{ `id`: \"abc\" }) " +
      "CREATE (`n0`) -[:`foo`{ `flag`: true }]-> (`bar0`:`Bar`{ `id`: `randomUUID`() }) " +
      "RETURN `bar0`.`id`"
    ).returns[String]

    "support setting node properties" in
    test(
      Match { case n @ Node("id" := "123") =>
        Update(n.set.foo = lit("qwerty"), n.set.bar = lit(765)) {
          n.id
        }
      },
      "MATCH (`n0`{ `id`: \"123\" }) " +
      "SET `n0`.`foo` = \"qwerty\", `n0`.`bar` = 765 " +
      "RETURN `id`(`n0`)"
    ).returns[Long]

    "support settings node properties from map" in
    test(
      Match { case n @ Node("id" := "123") =>
        Update(n := lit(Map("foo" -> lit("qwerty")))) {
          n.id
        }
      },
      "MATCH (`n0`{ `id`: \"123\" }) " +
      "SET `n0` = {`foo`: \"qwerty\"} " +
      "RETURN `id`(`n0`)"
    ).returns[Long]

    "support setting relationship properties" in
    test(
      Match { case (a @ Node("A")) - (e @ Rel("E")) > (b @ Node("B")) =>
        Update(e.set.test = a.prop[Int]("x") + b.prop[Int]("y")) {
          e
        }
      },
      "MATCH (`a0`:`A`) -[`e0`:`E`]-> (`b0`:`B`) " +
      "SET `e0`.`test` = `a0`.`x` + `b0`.`y` " +
      "RETURN `e0`"
    ).returns[GraphElem.Rel]

    "support deleting nodes and relationships" in
    test(
      Match { case (a @ Node("id" := 1)) - e > b =>
        Delete(a, e, b) {
          lit(true)
        }
      },
      "MATCH (`a0`{ `id`: 1 }) -[`e0`]-> (`b0`) " +
      "DELETE `a0`, `e0`, `b0` " +
      "RETURN true"
    ).returns[Boolean]

    "support detach deleting nodes" in
    test(
      Match { case (a @ Node("id" := 1)) - _ > b =>
        Delete.detach(a, b) {
          lit(true)
        }
      },
      "MATCH (`a0`{ `id`: 1 }) --> (`b0`) " +
      "DETACH DELETE `a0`, `b0` " +
      "RETURN true"
    ).returns[Boolean]

    "support returning no rows" in
    test(
      Create { case Node("Foo", "id" := 1) =>
        returnNothing
      },
      "CREATE (:`Foo`{ `id`: 1 }) "
    ).returns[Unit]
  }

}
