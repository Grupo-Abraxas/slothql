package com.arkondata.slothql.newcypher.syntax

/** Advanced [[CypherSyntax0Spec]]
 *  - Referencing paths
 *  - Clauses WITH, UNWIND and OPTIONAL MATCH
 *  - {{{if}}} guards
 *  - Query UNION
 *  - Return: All, Nothing, Options
 */
class CypherSyntaxReadSpec extends CypherSyntaxBaseSpec {
  "Slothql cypher syntax" should {
    "match paths" in
      test(
        Match { case path ::= (Node("Foo") -Rel(`**`)> Node("Bar")) =>
          (path.length, path.nodes, path.relationships)
        },
        "MATCH `path0` = (:`Foo`) -[*]-> (:`Bar`) RETURN `length`(`path0`), `nodes`(`path0`), `relationships`(`path0`)"
      ).returns[(Long, List[GraphElem.NodeElem], List[GraphElem.RelElem])]

    "support WITH clause (1)" in
      test(
        With(lit(10)) { n => n + lit(1) },
        "WITH 10 AS `n` RETURN `n` + 1"
      )

    "support WITH clause (2)" in
      test(
        Match { case (a@Node("Foo")) -Rel("bar")> b =>
        With(a.prop[String]("id"), collect(b)) { (id, bs) =>
        Match { case x@Node("Connected", "to" := `id`) =>
          (x.props, x.labels, bs)
        }}},
        "MATCH (`a0`:`Foo`) -[:`bar`]-> (`b0`) " +
        "WITH `a0`.`id` AS `id0`, `collect`(`b0`) AS `bs0` " +
        "MATCH (`x0`:`Connected`{ `to`: `id0` }) " +
        "RETURN `x0`, `labels`(`x0`), `bs0`"
      ).returns[(Map[String, Any], List[String], List[GraphElem.NodeElem])]

    "not allow to use identifiers excluded by WITH" in pending

  }
}
