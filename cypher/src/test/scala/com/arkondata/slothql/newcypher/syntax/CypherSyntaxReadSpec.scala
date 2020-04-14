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
  }
}
