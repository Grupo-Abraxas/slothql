package com.arkondata.slothql.newcypher.syntax

/** Simplest spec: match and return.
 *  - Basic matching
 *  - Basic expressions
 *  - Returning: expr (with `as`), tuple
 *  - Nesting matches
 */
class CypherSyntax0Spec extends CypherSyntaxBaseSpec {
  "Slothql cypher syntax" should {
    "build query returning properties of any node" in
      test(
        Match {
          case node =>
            assert(node).is[Node] // test
            node.props
        },
        "MATCH (`node`) RETURN `node`"
      ).returns[Map[String, Any]]

    "build query returning labels of any node" in
      test(
        Match {
          case node =>
            node.labels
        },
        "MATCH (`node`) RETURN `labels`(`node`)"
      ).returns[List[String]]

    "support returning tuples from queries" in
      test(
        Match {
          case node =>
            (node.props, node.labels)
        },
        "MATCH (`node`) RETURN `node`, `labels`(`node`)"
      ).returns[(Map[String, Any], List[String])]

    "build query matching connected nodes" in
      test(
        Match {
          case a -_> b =>
            assert(a).is[Node] // test
            assert(b).is[Node] // test
            (a.labels, a.props, b.labels, b.props)
        },
        "MATCH (`a`) --> (`b`) " +
        "RETURN `labels`(`a`), `a`, `labels`(`b`), `b`"
      ).returns[(List[String], Map[String, Any], List[String], Map[String, Any])]

    "build query matching nodes and edges" in
      test(
        Match {
          case a - e > b =>
            assert(a).is[Node] // test
            assert(e).is[Rel]  // test
            assert(b).is[Node] // test
            (a.props, e.props, b.props)
        },
        "MATCH (`a`) -[`e`]-> (`b`) " +
        "RETURN `a`, `e`, `b`"
      ).returns[(Map[String, Any], Map[String, Any], Map[String, Any])]

    "build query returning type of an edge" in
      test(
        Match {
          case _ - e > _ =>
            e.tpe
        },
        "MATCH () -[`e`]-> () " +
        "RETURN `type`(`e`)"
      ).returns[String]

    "support renaming returned column" in
      test(
        Match{
          case a -_> b =>
            (a.props, b.props as "a0")
        },
        "MATCH (`a`) --> (`b`) " +
        "RETURN `a`, `b` AS `a0`"
      ).returns[(Map[String, Any], Map[String, Any])]

    "support nested matches" in
      test(
        Match { case a -_> b =>
        Match { case c < e - d =>
          (a.props, b.props, e.props, c.props)
        }},
        "MATCH (`a`) --> (`b`) " +
        "MATCH (`c`) <-[`e`]- (`d`) " +
        "RETURN `a`, `b`, `e`, `c`"
      ).returns[(Map[String, Any], Map[String, Any], Map[String, Any], Map[String, Any])]
  }
}
