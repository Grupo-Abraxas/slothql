package com.arkondata.slothql02.newcypher.syntax

import com.arkondata.slothql02.newcypher.CypherFragment

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
        "MATCH (`node0`) RETURN `node0`"
      ).returns[Map[String, Any]]

    "build query returning labels of any node" in
      test(
        Match {
          case node =>
            node.labels
        },
        "MATCH (`node0`) RETURN `labels`(`node0`)"
      ).returns[List[String]]

    "support returning tuples from queries" in
      test(
        Match {
          case node =>
            (node.props, node.labels)
        },
        "MATCH (`node0`) RETURN `node0`, `labels`(`node0`)"
      ).returns[(Map[String, Any], List[String])]

    "build query matching connected nodes" in
      test(
        Match {
          case a -_> b =>
            assert(a).is[Node] // test
            assert(b).is[Node] // test
            (a.labels, a.props, b.labels, b.props)
        },
        "MATCH (`a0`) --> (`b0`) " +
        "RETURN `labels`(`a0`), `a0`, `labels`(`b0`), `b0`"
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
        "MATCH (`a0`) -[`e0`]-> (`b0`) " +
        "RETURN `a0`, `e0`, `b0`"
      ).returns[(Map[String, Any], Map[String, Any], Map[String, Any])]

    "build query returning type of an edge" in
      test(
        Match {
          case _ - e > _ =>
            e.tpe
        },
        "MATCH () -[`e0`]-> () " +
        "RETURN `type`(`e0`)"
      ).returns[String]

    "support renaming returned column" in
      test(
        Match{
          case a -_> b =>
            (a.props, b.props as "foo")
        },
        "MATCH (`a0`) --> (`b0`) " +
        "RETURN `a0`, `b0` AS `foo0`"
      ).returns[(Map[String, Any], Map[String, Any])]

    "support nested matches" in
      test(
        Match { case a -_> b =>
        Match { case c < e - d =>
          (a.props, b.props, e.props, c.props)
        }},
        "MATCH (`a0`) --> (`b0`) " +
        "MATCH (`c0`) <-[`e0`]- (`d0`) " +
        "RETURN `a0`, `b0`, `e0`, `c0`"
      ).returns[(Map[String, Any], Map[String, Any], Map[String, Any], Map[String, Any])]

    "avoid collisions of pattern expressions' aliases" in
      test(
        Match { case a -_> b =>
        Match { case a < e - c =>
          (a.props, b.props, e.props, c.props)
        }},
        "MATCH (`a0`) --> (`b0`) " +
        "MATCH (`a1`) <-[`e0`]- (`c0`) " +
        "RETURN `a1`, `b0`, `e0`, `c0`"
      ).returns[(Map[String, Any], Map[String, Any], Map[String, Any], Map[String, Any])]

    "avoid collisions of pattern expressions' aliases (2)" in {
      def match1[R](res: (Node, Node) => CypherFragment.Query.Query0[R]) = Match {
        case a -_> b => res(a, b)
      }
      test(
        match1 { (n1, n2) =>
        Match { case a -_> x =>
          (n1.props, n2.props, a.props, x.props)
        }},
        "MATCH (`a0`) --> (`b0`) " +
        "MATCH (`a1`) --> (`x0`) " +
        "RETURN `a0`, `b0`, `a1`, `x0`"
      ).returns[(Map[String, Any], Map[String, Any], Map[String, Any], Map[String, Any])]
    }

    "support reusing graph nodes at nested matches" in
      test(
        Match { case a -_> b =>
        Match { case `b` < e - c =>
          (a.props, b.props, e.props, c.props)
        }},
        "MATCH (`a0`) --> (`b0`) " +
        "MATCH (`b0`) <-[`e0`]- (`c0`) " +
        "RETURN `a0`, `b0`, `e0`, `c0`"
      ).returns[(Map[String, Any], Map[String, Any], Map[String, Any], Map[String, Any])]

    "support reusing graph nodes at nested matches (2)" in {
      def match1[R](res: (Node, Node) => CypherFragment.Query.Query0[R]) = Match {
        case a -_> b => res(a, b)
      }
      test(
        match1 { (n1, n2) =>
        Match { case `n1` -_> a =>
          (n1.props, n2.props, a.props)
        }},
        "MATCH (`a0`) --> (`b0`) " +
        "MATCH (`a0`) --> (`a1`) " +
        "RETURN `a0`, `b0`, `a1`"
      ).returns[(Map[String, Any], Map[String, Any], Map[String, Any])]
    }

  }
}
