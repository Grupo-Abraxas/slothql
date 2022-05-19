package com.arkondata.slothql.cypher.syntax

import shapeless.test.illTyped

import com.arkondata.slothql.cypher.{ CypherFragment, GraphElem }

/** Advanced [[CypherSyntax0Spec]]
  *  - Referencing paths
  *  - Clauses WITH, UNWIND and OPTIONAL MATCH
  *  - {{{if}}} guards
  *  - Query UNION
  *  - Return: All, Nothing, Options
  */
class CypherSyntaxReadSpec extends CypherSyntaxBaseSpec {

  "Slothql cypher syntax" should {
    "support `if` guards" in
    test(
      Match { case a - _ > b if a.prop[Int]("foo") === b.prop[Int]("bar") => b.props },
      "MATCH (`a0`) --> (`b0`) WHERE `a0`.`foo` = `b0`.`bar` RETURN `b0`"
    ).returns[Map[String, Any]]

    "support optional `if` guards (defined)" in {
      def condOpt(n: Node): Option[CypherFragment.Expr[Boolean]] = Some(n.prop[String]("key") === lit("foobar"))
      test(
        Match { case a - _ > b if condOpt(a) => b.props },
        "MATCH (`a0`) --> (`b0`) WHERE `a0`.`key` = \"foobar\" RETURN `b0`"
      ).returns[Map[String, Any]]
    }

    "support optional `if` guards (undefined)" in {
      def condOpt(n: Node): Option[CypherFragment.Expr[Boolean]] = None
      test(
        Match { case a - _ > b if condOpt(a) => b.props },
        "MATCH (`a0`) --> (`b0`) RETURN `b0`"
      ).returns[Map[String, Any]]
    }

    "match paths" in
    test(
      Match { case path ::= (Node("Foo") - Rel(`**`) > Node("Bar")) =>
        (path.length, path.nodes, path.relationships)
      },
      "MATCH `path0` = (:`Foo`) -[*]-> (:`Bar`) RETURN `length`(`path0`), `nodes`(`path0`), `relationships`(`path0`)"
    ).returns[(Long, List[GraphElem.Node], List[GraphElem.Rel])]

    "support optional matches" in
    test(
      Match { case a =>
        Match.optional { case `a` - _ > b =>
          (a.prop[String]("id"), collect(b.prop[String]("id")))
        }
      },
      "MATCH (`a0`) " +
      "OPTIONAL MATCH (`a0`) --> (`b0`) " +
      "RETURN `a0`.`id`, `collect`(`b0`.`id`)"
    ).returns[(String, List[String])]

    "support conditionally optional matches (strict)" in {
      def opt: Boolean = false
      test(
        Match.maybe(opt) { case a => a.props },
        "MATCH (`a0`) RETURN `a0`"
      ).returns[Map[String, Any]]
    }

    "support conditionally optional matches (optional)" in {
      def opt: Boolean = true
      test(
        Match.maybe(opt) { case a => a.props },
        "OPTIONAL MATCH (`a0`) RETURN `a0`"
      ).returns[Map[String, Any]]
    }

    "support WITH clause (1)" in
    test(
      With(lit(10))(n => n + lit(1)),
      "WITH 10 AS `n0` RETURN `n0` + 1"
    ).returns[Int]

    "support WITH clause (2)" in
    test(
      Match { case (a @ Node("Foo")) - Rel("bar") > b =>
        With(a.prop[String]("id"), collect(b)) { (id, bs) =>
          Match { case x @ Node("Connected", "to" := `id`) =>
            (x.props, x.labels, bs)
          }
        }
      },
      "MATCH (`a0`:`Foo`) -[:`bar`]-> (`b0`) " +
      "WITH `a0`.`id` AS `id0`, `collect`(`b0`) AS `bs0` " +
      "MATCH (`x0`:`Connected`{ `to`: `id0` }) " +
      "RETURN `x0`, `labels`(`x0`), `bs0`"
    ).returns[(Map[String, Any], List[String], List[GraphElem.Node])]

    "support wildcard at WITH clause (1)" in
    test(
      Match { case a =>
        With(**, a.prop[Int]("x") + a.prop[Int]("y")) { n =>
          Match {
            case c if c.prop[Int]("z") === n =>
              (a.props, c.props)
          }
        }
      },
      "MATCH (`a0`) " +
      "WITH *, `a0`.`x` + `a0`.`y` AS `n0` " +
      "MATCH (`c0`) WHERE `c0`.`z` = `n0` " +
      "RETURN `a0`, `c0`"
    ).returns[(Map[String, Any], Map[String, Any])]

    "support wildcard at WITH clause (2)" in
    test(
      Match { case a =>
        With(**) {
          With.orderBy(a.prop[Int]("time"), _.Descending)
          With.limit(lit(1))
          Match {
            case c if c.prop[Int]("z") === a.prop[Int]("xyz") =>
              (a.props, c.props)
          }
        }
      },
      "MATCH (`a0`) " +
      "WITH * " +
      "ORDER BY `a0`.`time` DESC " +
      "LIMIT 1 " +
      "MATCH (`c0`) WHERE `c0`.`z` = `a0`.`xyz` " +
      "RETURN `a0`, `c0`"
    ).returns[(Map[String, Any], Map[String, Any])]

    "support no aliasing with" in
    test(
      Match { case (a @ Node("Node")) - Rel("to") > (b @ Node("Node")) =>
        Match {
          case c if c.prop[Int]("z") === a.prop[Int]("xyz") =>
            With(With.preserving(a, b), c.prop[Int]("z")) { z =>
              (a.props, z)
            }
        }
      },
      "MATCH (`a0`:`Node`) -[:`to`]-> (`b0`:`Node`) " +
      "MATCH (`c0`) WHERE `c0`.`z` = `a0`.`xyz` " +
      "WITH `a0`, `b0`, `c0`.`z` AS `z0` " +
      "RETURN `a0`, `z0`"
    ).returns[(Map[String, Any], Int)]

    "not allow to use identifiers excluded by WITH" in pending

    "support WHERE condition at WITH clause" in
    test(
      Match { case a - _ > b =>
        With(a, collect(b)) { (a, bs) =>
          With.where(a.prop[Int]("x") >= lit(10))
          (a.props, bs)
        }
      },
      "MATCH (`a0`) --> (`b0`) " +
      "WITH `a0` AS `a1`, `collect`(`b0`) AS `bs0` " +
      "WHERE `a1`.`x` >= 10 " +
      "RETURN `a1`, `bs0`"
    ).returns[(Map[String, Any], List[GraphElem.Node])]

    "not allow to set WHERE condition more than once at WITH clause" in
    illTyped(
      """Match { case a -_> b =>
           With(a, collect(b)){ (a, bs) =>
           With.where(a.prop[Int]("x") >= lit(10))
           With.where(a.prop[Int]("y") <= lit(0))
             (a.props, bs)
           }}""",
      "Repeating options is not allowed: where\\..*"
    )

    "support DISTINCT at WITH clause" in
    test(
      Match { case a =>
        With(a.prop[String]("foo")) { foo =>
          With.distinct
          Match { case b =>
            b.prop[String]("id") -> foo
          }
        }
      },
      "MATCH (`a0`) " +
      "WITH DISTINCT `a0`.`foo` AS `foo0` " +
      "MATCH (`b0`) " +
      "RETURN `b0`.`id`, `foo0`"
    ).returns[(String, String)]

    "not allow to set DISTINCT more than once at WITH clause" in
    illTyped(
      """Match { case a -_> b =>
           With(a, collect(b)){ (a, bs) =>
           With.distinct
           With.distinct(false)
             (a.props, bs)
           }}""",
      "Repeating options is not allowed: distinct\\..*"
    )

    "support ORDER BY at WITH clause" in
    test(
      Match { case a =>
        With(a.prop[String]("foo")) { foo =>
          With.orderBy(foo)
          Match { case b =>
            b.prop[String]("id") -> foo
          }
        }
      },
      "MATCH (`a0`) " +
      "WITH `a0`.`foo` AS `foo0` " +
      "ORDER BY `foo0` ASC " +
      "MATCH (`b0`) " +
      "RETURN `b0`.`id`, `foo0`"
    ).returns[(String, String)]

    "support multiple ORDER BY at WITH clause" in
    test(
      Match { case a - _ > b =>
        With(a.prop[String]("foo"), collect(b).size) { (foo, n) =>
          With.orderBy(n, _.Descending)
          With.orderBy(foo)
          Match { case b =>
            b.prop[String]("id") -> foo
          }
        }
      },
      "MATCH (`a0`) --> (`b0`) " +
      "WITH `a0`.`foo` AS `foo0`, `size`(`collect`(`b0`)) AS `n0` " +
      "ORDER BY `n0` DESC, `foo0` ASC " +
      "MATCH (`b1`) " +
      "RETURN `b1`.`id`, `foo0`"
    ).returns[(String, String)]

    "support LIMIT at WITH clause" in
    test(
      Match { case a - _ > b =>
        With(a.prop[String]("foo"), collect(b)) { (foo, bs) =>
          With.limit(lit(10))
          bs
        }
      },
      "MATCH (`a0`) --> (`b0`) " +
      "WITH `a0`.`foo` AS `foo0`, `collect`(`b0`) AS `bs0` " +
      "LIMIT 10 " +
      "RETURN `bs0`"
    ).returns[List[GraphElem.Node]]

    "not allow to set LIMIT more than once at WITH clause" in
    illTyped(
      """Match { case a -_> b =>
           With(a, collect(b)){ (a, bs) =>
           With.limit(lit(10))
           With.limit(lit(20))
             (a.props, bs)
           }}""",
      "Repeating options is not allowed: limit\\..*"
    )

    "support SKIP at WITH clause" in
    test(
      Match { case a - _ > b =>
        With(a.prop[String]("foo"), collect(b)) { (foo, bs) =>
          With.skip(lit(10))
          bs
        }
      },
      "MATCH (`a0`) --> (`b0`) " +
      "WITH `a0`.`foo` AS `foo0`, `collect`(`b0`) AS `bs0` " +
      "SKIP 10 " +
      "RETURN `bs0`"
    ).returns[List[GraphElem.Node]]

    "not allow to set SKIP more than once at WITH clause" in
    illTyped(
      """Match { case a -_> b =>
           With(a, collect(b)){ (a, bs) =>
           With.skip(lit(10))
           With.skip(lit(20))
             (a.props, bs)
           }}""",
      "Repeating options is not allowed: skip\\..*"
    )

    "support SKIP AND LIMIT at WITH clause" in
    test(
      Match { case a - _ > b =>
        With(a.prop[String]("foo"), collect(b)) { (foo, bs) =>
          With.skip(lit(100))
          With.limit(lit(10))
          bs
        }
      },
      "MATCH (`a0`) --> (`b0`) " +
      "WITH `a0`.`foo` AS `foo0`, `collect`(`b0`) AS `bs0` " +
      "SKIP 100 " +
      "LIMIT 10 " +
      "RETURN `bs0`"
    ).returns[List[GraphElem.Node]]

    "support WHERE, DISTINCT, ORDER BYs, SKIP AND LIMIT at WITH clause" in
    test(
      Match { case a - _ > b =>
        With(a.prop[String]("foo"), collect(b)) { (foo, bs) =>
          With.where(foo startsWith lit("xyz"))
          With.orderBy(bs.size, _.Descending)
          With.orderBy(foo)
          With.distinct
          With.skip(lit(5))
          With.limit(lit(20))
          bs
        }
      },
      "MATCH (`a0`) --> (`b0`) " +
      "WITH DISTINCT `a0`.`foo` AS `foo0`, `collect`(`b0`) AS `bs0` " +
      "ORDER BY `size`(`bs0`) DESC, `foo0` ASC " +
      "SKIP 5 " +
      "LIMIT 20 " +
      "WHERE `foo0` STARTS WITH \"xyz\" " +
      "RETURN `bs0`"
    ).returns[List[GraphElem.Node]]

    "support WHERE, DISTINCT, ORDER BYs, SKIP AND LIMIT at wildcard WITH clause" in
    test(
      Match { case a - _ > b =>
        With(**, a.prop[String]("foo")) { foo =>
          With.where(foo startsWith lit("xyz"))
          With.orderBy(b.prop[String]("bar"), _.Descending)
          With.orderBy(foo)
          With.distinct
          With.skip(lit(5))
          With.limit(lit(20))
          (a.props, b.props)
        }
      },
      "MATCH (`a0`) --> (`b0`) " +
      "WITH DISTINCT *, `a0`.`foo` AS `foo0` " +
      "ORDER BY `b0`.`bar` DESC, `foo0` ASC " +
      "SKIP 5 " +
      "LIMIT 20 " +
      "WHERE `foo0` STARTS WITH \"xyz\" " +
      "RETURN `a0`, `b0`"
    ).returns[(Map[String, Any], Map[String, Any])]

    "require all WITH modifiers to be defined on top of WITH block" in
    illTyped(
      """With(lit(10)) { n =>
           val x: com.arkondata.slothql.cypher.CypherFragment.Expr[Int] = lit(100)
           With.where(lit(true))
           Match { case a =>
             (a.props, n, x)
           }}""",
      "All WITH modifiers must be defined at the beginning of the block"
    )

    "support UNWIND clauses" in
    test(
      Match { case a =>
        Unwind(a.prop[List[Int]]("xs")) { x =>
          Match {
            case `a` - e > b if e.prop[Int]("x") === x =>
              b.prop[String]("id")
          }
        }
      },
      "MATCH (`a0`) " +
      "UNWIND `a0`.`xs` AS `x0` " +
      "MATCH (`a0`) -[`e0`]-> (`b0`) " +
      "WHERE `e0`.`x` = `x0` " +
      "RETURN `b0`.`id`"
    ).returns[String]

    "support distincting results" in
    test(
      Match { case a => a.prop[String]("id").distinct },
      "MATCH (`a0`) RETURN DISTINCT `a0`.`id`"
    ).returns[String]

    "not allow to distinct results twice" in
    illTyped(
      """Match { case a => a.prop[String]("id").distinct.distinct }""",
      ".*Distinct has already been configured"
    )

    "support ordering results" in
    test(
      Match { case a =>
        a.prop[String]("id")
          .orderBy(a.prop[Int]("x"))
      },
      "MATCH (`a0`) RETURN `a0`.`id` ORDER BY `a0`.`x` ASC"
    ).returns[String]

    "support ordering results multiple times" in
    test(
      Match { case a =>
        a.prop[String]("id")
          .orderBy(a.prop[Int]("x"), _.Descending)
          .orderBy(a.prop[String]("foo"))
      },
      "MATCH (`a0`) RETURN `a0`.`id` ORDER BY `a0`.`x` DESC, `a0`.`foo` ASC"
    ).returns[String]

    "support limiting results" in
    test(
      Match { case a => a.prop[String]("id").limit(lit(10)) },
      "MATCH (`a0`) RETURN `a0`.`id` LIMIT 10"
    ).returns[String]

    "not allow to limit results twice" in
    illTyped(
      """Match { case a => a.prop[String]("id").limit(lit(10)).limit(lit(20)) }""",
      ".*Limit has already been configured"
    )

    "support skipping results" in
    test(
      Match { case a => a.prop[String]("id").skip(lit(10)) },
      "MATCH (`a0`) RETURN `a0`.`id` SKIP 10"
    ).returns[String]

    "not allow to skip results twice" in
    illTyped(
      """Match { case a => a.prop[String]("id").skip(lit(10)).skip(lit(20)) }""",
      ".*Skip has already been configured"
    )

    "support distincting, ordering and paginating results" in
    test(
      Match { case a =>
        a.prop[String]("id")
          .orderBy(a.prop[Int]("x"), _.Descending)
          .orderBy(a.prop[String]("foo"))
          .distinct
          .skip(lit(100))
          .limit(lit(10))
      },
      "MATCH (`a0`) RETURN DISTINCT `a0`.`id` " +
      "ORDER BY `a0`.`x` DESC, `a0`.`foo` ASC " +
      "SKIP 100 " +
      "LIMIT 10"
    ).returns[String]

    "support query unions" in test(
      Match { case n @ Node("Foo") => n } union Match { case n @ Node("Bar") => n },
      "MATCH (`n0`:`Foo`) RETURN `n0` " +
      "UNION " +
      "MATCH (`n1`:`Bar`) RETURN `n1`"
    ).returns[GraphElem.Node]
  }
}
