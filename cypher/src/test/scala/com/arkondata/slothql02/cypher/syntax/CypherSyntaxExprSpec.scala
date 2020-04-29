package com.arkondata.slothql02.cypher.syntax

/** Variety of [[com.arkondata.slothql02.cypher.CypherFragment.Expr]].
 *  - Input
 *  - Param
 *  - Lit
 *  - Null
 *  - ? Var
 *  - Func
 *    + built-in
 *  - MapExpr: MapDef, MapKey, MapDynKey, MapAdd
 *  - ListExpr: ListDef, InList, AtIndex, AtRange, Concat, Reduce, ListComprehension, ListPredicate (All/Any/None/Single)
 *  - StringExpr (StartsWith/EndsWith/Contains/Regex)
 *  - LogicExpr: Negate, Or, And, Xor
 *  - CompareExpr: IsNull, NotNull, Eq, Neq, Lt, Lte, Gte, Gt
 *  - MathematicalExpr: {{{unary_-}}}, {{{+}}}, {{{-}}}, {{{*}}}, {{{/}}}, {{{%}}}, {{{^}}}
 *  - Distinct
 *  - Exists
 *  - CaseExpr
 *    - SimpleCaseExpr
 *    - GenericCaseExpr
 */
class CypherSyntaxExprSpec extends CypherSyntaxBaseSpec {

  "Slothql cypher syntax for lists" should {
    "support defining lists" in
      test(
        Match { case a => list(a.prop[Int]("x"), a.prop[Int]("y"))},
        "MATCH (`a0`) RETURN [`a0`.`x`, `a0`.`y`]"
      ).returns[List[Int]]

    "support testing whether a value is in a list (IN predicate)" in
      test(
        Match { case a => lit("foo") in collect(a.prop[String]("bar")) },
        "MATCH (`a0`) RETURN \"foo\" IN `collect`(`a0`.`bar`)"
      ).returns[Boolean]

    "support selecting list element by index" in
      test(
        Match { case a -e> b => b.prop[List[String]]("data").at(e.prop[Int]("idx")) },
        "MATCH (`a0`) -[`e0`]-> (`b0`) RETURN `b0`.`data`[`e0`.`idx`]"
      ).returns[String]

    "support selecting list sub-range (open right)" in
      test(
        Match { case _ -e> b => b.prop[List[String]]("data").from(e.prop[Int]("from")) },
        "MATCH () -[`e0`]-> (`b0`) RETURN `b0`.`data`[`e0`.`from`..]"
      ).returns[List[String]]

    "support selecting list sub-range (open left)" in
      test(
        Match { case _ -e> b => b.prop[List[String]]("data").to(e.prop[Int]("to")) },
        "MATCH () -[`e0`]-> (`b0`) RETURN `b0`.`data`[..`e0`.`to`]"
      ).returns[List[String]]

    "support selecting list sub-range (closed)" in
      test(
        Match { case a -e> b => b.prop[List[String]]("data").slice(a.prop[Int]("from"), e.prop[Int]("to")) },
        "MATCH (`a0`) -[`e0`]-> (`b0`) RETURN `b0`.`data`[`a0`.`from`..`e0`.`to`]"
      ).returns[List[String]]

    "support concatenating lists" in
      test(
        Match { case a => a.prop[List[String]]("foo") ++ a.prop[List[String]]("bar") },
        "MATCH (`a0`) RETURN `a0`.`foo` + `a0`.`bar`"
      ).returns[List[String]]

    "support selecting list head" in
      test(
        Match { case a => a.prop[List[String]]("foo").head },
        "MATCH (`a0`) RETURN `head`(`a0`.`foo`)"
      ).returns[String]

    "support selecting list tail" in
      test(
        Match { case a => a.prop[List[String]]("foo").tail },
        "MATCH (`a0`) RETURN `tail`(`a0`.`foo`)"
      ).returns[List[String]]

    "support getting list size" in
      test(
        Match { case a => a.prop[List[String]]("foo").size },
        "MATCH (`a0`) RETURN `size`(`a0`.`foo`)"
      ).returns[Long]

    "support list comprehensions (filter only)" in
      test(
        Match { case a => a.prop[List[String]]("foo").filter(_ startsWith lit("abc")) },
        "MATCH (`a0`) RETURN [`#0` IN `a0`.`foo` WHERE `#0` STARTS WITH \"abc\"]"
      ).returns[List[String]]

    "support list comprehensions (map only)" in
      test(
        Match { case a => a.prop[List[Int]]("foo").map(_ * lit(7)) },
        "MATCH (`a0`) RETURN [`#0` IN `a0`.`foo` | `#0` * 7]"
      ).returns[List[Int]]

    "support list comprehensions (filter + map)" in
      test(
        Match { case a => a.prop[List[Int]]("foo").withFilter(_ > lit(0)).map(lit(2) ^ _) },
        "MATCH (`a0`) RETURN [`#0` IN `a0`.`foo` WHERE `#0` > 0 | 2 ^ `#0`]"
      ).returns[List[Int]]

    "support nested list comprehensions" in
      test(
        Match { case a =>
          a.prop[List[Int]]("foo")
            .withFilter(_ > lit(0))
            .map{ x =>
              val l = list[Int](a.prop("x1"), a.prop("x2"), a.prop("x3"))
              l.map(y => x ^ y)
            }
        },
        "MATCH (`a0`) RETURN [`#0` IN `a0`.`foo` WHERE `#0` > 0 | [`#1` IN [`a0`.`x1`, `a0`.`x2`, `a0`.`x3`] | `#0` ^ `#1`]]"
      ).returns[List[List[Int]]]

    "support reducing lists" in
      test(
        // TODO: using alias `a` instead of `b` in reduction function breaks query compilation
        Match { case a => a.prop[List[Int]]("foo").reduce(lit(0)) { (acc, b) => acc / lit(2) + b * lit(2) } },
        "MATCH (`a0`) RETURN reduce(`#0` = 0, `#1` IN `a0`.`foo` | `#0` / 2 + `#1` * 2)"
      ).returns[Int]

    "support list predicates (all, any, none, single)" in
      test(
        Match { case a =>
          val foo = a.prop[List[Int]]("foo")
          (
            foo.all(_ >= lit(0)),
            foo.any(_ <> lit(0)),
            foo.none(_ === lit(0)),
            foo.single(_ % lit(2) === a.prop[Int]("bar"))
          )
        },
        "MATCH (`a0`) RETURN " +
          "all(`#3` IN `a0`.`foo` WHERE `#3` >= 0), " +
          "any(`#2` IN `a0`.`foo` WHERE `#2` <> 0), " +
          "none(`#1` IN `a0`.`foo` WHERE `#1` = 0), " +
          "single(`#0` IN `a0`.`foo` WHERE `#0` % 2 = `a0`.`bar`)"
      ).returns[(Boolean, Boolean, Boolean, Boolean)]

  }

  "Slothql cypher syntax for maps" should {
    "support defining maps (by entry)" in
      test(
        Match { case a => dict("x" -> a.prop[Int]("x"), "y" -> a.prop[Int]("y")) },
        "MATCH (`a0`) RETURN { `x`: `a0`.`x`, `y`: `a0`.`y` }"
      ).returns[Map[String, Int]]

    "support defining maps (from Map)" in
      test(
        Match { case a =>
          val m = Map("x" -> a.prop[Int]("x"), "y" -> a.prop[Int]("y"))
          dict(m)
        },
        "MATCH (`a0`) RETURN { `x`: `a0`.`x`, `y`: `a0`.`y` }"
      ).returns[Map[String, Int]]

    "support selecting map key" in
      test(
        Match{ case a => a.prop[Map[String, Boolean]]("flags").value("1") },
        "MATCH (`a0`) RETURN `a0`.`flags`.`1`"
      ).returns[Boolean]

    "support selecting typed map key (from Map[String, Any])" in
      test(
        Match{ case a => a.props.value[Int]("x") * lit(-1) },
        "MATCH (`a0`) RETURN `a0`.`x` * -1"
      ).returns[Int]

    "support selecting dynamic map key" in
      test(
        Match{ case a => a.prop[Map[String, Int]]("vals").value(a.prop[String]("key")) },
        "MATCH (`a0`) RETURN `a0`.`vals`[`a0`.`key`]"
      ).returns[Int]

    "support selecting dynamic typed map key (from Map[String, Any])" in
      test(
        Match{ case a => a.props.value[Long](a.prop[String]("key")) },
        "MATCH (`a0`) RETURN `a0`[`a0`.`key`]"
      ).returns[Long]

    "support adding entries to maps" in
      test(
        Match{ case a => a.props.add("foo" -> lit(1)) },
        "MATCH (`a0`) RETURN `a0`{.*, `foo`: 1}"
      ).returns[Map[String, Any]]

    "support adding entries maps (from Map)" in
      test(
        Match{ case a =>
          val m = Map(
            "foo" -> lit(1),
            "bar" -> lit(2)
          )
          a.prop[Map[String, Any]]("data") add m
        },
        "MATCH (`a0`) RETURN `a0`.`data`{.*, `foo`: 1, `bar`: 2}"
      ).returns[Map[String, Any]]

    "support listing map keys" in
      test(
        Match{ case a => a.keys },
        "MATCH (`a0`) RETURN `keys`(`a0`)"
      ).returns[List[String]]
  }

}
