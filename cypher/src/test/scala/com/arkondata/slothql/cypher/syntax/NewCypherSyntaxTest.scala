package com.arkondata.slothql.cypher.syntax

import org.scalatest.Ignore

import com.arkondata.slothql.cypher.CypherFragment

/* TODO: restructure the spec

[M] CypherSyntaxMainSpec
[R] CypherSyntaxReadSpec
[E] CypherSyntaxExprSpec
[P] CypherSyntaxPatternSpec
[W] CypherSyntaxWriteSpec
[C] CypherSyntaxCallSpec

  - Clause
    - Read
      - Match  [M]
      - With   [R]
      - Unwind [R]
    - Write
      - Create   [W]
      - Delete   [W]
      - SetProps [W]
    - ReadWrite
      - Call [C]
  - Pattern
    - Let + Path [R]
    - long paths [P]
    - Node [P]
    - Rel  [P]
  - Expr [E]
    - Input
    - Param
    - Lit
    - Null
    - ? Var
    - Func
      + built-in
    - MapExpr: MapDef, MapKey, MapDynKey, MapAdd
    - ListExpr: ListDef, InList, AtIndex, AtRange, Concat, Reduce, ListComprehension, ListPredicate (All/Any/None/Single)
    - StringExpr (StartsWith/EndsWith/Contains/Regex)
    - LogicExpr: Negate, Or, And, Xor
    - CompareExpr: IsNull, NotNull, Eq, Neq, Lt, Lte, Gte, Gt
    - MathematicalExpr: unary_-, +, -, *, /, %, ^
    - Distinct
    - Exists
    - CaseExpr
      - SimpleCaseExpr
      - GenericCaseExpr
  - Query
    - Return: final return [M]
    - Clause: nesting [M]
  - Return
    - All [R]
    - Expr: as [M]
    - Tuple [M]
    - Nothing? [W], [R]
    - Options: distinct, orderBy, skip, limit [R]
 */
@Ignore
class NewCypherSyntaxTest extends CypherSyntaxBaseSpec {

  "Slothql cypher syntax" should {
    "build path pattern matching node label" in test(
      Match { case (x @ Node("foo")) < y - z =>
        x: Node
        y: Rel.Aux[Rel.Incoming]
        z: Node
        x.props
      },
      "MATCH (`x`:`foo`) <-[`y`]- (`z`) " +
      "RETURN `x`"
    ).returns[Map[String, Any]]

    "return tuples (1)" in test(
      Match { case x < y - z =>
        // case x < (y - z)
        x: Node
        y: Rel.Aux[Rel.Incoming]
        z: Node
        (x.prop[String]("a"), y.tpe, z.labels)
      },
      "MATCH (`x`) <-[`y`]- (`z`) " +
      "RETURN `x`.`a`, `type`(`y`), `labels`(`z`)"
    ).returns[(String, String, List[String])]

    "return tuples (2)" in test(
      Match { case user < _ - _ < _ - group =>
        (
          user.propOpt[String]("email"),
          user.propOpt[String]("name"),
          user.propOpt[Int]("age"),
          user.propOpt[Boolean]("confirmed"),
          group.prop[String]("name")
        )
      },
      "MATCH (`user`) <-[]- () <-[]- (`group`) " +
      "RETURN `user`.`email`, `user`.`name`, `user`.`age`, `user`.`confirmed`, `group`.`name`"
    ).returns[(Option[String], Option[String], Option[Int], Option[Boolean], String)]

    "support built-in operators" in test(
      Match { case x - y > z =>
        // case (x - y) > z =>
        x: Node
        y: Rel.Aux[Rel.Outgoing]
        z: Node
        x.id + x.prop[Long]("foo") > y.prop[Long]("bar")
      },
      "MATCH (`x`) -[`y`]-> (`z`) " +
      "RETURN `id`(`x`) + `x`.`foo` > `y`.`bar`"
    ).returns[Boolean]

    "support returning named expressions" in test(
      Match { case a - b > c < d - e =>
        // case ((a - b) > c) < (d - e) =>
        a: Node
        b: Rel.Aux[Rel.Outgoing]
        c: Node
        d: Rel.Aux[Rel.Incoming]
        e: Node
        a.props -> c.labels.as("qwerty")
      },
      "MATCH (`a`) -[`b`]-> (`c`) <-[`d`]- (`e`) " +
      "RETURN `a`, `labels`(`c`) AS `qwerty`"
    ).returns[(Map[String, Any], List[String])]

    "match vertex labels and properties, allow non-literal property values (1)" in pending /*{
      val id = "u1"
      test(
        Match {
          case (u@Node("User", "id" := `id`)) < _ - Node("Members") < _ - group =>
            (
              u.prop[String]("email"),
              u.prop[String]("name"),
              u.prop[Int]("age"),
              u.prop[Boolean]("confirmed"),
              group.prop[String]("name")
            )
        },
        "MATCH (`u`:`User`{ `id`: \"u1\" }) <-[]- (:`Members`) <-[]- (`group`) " +
        "RETURN `u`.`email`, `u`.`name`, `u`.`age`, `u`.`confirmed`, `group`.`name`"
      ).returns[(String, String, Int, Boolean, String)]
    }*/

    "match vertex labels and properties, allow non-literal property values (2)" in pending /*{
      val email = "user@example.com"
      test(
        Match {
          case Node("User", "email" := `email`) -(e)> (g@Node("Group")) =>
            e.props -> g.prop[String]("id")
        },
        "MATCH (:`User`{ `email`: \"user@example.com\" }) -[`e`]-> (`g`:`Group`) " +
        "RETURN `e`, `g`.`id`"
      ).returns[(Map[String, Any], String)]
    }*/

    "match relation types, support variable length paths (left direction)" in pending /*test(
      Match {
        case Node("Group") < _ *:(0 - _, Rel("parent")) - (g@Node("Group")) =>
          g.prop[String]("name")
      },
      "MATCH (:`Group`) <-[:`parent`*0..]- (`g`:`Group`) " +
      "RETURN `g`.`name`"
    ).returns[String]*/

    "match relation types, support variable length paths (right direction)" in pending /*test(
      Match {
        case Node("Group") - es *:(_, _) > (g@Node("Group")) =>
          es.map(_.props) -> g.prop[String]("name")
      },
      "MATCH (:`Group`) -[`es`*]-> (`g`:`Group`) " +
      "RETURN `es`, `g`.`name`"
    ).returns[(List[Map[String, Any]], String)]*/

    "assign paths to variables" in pending /*test(
      Match {
        case ps ::= (Node("Group") - _ *:(0 - _, Rel("foo", "bar")) > (g@Node("Group"))) =>
          (
            ps.nodes.map(_.props),
            ps.edges.map(_.props),
            ps.length
          )
      },
      "MATCH `ps` = (:`Group`) -[:`foo`|`bar`*0..]-> (`g`:`Group`) " +
      "RETURN " +
        "`nodes`(`ps`), " +
        "`relationships`(`ps`), " +
        "`length`(`ps`)"
    ).returns[(List[Map[String, Any]], List[Map[String, Any]], Long)]*/

    "assign paths to variables (2)" in pending /*test(
      Match {
        case ps ::= (Node("Group") - e > v - _ *:(0 - _, Rel("foo", "bar")) > (g@Node("Group"))) =>
          (
            ps.nodes.map(_.props),
            ps.edges.map(_.props),
            ps.length,
            e.tpe,
            v.labels
          )
      },
      "MATCH `ps` = (:`Group`) -[`e`]-> (`v`) -[:`foo`|`bar`*0..]-> (`g`:`Group`) " +
      "RETURN " +
        "`nodes`(`ps`), " +
        "`relationships`(`ps`), " +
        "`length`(`ps`), " +
        "`type`(`e`), " +
        "`labels`(`v`)"
    ).returns[(List[Map[String, Any]], List[Map[String, Any]], Long, String, List[String])]*/

    "support `with` expression" in pending /*test(
      Match {
        case x - _ > y =>
          `with`(_.orderBy(x.prop("name")), x, y.prop("data") as "data", "something else", Seq("more!")) {
            (x.props, CypherFragment.Expr.Var[String]("data"))
          }
      },
      "MATCH (`x`) -[]-> (`y`) " +
      "WITH `x`, `y`.`data` AS `data`, `something else`, `more!` " +
        "ORDER BY `x`.`name` " +
      "RETURN `x`, `data`"
    ).returns[(Map[String, Any], String)]*/

    "not allow to use unbound variables in `with` clause [experimental]" in pending
    /*
      shapeless.test.illTyped(
        """
          Match { case x - _ > y =>
            `with`(x) {
              (x, y)
            }
          }
        """,
        "Variable unbound by `with`: value y"
      )
     */

    "stored procedures calls (string name)" in pending /*test(
      "apoc.nodes.get".call(lit(0)).yielding { node: Node =>
        node.props
      },
      "CALL `apoc`.`nodes`.`get`(0) YIELD `node` " +
      "RETURN `node`"
    ).returns[Map[String, Any]]*/

    "stored procedures calls (symbol name)" in pending /*test(
      'foo.call(lit("bar")).yielding { i: Var[Int] =>
        i + 1
      },
      "CALL `foo`(\"bar\") YIELD `i` " +
      "RETURN `i` + 1"
    ).returns[Int]*/

    "stored procedures calls (rename outputs)" in pending /*test(
      'foo.call(lit("bar")).yieldingAs('value) { i: Var[Int] =>
        i + 1
      },
      "CALL `foo`(\"bar\") YIELD `value` AS `i` " +
      "RETURN `i` + 1"
    ).returns[Int]*/

    "stored procedures calls (void output)" in pending /*test(
      'foo.call(lit("bar")).void {
        lit("done")
      },
      "CALL `foo`(\"bar\") " +
      "RETURN \"done\""
    ).returns[String]*/

    "build function calls" in pending /*test(
      Match {
        case Node("Group") < _ *:(_, Rel("parent")) - (g@Node("Group")) =>
          (g.func[Long]("id"), g.func[Map[String, Any]]("properties"), "pi".func[Double]())
      },
      "MATCH (:`Group`) <-[:`parent`*]- (`g`:`Group`) " +
      "RETURN `id`(`g`), `properties`(`g`), `pi`()"
    ).returns[(Long, Map[String, Any], Double)]*/

    "provide syntax for common built-in functions" in ??? /*test(
      Match {
        case Node("Group") < (e@Rel("parent")) - (g@Node("Group")) => (
          g.id, g.count, g.keys, g.labels, e.id, e.count, e.keys, e.tpe
        )
      },
      "MATCH (:`Group`) <-[`e`:`parent`]- (`g`:`Group`) " +
      "RETURN `id`(`g`), `count`(`g`), `keys`(`g`), `labels`(`g`), `id`(`e`), `count`(`e`), `keys`(`e`), `type`(`e`)"
    ).returns[(Long, Long, List[String], List[String], Long, Long, List[String], String)]*/

    "allow to select edge list properties (from variable length match)" in pending /*test(
      Match {
        case Node("Group") < es*:(0 - _, Rel("parent")) - (g@Node("Group")) => es.map(_.props)
      },
      "MATCH (:`Group`) <-[`es`:`parent`*0..]- (`g`:`Group`) " +
      "RETURN `es`"
    ).returns[List[Map[String, Any]]]*/

    "support comparison expressions" in test(
      Match { case v < _ - _ =>
        (
          v.id,
          v.id === lit(2),
          v.id === lit("QWERTY"),
          v.id > lit(2L),
          !(v.id > lit(2L)),
          v.id > lit(2L) && v.id <= lit(4L),
          (v.id > lit(2L)) xor (v.id <= lit(4L)),
          v.isNull,
          !(v.isNull || v.id <= lit(2L))
        )
      },
      "MATCH (`v`) <-[]- () " +
      "RETURN " +
      "`id`(`v`), " +
      "`id`(`v`) = 2, " +
      "`id`(`v`) = \"QWERTY\", " +
      "`id`(`v`) > 2, " +
      "NOT `id`(`v`) > 2, " +
      "`id`(`v`) > 2 AND `id`(`v`) <= 4, " +
      "`id`(`v`) > 2 XOR `id`(`v`) <= 4, " +
      "`v` IS NULL, " +
      "NOT (`v` IS NULL OR `id`(`v`) <= 2)"
    ).returns[(Long, Boolean, Boolean, Boolean, Boolean, Boolean, Boolean, Boolean, Boolean)]

    "support string comparison expressions" in test(
      Match { case v =>
        val name = v.prop[String]("name")
        (
          name contains lit("foo"),
          name startsWith lit("bar"),
          name endsWith lit("baz"),
          name matches lit("foo.*bar")
        )
      },
      "MATCH (`v`) " +
      "RETURN " +
      "`v`.`name` CONTAINS \"foo\", " +
      "`v`.`name` STARTS WITH \"bar\", " +
      "`v`.`name` ENDS WITH \"baz\", " +
      "`v`.`name` =~ \"foo.*bar\""
    ).returns[(Boolean, Boolean, Boolean, Boolean)]

    "support cypher's `toString` built-in function for numbers, booleans and strings" in test(
      Match { case v =>
        (
          v.prop[Long]("long").asString,
          v.prop[Float]("float").asString,
          v.prop[String]("string").asString,
          v.prop[Boolean]("boolean").asString
        )
      },
      "MATCH (`v`) " +
      "RETURN " +
      "`toString`(`v`.`long`), " +
      "`toString`(`v`.`float`), " +
      "`toString`(`v`.`string`), " +
      "`toString`(`v`.`boolean`)"
    ).returns[(String, String, String, String)]

    "not allow to use cypher's `toString` built-in function for other types" in ???
//      shapeless.test.illTyped(
//        """Match { case v => v.prop[List[Int]]("data").asString }""",
//        """value asString is not a member of com\.arkondata\.slothql\.cypher\.CypherFragment\.Expr\.MapKey\[List\[Int\]\]"""
//      )

    "support built-in functions for strings" in test(
      Match { case v =>
        val name = v.prop[String]("name")
        (
          name.toLower,
          name.toUpper,
          name.size,
          name.toBoolean,
          name.toDouble,
          name.toLong,
          name takeLeft lit(4L),
          name takeRight lit(5L),
          name.replace(lit("foo"), lit("bar")),
          name.reverse,
          name split lit("."),
          name substring lit(2L),
          name.substring(lit(2L), lit(10L)),
          name.trim,
          name.trimRight,
          name.trimLeft
        )
      },
      "MATCH (`v`) " +
      "RETURN " +
      "`toLower`(`v`.`name`), " +
      "`toUpper`(`v`.`name`), " +
      "`size`(`v`.`name`), " +
      "`toBoolean`(`v`.`name`), " +
      "`toFloat`(`v`.`name`), " +
      "`toInteger`(`v`.`name`), " +
      "`left`(`v`.`name`, 4), " +
      "`right`(`v`.`name`, 5), " +
      "`replace`(`v`.`name`, \"foo\", \"bar\"), " +
      "`reverse`(`v`.`name`), " +
      "`split`(`v`.`name`, \".\"), " +
      "`substring`(`v`.`name`, 2), " +
      "`substring`(`v`.`name`, 2, 10), " +
      "`trim`(`v`.`name`), " +
      "`rTrim`(`v`.`name`), " +
      "`lTrim`(`v`.`name`)"
    ).returns[
      (
        String,
        String,
        Long,
        Boolean,
        Double,
        Long,
        String,
        String,
        String,
        String,
        List[String],
        String,
        String,
        String,
        String,
        String
      )
    ]

    "support mathematical operators" in test(
      Match { case v =>
        val i = v.prop[Long]("i")
        val j = v.prop[Long]("j")
        (
          i + j,
          i - j,
          i * j,
          i / j,
          i % j,
          i ^ j,
          -i
        )
      },
      "MATCH (`v`) " +
      "RETURN " +
      "`v`.`i` + `v`.`j`, " +
      "`v`.`i` - `v`.`j`, " +
      "`v`.`i` * `v`.`j`, " +
      "`v`.`i` / `v`.`j`, " +
      "`v`.`i` % `v`.`j`, " +
      "`v`.`i` ^ `v`.`j`, " +
      "-`v`.`i`"
    ).returns[(Long, Long, Long, Long, Long, Long, Long)]

    /*
    def supportLists[E <: CypherFragment.Expr[List[String]]: CypherFragment](l: E) = test(
      Match {
        case v < e - _ => (
          list(v.id, e.id),
          l,
          v.labels ++ list(e.tpe),
          v.labels.size,
          e.tpe in l,
          v.labels at lit(0),
          l at (lit(1), lit(2L)),
          l from lit(2),
          l to lit(2L)
        )
      },
      "MATCH (`v`) <-[`e`]- () " +
      "RETURN " +
        "[ `id`(`v`), `id`(`e`) ], " +
        "[ \"Admin\", \"Share\", \"Create\" ], " +
        "`labels`(`v`) + [ `type`(`e`) ], " +
        "`size`(`labels`(`v`)), " +
        "`type`(`e`) IN [ \"Admin\", \"Share\", \"Create\" ], " +
        "`labels`(`v`)[0], " +
        "[ \"Admin\", \"Share\", \"Create\" ][1..2], " +
        "[ \"Admin\", \"Share\", \"Create\" ][2..], " +
        "[ \"Admin\", \"Share\", \"Create\" ][..2]"
    ).returns[(List[Long], List[String], List[String], Long, Boolean, String, List[String], List[String], List[String])]
     */

    "support lists of literals" in pending // supportLists(list("Admin", "Share", "Create"))

    "support literal lists" in pending // supportLists(lit(List("Admin", "Share", "Create")))

    "support `if` guards" in pending /*test(
      Match {
        case v < e - _ if e.tpe in list("Admin", "Share") => v.props
      },
      "MATCH (`v`) <-[`e`]- () " +
      "WHERE `type`(`e`) IN [ \"Admin\", \"Share\" ] " +
      "RETURN `v`"
    ).returns[Map[String, Any]]*/

    "allow returning lists of vertex properties" in pending /*test(
      Match {
        case v1 -> v2 `<-` v3 => list(v1.props, v2.props, v3.props)
      },
      "MATCH (`v1`) -[]-> (`v2`) <-[]- (`v3`) " +
      "RETURN [ `v1`, `v2`, `v3` ]"
    ).returns[List[Map[String, Any]]]*/

    "order results" in pending /*test(
      Match {
        case v@Node("Group") =>
          val name = v.prop[String]("name")
          (v.id, name)
            .orderBy(name, v.id)
      },
      "MATCH (`v`:`Group`) " +
      "RETURN `id`(`v`), `v`.`name` " +
      "ORDER BY `v`.`name`, `id`(`v`)"
    ).returns[(Long, String)]*/

    "allow multiple ordering directives, support descending order" in pending /*test(
      Match {
        case v@Node("Group") =>
          val name = v.prop[String]("name")
          (v.id, name)
            .orderBy(name.desc)
            .orderBy(v.id)
      },
      "MATCH (`v`:`Group`) " +
      "RETURN `id`(`v`), `v`.`name` " +
      "ORDER BY `v`.`name` DESC, `id`(`v`)"
    ).returns[(Long, String)]*/

    "paginate results" in pending /*test(
      Match {
        case v@Node("Group") =>
          val name = v.prop[String]("name")
          (v.id, name)
            .orderBy(name.desc)
            .skip(1)
            .limit(1)
      },
      "MATCH (`v`:`Group`) " +
      "RETURN `id`(`v`), `v`.`name` " +
      "ORDER BY `v`.`name` DESC " +
      "SKIP 1 LIMIT 1"
    ).returns[(Long, String)]*/

    "distinct results" in pending /*test(
      Match {
        case v@Node("Group") =>
          val name = v.prop[String]("name")
          (v.id, name)
            .orderBy(name)
            .distinct
      },
      "MATCH (`v`:`Group`) " +
      "RETURN DISTINCT `id`(`v`), `v`.`name` " +
      "ORDER BY `v`.`name`"
    ).returns[(Long, String)]*/

    "match vertex properties optionally (1)" in pending /*{
      val id = Some("g1")
      test(
        Match {
          case g@Node("Group", "id" :?= `id`) => g.props
        },
        "MATCH (`g`:`Group`{ `id`: \"g1\" }) " +
        "RETURN `g`"
      ).returns[Map[String, Any]]
    }*/

    "match vertex properties optionally (2)" in pending /*{
      val id = Option.empty[String]
      test(
        Match {
          case g@Node("Group", "id" :?= `id`) => g.props
        },
        "MATCH (`g`:`Group`) " +
        "RETURN `g`"
      ).returns[Map[String, Any]]
    }*/

    "support nested matches (1)" in pending /*test(
      Match {/*
        case u@Node("User") =>
          Match {
            case (g@Node("Group")) -_> Node("Members") -Rel("Admin")> u2 if u === u2 =>
              u.prop[String]("email") -> g.prop[String]("name")
          }
      },
      "MATCH (`u`:`User`) " +
      "MATCH (`g`:`Group`) -[]-> (:`Members`) -[:`Admin`]-> (`u2`) " +
      "WHERE `u` = `u2` " +
      "RETURN `u`.`email`, `g`.`name`"
    ).returns[(String, String)]*/*/

    "support nested matches (2)" in ??? /*test(
      Match {
        case u@Node("User") =>
          Match {
            case (g@Node("Group")) -_> Node("Members") -Rel("Admin")> Node(`u`) =>
              u.prop[String]("email") -> g.prop[String]("name")
          }
      },
      "MATCH (`u`:`User`) " +
      "MATCH (`g`:`Group`) -[]-> (:`Members`) -[:`Admin`]-> (`u`) " +
      "RETURN `u`.`email`, `g`.`name`"
    ).returns[(String, String)]*/

    "support nested matches (separated, vertex)" in ??? /*{
      def inner(u: Node) = Match {
        case (g@Node("Group")) -_> Node("Members") -Rel("Admin")> Node(`u`) =>
          u.prop[String]("email") -> g.prop[String]("name")
      }
      test(
        Match {
          case user@Node("User") => inner(user)
        },
        "MATCH (`user`:`User`) " +
        "MATCH (`g`:`Group`) -[]-> (:`Members`) -[:`Admin`]-> (`user`) " +
        "RETURN `user`.`email`, `g`.`name`"
      ).returns[(String, String)]
    }
     */
    "support nested matches (separated, edge)" in ??? /*{
      def inner(e0: Rel) = Match {
        case (foo@Node("Foo")) -Rel(`e0`)> Node("Bar") =>
          foo.prop[String]("name")
      }
      test(
        Match {
          case Node("User") -e> _ => inner(e)
        },
        "MATCH (:`User`) -[`e`]-> () " +
        "MATCH (`foo`:`Foo`) -[`e`]-> (:`Bar`) " +
        "RETURN `foo`.`name`"
      ).returns[String]
    }
     */
    "support optional matches" in pending /*test(
      Match {
        case u@Node("User") =>
          Match.optional {
            case (g@Node("Group")) -> Node("Members") -Rel("FooBar")> u2 if u === u2 =>
              u.prop[String]("email") -> g.prop[String]("name")
          }
      },
      "MATCH (`u`:`User`) " +
      "OPTIONAL MATCH (`g`:`Group`) -[]-> (:`Members`) -[:`FooBar`]-> (`u2`) " +
      "WHERE `u` = `u2` " +
      "RETURN `u`.`email`, `g`.`name`"
    ).returns[(String, String)]*/

    "support maps/dictionaries" in pending /*test(
      Match {
        case u@Node("User") => dict(
          "id" -> u.prop[String]("id"),
          "personal" -> dict(
            "name" -> u.prop[String]("name"),
            "age"  -> u.prop[Int]("age")
          ),
          "account" -> dict(
            "email"     -> u.prop[String]("email"),
            "confirmed" -> u.prop[Boolean]("confirmed")
          )
        )
      },
      "MATCH (`u`:`User`) " +
      "RETURN { " +
        "`id`: `u`.`id`, " +
        "`personal`: { `name`: `u`.`name`, `age`: `u`.`age` }, " +
        "`account`: { `email`: `u`.`email`, `confirmed`: `u`.`confirmed` } " +
        "}"
    ).returns[Map[String, Any]]
     */
    "collect results to lists" in pending /*test(
      Match {
        case u@Node("User") =>
          // TODO: syntax: reuse `u` in second pattern =================================================================
          Match.optional {
            case (u0@Node("User")) <(role)- Node("Members") `<-` (g@Node("Group")) if u === u0 =>
              dict(
                "user" -> u.prop("email"),
                "groups" -> collect(dict(
                  "id" -> g.prop("id"),
                  "role" -> role.tpe
                ))
              )
          }
      },
      "MATCH (`u`:`User`) " +
      "OPTIONAL MATCH (`u0`:`User`) <-[`role`]- (:`Members`) <-[]- (`g`:`Group`) " +
      "WHERE `u` = `u0` " +
      "RETURN { " +
        "`user`: `u`.`email`, " +
        "`groups`: `collect`({ `id`: `g`.`id`, `role`: `type`(`role`) }) " +
        "}"
    ).returns[Map[String, Any]]*/

    "reduce lists" in pending /*{
      val reduceExpr@CypherFragment.Expr.ReduceList(_, elem, _, acc, _) = testVar[List[String]]()
        .reduce(0L)((name, acc) => acc + name.size )
      test(
        Match { case v => reduceExpr.copy(list = v.prop[List[String]]("names")) },
         "MATCH (`v`) " +
        s"RETURN reduce(`$acc` = 0, `$elem` IN `v`.`names` | `$acc` + `size`(`$elem`))"
      ).returns[Long]
    }*/

    "support list comprehensions (filter)" in pending /*{
      val expr@CypherFragment.Expr.ListComprehension(_, elem, _, _) = testVar[List[Long]]()
        .filter(_ % lit(2L) === lit(0))
      test(
        Match { case v => expr.copy(list = v.prop[List[Long]]("data")) },
         "MATCH (`v`) " +
        s"RETURN [`$elem` IN `v`.`data` WHERE `$elem` % 2 = 0]"
      ).returns[List[Long]]
    }*/

    "support list comprehensions (map)" in pending /*{
      val expr@CypherFragment.Expr.ListComprehension(_, elem, _, _) = testVar[List[Long]]()
        .map(_ * lit(10L))
      test(
        Match { case v => expr.copy(list = v.prop[List[Long]]("data")) },
         "MATCH (`v`) " +
        s"RETURN [`$elem` IN `v`.`data` | `$elem` * 10]"
      ).returns[List[Long]]
    }*/

    "support list comprehensions (filter + map)" in pending /*{
      val expr@CypherFragment.Expr.ListComprehension(_, elem, _, _) = testVar[List[Long]]()
        .withFilter(x => x > lit(0L) && x < lit(1000L))
        .map(_.asString)
      test(
        Match { case v => expr.copy(list = v.prop[List[Long]]("data")) },
         "MATCH (`v`) " +
        s"RETURN [`$elem` IN `v`.`data` WHERE `$elem` > 0 AND `$elem` < 1000 | `toString`(`$elem`)]"
      ).returns[List[String]]
    }*/

    "[manual fragment] avoid list comprehension if neither filter nor map is defined" in test(
      Match { case v =>
        CypherFragment.Expr.ListComprehension[Long, Long](
          v.prop[List[Long]]("data"),
          filter = None,
          map    = None
        )
      },
      "MATCH (`v`) " +
      "RETURN `v`.`data`"
    ).returns[List[Long]]

    "support list predicates" in ??? /*{
      val all@CypherFragment.Expr.ListPredicate(_, elem1, _, _) = testVar[List[Long]]()
        .all(_ > lit(0L))
      val any@CypherFragment.Expr.ListPredicate(_, elem2, _, _) = testVar[List[Long]]()
        .any(_ > lit(0L))
      val none@CypherFragment.Expr.ListPredicate(_, elem4, _, _) = testVar[List[Long]]()
        .none(_ > lit(0L))
      val single@CypherFragment.Expr.ListPredicate(_, elem5, _, _) = testVar[List[Long]]()
        .single(_ > lit(0L))
      test(
        Match { case v =>
          (
            all   .copy(list = v.prop[List[Long]]("data")),
            any   .copy(list = v.prop[List[Long]]("data")),
            none  .copy(list = v.prop[List[Long]]("data")),
            single.copy(list = v.prop[List[Long]]("data"))
          )
        },
        "MATCH (`v`) " +
        "RETURN " +
          s"all(`$elem1` IN `v`.`data` WHERE `$elem1` > 0), " +
          s"any(`$elem2` IN `v`.`data` WHERE `$elem2` > 0), " +
          s"none(`$elem4` IN `v`.`data` WHERE `$elem4` > 0), " +
          s"single(`$elem5` IN `v`.`data` WHERE `$elem5` > 0)"
      ).returns[(Boolean, Boolean, Boolean, Boolean)]
    }
     */
    "slice collected lists" in pending /*test(
      Match {
        case u@Node("User") =>
        withWildcard(_.orderBy(u.prop[String]("name")).limit(5)) {
            // TODO: syntax: reuse `u` in second pattern ===================================================================
            Match.optional {
              case (u0@Node("User")) <(role)- Node("Members") `<-` (g@Node("Group")) if u === u0 =>
                dict(
                  "user" -> u.prop("email"),
                  "groups" -> collect(dict(
                    "id" -> g.prop("id"),
                    "role" -> role.tpe
                  )).to(lit(1))
                )
            }
          }
      },
      "MATCH (`u`:`User`) " +
      "WITH * ORDER BY `u`.`name` LIMIT 5 " +
      "OPTIONAL MATCH (`u0`:`User`) <-[`role`]- (:`Members`) <-[]- (`g`:`Group`) " +
      "WHERE `u` = `u0` " +
      "RETURN { " +
        "`user`: `u`.`email`, " +
        "`groups`: `collect`({ `id`: `g`.`id`, `role`: `type`(`role`) })[..1] " +
        "}"
    ).returns[Map[String, Any]]*/

    "allow to use non-literal labels in matches, support non-literal property names" in {
      val label = "User"
      val prop  = "name"
      test(
        Match { case v @ Node(`label`) =>
          v.prop[Any](prop)
        },
        "MATCH (`v`:`User`) " +
        "RETURN `v`.`name`"
      ).returns[Any]
    }

    "allow to use non-literal value maps in matches" in ???
    /*
      test(
        Match {
          case v0@Node("Foo") =>
            val values = Map(
              "foo" -> (v0.prop[Int]("n") > lit(0)),
              "bar" -> lit("BAR")
            )
            Match {
              case v@Node(`values`) => v.prop[Any]("something")
            }
        },
        "MATCH (`v0`:`Foo`) " +
        "MATCH (`v`{ `foo`: `v0`.`n` > 0, `bar`: \"BAR\" }) " +
        "RETURN `v`.`something`"
      ).returns[Any]
     */

    "allow to use non-literal label/type iterables in matches" in ??? /*{
      val labels = "User" :: Nil
      val types  = Set("Admin", "Share")
      test(
        Match {
          case (v@Node(`labels`)) - Rel(`types`) > x => v.props -> x.props
        },
        "MATCH (`v`:`User`) -[:`Admin`|`Share`]-> (`x`) " +
        "RETURN `v`, `x`"
      ).returns[(Map[String, Any], Map[String, Any])]
    }
     */
    "allow to use non-literal, known optional conditions in `if` guard [defined]" in pending /*{
      def cond(v: Node): Option[CypherFragment.Known[CypherFragment.Expr[Boolean]]] = Some(v.prop[Int]("age") >= lit(18))
      test(
        Match { case v@Node("User") if cond(v) => v.prop[String]("name") },
        "MATCH (`v`:`User`) " +
        "WHERE `v`.`age` >= 18 " +
        "RETURN `v`.`name`"
      ).returns[String]
    }*/

    "allow to use non-literal, known optional conditions in `if` guard [undefined]" in pending /*{
      def cond(v: Node): Option[CypherFragment.Known[CypherFragment.Expr[Boolean]]] = None
      test(
        Match { case v@Node("User") if cond(v) => v.prop[String]("name") },
        "MATCH (`v`:`User`) " +
        "RETURN `v`.`name`"
      ).returns[String]
    }*/

    "raise compilation error when unknown optional conditions are used in `if` guard" in pending /*{
      shapeless.test.illTyped(
        """
          def cond(v: Node): Option[CypherFragment.Expr[Boolean]] = ???
          Match { case v if cond(v) => v.prop[String]("name") }
        """,
        """Cannot use unknown optional expressions in `if` guard, make it Option\[Known\[Expr\[Boolean\]\]\]"""
      )
    }*/

    "union queries" in pending /*{
      val query1 = Match { case v@Node("User")  => v.prop[String]("name") }
      val query2 = Match { case v@Node("Group") => v.prop[String]("name") }
      test(
        query1 union query2,
        "MATCH (`v`:`User`) " +
        "RETURN `v`.`name` " +
        "UNION " +
        "MATCH (`v`:`Group`) " +
        "RETURN `v`.`name`"
      ).returns[String]
    }*/

    "set aliases for returned values" in pending /*{
      val query1 = Match { case v@Node("User")  => (v.prop[String]("name") as "name", v.prop[Int]("age") as "N", lit("user") as "type") }
      val query2 = Match { case v@Node("Group") => (v.prop[String]("name") as "name", v.prop[Int]("count") as "N", lit("group") as "type") }
      test(
        query1 union query2,
        "MATCH (`v`:`User`) " +
        "RETURN `v`.`name` AS `name`, `v`.`age` AS `N`, \"user\" AS `type` " +
        "UNION " +
        "MATCH (`v`:`Group`) " +
        "RETURN `v`.`name` AS `name`, `v`.`count` AS `N`, \"group\" AS `type`"
      ).returns[(String, Int, String)]
    }*/

    "unwind literal expressions iterable (1)" in pending /*{
      val query = unwind(lit(List(1L, 2L, 3L))) { i =>
         Match { case v@Node("User") if v.id === i => v.prop[String]("name") }
      }
      test(
        query,
        "UNWIND [ 1, 2, 3 ] AS `i` " +
        "MATCH (`v`:`User`) " +
        "WHERE `id`(`v`) = `i` " +
        "RETURN `v`.`name`"
      ).returns[String]
    }*/

    "unwind literal expressions iterable (2)" in pending /*{
      val query = unwind(lit(Vector(1L, 2L, 3L))) { i =>
         Match { case v@Node("User", "id" := `i`) => v.prop[String]("name") }
      }
      test(
        query,
        "UNWIND [ 1, 2, 3 ] AS `i` " +
        "MATCH (`v`:`User`{ `id`: `i` }) " +
        "RETURN `v`.`name`"
      ).returns[String]
    }*/

    "unwind expressions list" in pending /*{
      val query = Match {
        case v@Node("User") =>
          val collected = collect(distinct(v.prop[String]("name")))
          unwind(collected) { name =>
            Match { case v2 if v2.prop[String]("name") === name => (name, v2.id, v2.labels) }
          }
      }
      test(
        query,
        "MATCH (`v`:`User`) " +
        "UNWIND `collect`(DISTINCT `v`.`name`) AS `name` " +
        "MATCH (`v2`) " +
        "WHERE `v2`.`name` = `name` " +
        "RETURN `name`, `id`(`v2`), `labels`(`v2`)"
      ).returns[(String, Long, List[String])]
    }*/

    "support lists of literals of mixed type" in pending /*test(
      unwind(list(1, "a", true)){ i => i },
      "UNWIND [ 1, \"a\", true ] AS `i` " +
      "RETURN `i`"
    ).returns[Any]*/

    "allow to use cypher expression variables in property matches" in pending /*{
      val query = unwind(lit(List(1L, 2L, 3L))) { i =>
         Match { case v@Node("User", "id" := `i`) => v.prop[String]("name") }
      }
      test(
        query,
        "UNWIND [ 1, 2, 3 ] AS `i` " +
        "MATCH (`v`:`User`{ `id`: `i` }) " +
        "RETURN `v`.`name`"
      ).returns[String]
    }*/

    "allow to match multiple properties" in pending /*{
      val admin = "System"
      val query = unwind(lit(List(1L, 2L, 3L))) { i =>
         Match { case v@Node("User", "id" := `i`, "isAdmin" := `admin`) => v.prop[String]("name") }
      }
      test(
        query,
        "UNWIND [ 1, 2, 3 ] AS `i` " +
        "MATCH (`v`:`User`{ `id`: `i`, `isAdmin`: \"System\" }) " +
        "RETURN `v`.`name`"
      ).returns[String]
    }*/

    "support `exists` predicate" in pending /*test(
      exists{ case v -e> x - _ > y => }.`return`,
      "RETURN EXISTS ((`v`) -[`e`]-> (`x`) -[]-> (`y`))"
    ).returns[Boolean]*/

    "get map value by key" in pending /*test(
      Match{ case v => v.value[String]("name") },
      "MATCH (`v`) " +
      "RETURN `v`.`name`"
    ).returns[String]*/

    "get map value by dynamic key" in pending /*test(
      Match{ case v => v.props.value[Boolean](v.prop[String]("prop-name")) },
      "MATCH (`v`) " +
      "RETURN `v`[`v`.`prop-name`]"
    ).returns[Boolean]*/

    "add keys to a map" in pending /*{
      val query = Match{ case v => v.props add ("foo" -> lit("bar"), "labels" -> v.labels) }
      test(
        query,
        "MATCH (`v`) " +
        "RETURN `v`{.*, `foo`: \"bar\", `labels`: `labels`(`v`)}"
      ).returns[Map[String, Any]]
    }*/

    "support `null` values" in pending /*test(
      Match { case v => (v.id, cypherNull[String]) },
      "MATCH (`v`) " +
      "RETURN `id`(`v`), null"
    ).returns[(Long, String)]*/

    "return products by flattening" in pending /*test(
      Match {
        case v =>
          val x = v.labels -> (v.prop[String]("name"), (v.prop[Int]("x"), v.prop[Int]("y")), v.prop[Boolean]("z"))
          toReturnOps(x)
      },
      "MATCH (`v`) " +
      "RETURN " +
        "`labels`(`v`), " +
        "`v`.`name`, " +
        "`v`.`x`, " +
        "`v`.`y`, " +
        "`v`.`z`"
    ).returns[(List[String], (String, (Int, Int), Boolean))]*/

    "Not allow to return nothing at read queries" in pending /*shapeless.test.illTyped(
      "Match { case a => returnNothing }",
      "type mismatch.*"
    )*/

    // // // WRITE // // //

    "Create paths" in pending /*{
      val id = 123
      val name = "foo"
      test (
        Match { case g@Node("Group", "id" := `id`) =>
          Create { case (u@Node("User", "name" := `name` )) < Rel("Admin")- Node("Members") `<-` Node(`g`) =>
            Match { case Node(`u`) -> (foo@Node("Foo")) =>
              Create { case Node(`foo`) -Rel("bar") > (bar@Node("Bar")) =>
                (u.prop[String]("id"), g.prop[String]("id"), collect(bar.prop[String]("id")))
              }
            }
          }
        },
        "MATCH (`g`:`Group`{ `id`: 123 }) " +
        "CREATE (`u`:`User`{ `name`: \"foo\" }) <-[:`Admin`]- (:`Members`) <-[]- (`g`) " +
        "MATCH (`u`) -[]-> (`foo`:`Foo`) " +
        "CREATE (`foo`) -[:`bar`]-> (`bar`:`Bar`) " +
        "RETURN `u`.`id`, `g`.`id`, `collect`(`bar`.`id`)"
      ).returns[(String, String, List[String])]
    }
     */
    "Allow to return nothing at write queries (create)" in pending /*test(
      Create { case Node("A") => returnNothing },
      "CREATE (:`A`) "
    ).returns[Unit]*/

    "Delete nodes and edges" in pending /*{
      test(
        Match { case (g@Node("Group", "id" := "123")) -foo> bar =>
          Delete(foo, bar) {
            g.props
          }
        },
        "MATCH (`g`:`Group`{ `id`: \"123\" }) -[`foo`]-> (`bar`) " +
        "DELETE `foo`, `bar` " +
        "RETURN `g`"
      ).returns[Map[String, Any]]
    }*/

    "Allow to return nothing at write queries (delete)" in pending /*test(
      Match { case x =>
        Delete(x) {
          returnNothing
        }
      },
      "MATCH (`x`) " +
      "DELETE `x` "
    ).returns[Unit]*/

    "Set props at nodes and edges" in pending /*{
      test(
        Match { case (g@Node("Group", "id" := "123")) -foo> bar =>
          SetProp(g.set("x") = lit("baz"),
                  foo.set("i") = bar.prop[Int]("j")) {
            foo.props
          }
        },
        "MATCH (`g`:`Group`{ `id`: \"123\" }) -[`foo`]-> (`bar`) " +
        "SET `g`.`x` = \"baz\", `foo`.`i` = `bar`.`j` " +
        "RETURN `foo`"
      ).returns[Map[String, Any]]
    }*/

    "Allow to return nothing at write queries (set prop)" in pending /*test(
      Match{ case a =>
        SetProp(a.set("x") = lit("y")) {
          returnNothing
        }
      },
      "MATCH (`a`) " +
      "SET `a`.`x` = \"y\" "
    ).returns[Unit]
  }*/
  }

}
