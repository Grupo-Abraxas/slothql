package com.abraxas.slothql.cypher.syntax

import org.scalatest.{ Assertion, Matchers, WordSpec }

import com.abraxas.slothql.cypher.CypherFragment
import com.abraxas.slothql.neo4j.Neo4jCypherTransactor.RecordReader

class CypherSyntaxTest extends WordSpec with Matchers {

  private lazy val `supported path matchers` = (??? : Graph) match {
    case a -> b =>
    case a `<-` b =>
    case a -(b)> c =>
    case a <(b)- c =>

    case a -> b -> c =>
    case a `<-` b `<-` c =>

    case  a -(b)> c -(d)> e =>
    case  a -(b)> c -(d)> e -(f)> g -(h)> i =>
    case  a <(b)- c <(d)- e  =>
    case  a -(b)> c -(d)> e <(f)- g =>

    case a -- b =>
    case a -- b -- c =>

    //    case a -(b)- c -(d)- e =>
    //    case a -(b)> c -(d)- e <(f)- g =>
  }

  private def test[T](query: CypherFragment.Query[T], expected: String)(implicit reader: RecordReader[T]): Test[T, reader.Out] =
    new Test[T, reader.Out](query, expected, reader)
  private class Test[T, TR](query: CypherFragment.Query[T], expected: String, reader: RecordReader.Aux[T, TR]) {
    def returns[R](implicit correct: TR =:= R): Assertion = query.known.toCypher shouldEqual expected
  }

  private def testVar[T](name: String = "") = CypherFragment.Expr.Var[T](name)

  "Slothql cypher syntax" should {

    "build path patterns" in test(
      Match {
        case a -(b)> c -(d)> e <(f)- g => a.prop[Int]("count")
      },
      "MATCH (`a`) -[`b`]-> (`c`) -[`d`]-> (`e`) <-[`f`]- (`g`) " +
      "RETURN `a`.`count`"
    ).returns[Int]

    "return tuples" in test(
      Match {
        case user < _ - _ < _ - group =>
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

    "match vertex labels and properties, allow non-literal property values (1)" in {
      val id = "u1"
      test(
        Match {
          case (u@Vertex("User", "id" := `id`)) < _ - Vertex("Members") < _ - group =>
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
    }

    "match vertex labels and properties, allow non-literal property values (2)" in  {
      val email = "user@example.com"
      test(
        Match {
          case Vertex("User", "email" := `email`) -(e)> (g@Vertex("Group")) =>
            e -> g.prop[String]("id")
        },
        "MATCH (:`User`{ `email`: \"user@example.com\" }) -[`e`]-> (`g`:`Group`) " +
        "RETURN `e`, `g`.`id`"
      ).returns[(Map[String, Any], String)]
    }

    "match relation types, support variable length paths (left direction)" in test(
      Match {
        case Vertex("Group") < _ *:(0 - _, Edge("parent")) - (g@Vertex("Group")) =>
          g.prop[String]("name")
      },
      "MATCH (:`Group`) <-[:`parent`*0..]- (`g`:`Group`) " +
      "RETURN `g`.`name`"
    ).returns[String]

    "match relation types, support variable length paths (right direction)" in test(
      Match {
        case Vertex("Group") - es *:(_, _) > (g@Vertex("Group")) =>
          es -> g.prop[String]("name")
      },
      "MATCH (:`Group`) -[`es`*]-> (`g`:`Group`) " +
      "RETURN `es`, `g`.`name`"
    ).returns[(List[Map[String, Any]], String)]

    "build function calls" in test(
      Match {
        case Vertex("Group") < _ *:(_, Edge("parent")) - (g@Vertex("Group")) =>
          (g, g.call[Map[String, Any]]("properties"), 'pi.call[Double]())
      },
      "MATCH (:`Group`) <-[:`parent`*]- (`g`:`Group`) " +
      "RETURN `g`, `properties`(`g`), `pi`()"
    ).returns[(Map[String, Any], Map[String, Any], Double)]

    "provide syntax for common built-in functions" in test(
      Match {
        case Vertex("Group") < (e@Edge("parent")) - (g@Vertex("Group")) => (
          g.id, g.count, g.keys, g.labels, e.id, e.count, e.keys, e.tpe
        )
      },
      "MATCH (:`Group`) <-[`e`:`parent`]- (`g`:`Group`) " +
      "RETURN `id`(`g`), `count`(`g`), `keys`(`g`), `labels`(`g`), `id`(`e`), `count`(`e`), `keys`(`e`), `type`(`e`)"
    ).returns[(Long, Long, List[String], List[String], Long, Long, List[String], String)]

    "allow to select edge lists (from variable length match)" in test(
      Match {
        case Vertex("Group") < es*:(0 - _, Edge("parent")) - (g@Vertex("Group")) => es
      },
      "MATCH (:`Group`) <-[`es`:`parent`*0..]- (`g`:`Group`) " +
      "RETURN `es`"
    ).returns[List[Map[String, Any]]]

    "support comparison expressions" in test(
      Match {
        case v < _ - _ => (
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
      Match {
        case v =>
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

    "not allow to use cypher's `toString` built-in function for other types" in
      shapeless.test.illTyped(
        """Match { case v => v.prop[List[Int]]("data").asString }""",
        """value asString is not a member of com\.abraxas\.slothql\.cypher\.CypherFragment\.Expr\.MapKey\[List\[Int\]\]"""
      )

    "support built-in functions for strings" in test(
      Match {
        case v =>
          val name = v.prop[String]("name")
          (
            name.toLower,
            name.toUpper,
            name.length,
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
        "`length`(`v`.`name`), " +
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
    ).returns[(String, String, Long, Boolean, Double, Long, String, String, String, String, List[String], String, String, String, String, String)]

    "support mathematical operators" in test(
      Match {
        case v =>
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

    def supportLists[E <: CypherFragment.Expr[List[String]]: CypherFragment](l: E) = test(
      Match {
        case v < e - _ => (
          list(v.id, e.id),
          l,
          v.labels ++ list(e.tpe),
          v.labels.length,
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
        "`length`(`labels`(`v`)), " +
        "`type`(`e`) IN [ \"Admin\", \"Share\", \"Create\" ], " +
        "`labels`(`v`)[0], " +
        "[ \"Admin\", \"Share\", \"Create\" ][1..2], " +
        "[ \"Admin\", \"Share\", \"Create\" ][2..], " +
        "[ \"Admin\", \"Share\", \"Create\" ][..2]"
    ).returns[(List[Long], List[String], List[String], Long, Boolean, String, List[String], List[String], List[String])]


    "support lists of literals" in supportLists(list("Admin", "Share", "Create"))

    "support literal lists" in supportLists(lit(List("Admin", "Share", "Create")))

    "support `if` guards" in test(
      Match {
        case v < e - _ if e.tpe in list("Admin", "Share") => v
      },
      "MATCH (`v`) <-[`e`]- () " +
      "WHERE `type`(`e`) IN [ \"Admin\", \"Share\" ] " +
      "RETURN `v`"
    ).returns[Map[String, Any]]

    "allow returning lists of vertices" in test(
      Match {
        case v1 -> v2 `<-` v3 => list(v1, v2, v3)
      },
      "MATCH (`v1`) -[]-> (`v2`) <-[]- (`v3`) " +
      "RETURN [ `v1`, `v2`, `v3` ]"
    ).returns[List[Map[String, Any]]]

    "~~ relation direction (1) ~~" in test(
      Match {
        case v1 -> v2 -(e)> v3 `<-` v4 => list(v1, v2, v3, v4) -> e.tpe
      },
      "MATCH (`v1`) -[]-> (`v2`) -[`e`]-> (`v3`) <-[]- (`v4`) " +
      "RETURN [ `v1`, `v2`, `v3`, `v4` ], `type`(`e`)"
    ).returns[(List[Map[String, Any]], String)]

    "~~ relation direction (2) ~~" in test(
      Match {
        case v1 -> v2 <(e)- v3 `<-` v4 => list(v1, v2, v3, v4) -> e.tpe
      },
      "MATCH (`v1`) -[]-> (`v2`) <-[`e`]- (`v3`) <-[]- (`v4`) " +
      "RETURN [ `v1`, `v2`, `v3`, `v4` ], `type`(`e`)"
    ).returns[(List[Map[String, Any]], String)]

    "order results" in test(
      Match {
        case v@Vertex("Group") =>
          val name = v.prop[String]("name")
          (v.id, name)
            .orderBy(name, v.id)
      },
      "MATCH (`v`:`Group`) " +
      "RETURN `id`(`v`), `v`.`name` " +
      "ORDER BY `v`.`name`, `id`(`v`)"
    ).returns[(Long, String)]

    "allow multiple ordering directives, support descending order" in test(
      Match {
        case v@Vertex("Group") =>
          val name = v.prop[String]("name")
          (v.id, name)
            .orderBy(name.desc)
            .orderBy(v.id)
      },
      "MATCH (`v`:`Group`) " +
      "RETURN `id`(`v`), `v`.`name` " +
      "ORDER BY `v`.`name` DESC, `id`(`v`)"
    ).returns[(Long, String)]

    "paginate results" in test(
      Match {
        case v@Vertex("Group") =>
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
    ).returns[(Long, String)]

    "distinct results" in test(
      Match {
        case v@Vertex("Group") =>
          val name = v.prop[String]("name")
          (v.id, name)
            .orderBy(name)
            .distinct
      },
      "MATCH (`v`:`Group`) " +
      "RETURN DISTINCT `id`(`v`), `v`.`name` " +
      "ORDER BY `v`.`name`"
    ).returns[(Long, String)]

    "match vertex properties optionally (1)" in {
      val id = Some("g1")
      test(
        Match {
          case g@Vertex("Group", "id" :?= `id`) => g
        },
        "MATCH (`g`:`Group`{ `id`: \"g1\" }) " +
        "RETURN `g`"
      ).returns[Map[String, Any]]
    }

    "match vertex properties optionally (2)" in {
      val id = Option.empty[String]
      test(
        Match {
          case g@Vertex("Group", "id" :?= `id`) => g
        },
        "MATCH (`g`:`Group`) " +
        "RETURN `g`"
      ).returns[Map[String, Any]]
    }

    "support nested matches (1)" in test(
      Match {
        case u@Vertex("User") =>
          Match {
            case (g@Vertex("Group")) -> Vertex("Members") -Edge("Admin")> u2 if u === u2 =>
              u.prop[String]("email") -> g.prop[String]("name")
          }
      },
      "MATCH (`u`:`User`) " +
      "MATCH (`g`:`Group`) -[]-> (:`Members`) -[:`Admin`]-> (`u2`) " +
      "WHERE `u` = `u2` " +
      "RETURN `u`.`email`, `g`.`name`"
    ).returns[(String, String)]

    "support nested matches (2)" in test(
      Match {
        case u@Vertex("User") =>
          Match {
            case (g@Vertex("Group")) -> Vertex("Members") -Edge("Admin")> `u` =>
              u.prop[String]("email") -> g.prop[String]("name")
          }
      },
      "MATCH (`u`:`User`) " +
      "MATCH (`g`:`Group`) -[]-> (:`Members`) -[:`Admin`]-> (`u`) " +
      "RETURN `u`.`email`, `g`.`name`"
    ).returns[(String, String)]

    "support optional matches" in test(
      Match {
        case u@Vertex("User") =>
          Match.optional {
            case (g@Vertex("Group")) -> Vertex("Members") -Edge("FooBar")> u2 if u === u2 =>
              u.prop[String]("email") -> g.prop[String]("name")
          }
      },
      "MATCH (`u`:`User`) " +
      "OPTIONAL MATCH (`g`:`Group`) -[]-> (:`Members`) -[:`FooBar`]-> (`u2`) " +
      "WHERE `u` = `u2` " +
      "RETURN `u`.`email`, `g`.`name`"
    ).returns[(String, String)]

    "support maps/dictionaries" in test(
      Match {
        case u@Vertex("User") => dict(
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

    "collect results to lists" in test(
      Match {
        case u@Vertex("User") =>
          // TODO: syntax: reuse `u` in second pattern =================================================================
          Match.optional {
            case (u0@Vertex("User")) <(role)- Vertex("Members") `<-` (g@Vertex("Group")) if u === u0 =>
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
    ).returns[Map[String, Any]]

    "reduce lists" in {
      val reduceExpr@CypherFragment.Expr.ReduceList(_, elem, _, acc, _) = testVar[List[String]]()
        .reduce(0L)((name, acc) => acc + name.length )
      test(
        Match { case v => reduceExpr.copy(list = v.prop[List[String]]("names")) },
         "MATCH (`v`) " +
        s"RETURN reduce(`$acc` = 0, `$elem` IN `v`.`names` | `$acc` + `length`(`$elem`))"
      ).returns[Long]
    }

    "support list comprehensions (filter)" in {
      val expr@CypherFragment.Expr.ListComprehension(_, elem, _, _) = testVar[List[Long]]()
        .filter(_ % lit(2L) === lit(0))
      test(
        Match { case v => expr.copy(list = v.prop[List[Long]]("data")) },
         "MATCH (`v`) " +
        s"RETURN [`$elem` IN `v`.`data` WHERE `$elem` % 2 = 0]"
      ).returns[List[Long]]
    }

    "support list comprehensions (map)" in {
      val expr@CypherFragment.Expr.ListComprehension(_, elem, _, _) = testVar[List[Long]]()
        .map(_ * lit(10L))
      test(
        Match { case v => expr.copy(list = v.prop[List[Long]]("data")) },
         "MATCH (`v`) " +
        s"RETURN [`$elem` IN `v`.`data` | `$elem` * 10]"
      ).returns[List[Long]]
    }

    "support list comprehensions (filter + map)" in {
      val expr@CypherFragment.Expr.ListComprehension(_, elem, _, _) = testVar[List[Long]]()
        .withFilter(x => x > lit(0L) && x < lit(1000L))
        .map(_.asString)
      test(
        Match { case v => expr.copy(list = v.prop[List[Long]]("data")) },
         "MATCH (`v`) " +
        s"RETURN [`$elem` IN `v`.`data` WHERE `$elem` > 0 AND `$elem` < 1000 | `toString`(`$elem`)]"
      ).returns[List[String]]
    }

    "[manual fragment] avoid list comprehension if neither filter nor map is defined" in test(
      Match { case v =>
        CypherFragment.Expr.ListComprehension[Long, Long](
          v.prop[List[Long]]("data"),
          "x",
          filter = None,
          map = None
        )
      },
      "MATCH (`v`) " +
      "RETURN `v`.`data`"
    ).returns[List[Long]]

    "support list predicates" in {
      val all@CypherFragment.Expr.ListPredicate(_, elem1, _, _) = testVar[List[Long]]()
        .all(_ > lit(0L))
      val any@CypherFragment.Expr.ListPredicate(_, elem2, _, _) = testVar[List[Long]]()
        .any(_ > lit(0L))
      val exists@CypherFragment.Expr.ListPredicate(_, elem3, _, _) = testVar[List[Long]]()
        .exists(_ > lit(0L))
      val none@CypherFragment.Expr.ListPredicate(_, elem4, _, _) = testVar[List[Long]]()
        .none(_ > lit(0L))
      val single@CypherFragment.Expr.ListPredicate(_, elem5, _, _) = testVar[List[Long]]()
        .single(_ > lit(0L))
      test(
        Match { case v =>
          (
            all   .copy(list = v.prop[List[Long]]("data")),
            any   .copy(list = v.prop[List[Long]]("data")),
            exists.copy(list = v.prop[List[Long]]("data")),
            none  .copy(list = v.prop[List[Long]]("data")),
            single.copy(list = v.prop[List[Long]]("data"))
          )
        },
        "MATCH (`v`) " +
        "RETURN " +
          s"all(`$elem1` IN `v`.`data` WHERE `$elem1` > 0), " +
          s"any(`$elem2` IN `v`.`data` WHERE `$elem2` > 0), " +
          s"exists(`$elem3` IN `v`.`data` WHERE `$elem3` > 0), " +
          s"none(`$elem4` IN `v`.`data` WHERE `$elem4` > 0), " +
          s"single(`$elem5` IN `v`.`data` WHERE `$elem5` > 0)"
      ).returns[(Boolean, Boolean, Boolean, Boolean, Boolean)]
    }

    "slice collected lists" in test(
      Match {
        case u@Vertex("User") =>
          `with`(_.orderBy(u.prop[String]("name")).limit(5)) {
            // TODO: syntax: reuse `u` in second pattern ===================================================================
            Match.optional {
              case (u0@Vertex("User")) <(role)- Vertex("Members") `<-` (g@Vertex("Group")) if u === u0 =>
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
    ).returns[Map[String, Any]]

    "allow to use non-literal labels in matches, support non-literal property names" in {
      val label = "User"
      val prop  = "name"
      test(
        Match {
          case v@Vertex(`label`) => v.prop[Any](prop)
        },
        "MATCH (`v`:`User`) " +
        "RETURN `v`.`name`"
      ).returns[Any]
    }

    "allow to use non-literal label/type iterables in matches" in {
      val labels = "User" :: Nil
      val types  = Set("Admin", "Share")
      test(
        Match {
          case (v@Vertex(`labels`)) - Edge(`types`) > x => v -> x
        },
        "MATCH (`v`:`User`) -[:`Admin`|`Share`]-> (`x`) " +
        "RETURN `v`, `x`"
      ).returns[(Map[String, Any], Map[String, Any])]
    }

    "allow to use non-literal, known optional conditions in `if` guard [defined]" in {
      def cond(v: Vertex): Option[CypherFragment.Known[CypherFragment.Expr[Boolean]]] = Some(v.prop[Int]("age") >= lit(18))
      test(
        Match { case v@Vertex("User") if cond(v) => v.prop[String]("name") },
        "MATCH (`v`:`User`) " +
        "WHERE `v`.`age` >= 18 " +
        "RETURN `v`.`name`"
      ).returns[String]
    }
    "allow to use non-literal, known optional conditions in `if` guard [undefined]" in {
      def cond(v: Vertex): Option[CypherFragment.Known[CypherFragment.Expr[Boolean]]] = None
      test(
        Match { case v@Vertex("User") if cond(v) => v.prop[String]("name") },
        "MATCH (`v`:`User`) " +
        "RETURN `v`.`name`"
      ).returns[String]
    }

    "raise compilation error when unknown optional conditions are used in `if` guard" in {
      shapeless.test.illTyped(
        """
          def cond(v: Vertex): Option[CypherFragment.Expr[Boolean]] = ???
          Match { case v if cond(v) => v.prop[String]("name") }
        """,
        """Cannot use unknown optional expressions in `if` guard, make it Option\[Known\[Expr\[Boolean\]\]\]"""
      )
    }

    "union queries" in {
      val query1 = Match { case v@Vertex("User")  => v.prop[String]("name") }
      val query2 = Match { case v@Vertex("Group") => v.prop[String]("name") }
      test(
        query1 union query2,
        "MATCH (`v`:`User`) " +
        "RETURN `v`.`name` " +
        "UNION " +
        "MATCH (`v`:`Group`) " +
        "RETURN `v`.`name`"
      ).returns[String]
    }

    "set aliases for returned values" in {
      val query1 = Match { case v@Vertex("User")  => (v.prop[String]("name") as "name", v.prop[Int]("age") as "N", lit("user") as "type") }
      val query2 = Match { case v@Vertex("Group") => (v.prop[String]("name") as "name", v.prop[Int]("count") as "N", lit("group") as "type") }
      test(
        query1 union query2,
        "MATCH (`v`:`User`) " +
        "RETURN `v`.`name` AS `name`, `v`.`age` AS `N`, \"user\" AS `type` " +
        "UNION " +
        "MATCH (`v`:`Group`) " +
        "RETURN `v`.`name` AS `name`, `v`.`count` AS `N`, \"group\" AS `type`"
      ).returns[(String, Int, String)]
    }

    "unwind literal expressions iterable (1)" in {
      val query = unwind(lit(List(1L, 2L, 3L))) { i =>
         Match { case v@Vertex("User") if v.id === i => v.prop[String]("name") }
      }
      test(
        query,
        "UNWIND [ 1, 2, 3 ] AS `i` " +
        "MATCH (`v`:`User`) " +
        "WHERE `id`(`v`) = `i` " +
        "RETURN `v`.`name`"
      ).returns[String]
    }

    "unwind literal expressions iterable (2)" in {
      val query = unwind(lit(Vector(1L, 2L, 3L))) { i =>
         Match { case v@Vertex("User", "id" := `i`) => v.prop[String]("name") }
      }
      test(
        query,
        "UNWIND [ 1, 2, 3 ] AS `i` " +
        "MATCH (`v`:`User`{ `id`: `i` }) " +
        "RETURN `v`.`name`"
      ).returns[String]
    }

    "unwind expressions list" in {
      val query = Match {
        case v@Vertex("User") =>
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
    }

    "support lists of literals of mixed type" in test(
      unwind(list(1, "a", true)){ i => i },
      "UNWIND [ 1, \"a\", true ] AS `i` " +
      "RETURN `i`"
    ).returns[Any]

    "allow to use cypher expression variables in property matches" in {
      val query = unwind(lit(List(1L, 2L, 3L))) { i =>
         Match { case v@Vertex("User", "id" := `i`) => v.prop[String]("name") }
      }
      test(
        query,
        "UNWIND [ 1, 2, 3 ] AS `i` " +
        "MATCH (`v`:`User`{ `id`: `i` }) " +
        "RETURN `v`.`name`"
      ).returns[String]
    }

    "allow to match multiple properties" in {
      val admin = "System"
      val query = unwind(lit(List(1L, 2L, 3L))) { i =>
         Match { case v@Vertex("User", "id" := `i`, "isAdmin" := `admin`) => v.prop[String]("name") }
      }
      test(
        query,
        "UNWIND [ 1, 2, 3 ] AS `i` " +
        "MATCH (`v`:`User`{ `id`: `i`, `isAdmin`: \"System\" }) " +
        "RETURN `v`.`name`"
      ).returns[String]
    }

    "get map value by key" in test(
      Match{ case v => v.value[String]("name") },
      "MATCH (`v`) " +
      "RETURN `v`.`name`"
    ).returns[String]

    "add keys to a map" in {
      val query = Match{ case v => v add ("foo" -> lit("bar"), "labels" -> v.labels) }
      test(
        query,
        "MATCH (`v`) " +
        "RETURN `v`{.*, `foo`: \"bar\", `labels`: `labels`(`v`)}"
      ).returns[Map[String, Any]]
    }

    "support `null` values" in test(
      Match { case v => (v.id, cypherNull[String]) },
      "MATCH (`v`) " +
      "RETURN `id`(`v`), null"
    ).returns[(Long, String)]

    "support simple CASE expressions (without default)" in test(
      Match { case v =>
        v.prop[Int]("status") whenUnsafe (
          lit(1) -> lit("On"),
          lit(0) -> lit("Off")
        )
      },
      "MATCH (`v`) " +
      "RETURN " +
        "CASE `v`.`status` " +
          "WHEN 1 THEN \"On\" " +
          "WHEN 0 THEN \"Off\" " +
        "END"
    ).returns[String]

    "support simple CASE expressions (with default)" in test(
      Match { case v =>
        v.prop[Int]("status") when (
          lit(0) -> lit("OK")
        ) otherwise "Failed"
      },
      "MATCH (`v`) " +
      "RETURN " +
        "CASE `v`.`status` " +
          "WHEN 0 THEN \"OK\" " +
          "ELSE \"Failed\" " +
        "END"
    ).returns[String]

    "support generic CASE expressions (without default)" in test(
      Match { case v =>
        val foo = v.prop[Int]("foo")
        val bar = v.prop[Int]("bar")
        whenUnsafe(
          (foo > lit(0) && bar > lit(0)) -> foo * bar,
          (foo <= lit(0))                -> -bar
        )
      },
      "MATCH (`v`) " +
        "RETURN " +
        "CASE " +
          "WHEN `v`.`foo` > 0 AND `v`.`bar` > 0 THEN `v`.`foo` * `v`.`bar` " +
          "WHEN `v`.`foo` <= 0 THEN -`v`.`bar` " +
        "END"
    ).returns[Int]


    "support generic CASE expressions (with default)" in test(
      Match { case v =>
        val age = v.prop[Int]("age")
        when(
          (age < lit(18))  -> lit("You must be at least 18 years old"),
          (age > lit(100)) -> lit("Oh really?")
        ).otherwise(          lit("You can proceed"))
      },
      "MATCH (`v`) " +
        "RETURN " +
        "CASE " +
          "WHEN `v`.`age` < 18 THEN \"You must be at least 18 years old\" " +
          "WHEN `v`.`age` > 100 THEN \"Oh really?\" " +
          "ELSE \"You can proceed\" " +
        "END"
    ).returns[String]

  }
}


// // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // //
// TODO: fails to compile ============================================================================================
//  {
//    val email = "user@example.com"
//    Match {
//      case Vertex("User", "email" := `email`) -(e)> (g@Vertex("Group")) => g.prop[String]("id")
//    }
//  }
