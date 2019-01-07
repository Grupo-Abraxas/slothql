package com.abraxas.slothql.cypher.syntax

import scala.collection.convert.decorateAsJava.seqAsJavaListConverter

import org.scalatest.{ Assertion, Matchers, WordSpec }

import com.abraxas.slothql.cypher.CypherFragment
import com.abraxas.slothql.cypher.CypherFragment.ParamCollector.defaultThreadUnsafeParamCollector
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

  private def test[T](query: CypherFragment.Query[T], expected: String, params: Any*)(implicit reader: RecordReader[T]): Test[T, reader.Out] =
    new Test[T, reader.Out](query, CypherFragment.Statement(expected, params.zipWithIndex.map{ case (v, i) => i.toString -> v }.toMap), reader)
  private class Test[T, TR](query: CypherFragment.Query[T], expected: CypherFragment.Statement, reader: RecordReader.Aux[T, TR]) {
    def returns[R](implicit correct: TR =:= R): Assertion = query.known.toCypher shouldEqual expected
  }

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
        "MATCH (`u`:`User`{ `id`: $0 }) <-[]- (:`Members`) <-[]- (`group`) " +
        "RETURN `u`.`email`, `u`.`name`, `u`.`age`, `u`.`confirmed`, `group`.`name`",
        params = id
      ).returns[(String, String, Int, Boolean, String)]
    }

    "match vertex labels and properties, allow non-literal property values (2)" in  {
      val email = "user@example.com"
      test(
        Match {
          case Vertex("User", "email" := `email`) -(e)> (g@Vertex("Group")) =>
            e -> g.prop[String]("id")
        },
        "MATCH (:`User`{ `email`: $0 }) -[`e`]-> (`g`:`Group`) " +
        "RETURN `e`, `g`.`id`",
        params = email
      ).returns[(Map[String, Any], String)]
    }

    "match relation types, support variable length paths (left direction)" in test(
      Match {
        case Vertex("Group") < _ *:(0 - _, Edge("parent")) - (g@Vertex("Group")) =>
          g.prop[String]("name")
      },
      "MATCH (:`Group`) <-[:`parent`*$0..]- (`g`:`Group`) " +
      "RETURN `g`.`name`",
      params = 0
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
      "MATCH (:`Group`) <-[`es`:`parent`*$0..]- (`g`:`Group`) " +
      "RETURN `es`",
      params = 0
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
          v.isNull
        )
      },
      "MATCH (`v`) <-[]- () " +
      "RETURN " +
        "`id`(`v`), " +
        "`id`(`v`) = $0, " +
        "`id`(`v`) = $1, " +
        "`id`(`v`) > $2, " +
        "NOT `id`(`v`) > $3, " +
        "`id`(`v`) > $4 AND `id`(`v`) <= $5, " +
        "`id`(`v`) > $6 XOR `id`(`v`) <= $7, " +
        "`v` IS NULL",
      params = 2, "QWERTY", 2, 2, 2, 4, 2, 4
    ).returns[(Long, Boolean, Boolean, Boolean, Boolean, Boolean, Boolean, Boolean)]

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
        "`v`.`name` CONTAINS $0, " +
        "`v`.`name` STARTS WITH $1, " +
        "`v`.`name` ENDS WITH $2, " +
        "`v`.`name` =~ $3",
      params = "foo", "bar", "baz", "foo.*bar"
    ).returns[(Boolean, Boolean, Boolean, Boolean)]

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
        "`left`(`v`.`name`, $0), " +
        "`right`(`v`.`name`, $1), " +
        "`replace`(`v`.`name`, $2, $3), " +
        "`reverse`(`v`.`name`), " +
        "`split`(`v`.`name`, $4), " +
        "`substring`(`v`.`name`, $5), " +
        "`substring`(`v`.`name`, $6, $7), " +
        "`trim`(`v`.`name`), " +
        "`rTrim`(`v`.`name`), " +
        "`lTrim`(`v`.`name`)",
      params = 4, 5, "foo", "bar", ".", 2, 2, 10
    ).returns[(String, String, Long, Boolean, Double, Long, String, String, String, String, List[String], String, String, String, String, String)]

    def supportLists[E <: CypherFragment.Expr[List[String]]: CypherFragment](l: E) = test(
      Match {
        case v < e - _ => (
          list(v.id, e.id),
          l,
          v.labels ++ list(e.tpe),
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
        "$0, " +
        "`labels`(`v`) + [ `type`(`e`) ], " +
        "`type`(`e`) IN $0, " +
        "`labels`(`v`)[$1], " +
        "$0[$2..$3], " +
        "$0[$4..], " +
        "$0[..$5]",
      params = List("Admin", "Share", "Create").asJava, 0, 1, 2, 2, 2
    ).returns[(List[Long], List[String], List[String], Boolean, String, List[String], List[String], List[String])]


    "support lists of literals" in supportLists(list("Admin", "Share", "Create"))

    "support literal lists" in supportLists(lit(List("Admin", "Share", "Create")))

    "support `if` guards" in test(
      Match {
        case v < e - _ if e.tpe in list("Admin", "Share") => v
      },
      "MATCH (`v`) <-[`e`]- () " +
      "WHERE `type`(`e`) IN $0 " +
      "RETURN `v`",
      params = List("Admin", "Share").asJava
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
        "MATCH (`g`:`Group`{ `id`: $0 }) " +
        "RETURN `g`",
        params = id.get
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
        "`groups`: `collect`({ `id`: `g`.`id`, `role`: `type`(`role`) })[..$0] " +
        "}",
      params = 1
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
        "WHERE `v`.`age` >= $0 " +
        "RETURN `v`.`name`",
        params = 18
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
        "RETURN `v`.`name` AS `name`, `v`.`age` AS `N`, $0 AS `type` " +
        "UNION " +
        "MATCH (`v`:`Group`) " +
        "RETURN `v`.`name` AS `name`, `v`.`count` AS `N`, $1 AS `type`",
        params = "user", "group"
      ).returns[(String, Int, String)]
    }

    "unwind literal expressions iterable (1)" in {
      val query = unwind(lit(List(1L, 2L, 3L))) { i =>
         Match { case v@Vertex("User") if v.id === i => v.prop[String]("name") }
      }
      test(
        query,
        "UNWIND $0 AS `i` " +
        "MATCH (`v`:`User`) " +
        "WHERE `id`(`v`) = `i` " +
        "RETURN `v`.`name`",
        params = List(1L, 2L, 3L).asJava
      ).returns[String]
    }

    "unwind literal expressions iterable (2)" in {
      val query = unwind(lit(Vector(1L, 2L, 3L))) { i =>
         Match { case v@Vertex("User", "id" := `i`) => v.prop[String]("name") }
      }
      test(
        query,
        "UNWIND $0 AS `i` " +
        "MATCH (`v`:`User`{ `id`: `i` }) " +
        "RETURN `v`.`name`",
        params = List(1L, 2L, 3L).asJava
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
      "UNWIND $0 AS `i` " +
      "RETURN `i`",
      params = List(1, "a", true).asJava
    ).returns[Any]

    "allow to use cypher expression variables in property matches" in {
      val query = unwind(lit(List(1L, 2L, 3L))) { i =>
         Match { case v@Vertex("User", "id" := `i`) => v.prop[String]("name") }
      }
      test(
        query,
        "UNWIND $0 AS `i` " +
        "MATCH (`v`:`User`{ `id`: `i` }) " +
        "RETURN `v`.`name`",
        params = List(1L, 2L, 3L).asJava
      ).returns[String]
    }

    "allow to match multiple properties" in {
      val admin = "System"
      val query = unwind(lit(List(1L, 2L, 3L))) { i =>
         Match { case v@Vertex("User", "id" := `i`, "isAdmin" := `admin`) => v.prop[String]("name") }
      }
      test(
        query,
        "UNWIND $0 AS `i` " +
        "MATCH (`v`:`User`{ `id`: `i`, `isAdmin`: $1 }) " +
        "RETURN `v`.`name`",
        params = List(1L, 2L, 3L).asJava, "System"
      ).returns[String]
    }

    "add keys to a map" in {
      val query = Match{ case v => v add ("foo" -> lit("bar"), "labels" -> v.labels) }
      test(
        query,
        "MATCH (`v`) " +
        "RETURN `v`{.*, `foo`: $0, `labels`: `labels`(`v`)}",
        params = "bar"
      ).returns[Map[String, Any]]
    }
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
