package com.abraxas.slothql.cypher.apoc

import org.scalatest.{ Assertion, Matchers, WordSpec }

import com.abraxas.slothql.cypher.CypherFragment
import com.abraxas.slothql.cypher.syntax._
import com.abraxas.slothql.neo4j.Neo4jCypherTransactor.RecordReader

class ApocSyntaxSpec extends WordSpec with Matchers {

  // TODO: copied from `CypherSyntaxTest`
  private def test[T](query: CypherFragment.Query[T], expected: String)(implicit reader: RecordReader[T]): Test[T, reader.Out] =
    new Test[T, reader.Out](query, expected, reader)
  private class Test[T, TR](query: CypherFragment.Query[T], expected: String, reader: RecordReader.Aux[T, TR]) {
    def returns[R](implicit correct: TR =:= R): Assertion = query.known.toCypher shouldEqual expected
  }

  "Support [apoc.case]" in test(
    Match { case v -e> _ =>
      APOC.`case`(
        v.isNull -> parameterized {
                     (foo: Param[String], bar: Param[Int]) =>
                       Match { case a@Vertex("foo" := `foo`) =>
                         a.prop[Int]("n") + bar
                       }
                    },
        (e.tpe === "Foo") -> parameterized {
                              (foo: Param[String], baz: Param[Boolean]) =>
                                Match { case a@Vertex("foo" := `foo`) if baz =>
                                  a.prop[Int]("n")
                                }
                             }
      ).otherwise(parameterized { (n: Param[Int]) =>
                         Match { case a => n + a.prop[Int]("n") + lit(100) }
                       })
        .withParams(
          foo = e.prop[String]("foo").known,
          bar = v.prop[Int]("bar").known,
          baz = v.prop[Boolean]("isBaz").known,
          n = knownLit(10)
        )
        .apply( res =>
          lit("Result: %n").replace("%n", res.asString)
        )
    },
    "MATCH (`v`) -[`e`]-> () " +
    "CALL `apoc`.`case`(" +
      "[ " +
        "`v` IS NULL, \"MATCH (`a`{ `foo`: $`foo` }) RETURN `a`.`n` + $`bar`\", " +
        "`type`(`e`) = \"Foo\", \"MATCH (`a`{ `foo`: $`foo` }) WHERE $`baz` RETURN `a`.`n`\" " +
      "], " +
      "\"MATCH (`a`) RETURN $`n` + `a`.`n` + 100\", " +
      "{ `n`: 10, `baz`: `v`.`isBaz`, `bar`: `v`.`bar`, `foo`: `e`.`foo` }) " +
    "YIELD `value` AS `res` " +
    "RETURN `replace`(\"Result: %n\", \"%n\", `toString`(`res`))"
  ).returns[String]


  "Support [apoc.cypher.runFirstColumnSingle] function (plain query)" in test(
    APOC.runFirstColumnSingle(
      Match { case a@Vertex("A") => a.prop[String]("a") }
    ).`return`,
    "RETURN `apoc`.`cypher`.`runFirstColumnSingle`(" +
      "\"MATCH (`a`:`A`) RETURN `a`.`a`\", " +
      "null" +
    ")"
  ).returns[String]

  "Support [apoc.cypher.runFirstColumnSingle] function (parameterized query)" in test(
    APOC.runFirstColumnSingle(
      parameterized {
        (k: Param[String], x: Param[Int]) =>
          Match { case a@Vertex("A", "key" := `k`) => a.prop[Int]("n") + x }
      }
    ).withParams(
      k = knownLit("A-B-C"),
      x = knownLit(123)
    ).`return`,
    "RETURN `apoc`.`cypher`.`runFirstColumnSingle`(" +
      "\"MATCH (`a`:`A`{ `key`: $`k` }) RETURN `a`.`n` + $`x`\", " +
      "{ `x`: 123, `k`: \"A-B-C\" }" +
    ")"
  ).returns[Int]

  "Support [apoc.util.validate]" in test(
    Match { case v@Vertex("Foo") =>
      APOC.failingIf(v.prop[Int]("n") < lit(0), "The number is too small: %s", v.prop[Int]("n")) {
        v.prop[String]("name")
      }
    },
    "MATCH (`v`:`Foo`) " +
    "CALL `apoc`.`util`.`validate`(" +
      "`v`.`n` < 0, " +
      "\"The number is too small: %s\", " +
      "[ `v`.`n` ]" +
    ") " +
    "RETURN `v`.`name`"
  ).returns[String]
}
