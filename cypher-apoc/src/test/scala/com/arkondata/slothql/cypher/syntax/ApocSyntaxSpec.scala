package com.arkondata.slothql.cypher.syntax

class ApocSyntaxSpec extends CypherSyntaxBaseSpec {

  "APOC syntax" should {
    "support `apoc.when` call" in
      test(
        Match { case x =>
          APOC.when(
            lit("foo") in x.labels,
            parameterized{ a: Param[Int]    => Match { case v if v.id === a => v.props } },
            parameterized{ b: Param[String] => Match { case v@Node("bar" := `b`) => v.props } }
          ).withParams(a = x.prop[Int]("foo"), b = x.prop[String]("bar")).continue {
            _.value[String]("baz")
          }
        },
        "MATCH (`x0`) " +
        "CALL `apoc`.`when`(" +
          "\"foo\" IN `labels`(`x0`), " +
          "\"MATCH (`v0`) WHERE `id`(`v0`) = $`a` RETURN `v0`\", " +
          "\"MATCH (`v0`{ `bar`: $`b` }) RETURN `v0`\", " +
          "{ " +
            "`b`: `x0`.`bar`, " +
            "`a`: `x0`.`foo`" +
          " }" +
        ") YIELD `value` AS `yielded0` " +
        "WITH *, `yielded0`[`head`(`keys`(`yielded0`))] AS `v0` " +
        "RETURN `v0`.`baz`"
      ).returns[String]

    "support `apoc.case` call" in test(
      Match { case v -e> _ =>
        APOC.`case`(
          v.isNull -> parameterized {
                       (foo: Param[String], bar: Param[Int]) =>
                         Match { case a@Node("foo" := `foo`) =>
                           a.prop[Int]("n") + bar
                         }
                      },
          (e.tpe === lit("Foo")) -> parameterized {
                                     (foo: Param[String], baz: Param[Boolean]) =>
                                       Match { case a@Node("foo" := `foo`) if baz =>
                                         a.prop[Int]("n")
                                       }
                                    }
        ).otherwise(parameterized { (n: Param[Int]) =>
                           Match { case a => n + a.prop[Int]("n") + lit(100) }
                         })
          .withParams(
            foo = e.prop[String]("foo"),
            bar = v.prop[Int]("bar"),
            baz = v.prop[Boolean]("isBaz"),
            n = lit(10)
          )
          .apply( res =>
            lit("Result: %n").replace(lit("%n"), res.asString)
          )
      },
      "MATCH (`v0`) -[`e0`]-> () " +
      "CALL `apoc`.`case`(" +
        "[" +
          "`v0` IS NULL, \"MATCH (`a0`{ `foo`: $`foo` }) RETURN `a0`.`n` + $`bar`\", " +
          "`type`(`e0`) = \"Foo\", \"MATCH (`a0`{ `foo`: $`foo` }) WHERE $`baz` RETURN `a0`.`n`\"" +
        "], " +
        "\"MATCH (`a0`) RETURN $`n` + `a0`.`n` + 100\", " +
        "{ `n`: 10, `baz`: `v0`.`isBaz`, `bar`: `v0`.`bar`, `foo`: `e0`.`foo` }) " +
      "YIELD `value` AS `yielded0` " +
      "WITH *, `yielded0`[`head`(`keys`(`yielded0`))] AS `v1` " +
      "RETURN `replace`(\"Result: %n\", \"%n\", `toString`(`v1`))"
    ).returns[String]

  }
}
