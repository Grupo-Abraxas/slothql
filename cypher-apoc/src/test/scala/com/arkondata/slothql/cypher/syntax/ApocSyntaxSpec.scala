package com.arkondata.slothql.cypher.syntax

import com.arkondata.slothql.cypher.APOC

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
  }
}
