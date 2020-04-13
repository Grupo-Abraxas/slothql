package com.arkondata.slothql.newcypher.syntax

import scala.util.Random

import com.arkondata.slothql.newcypher.CypherFragment

/** Advanced pattern matching.
 *  - Node: labels, props
 *  - Rel: labels, props, variable length paths
 *  - Matching paths with many elements (macro test)
 */
class CypherSyntaxPatternSpec extends CypherSyntaxBaseSpec {
  "Slothql cypher syntax" should {
    "match literal node label" in
      test(
        Match { case n@Node("Foo") => n.props },
        "MATCH (`n0`:`Foo`) RETURN `n0`"
      ).returns[Map[String, Any]]

    "match literal node label (1)" in
      test(
        Match { case Node("Foo") -_> n => n.props },
        "MATCH (:`Foo`) --> (`n0`) RETURN `n0`"
      ).returns[Map[String, Any]]

    "match multiple literal node labels" in
      test(
        Match { case n@Node("Foo", "Bar", "Baz") => n.props },
        "MATCH (`n0`:`Foo`:`Bar`:`Baz`) RETURN `n0`"
      ).returns[Map[String, Any]]

    "match node label from string value" in {
      val label = "Foo"
      test(
        Match { case n@Node(`label`) => n.props },
        "MATCH (`n0`:`Foo`) RETURN `n0`"
      ).returns[Map[String, Any]]
    }

    "match node label from multiple string values" in {
      val label1 = "Foo"
      val label2 = "Bar"
      test(
        Match { case n@Node(`label1`, `label2`) => n.props },
        "MATCH (`n0`:`Foo`:`Bar`) RETURN `n0`"
      ).returns[Map[String, Any]]
    }

    "match node label from string iterable values" in {
      val labels = Seq("Foo", "Bar")
      test(
        Match { case n@Node(`labels`) => n.props },
        "MATCH (`n0`:`Foo`:`Bar`) RETURN `n0`"
      ).returns[Map[String, Any]]
    }

    "match node label from string values and iterable values" in {
      val labels = Seq("Foo", "Bar")
      val label0 = "Baz"
      test(
        Match { case n@Node(`labels`, `label0`) => n.props },
        "MATCH (`n0`:`Foo`:`Bar`:`Baz`) RETURN `n0`"
      ).returns[Map[String, Any]]
    }

    "match node label from string values, iterable values and literal values" in {
      val labels = Seq("Foo", "Bar")
      val label0 = "Baz"
      test(
        Match { case n@Node(`labels`, `label0`, "Test") => n.props },
        "MATCH (`n0`:`Foo`:`Bar`:`Baz`:`Test`) RETURN `n0`"
      ).returns[Map[String, Any]]
    }

    "match node property from literal (String)" in
      test(
        Match{ case n@Node("id" := "ABC") => n.props },
        "MATCH (`n0`{ `id`: \"ABC\" }) RETURN `n0`"
      ).returns[Map[String, Any]]

    "match node property from literal (Int)" in
      test(
        Match{ case n@Node("index" := 123) => n.props },
        "MATCH (`n0`{ `index`: 123 }) RETURN `n0`"
      ).returns[Map[String, Any]]

    "match node property from literal (Long)" in
      test(
        Match{ case n@Node("id" := 321L) => n.props },
        "MATCH (`n0`{ `id`: 321 }) RETURN `n0`"
      ).returns[Map[String, Any]]

    "match node property from literal (Boolean)" in
      test(
        Match{ case n@Node("enabled" := true) => n.props },
        "MATCH (`n0`{ `enabled`: true }) RETURN `n0`"
      ).returns[Map[String, Any]]

    "match node property from value (String)" in {
      val id: String = "foobar"
      test(
        Match { case n@Node("id" := `id`) => n },
        "MATCH (`n0`{ `id`: \"foobar\" }) RETURN `n0`"
      ).returns[Map[String, Any]]
    }

    "match node property from value (Int)" in {
      val idx: Int = Random.nextInt().abs
      test(
        Match { case n@Node("index" := `idx`) => n },
        s"MATCH (`n0`{ `index`: $idx }) RETURN `n0`"
      ).returns[Map[String, Any]]
    }

    "match node property from value (Long)" in {
      val id: Long = Random.nextLong().abs
      test(
        Match { case n@Node("id" := `id`) => n },
        s"MATCH (`n0`{ `id`: $id }) RETURN `n0`"
      ).returns[Map[String, Any]]
    }

    "match node property from value (Boolean)" in {
      val enabled: Boolean = Random.nextBoolean()
      test(
        Match { case n@Node("enabled" := `enabled`) => n },
        s"MATCH (`n0`{ `enabled`: $enabled }) RETURN `n0`"
      ).returns[Map[String, Any]]
    }

    "match node property from value (String List)" in {
      val list: List[String] = List("A", "B", "C")
      test(
        Match { case n@Node("chain" := `list`) => n },
        "MATCH (`n0`{ `chain`: [\"A\", \"B\", \"C\"] }) RETURN `n0`"
      ).returns[Map[String, Any]]
    }

    "match node property from value (Int List List)" in {
      val list: List[List[Int]] = List(List(1), List(2, 3), List())
      test(
        Match { case n@Node("data" := `list`) => n },
        "MATCH (`n0`{ `data`: [[1], [2, 3], []] }) RETURN `n0`"
      ).returns[Map[String, Any]]
    }

    "match node property from value (Map)" in {
      val map: Map[String, CypherFragment.Expr.Lit[_]] = Map(
        "bar" -> lit(12),
        "baz" -> lit("abc"),
        "foo" -> lit(List(true, false, true))
      )
      test(
        Match { case n@Node("data" := `map`) => n },
        "MATCH (`n0`{ `data`: {`bar`: 12, `baz`: \"abc\", `foo`: [true, false, true]} }) RETURN `n0`"
      ).returns[Map[String, Any]]
    }

  }
}
