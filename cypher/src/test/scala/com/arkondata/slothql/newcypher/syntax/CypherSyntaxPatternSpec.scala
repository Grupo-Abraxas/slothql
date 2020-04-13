package com.arkondata.slothql.newcypher.syntax

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
  }
}
