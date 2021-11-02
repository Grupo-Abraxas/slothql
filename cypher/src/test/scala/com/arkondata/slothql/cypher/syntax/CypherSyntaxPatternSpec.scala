package com.arkondata.slothql.cypher.syntax

import scala.util.Random

import com.arkondata.slothql.cypher.{ CypherFragment, GraphElem }

/** Advanced pattern matching.
  *  - Node: labels, props
  *  - Rel: labels, props, variable length
  *  - Matching paths with many elements (macro test)
  */
class CypherSyntaxPatternSpec extends CypherSyntaxBaseSpec {
  "Slothql cypher syntax" should {
    "match literal node label" in
    test(
      Match { case n @ Node("Foo") => n.props },
      "MATCH (`n0`:`Foo`) RETURN `n0`"
    ).returns[Map[String, Any]]

    "match literal node label (1)" in
    test(
      Match { case Node("Foo") - _ > n => n.props },
      "MATCH (:`Foo`) --> (`n0`) RETURN `n0`"
    ).returns[Map[String, Any]]

    "match multiple literal node labels" in
    test(
      Match { case n @ Node("Foo", "Bar", "Baz") => n.props },
      "MATCH (`n0`:`Foo`:`Bar`:`Baz`) RETURN `n0`"
    ).returns[Map[String, Any]]

    "match node label from string value" in {
      val label = "Foo"
      test(
        Match { case n @ Node(`label`) => n.props },
        "MATCH (`n0`:`Foo`) RETURN `n0`"
      ).returns[Map[String, Any]]
    }

    "match node label from multiple string values" in {
      val label1 = "Foo"
      val label2 = "Bar"
      test(
        Match { case n @ Node(`label1`, `label2`) => n.props },
        "MATCH (`n0`:`Foo`:`Bar`) RETURN `n0`"
      ).returns[Map[String, Any]]
    }

    "match node label from string iterable values" in {
      val labels = Seq("Foo", "Bar")
      test(
        Match { case n @ Node(`labels`) => n.props },
        "MATCH (`n0`:`Foo`:`Bar`) RETURN `n0`"
      ).returns[Map[String, Any]]
    }

    "match node label from string values and iterable values" in {
      val labels = Seq("Foo", "Bar")
      val label0 = "Baz"
      test(
        Match { case n @ Node(`labels`, `label0`) => n.props },
        "MATCH (`n0`:`Foo`:`Bar`:`Baz`) RETURN `n0`"
      ).returns[Map[String, Any]]
    }

    "match node label from string values, iterable values and literal values" in {
      val labels = Seq("Foo", "Bar")
      val label0 = "Baz"
      test(
        Match { case n @ Node(`labels`, `label0`, "Test") => n.props },
        "MATCH (`n0`:`Foo`:`Bar`:`Baz`:`Test`) RETURN `n0`"
      ).returns[Map[String, Any]]
    }

    "match node property from literal (String)" in
    test(
      Match { case n @ Node("id" := "ABC") => n.props },
      "MATCH (`n0`{ `id`: \"ABC\" }) RETURN `n0`"
    ).returns[Map[String, Any]]

    "match node property from literal (Int)" in
    test(
      Match { case n @ Node("index" := 123) => n.props },
      "MATCH (`n0`{ `index`: 123 }) RETURN `n0`"
    ).returns[Map[String, Any]]

    "match node property from literal (Long)" in
    test(
      Match { case n @ Node("id" := 321L) => n.props },
      "MATCH (`n0`{ `id`: 321 }) RETURN `n0`"
    ).returns[Map[String, Any]]

    "match node property from literal (Boolean)" in
    test(
      Match { case n @ Node("enabled" := true) => n.props },
      "MATCH (`n0`{ `enabled`: true }) RETURN `n0`"
    ).returns[Map[String, Any]]

    "match node property from value (String)" in {
      val id: String = "foobar"
      test(
        Match { case n @ Node("id" := `id`) => n },
        "MATCH (`n0`{ `id`: \"foobar\" }) RETURN `n0`"
      ).returns[GraphElem.Node]
    }

    "match node property from value (Int)" in {
      val idx: Int = Random.nextInt().abs
      test(
        Match { case n @ Node("index" := `idx`) => n },
        s"MATCH (`n0`{ `index`: $idx }) RETURN `n0`"
      ).returns[GraphElem.Node]
    }

    "match node property from value (Long)" in {
      val id: Long = Random.nextLong().abs
      test(
        Match { case n @ Node("id" := `id`) => n },
        s"MATCH (`n0`{ `id`: $id }) RETURN `n0`"
      ).returns[GraphElem.Node]
    }

    "match node property from value (Boolean)" in {
      val enabled: Boolean = Random.nextBoolean()
      test(
        Match { case n @ Node("enabled" := `enabled`) => n },
        s"MATCH (`n0`{ `enabled`: $enabled }) RETURN `n0`"
      ).returns[GraphElem.Node]
    }

    "match node property from value (String List)" in {
      val list: List[String] = List("A", "B", "C")
      test(
        Match { case n @ Node("chain" := `list`) => n },
        "MATCH (`n0`{ `chain`: [\"A\", \"B\", \"C\"] }) RETURN `n0`"
      ).returns[GraphElem.Node]
    }

    "match node property from value (Int List List)" in {
      val list: List[List[Int]] = List(List(1), List(2, 3), List())
      test(
        Match { case n @ Node("data" := `list`) => n },
        "MATCH (`n0`{ `data`: [[1], [2, 3], []] }) RETURN `n0`"
      ).returns[GraphElem.Node]
    }

    "match node property from value (Map)" in {
      val map: Map[String, CypherFragment.Expr.Lit[_]] = Map(
        "bar" -> lit(12),
        "baz" -> lit("abc"),
        "foo" -> lit(List(true, false, true))
      )
      test(
        Match { case n @ Node("data" := `map`) => n },
        "MATCH (`n0`{ `data`: {`bar`: 12, `baz`: \"abc\", `foo`: [true, false, true]} }) RETURN `n0`"
      ).returns[GraphElem.Node]
    }

    "match node property from expression" in {
      test(
        Match { case a =>
          val foo = a.prop[Int]("foo")
          Match { case b @ Node("foo" := `foo`) =>
            (a.props, b.props)
          }
        },
        "MATCH (`a0`) " +
        "MATCH (`b0`{ `foo`: `a0`.`foo` }) " +
        "RETURN `a0`, `b0`"
      ).returns[(Map[String, Any], Map[String, Any])]
    }

    "match node labels and properties" in {
      val label = "Foo"
      val id    = "abcd"
      test(
        Match { case n @ Node(`label`, "Bar", "id" := `id`, "type" := "baz") => n },
        "MATCH (`n0`:`Foo`:`Bar`{ `id`: \"abcd\", `type`: \"baz\" }) RETURN `n0`"
      ).returns[GraphElem.Node]
    }

    "match literal relationship type" in
    test(
      Match { case n - Rel("foo") > _ => n.props },
      "MATCH (`n0`) -[:`foo`]-> () RETURN `n0`"
    ).returns[Map[String, Any]]

    "match literal relationship type (1)" in
    test(
      Match { case n - (r @ Rel("foo")) > _ => (n.props, r.prop[Int]("index")) },
      "MATCH (`n0`) -[`r0`:`foo`]-> () RETURN `n0`, `r0`.`index`"
    ).returns[(Map[String, Any], Int)]

    "match alternative literal relationships types" in
    test(
      Match { case n - Rel("foo", "bar") > _ => n.props },
      "MATCH (`n0`) -[:`foo`|`bar`]-> () RETURN `n0`"
    ).returns[Map[String, Any]]

    "match relationship type from string value" in {
      val tpe = "baz"
      test(
        Match { case n - Rel(`tpe`) > _ => n.props },
        "MATCH (`n0`) -[:`baz`]-> () RETURN `n0`"
      ).returns[Map[String, Any]]
    }

    "match alternative relationship types from multiple string values" in {
      val foo = "foo"
      val bar = "bar"
      test(
        Match { case n - Rel(`foo`, `bar`) > _ => n.props },
        "MATCH (`n0`) -[:`foo`|`bar`]-> () RETURN `n0`"
      ).returns[Map[String, Any]]
    }

    "match alternative relationship types from string iterable values" in {
      val types = Seq("foo", "bar")
      test(
        Match { case n - Rel(`types`) > _ => n.props },
        "MATCH (`n0`) -[:`foo`|`bar`]-> () RETURN `n0`"
      ).returns[Map[String, Any]]
    }

    "match alternative relationship types from string values and iterable values" in {
      val types = Seq("foo", "bar")
      test(
        Match { case n - Rel("baz", `types`) > _ => n.props },
        "MATCH (`n0`) -[:`baz`|`foo`|`bar`]-> () RETURN `n0`"
      ).returns[Map[String, Any]]
    }

    "match alternative relationship types from string values, iterable values and literal values" in {
      val types = Seq("foo", "bar")
      val tpe   = "baz"
      test(
        Match { case n - Rel(`tpe`, `types`, "Test") > _ => n.props },
        "MATCH (`n0`) -[:`baz`|`foo`|`bar`|`Test`]-> () RETURN `n0`"
      ).returns[Map[String, Any]]
    }

    "match relationship property from literal" in
    test(
      Match { case n - Rel("index" := 12) > _ => n.props },
      "MATCH (`n0`) -[{ `index`: 12 }]-> () RETURN `n0`"
    ).returns[Map[String, Any]]

    "match relationship property from value" in {
      val flag = Random.nextBoolean()
      test(
        Match { case n - Rel("flag" := `flag`) > _ => n.props },
        s"MATCH (`n0`) -[{ `flag`: $flag }]-> () RETURN `n0`"
      ).returns[Map[String, Any]]
    }

    "match relationship property from expression" in {
      test(
        Match { case a =>
          val foo = a.prop[Int]("foo")
          Match { case n - Rel("foo" := `foo`) > _ =>
            n.props
          }
        },
        "MATCH (`a0`) " +
        "MATCH (`n0`) -[{ `foo`: `a0`.`foo` }]-> () " +
        "RETURN `n0`"
      ).returns[Map[String, Any]]
    }

    "match relationship types and properties" in {
      val tpe   = "Foo"
      val index = Random.nextInt()
      test(
        Match { case n - Rel(`tpe`, "Bar", "flag" := true, "index" := `index`) > _ => n.props },
        s"MATCH (`n0`) -[:`Foo`|`Bar`{ `flag`: true, `index`: $index }]-> () RETURN `n0`"
      ).returns[Map[String, Any]]
    }

    "match relationship of variable length (no limits)" in
    test(
      Match { case n - Rel(`**`) > _ => n.props },
      "MATCH (`n0`) -[*]-> () RETURN `n0`"
    ).returns[Map[String, Any]]

    "match relationship of variable length (no limits) (1)" in
    test(
      Match { case n - Rel(_ ** _) > _ => n.props },
      "MATCH (`n0`) -[*]-> () RETURN `n0`"
    ).returns[Map[String, Any]]

    "match relationship of variable length (min length, from literal)" in
    test(
      Match { case n - Rel(0 ** _) > _ => n.props },
      "MATCH (`n0`) -[*0..]-> () RETURN `n0`"
    ).returns[Map[String, Any]]

    "match relationship of variable length (max length, from literal)" in
    test(
      Match { case n - Rel(_ ** 10) > _ => n.props },
      "MATCH (`n0`) -[*..10]-> () RETURN `n0`"
    ).returns[Map[String, Any]]

    "match relationship of variable length (both limits, from literal)" in
    test(
      Match { case n - Rel(0 ** 10) > _ => n.props },
      "MATCH (`n0`) -[*0..10]-> () RETURN `n0`"
    ).returns[Map[String, Any]]

    "match relationship of variable length (both limits, from values)" in {
      val min = 1
      val max = 51
      test(
        Match { case n - Rel(`min` ** `max`) > _ => n.props },
        "MATCH (`n0`) -[*1..51]-> () RETURN `n0`"
      ).returns[Map[String, Any]]
    }

    "match relationship of variable length with types" in {
      test(
        Match { case n - Rel("foo", "bar", 0 ** 10) > _ => n.props },
        "MATCH (`n0`) -[:`foo`|`bar`*0..10]-> () RETURN `n0`"
      ).returns[Map[String, Any]]
    }

    "match relationship of variable length with types and properties" in {
      val min   = 0
      val max   = 10
      val tpe   = "Foo"
      val index = Random.nextInt()
      test(
        Match { case n - Rel(`min` ** `max`, `tpe`, "Bar", "flag" := true, "index" := `index`) > _ => n.props },
        s"MATCH (`n0`) -[:`Foo`|`Bar`*0..10{ `flag`: true, `index`: $index }]-> () RETURN `n0`"
      ).returns[Map[String, Any]]
    }

    "build long path patterns (1)" in test(
      Match { case a < b - c - d > e =>
        // case (a < ((b - c) - d)) > e =>
        assert(a).is[Node] // test
        assert(b).is[Rel] // test
        assert(c).is[Node] // test
        assert(d).is[Rel] // test
        assert(e).is[Node] // test
        a.props
      },
      "MATCH (`a0`) <-[`b0`]- (`c0`) -[`d0`]-> (`e0`) " +
      "RETURN `a0`"
    ).returns[Map[String, Any]]

    "build long path patterns (2)" in test(
      Match { case a - b > c < d - e - f > g =>
        // case (((a - b) > c) < ((d - e) - f)) > g =>
        assert(a).is[Node] // test
        assert(b).is[Rel] // test
        assert(c).is[Node] // test
        assert(d).is[Rel] // test
        assert(e).is[Node] // test
        assert(f).is[Rel] // test
        assert(g).is[Node] // test
        a.props
      },
      "MATCH (`a0`) -[`b0`]-> (`c0`) <-[`d0`]- (`e0`) -[`f0`]-> (`g0`) " +
      "RETURN `a0`"
    ).returns[Map[String, Any]]

    "build long path patterns (3)" in test(
      Match { case a < b - c - d > e < f - g =>
        // case ((a < ((b - c) - d)) > e) < (f - g) =>
        assert(a).is[Node] // test
        assert(b).is[Rel] // test
        assert(c).is[Node] // test
        assert(d).is[Rel] // test
        assert(e).is[Node] // test
        assert(f).is[Rel] // test
        assert(g).is[Node] // test
        a.props
      },
      "MATCH (`a0`) <-[`b0`]- (`c0`) -[`d0`]-> (`e0`) <-[`f0`]- (`g0`) " +
      "RETURN `a0`"
    ).returns[Map[String, Any]]

    "build long path patterns (4)" in test(
      Match { case a < (b) - c < (d) - e < (f) - g - (h) > i =>
        // case (((a < (b - c)) < (d - e)) < ((f - g) - h)) > i =>
        assert(a).is[Node] // test
        assert(b).is[Rel] // test
        assert(c).is[Node] // test
        assert(d).is[Rel] // test
        assert(e).is[Node] // test
        assert(f).is[Rel] // test
        assert(g).is[Node] // test
        assert(h).is[Rel] // test
        assert(i).is[Node] // test
        a.props
      },
      "MATCH (`a0`) <-[`b0`]- (`c0`) <-[`d0`]- (`e0`) <-[`f0`]- (`g0`) -[`h0`]-> (`i0`) " +
      "RETURN `a0`"
    ).returns[Map[String, Any]]

    "build long path patterns (5)" in test(
      Match { case a < (b) - c < (d) - e < (f) - g - (h) > i - (j) > k =>
        // case ((((a < (b - c)) < (d - e)) < (f - g - h)) > (i - j)) > k =>
        assert(a).is[Node] // test
        assert(b).is[Rel] // test
        assert(c).is[Node] // test
        assert(d).is[Rel] // test
        assert(e).is[Node] // test
        assert(f).is[Rel] // test
        assert(g).is[Node] // test
        assert(h).is[Rel] // test
        assert(i).is[Node] // test
        assert(j).is[Rel] // test
        assert(k).is[Node] // test
        (a.props, j.props, k.props)
      },
      "MATCH (`a0`) <-[`b0`]- (`c0`) <-[`d0`]- (`e0`) <-[`f0`]- (`g0`) -[`h0`]-> (`i0`) -[`j0`]-> (`k0`) " +
      "RETURN `a0`, `j0`, `k0`"
    ).returns[(Map[String, Any], Map[String, Any], Map[String, Any])]

    "build long path patterns (6)" in test(
      Match { case a < (b) - c < (d) - e < (f) - g - (h) > i - (j) > k < (l) - m < (n) - o =>
        // case ((((((a < (b - c)) < (d - e)) < ((f - g) - h)) > (i - j)) > k) < (l - m)) < (n - o) =>
        assert(a).is[Node] // test
        assert(b).is[Rel] // test
        assert(c).is[Node] // test
        assert(d).is[Rel] // test
        assert(e).is[Node] // test
        assert(f).is[Rel] // test
        assert(g).is[Node] // test
        assert(h).is[Rel] // test
        assert(i).is[Node] // test
        assert(j).is[Rel] // test
        assert(k).is[Node] // test
        assert(l).is[Rel] // test
        assert(m).is[Node] // test
        assert(n).is[Rel] // test
        assert(o).is[Node] // test
        a.props
      },
      "MATCH (`a0`) <-[`b0`]- (`c0`) <-[`d0`]- (`e0`) <-[`f0`]- (`g0`) -[`h0`]-> (`i0`) -[`j0`]-> (`k0`) <-[`l0`]- (`m0`) <-[`n0`]- (`o0`) " +
      "RETURN `a0`"
    ).returns[Map[String, Any]]

    "build long path patterns (7)" in test(
      Match { case a - b > c - d > e < f - g < h - i - j > k - l > m < n - o =>
        // case (((((((a - b) > (c - d)) > e) < (f - g)) < ((h - i) - j)) > (k - l)) > m) < (n - o) =>
        assert(a).is[Node] // test
        assert(b).is[Rel] // test
        assert(c).is[Node] // test
        assert(d).is[Rel] // test
        assert(e).is[Node] // test
        assert(f).is[Rel] // test
        assert(g).is[Node] // test
        assert(h).is[Rel] // test
        assert(i).is[Node] // test
        assert(j).is[Rel] // test
        assert(k).is[Node] // test
        assert(l).is[Rel] // test
        assert(m).is[Node] // test
        assert(n).is[Rel] // test
        assert(o).is[Node] // test
        a.props
      },
      "MATCH (`a0`) -[`b0`]-> (`c0`) -[`d0`]-> (`e0`) <-[`f0`]- (`g0`) <-[`h0`]- (`i0`) -[`j0`]-> (`k0`) -[`l0`]-> (`m0`) <-[`n0`]- (`o0`) " +
      "RETURN `a0`"
    ).returns[Map[String, Any]]
  }
}
