package com.abraxas.slothql.newcypher.syntax

import org.scalatest.{ Assertion, Matchers, WordSpec }

import com.abraxas.slothql.newcypher.{ CypherFragment, CypherStatement }


class NewCypherSyntaxTest extends WordSpec with Matchers {

  class StubIdGen extends CypherStatement.Gen {
    def nextAlias(prefix: String): (String, CypherStatement.Gen) = (prefix, this)
    def nextParam(prefix: String): (String, CypherStatement.Gen) = (prefix, this)
  }

  private def test[A](query: CypherFragment.Query[A], expectedTemplate: String, expectedParams: Map[String, CypherStatement.LiftValue[_]] = Map()): Test[A] =
    new Test[A](query, expectedTemplate, expectedParams)
  private class Test[T](query: CypherFragment.Query[T], expectedTemplate: String, expectedParams: Map[String, CypherStatement.LiftValue[_]]) {
    def returns[R](implicit correct: T =:= R): Assertion = {
      val (CypherStatement.Complete(template, params), _) = query.toCypherF(new StubIdGen)
      template shouldBe expectedTemplate
      params   shouldBe expectedParams
    }
  }

  // // // // // // // // // // // // // // // // // // // // // // // //

  "Slothql cypher syntax" should {
    "build path pattern matching node label" in test(
      Match  {
        case (x@Node("foo")) < y - z =>
          x: Node
          y: Rel.Aux[Rel.Incoming]
          z: Node
          x.props
      },
      "MATCH (`x`:`foo`) <-[`y`]- (`z`) " +
      "RETURN `x`"
    ).returns[Map[String, Any]]

    "return tuples (1)" in test(
      Match  {
        case x < y - z =>
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


    "support built-in operators" in test(
      Match  {
        case x - y > z =>
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
      Match  {
        case a - b > c < d - e =>
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

    "build long path patterns (1)" in test(
      Match  {
        case a < b - c - d > e =>
          // case (a < ((b - c) - d)) > e =>
          a: Node
          b: Rel.Aux[Rel.Incoming]
          c: Node
          d: Rel.Aux[Rel.Outgoing]
          e: Node
          a.props
      },
      "MATCH (`a`) <-[`b`]- (`c`) -[`d`]-> (`e`) " +
      "RETURN `a`"
    ).returns[Map[String, Any]]

    "build long path patterns (2)" in test(
      Match  {
        case a - b > c < d - e - f > g =>
          // case (((a - b) > c) < ((d - e) - f)) > g =>
          a: Node
          b: Rel.Aux[Rel.Outgoing]
          c: Node
          d: Rel.Aux[Rel.Incoming]
          e: Node
          f: Rel.Aux[Rel.Outgoing]
          g: Node
          a.props
      },
      "MATCH (`a`) -[`b`]-> (`c`) <-[`d`]- (`e`) -[`f`]-> (`g`) " +
      "RETURN `a`"
    ).returns[Map[String, Any]]

    "build long path patterns (3)" in test(
      Match  {
        case a < b - c - d > e < f - g =>
          // case ((a < ((b - c) - d)) > e) < (f - g) =>
          a: Node
          b: Rel.Aux[Rel.Incoming]
          c: Node
          d: Rel.Aux[Rel.Outgoing]
          e: Node
          f: Rel.Aux[Rel.Incoming]
          g: Node
          a.props
      },
      "MATCH (`a`) <-[`b`]- (`c`) -[`d`]-> (`e`) <-[`f`]- (`g`) " +
      "RETURN `a`"
    ).returns[Map[String, Any]]

    "build long path patterns (4)" in test(
      Match  {
        case a <(b)- c <(d)- e <(f)- g -(h)> i =>
          // case (((a < (b - c)) < (d - e)) < ((f - g) - h)) > i =>
          a: Node
          b: Rel.Aux[Rel.Incoming]
          c: Node
          d: Rel.Aux[Rel.Incoming]
          e: Node
          f: Rel.Aux[Rel.Incoming]
          g: Node
          h: Rel.Aux[Rel.Outgoing]
          i: Node
          a.props
      },
      "MATCH (`a`) <-[`b`]- (`c`) <-[`d`]- (`e`) <-[`f`]- (`g`) -[`h`]-> (`i`) " +
      "RETURN `a`"
    ).returns[Map[String, Any]]

    "build long path patterns (5)" in test(
      Match  {
        case a <(b)- c <(d)- e <(f)- g -(h)> i -(j)> k =>
          // case ((((a < (b - c)) < (d - e)) < (f - g - h)) > (i - j)) > k =>
          a: Node
          b: Rel.Aux[Rel.Incoming]
          c: Node
          d: Rel.Aux[Rel.Incoming]
          e: Node
          f: Rel.Aux[Rel.Incoming]
          g: Node
          h: Rel.Aux[Rel.Outgoing]
          i: Node
          j: Rel.Aux[Rel.Outgoing]
          k: Node
          (a.props, j.props, k.props)
      },
      "MATCH (`a`) <-[`b`]- (`c`) <-[`d`]- (`e`) <-[`f`]- (`g`) -[`h`]-> (`i`) -[`j`]-> (`k`) " +
      "RETURN `a`, `j`, `k`"
    ).returns[(Map[String, Any], Map[String, Any], Map[String, Any])]

    "build long path patterns (6)" in test(
      Match  {
        case a <(b)- c <(d)- e <(f)- g -(h)> i -(j)> k <(l)- m <(n)- o =>
          // case ((((((a < (b - c)) < (d - e)) < ((f - g) - h)) > (i - j)) > k) < (l - m)) < (n - o) =>
          a: Node
          b: Rel.Aux[Rel.Incoming]
          c: Node
          d: Rel.Aux[Rel.Incoming]
          e: Node
          f: Rel.Aux[Rel.Incoming]
          g: Node
          h: Rel.Aux[Rel.Outgoing]
          i: Node
          j: Rel.Aux[Rel.Outgoing]
          k: Node
          l: Rel.Aux[Rel.Incoming]
          m: Node
          n: Rel.Aux[Rel.Incoming]
          o: Node
          a.props
      },
      "MATCH (`a`) <-[`b`]- (`c`) <-[`d`]- (`e`) <-[`f`]- (`g`) -[`h`]-> (`i`) -[`j`]-> (`k`) <-[`l`]- (`m`) <-[`n`]- (`o`) " +
      "RETURN `a`"
    ).returns[Map[String, Any]]

    "build long path patterns (7)" in test(
      Match  {
        case a - b > c - d > e < f - g < h - i - j > k - l > m < n - o =>
          // case (((((((a - b) > (c - d)) > e) < (f - g)) < ((h - i) - j)) > (k - l)) > m) < (n - o) =>
          a: Node
          b: Rel.Aux[Rel.Outgoing]
          c: Node
          d: Rel.Aux[Rel.Outgoing]
          e: Node
          f: Rel.Aux[Rel.Incoming]
          g: Node
          h: Rel.Aux[Rel.Incoming]
          i: Node
          j: Rel.Aux[Rel.Outgoing]
          k: Node
          l: Rel.Aux[Rel.Outgoing]
          m: Node
          n: Rel.Aux[Rel.Incoming]
          o: Node
          a.props
      },
      "MATCH (`a`) -[`b`]-> (`c`) -[`d`]-> (`e`) <-[`f`]- (`g`) <-[`h`]- (`i`) -[`j`]-> (`k`) -[`l`]-> (`m`) <-[`n`]- (`o`) " +
      "RETURN `a`"
    ).returns[Map[String, Any]]
  }

//  val baz = lit("baz")
//  this show Match {
//    case x < y - (z@Node("foo", "bar" := `baz`)) =>
//      x: Node
//      y: Rel.Aux[Rel.Incoming]
//      z: Node
//      (x, y, z)
//  }

}
