package com.abraxas.slothql.newcypher.syntax

import com.abraxas.slothql.newcypher.{ CypherFragment, CypherStatement }


object NewCypherSyntaxTest extends App {

  class StubIdGen extends CypherStatement.Gen {
    def nextAlias(prefix: String): (String, CypherStatement.Gen) = (prefix, this)
    def nextParam(prefix: String): (String, CypherStatement.Gen) = (prefix, this)
  }

  def show[A](query: CypherFragment.Query[A]): Unit = {
    val (CypherStatement.Complete(template, params), _) = query.toCypherF(new StubIdGen)
    println(
      s"""$template
         |$params
         |$query
         |""".stripMargin
    )
  }

  // // // // // // // // // // // // // // // // // // // // // // // //


  this show Match  {
    case (x@Node("foo")) < y - z =>
      x: Node
      y: Rel.Aux[Rel.Incoming]
      z: Node
      x.props
  }

  this show Match  {
    case x < y - z =>
      // case x < (y - z)
      x: Node
      y: Rel.Aux[Rel.Incoming]
      z: Node
      x.prop[String]("a")
  }

  this show Match  {
    case x - y > z =>
      // case (x - y) > z =>
      x: Node
      y: Rel.Aux[Rel.Outgoing]
      z: Node
      x.id + x.prop[Long]("foo") > y.prop[Long]("bar")
  }

  this show Match  {
    case a - b > c < d - e =>
      // case ((a - b) > c) < (d - e) =>
      a: Node
      b: Rel.Aux[Rel.Outgoing]
      c: Node
      d: Rel.Aux[Rel.Incoming]
      e: Node
      c.labels as "qwerty"
  }

  this show Match  {
    case a < b - c - d > e =>
      // case (a < ((b - c) - d)) > e =>
      a: Node
      b: Rel.Aux[Rel.Incoming]
      c: Node
      d: Rel.Aux[Rel.Outgoing]
      e: Node
      a.props
  }

  this show Match  {
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
  }

  this show Match  {
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
  }

  this show Match  {
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
  }

  this show Match  {
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
      a.props
  }

  this show Match  {
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
  }
  this show Match  {
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
