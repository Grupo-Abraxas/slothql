package com.abraxas.slothql.newcypher.syntax

import com.abraxas.slothql.newcypher.{ CypherFragment, CypherStatement }


object NewCypherSyntaxTest extends App {

  val stub = CypherFragment.Query.Return(CypherFragment.Return.Expr(CypherFragment.Expr.Var("stub"), as = None))

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
      stub
  }

  this show Match  {
    case x < y - z =>
      // case x < (y - z)
      x: Node
      y: Rel.Aux[Rel.Incoming]
      z: Node
      stub
  }

  this show Match  {
    case x - y > z =>
      // case (x - y) > z =>
      x: Node
      y: Rel.Aux[Rel.Outgoing]
      z: Node
      stub
  }

  this show Match  {
    case a - b > c < d - e =>
      // case ((a - b) > c) < (d - e) =>
      a: Node
      b: Rel.Aux[Rel.Outgoing]
      c: Node
      d: Rel.Aux[Rel.Incoming]
      e: Node
      stub
  }

  this show Match  {
    case a < b - c - d > e =>
      // case (a < ((b - c) - d)) > e =>
      a: Node
      b: Rel.Aux[Rel.Incoming]
      c: Node
      d: Rel.Aux[Rel.Outgoing]
      e: Node
      stub
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
      stub
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
      stub
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
      stub
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
      stub
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
      stub
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
      stub
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
