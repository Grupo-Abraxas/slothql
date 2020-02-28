package com.abraxas.slothql.cypher.newsyntax

import com.abraxas.slothql.cypher.syntax

class NewCypherSyntaxTest {

  Match  {
    case (x@Node("foo")) < y - z =>
      x: Node
      y: Rel.Aux[Rel.Incoming]
      z: Node
      ()
  }

  // // // // // // // // // // // // // // // // // // // // // // // //

  Match  {
    case x < y - z =>
      // case x < (y - z)
      x: Node
      y: Rel.Aux[Rel.Incoming]
      z: Node
      ()
  }

  Match  {
    case x - y > z =>
      // case (x - y) > z =>
      x: Node
      y: Rel.Aux[Rel.Outgoing]
      z: Node
      ()
  }

  Match  {
    case a - b > c < d - e =>
      // case ((a - b) > c) < (d - e) =>
      a: Node
      b: Rel.Aux[Rel.Outgoing]
      c: Node
      d: Rel.Aux[Rel.Incoming]
      e: Node
      ()
  }

  Match  {
    case a < b - c - d > e =>
      // case (a < ((b - c) - d)) > e =>
      a: Node
      b: Rel.Aux[Rel.Incoming]
      c: Node
      d: Rel.Aux[Rel.Outgoing]
      e: Node
      ()
  }

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
      ()
  }

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
      ()
  }

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
      ()
  }

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
      ()
  }

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
      ()
  }
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
      ()
  }


  val baz = syntax.lit("baz")
  val q1 = Match {
    case x < y - (z@Node("foo", "bar" := `baz`)) =>
      x: Node
      y: Rel.Aux[Rel.Incoming]
      z: Node
      (x, y, z)
  }

  val q2 = Match {
    case a <(b)- c <(d)- e <(f)- g -(h)> i -(j)> k =>
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
      (a, j, k)
  }


}