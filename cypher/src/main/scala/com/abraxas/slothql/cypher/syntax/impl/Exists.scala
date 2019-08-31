package com.abraxas.slothql.cypher.syntax.impl

import scala.reflect.macros.whitebox

import com.abraxas.slothql.cypher.CypherFragment.Expr
import com.abraxas.slothql.cypher.syntax.{ Graph, Match }

object Exists {
  def impl(c: whitebox.Context)(pattern: c.Expr[Graph => Unit]): c.Expr[Expr.Exists] = {
    val m = new Match.InternalImpl[c.type](c)
    import c.universe._

    val (pat, _, guard) = m.matchPattern(pattern)
    if (guard.isDefined) c.abort(guard.get.tree.pos, "WHEN clause is not supported at EXISTS predicate")
    c.Expr(q"_root_.com.abraxas.slothql.cypher.CypherFragment.Expr.Exists($pat)")
  }
}
