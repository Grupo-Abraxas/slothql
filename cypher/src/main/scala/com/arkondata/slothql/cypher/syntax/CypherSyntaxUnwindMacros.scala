package com.arkondata.slothql.cypher.syntax

import scala.reflect.macros.blackbox

import com.arkondata.slothql.cypher.{ CypherFragment => CF }

class CypherSyntaxUnwindMacros(override val c: blackbox.Context) extends CypherSyntaxPatternMacros(c) {
  import c.universe._

  def unwind[A: WeakTypeTag, R: WeakTypeTag](list: c.Expr[CF.Expr[List[A]]])(func: c.Expr[CF.Expr[A] => CF.Query.Query0[R]]): c.Expr[CF.Query.Query0[R]] =
    func.tree match {
      case Function(List(arg), body) =>
        val nme = arg.name.toString
        val name = c.freshName(nme)
        val rebind = Map(nme -> name)
        c.Expr[CF.Query.Query0[R]](
          q"""
            val ${TermName(name)} = _root_.com.arkondata.slothql.cypher.CypherFragment.Expr.Alias[${weakTypeOf[A]}]($nme)
            _root_.com.arkondata.slothql.cypher.CypherFragment.Query.Clause(
              _root_.com.arkondata.slothql.cypher.CypherFragment.Clause.Unwind($list, ${TermName(name)}),
              ${transformBody(rebind, body)}
            )
          """
        )
    }
}
