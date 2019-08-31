package com.abraxas.slothql.cypher.syntax.impl

import scala.reflect.macros.blackbox

import com.abraxas.slothql.cypher.CypherFragment
import com.abraxas.slothql.cypher.CypherFragment.{ Expr, Known, Query }
import com.abraxas.slothql.cypher.syntax.Match

object Unwind {
  def apply[A, R](exprs: Known[Expr[Seq[A]]], alias: String, next: Known[Query.Query0[R]]): Match.Result[R] =
    Match.Result.manually {
      Query.Clause(CypherFragment.Clause.Unwind(exprs, as = alias), next)
    }

  def instanceImpl[A: c.WeakTypeTag, R: c.WeakTypeTag]
                  (c: blackbox.Context)
                  (expr: c.Expr[Known[Expr[Seq[A]]]])
                  (f: c.Expr[Expr.Var[A] => Match.Result[R]])
                  : c.Expr[Match.Result[R]] = {
    import c.universe._
    val paramName = (f.tree: @unchecked) match {
      case Function(ValDef(_, name, _, _) :: Nil, _) => name.toString
    }
    val rowExprTree = q"_root_.com.abraxas.slothql.cypher.CypherFragment.Expr.Var[${weakTypeOf[A]}]($paramName)"
    val rowExpr = c.Expr[CypherFragment.Expr.Var[A]](rowExprTree)
    reify {
      Unwind[A, R](
        expr.splice,
        c.Expr[String](Literal(Constant(paramName))).splice,
        f.splice(rowExpr.splice).result
      )
    }
  }
}
