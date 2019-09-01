package com.abraxas.slothql.cypher.apoc

import shapeless.{ HList, ProductArgs }

import com.abraxas.slothql.cypher.CypherFragment.{ Expr, Known }
import com.abraxas.slothql.cypher.apoc.impl._
import com.abraxas.slothql.cypher.syntax._

object APOC {
  def typeOf(expr: Known[Expr[_]]): Known[Expr[String]] = Symbol("apoc.meta.cypher.type").func(expr)

  object `case` extends ProductArgs {
    def applyProduct[Cases <: HList](cases: Cases)(implicit b: Case.Builder[Cases]): Case.OtherwiseSyntax[b.Params, b.Out] =
      new Case.OtherwiseSyntax(b.toList(cases))
  }
}
