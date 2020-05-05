package com.arkondata.slothql.cypher.syntax

import shapeless.{ HList, ProductArgs }

import com.arkondata.slothql.cypher.ParameterizedCypherQuery
import com.arkondata.slothql.cypher.syntax.apoc._

object APOC {
  def when[PT <: HList, PE <: HList, Ps <: HList, A](
    cond: Expr[Boolean],
    thenQuery: ParameterizedCypherQuery[PT, A],
    elseQuery: ParameterizedCypherQuery[PE, A]
  )(implicit b: When.Builder[PT, PE]): When.ParamsSyntax[b.Params, A] =
    new When.ParamsSyntax(cond, thenQuery, elseQuery)(b.toMap)

  object `case` extends ProductArgs {
    def applyProduct[Cases <: HList](cases: Cases)(implicit b: Case.Builder[Cases]): Case.OtherwiseSyntax[b.Params, b.Out] =
      new Case.OtherwiseSyntax(b.toList(cases))
  }

}
