package com.arkondata.slothql.cypher

import shapeless.HList

import com.arkondata.slothql.cypher.syntax._

object APOC {
  def when[PT <: HList, PE <: HList, Ps <: HList, A](
    cond: Expr[Boolean],
    thenQuery: ParametrizedCypherQuery[PT, A],
    elseQuery: ParametrizedCypherQuery[PE, A]
  )(implicit b: apoc.When.Builder[PT, PE]): apoc.When.ParamsSyntax[b.Params, A] =
    new apoc.When.ParamsSyntax(cond, thenQuery, elseQuery)(b.toMap)
}
