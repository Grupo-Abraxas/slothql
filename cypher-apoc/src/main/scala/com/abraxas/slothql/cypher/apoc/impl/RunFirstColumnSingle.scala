package com.abraxas.slothql.cypher.apoc.impl

import shapeless.{ ops, HList, Poly1, RecordArgs }

import com.abraxas.slothql.cypher.CypherFragment.{ Expr, Known }
import com.abraxas.slothql.cypher.syntax._


protected[apoc] class RunFirstColumnSingle[Params <: HList, ParamExprs <: HList, A](
    query: Match.ParameterizedQuery[Params, A],
    paramsToMap: ParamExprs => Map[String, Known[Expr[_]]]
  ) extends RecordArgs
{
  /** Requires the parameters to be `Known` */
  def withParamsRecord(params: ParamExprs): Known[Expr[A]] =
    "apoc.cypher.runFirstColumnSingle".func(lit(query.prepared.template), dict(paramsToMap(params)))
}

protected[apoc] object RunFirstColumnSingle {
  sealed trait Builder[Params <: HList, A] {
    type ParamExprs <: HList
    def apply(query: Match.ParameterizedQuery[Params, A]): RunFirstColumnSingle[Params, ParamExprs, A]
  }

  object Builder {
    type Aux[Params <: HList, ParamExprs0 <: HList, A] = Builder[Params, A] { type ParamExprs = ParamExprs0 }

    private object WrapCypherExprPoly extends Poly1 {
      implicit def impl[A]: Case.Aux[A, Known[Expr[A]]] = null
    }
    implicit def impl[Params <: HList, Exprs <: HList, A](
      implicit
      paramsExprs: ops.record.MapValues.Aux[WrapCypherExprPoly.type, Params, Exprs],
      paramsToMap: ops.record.ToMap.Aux[Exprs, Symbol, Known[Expr[_]]]
    ): Builder.Aux[Params, Exprs, A] =
      new Builder[Params, A] {
        type ParamExprs = Exprs
        def apply(query: Match.ParameterizedQuery[Params, A]): RunFirstColumnSingle[Params, Exprs, A] =
          new RunFirstColumnSingle(query, paramsToMap(_).map{ case (k, v) => k.name -> v })
      }
  }
}
