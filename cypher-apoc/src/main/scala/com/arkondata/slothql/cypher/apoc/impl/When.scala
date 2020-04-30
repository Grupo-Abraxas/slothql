package com.arkondata.slothql.cypher.apoc.impl

import shapeless.{ HList, Poly1, RecordArgs, ops }

import com.arkondata.slothql.cypher.CypherFragment.{ Expr, Known }
import com.arkondata.slothql.cypher.syntax.Match.ParameterizedQuery
import com.arkondata.slothql.cypher.syntax._

object When {
  protected[apoc] trait Builder[ParamsThen <: HList, ParamsElse <: HList] {
    type Params <: HList

    val toMap: ops.record.ToMap.Aux[Params, Symbol, Known[Expr[_]]]
  }

  object Builder {
    type Aux[ParamsThen <: HList, ParamsElse <: HList, Ps <: HList] = Builder[ParamsThen, ParamsElse] { type Params = Ps }

    implicit def builder[PT <: HList, PE <: HList, Ps0 <: HList, Ps <: HList](
      implicit
      merge: ops.record.Merger.Aux[PT, PE, Ps0],
      wrap: ops.record.MapValues.Aux[WrapKnownExprNull.type, Ps0, Ps],
      map: ops.record.ToMap.Aux[Ps, Symbol, Known[Expr[_]]]
    ): Builder.Aux[PT, PE, Ps] = new Builder[PT, PE] {
      type Params = Ps
      val toMap: ops.record.ToMap.Aux[Ps, Symbol, Known[Expr[_]]] = map
    }

    private object WrapKnownExprNull extends Poly1 {
      implicit def wrap[A]: Case.Aux[A, Known[Expr[A]]] = at(_ => null)
    }
  }

  protected[apoc] class ParamsSyntax[ParamExprs <: HList, A]
                        (cond: Known[Expr[Boolean]], thenQ: ParameterizedQuery[_, A], elseQ: Match.ParameterizedQuery[_, A])
                        (implicit toMap: ops.record.ToMap.Aux[ParamExprs, Symbol, Known[Expr[_]]]) extends RecordArgs {
    def withParamsRecord(params: ParamExprs): QuerySyntax[A] =
      new QuerySyntax(cond, thenQ, elseQ, toMap(params).map{ case (k, v) => k.name -> v })
  }

  protected[apoc] class QuerySyntax[A](
      protected val cond: Known[Expr[Boolean]],
      protected val thenQ: ParameterizedQuery[_, A],
      protected val elseQ: Match.ParameterizedQuery[_, A],
      protected val params: Map[String, Known[Expr[_]]]
  ) {
    def apply[R](f: Known[Expr[A]] => Match.Result[R]): Match.Result[R] =
      "apoc.when".call(cond, lit(thenQ.prepared.template), lit(elseQ.prepared.template), dict(params)).yielding {
        value: Expr.Var[Map[String, Any]] => f(value.value(value.keys.at(lit(0))))
      }
  }
}
