package com.arkondata.slothql.cypher.apoc

import shapeless._

import com.arkondata.slothql.cypher.ParametrizedCypherQuery
import com.arkondata.slothql.cypher.syntax._

object When {
  protected[cypher] trait Builder[ParamsThen <: HList, ParamsElse <: HList] {
    type Params <: HList

    val toMap: ops.record.ToMap.Aux[Params, Symbol, Expr[_]]
  }

  object Builder {
    type Aux[ParamsThen <: HList, ParamsElse <: HList, Ps <: HList] = Builder[ParamsThen, ParamsElse] { type Params = Ps }

    implicit def builder[PT <: HList, PE <: HList, Ps0 <: HList, Ps <: HList](
      implicit
      merge: ops.record.Merger.Aux[PT, PE, Ps0],
      exprs: ops.record.MapValues.Aux[WrapExprHF.type, Ps0, Ps],
      map: ops.record.ToMap.Aux[Ps, Symbol, Expr[_]]
    ): Builder.Aux[PT, PE, Ps] = new Builder[PT, PE] {
      type Params = Ps
      val toMap: ops.record.ToMap.Aux[Ps, Symbol, Expr[_]] = map
    }

    object WrapExprHF extends Poly1 {
      implicit def impl[A]: Case.Aux[A, Expr[A]] = null
    }
  }

  protected[cypher] class ParamsSyntax[ParamExprs <: HList, A]
                        (cond: Expr[Boolean], thenQ: ParametrizedCypherQuery[_, A], elseQ: ParametrizedCypherQuery[_, A])
                        (implicit toMap: ops.record.ToMap.Aux[ParamExprs, Symbol, Expr[_]]) extends RecordArgs {
    def withParamsRecord(params: ParamExprs): QuerySyntax[A] =
      new QuerySyntax(cond, thenQ, elseQ, toMap(params).map{ case (k, v) => k.name -> v })
  }

  protected[cypher] class QuerySyntax[A](
      protected val cond: Expr[Boolean],
      protected val thenQ: ParametrizedCypherQuery[_, A],
      protected val elseQ: ParametrizedCypherQuery[_, A],
      protected val params: Map[String, Expr[_]]
  ) {
    def continue[R](f: Expr[A] => Query[R]): Query[R] =
      Call("apoc.when",
        cond,
        lit(thenQ.statement.template),
        lit(elseQ.statement.template),
        dict(params)
      ).yielding("value") { (yielded: Expr[Map[String, Any]]) =>
          With(**, yielded.value[A](yielded.keys.head)) { v =>
            f(v)
          }}
  }
}
