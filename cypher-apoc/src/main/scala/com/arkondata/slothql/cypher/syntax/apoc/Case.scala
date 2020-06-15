package com.arkondata.slothql.cypher.syntax.apoc

import shapeless._

import com.arkondata.slothql.cypher.ParameterizedCypherQuery
import com.arkondata.slothql.cypher.syntax._

case class Case[Params <: HList, A](condition: Expr[Boolean], query: ParameterizedCypherQuery[Params, A])

object Case {
  import com.arkondata.slothql.cypher.syntax.apoc.{ Case => CaseApoc }

  protected[cypher] trait Builder[Cases <: HList] {
    type Out
    type Params <: HList
    def toList(cases: Cases): List[Case[_, Out]]
  }
  protected[cypher] object Builder {
    type Aux[Cases <: HList, Out0, Params0 <: HList] = Builder[Cases]{ type Out = Out0; type Params = Params0 }

    object BuildCasePoly extends Poly1 {
      private type PCQ[A] = ParameterizedCypherQuery[_, A]

      implicit def asIs[A, Params <: HList, R](
        implicit asCase: A <:< CaseApoc[Params, R]
      ): Case.Aux[A, CaseApoc[Params, R]] = at[A](asCase)

      implicit def conditionPair[A, A1, A2, Params <: HList, R, X](
        implicit
        pair: A <:< (A1, A2),
        condA1: A1 <:< Expr[Boolean],
        asCaseA2: A2 <:< ParameterizedCypherQuery[Params, _],
        unpack: Unpack1[A2, PCQ, R]
      ): Case.Aux[A, CaseApoc[Params, R]] =
        at[A](p => CaseApoc(p._1, p._2.asInstanceOf[ParameterizedCypherQuery[Params, R]]))

    }

    object MergeParamsPoly extends Poly2 {
      implicit def impl[Params0 <: HList, Params <: HList, R](
        implicit merge: ops.record.Merger[Params0, Params]
      ): Case.Aux[Params0, CaseApoc[Params, R], merge.Out] = null
    }

    object ExtractCaseOut extends Poly1 {
      implicit def impl[Params <: HList, R]: Case.Aux[CaseApoc[Params, R], R] = null
    }

    implicit def builderImpl[Cases0 <: HList, Cases <: HList, Ps <: HList, Rs <: HList, Rc <: Coproduct, R](
      implicit
      cases: ops.hlist.Mapper.Aux[BuildCasePoly.type, Cases0, Cases],
      casesToList: ops.hlist.ToTraversable.Aux[Cases, List, Case[_, _]],
      params: ops.hlist.LeftFolder.Aux[Cases, HNil, MergeParamsPoly.type, Ps],
      outs: ops.hlist.Mapper.Aux[ExtractCaseOut.type, Cases, Rs],
      outc: ops.hlist.ToCoproduct.Aux[Rs, Rc], // TODO: not efficient - converting HList to Coproduct just to unify it
      out: ops.coproduct.Unifier.Aux[Rc, R]    // TODO: not efficient - converting HList to Coproduct just to unify it
    ): Builder.Aux[Cases0, R, Ps] =
      new Builder[Cases0] {
        type Out = R
        type Params = Ps

        def toList(cases0: Cases0): List[Case[_, R]] = casesToList(cases(cases0)).asInstanceOf[List[Case[_, R]]]
      }
  }

  object WrapCypherExprPoly extends Poly1 {
    implicit def impl[A]: Case.Aux[A, Expr[A]] = null
  }

  protected[cypher] class OtherwiseSyntax[CasesParams <: HList, A](cases: List[Case[_, A]]) {
    def otherwise[OtherwiseParams <: HList, AllParams <: HList, ParamExprs <: HList]
        (default: ParameterizedCypherQuery[OtherwiseParams, A])
        (implicit mergeParams: ops.record.Merger.Aux[CasesParams, OtherwiseParams, AllParams],
                  paramsExprs: ops.record.MapValues.Aux[WrapCypherExprPoly.type, AllParams, ParamExprs],
                  paramsToMap: ops.record.ToMap.Aux[ParamExprs, Symbol, Expr[_]]
        ): ParamsSyntax[ParamExprs, A] = new ParamsSyntax(cases, default)
  }

  protected[cypher] class ParamsSyntax[ParamExprs <: HList, A]
                        (cases: Seq[Case[_, A]], default: ParameterizedCypherQuery[_, A])
                        (implicit toMap: ops.record.ToMap.Aux[ParamExprs, Symbol, Expr[_]]) extends RecordArgs {
    def withParamsRecord(params: ParamExprs): QuerySyntax[A] =
      new QuerySyntax(cases, default, toMap(params).map{ case (k, v) => k.name -> v })
  }

  protected[cypher] class QuerySyntax[A](
      protected val cases0: Seq[Case[_, A]],
      protected val default: ParameterizedCypherQuery[_, A],
      protected val params: Map[String, Expr[_]]
  ) {
    private val cases = cases0.flatMap(c => c.condition :: lit(c.query.statement.template) :: Nil)

    def withOneColumn[R](f: Expr[A] => Query[R]): Query[R] =
      withAllColumns { yielded =>
        With(**, yielded.value(yielded.keys.head)) { v =>
          f(v)
        }
      }

    def withAllColumns[R](f: Expr[Map[String, Any]] => Query[R]): Query[R] =
      Call("apoc.case",
        list(cases: _*),                 // [cond, query, ...]
        lit(default.statement.template), // else
        dict(params)                     // params
      ).yielding("value") { yielded: Expr[Map[String, Any]] =>
        f(yielded)
      }

  }

}