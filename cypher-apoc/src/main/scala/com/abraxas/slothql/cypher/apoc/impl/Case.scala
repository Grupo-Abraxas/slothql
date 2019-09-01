package com.abraxas.slothql.cypher.apoc.impl

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

import shapeless.{ Coproduct, HList, HNil, Poly1, Poly2, RecordArgs, Unpack1, ops }

import com.abraxas.slothql.cypher.CypherFragment
import com.abraxas.slothql.cypher.CypherFragment.{ Expr, Known }
import com.abraxas.slothql.cypher.syntax._


case class Case[Params <: HList, A](condition: Known[Expr[Boolean]], query: Match.ParameterizedQuery[Params, A])

object Case {
  import com.abraxas.slothql.cypher.apoc.impl.{ Case => CaseApoc }

  protected[apoc] trait Builder[Cases <: HList] {
    type Out
    type Params <: HList
    def toList(cases: Cases): List[Case[_, Out]]
  }
  protected[apoc] object Builder {
    type Aux[Cases <: HList, Out0, Params0 <: HList] = Builder[Cases]{ type Out = Out0; type Params = Params0 }

    private object BuildCasePoly extends Poly1 {
      private type PCQ[A] = Match.ParameterizedQuery[_, A]

      implicit def asIs[A, Params <: HList, R](
        implicit asCase: A <:< CaseApoc[Params, R]
      ): Case.Aux[A, CaseApoc[Params, R]] = at[A](asCase)

      implicit def preKnownConditionPair[A, A1, A2, Params <: HList, R, X](
        implicit
        pair: A <:< (A1, A2),
        condA1: A1 <:< Known[Expr[Boolean]],
        asCaseA2: A2 <:< Match.ParameterizedQuery[Params, _],
        unpack: Unpack1[A2, PCQ, R],
      ): Case.Aux[A, CaseApoc[Params, R]] =
        at[A](p => CaseApoc(p._1, p._2.asInstanceOf[Match.ParameterizedQuery[Params, R]]))

      implicit def knownConditionPair[A, A1, A2, Params <: HList, R](
        implicit
        pair: A <:< (A1, A2),
        condA1: A1 <:< Expr[Boolean],
        fragA1: CypherFragment[A1],
        asCaseA2: A2 <:< Match.ParameterizedQuery[Params, _],
        unpack: Unpack1[A2, PCQ, R],
      ): Case.Aux[A, CaseApoc[Params, R]] =
        at[A](p => CaseApoc(Known(p._1).widen[Expr[Boolean]], p._2.asInstanceOf[Match.ParameterizedQuery[Params, R]]))
    }

    private object MergeParamsPoly extends Poly2 {
      implicit def impl[Params0 <: HList, Params <: HList, R](
        implicit merge: ops.record.Merger[Params0, Params]
      ): Case.Aux[Params0, CaseApoc[Params, R], merge.Out] = null
    }

    private object ExtractCaseOut extends Poly1 {
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

  private object WrapCypherExprPoly extends Poly1 {
    implicit def impl[A]: Case.Aux[A, Known[Expr[A]]] = null
  }

  protected[apoc] class OtherwiseSyntax[CasesParams <: HList, A](cases: List[Case[_, A]]) {
    def otherwise[OtherwiseParams <: HList, AllParams <: HList, ParamExprs <: HList]
        (default: Match.ParameterizedQuery[OtherwiseParams, A])
        (implicit mergeParams: ops.record.Merger.Aux[CasesParams, OtherwiseParams, AllParams],
                  paramsExprs: ops.record.MapValues.Aux[WrapCypherExprPoly.type, AllParams, ParamExprs],
                  paramsToMap: ops.record.ToMap.Aux[ParamExprs, Symbol, Known[Expr[_]]]
        ): ParamsSyntax[ParamExprs, A] = new ParamsSyntax(cases, default)
  }

  protected[apoc] class ParamsSyntax[ParamExprs <: HList, A]
                        (cases: Seq[Case[_, A]], default: Match.ParameterizedQuery[_, A])
                        (implicit toMap: ops.record.ToMap.Aux[ParamExprs, Symbol, Known[Expr[_]]]) extends RecordArgs {
    def withParamsRecord(params: ParamExprs): QuerySyntax[A] =
      new QuerySyntax(cases, default, toMap(params).map{ case (k, v) => k.name -> v })
  }

  protected[apoc] class QuerySyntax[A](
      protected val cases: Seq[Case[_, A]],
      protected val default: Match.ParameterizedQuery[_, A],
      protected val params: Map[String, Known[Expr[_]]]
  ) {
    def apply[R](f: Known[Expr[A]] => Match.Result[R]): Match.Result[R] = macro QuerySyntax.applyImpl[A, R]
  }
  object QuerySyntax {
    def applyImpl[A: c.WeakTypeTag, R: c.WeakTypeTag](c: whitebox.Context)(f: c.Tree): c.Tree = {
      import c.universe._

      val A = weakTypeOf[A]
      val R = weakTypeOf[R]
      val Function(List(arg), _) = f

      val name = arg.name.decodedName.toString
      val resultTree = q"$f(_root_.com.abraxas.slothql.cypher.CypherFragment.Expr.Var[$A](${Literal(Constant(name))}))"
      q"_root_.com.abraxas.slothql.cypher.apoc.impl.Case.QuerySyntax.mk[$R](${c.prefix.tree}, $name, $resultTree)"
    }

    def mk[R](syntax: QuerySyntax[_], valueAlias: String, res: Match.Result[R]): Match.Result[R] = {
      val cases = syntax.cases.flatMap {
        case Case(condition, query) => condition :: knownLit(query.prepared.template) :: Nil
      }
      impl.Call(
        "apoc.case",
        List(
          list(cases: _*),                       // [cond, query, ...]
          lit(syntax.default.prepared.template), // else
          dict(syntax.params)                    // params
        ),
        List(Expr.Var[Any]("value").as(valueAlias).known),
        res
      )
    }
  }
}
