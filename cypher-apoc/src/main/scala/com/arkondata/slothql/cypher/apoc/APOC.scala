package com.arkondata.slothql.cypher.apoc

import shapeless.{ HList, ProductArgs }

import com.arkondata.slothql.cypher.CypherFragment.{ Expr, Known }
import com.arkondata.slothql.cypher.apoc.impl._
import com.arkondata.slothql.cypher.syntax._

object APOC {
  def typeOf(expr: Known[Expr[_]]): Known[Expr[String]] = Symbol("apoc.meta.cypher.type").func(expr)

  object `case` extends ProductArgs {
    def applyProduct[Cases <: HList](cases: Cases)(implicit b: Case.Builder[Cases]): Case.OtherwiseSyntax[b.Params, b.Out] =
      new Case.OtherwiseSyntax(b.toList(cases))
  }

  def runFirstColumnSingle[A](query: Match.Result[A]): Known[Expr[A]] =
    "apoc.cypher.runFirstColumnSingle".func(lit(query.result.known.toCypher), cypherNull[Any])

  def runFirstColumnSingle[Params <: HList, A](query: Match.ParameterizedQuery[Params, A])
                                              (implicit b: RunFirstColumnSingle.Builder[Params, A])
      : RunFirstColumnSingle[Params, b.ParamExprs, A] = b(query)

  def failingIf[R](cond: Known[Expr[Boolean]], message: Known[Expr[String]], params: Known[Expr[_]]*)(res: Match.Result[R]): Match.Result[R] =
    "apoc.util.validate".call(cond, message, list[Any](params: _*)).void { res }
}
