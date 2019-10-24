package com.abraxas.slothql.cypher.syntax

import cats.data.NonEmptyList

import com.abraxas.slothql.cypher.CypherFragment.{ Clause, Expr, Known, Query }

object SetProp {
  def apply[R](e0: Internal.Set, es: Internal.Set*)(res: Match.Result[R]): Match.Result[R] =
    Match.Result.manually {
      val set = NonEmptyList(e0, es.toList).map{
        case Internal.Set(elem, key, value) =>
          Clause.SetProps.One(elem.asInstanceOf[Known[Expr[Map[String, Any]]]], key, value)
      }
      Query.Clause(Clause.SetProps(set), res.result)
    }

  protected[syntax] object Internal {
    case class Set(elem: Known[Expr[Graph.Atom]], key: String, value: Known[Expr[_]])
  }
}
