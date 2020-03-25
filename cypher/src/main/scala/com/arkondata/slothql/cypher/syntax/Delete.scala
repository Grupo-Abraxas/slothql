package com.arkondata.slothql.cypher.syntax

import cats.data.NonEmptyList

import com.arkondata.slothql.cypher.CypherFragment.{ Clause, Expr, Known, Query }

object Delete {
  def apply[R](e0: Known[Expr[Graph.Atom]], es: Known[Expr[Graph.Atom]]*)(res: Match.OptionalResult[R]): Match.Result[R] =
    Match.Result.manually {
      Query.Clause(Clause.Delete(NonEmptyList(e0, es.toList)), res.resultOrNothing)
    }
}
