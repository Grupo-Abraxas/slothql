package com.arkondata.slothql02.cypher.syntax

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

import cats.data.NonEmptyList

import com.arkondata.slothql02.cypher.CypherFragment.{ Clause, Query }
import com.arkondata.slothql02.cypher.syntax.Match.OptionalResult

object Create {
  def apply[R](f: Graph => OptionalResult[R]): Query[R] = macro Internal.impl[R]

  object Internal {
    def impl[R: c.WeakTypeTag](c: whitebox.Context)(f: c.Expr[Graph => OptionalResult[R]]): c.Expr[Query[R]] = {
      val m = new Match.InternalImpl[c.type](c)
      import c.universe._
      m.mkClause(f) {
        case (_, Some(guardPos), _) => c.abort(guardPos, "No `if` guard is allowed at Create")
        case (pattern, _, _) =>
          reify {
            Clause.Create(NonEmptyList(pattern.splice, Nil))
          }
      }
    }
  }
}
