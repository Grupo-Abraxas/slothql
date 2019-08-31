package com.abraxas.slothql.cypher.syntax.impl

import scala.reflect.macros.whitebox

import com.abraxas.slothql.cypher.CypherFragment
import com.abraxas.slothql.cypher.CypherFragment.{ Clause, Expr, Known, Query, Return }
import com.abraxas.slothql.cypher.syntax.{ Match, ProcedureStringOps, ProcedureSymbolOps }

object Call {
  def apply[R](procedure: String, params: List[Known[Expr[_]]], outputs: List[Known[Return.Expr[_]]], res: Match.Result[R]): Match.Result[R] =
    Match.Result.manually {
      val out = Return.UntypedExpressions.returnsUnsafe(outputs)
      Query.Clause(Clause.Call(procedure, params, Some(out), None), res.result)
    }
  def noAlias[R](procedure: String, params: List[Known[Expr[_]]], outputs: List[Known[Expr[_]]], res: Match.Result[R]): Match.Result[R] =
    apply(procedure, params, outputs.map(Return.Expr[Any](_, as = None).known), res)

  def impl(c: whitebox.Context)(f: c.Tree): c.Tree = {
    import c.universe._

    val VarSymbol = symbolOf[CypherFragment.Expr.Var[_]]
    val ProcedureStringOpsType = typeOf[ProcedureStringOps]
    val ProcedureSymbolOpsType = typeOf[ProcedureSymbolOps]

    f match {
      case Function(params, _) =>
        val paramTrees = params.map{ p =>
          val tpe = p.tpt.tpe.dealias match {
            case TypeRef(_, VarSymbol, List(t)) => t
            case other => c.abort(p.pos, s"`yielding` arguments must be of type `Expr.Var[?]`, got $other")
          }
          q"_root_.com.abraxas.slothql.cypher.CypherFragment.Expr.Var[$tpe](${p.name.decodedName.toString})"
        }
        val (procedure, params0Trees) = c.prefix.tree match {
          case q"$ops($procedure).call(..$args)" if ops.tpe.resultType =:= ProcedureStringOpsType =>
            procedure -> args
          case q"$ops($procedure).call(..$args)" if ops.tpe.resultType =:= ProcedureSymbolOpsType =>
            q"$procedure.name" -> args
        }
        q"""
          _root_.com.abraxas.slothql.cypher.syntax.impl.Call.noAlias(
            $procedure,
            _root_.scala.List(..$params0Trees),
            _root_.scala.List(..${paramTrees.map(t => q"$t.known")}),
            $f(..$paramTrees)
          )
         """
      case _ =>
        c.abort(c.enclosingPosition, "Expecting a function (Var[A1], Var[A2], ...) => Match.Result[R]")
    }
  }
}
