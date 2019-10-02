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

  def void[R](procedure: String, params: List[Known[Expr[_]]], res: Match.Result[R]): Match.Result[R] =
    Match.Result.manually {
      Query.Clause(Clause.Call(procedure, params, None, None), res.result)
    }
}

class Call(val c: whitebox.Context) {
  import c.universe._

  def impl(f: Tree): Tree = {
    val aTrees = argTrees(f)
    q"""
      _root_.com.abraxas.slothql.cypher.syntax.impl.Call.noAlias(
        $procedure,
        _root_.scala.List(..$params0Trees),
        _root_.scala.List(..${aTrees.map(t => q"$t.known")}),
        $f(..$aTrees)
      )
    """
  }

  def implAs(outputs: Tree*)(f: Tree): Tree = {
    val aTrees = argTrees(f)
    if (outputs.size != aTrees.length) c.abort(f.pos,
      s"The number of specified outputs (${outputs.size}) doesn't correspond to function arguments ${aTrees.length}")

    val returns = aTrees zip outputs map {
      case (arg, q"scala.Symbol.apply(${name: String})") =>
        val outExpr = mkVar(typeOf[Any], name)
        val asExpr = q"_root_.scala.Some($arg.name)"
        q"_root_.com.abraxas.slothql.cypher.CypherFragment.Return.Expr[_root_.scala.Any]($outExpr, as = $asExpr).known"
    }
    q"""
      _root_.com.abraxas.slothql.cypher.syntax.impl.Call(
        $procedure,
        _root_.scala.List(..$params0Trees),
        _root_.scala.List(..$returns),
        $f(..$aTrees)
      )
    """
  }

  private def argTrees(f: Tree) = f match {
    case Function(params, _) =>
      params.map{ p =>
        val tpe = p.tpt.tpe.dealias match {
          case TypeRef(_, VarSymbol, List(t)) => t
          case other => c.abort(p.pos, s"`yielding` arguments must be of type `Expr.Var[?]`, got $other")
        }
        mkVar(tpe, p.name)
      }
    case _ =>
      c.abort(c.enclosingPosition, "Expecting a function (Var[A1], Var[A2], ...) => Match.Result[R]")
  }
  private def mkVar(tpe: Type, name: Name): Tree = mkVar(tpe, name.decodedName.toString)
  private def mkVar(tpe: Type, name: String): Tree =
    q"_root_.com.abraxas.slothql.cypher.CypherFragment.Expr.Var[$tpe](${name})"

  private lazy val (procedure, params0Trees) = c.prefix.tree match {
    case q"$ops($procedure).call(..$args)" if ops.tpe.resultType =:= ProcedureStringOpsType =>
      procedure -> args
    case q"$ops($procedure).call(..$args)" if ops.tpe.resultType =:= ProcedureSymbolOpsType =>
      q"$procedure.name" -> args
  }

  private val VarSymbol = symbolOf[CypherFragment.Expr.Var[_]]
  private val ProcedureStringOpsType = typeOf[ProcedureStringOps]
  private val ProcedureSymbolOpsType = typeOf[ProcedureSymbolOps]

}
