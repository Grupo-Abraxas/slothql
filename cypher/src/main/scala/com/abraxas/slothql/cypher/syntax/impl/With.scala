package com.abraxas.slothql.cypher.syntax.impl

import scala.language.implicitConversions
import scala.reflect.macros.blackbox

import com.abraxas.slothql.cypher.{ CypherFragment, syntax }
import com.abraxas.slothql.cypher.CypherFragment.{ Clause, Expr, Known, Query, Return }
import com.abraxas.slothql.cypher.syntax.{ Match, ReturnOps }

object With {
  def apply[R](wildcard: Boolean, ops: ReturnOps[Any] => ReturnOps[Any], exprs: Seq[Known[Return.Expr[_]]], res: Match.Result[R]): Match.Result[R] =
    Match.Result.manually {
      val ret0 = Return.Untyped.returns(wildcard = wildcard, exprs: _*).asInstanceOf[Return.Return0[R]]
      val ret = ops(ReturnOps(CypherFragment.Return.Wildcard)).copy(ret0).ret
      Query.Clause(Clause.With(ret, where = None), res.result)
    }

  def filter[R](f: Known[Expr[Boolean]], res: Match.Result[R]): Match.Result[R] =
    Match.Result.manually[R] {
      Query.Clause(Clause.With(Return.Wildcard.as[R], where = Some(f)), res.result)
    }

  sealed trait Var
  implicit object Var {
    final case class Expr(expr: Known[CypherFragment.Expr[_]]) extends Var
    final case class ReturnExpr(expr: Return.Expr[_])          extends Var
    final case class Name(name: String)                        extends Var
    final case class Names(names: Iterable[String])            extends Var

    implicit def wrapExpr[E <: CypherFragment.Expr[_]: CypherFragment](expr: E): Var = Expr(expr.known)
    implicit def wrapKnownExpr(expr: Known[CypherFragment.Expr[_]]): Var = Expr(expr)
    implicit def wrapReturnExpr(expr: Return.Expr[_]): Var = ReturnExpr(expr)
    implicit def wrapName(name: String): Var = Name(name)
    implicit def wrapNames(names: Iterable[String]): Var = Names(names)
  }

  protected[syntax] class WithMacros(val c: blackbox.Context) {
    def implFOV[R: c.WeakTypeTag](ops: c.Expr[ReturnOps[Any] => ReturnOps[Any]], vars: c.Expr[With.Var]*)
                                 (res: c.Expr[Match.Result[R]]): c.Expr[Match.Result[R]] =
      implWOV(c.universe.reify(false), ops, vars: _*)(res)
    def implTOV[R: c.WeakTypeTag](ops: c.Expr[ReturnOps[Any] => ReturnOps[Any]], var0: c.Expr[With.Var], vars: c.Expr[With.Var]*)
                                 (res: c.Expr[Match.Result[R]]): c.Expr[Match.Result[R]] =
      implWOV(c.universe.reify(true), ops, var0 +: vars: _*)(res)
    def implTO[R: c.WeakTypeTag](ops: c.Expr[ReturnOps[Any] => ReturnOps[Any]])
                                (res: c.Expr[Match.Result[R]]): c.Expr[Match.Result[R]] =
      implWOV(c.universe.reify(true), ops)(res)
    def implWV[R: c.WeakTypeTag](wildcard: c.Expr[Boolean], vars: c.Expr[With.Var]*)
                                (res: c.Expr[Match.Result[R]]): c.Expr[Match.Result[R]] =
      implWOV(wildcard, c.universe.reify(locally[ReturnOps[Any]]), vars: _*)(res)
    def implFV[R: c.WeakTypeTag](vars: c.Expr[With.Var]*)
                                (res: c.Expr[Match.Result[R]]): c.Expr[Match.Result[R]] =
      implWOV(c.universe.reify(false), c.universe.reify(locally[ReturnOps[Any]]), vars: _*)(res)
    def implTV[R: c.WeakTypeTag](vars: c.Expr[With.Var]*)
                                (res: c.Expr[Match.Result[R]]): c.Expr[Match.Result[R]] =
      implWOV(c.universe.reify(true), c.universe.reify(locally[ReturnOps[Any]]), vars: _*)(res)

    def implWOV[R: c.WeakTypeTag](wildcard: c.Expr[Boolean], ops: c.Expr[ReturnOps[Any] => ReturnOps[Any]], vars: c.Expr[With.Var]*)
                                 (res: c.Expr[Match.Result[R]]): c.Expr[Match.Result[R]] = {
      import c.universe._

      val isWildcard = wildcard.tree match {
        case q"${b: Boolean}" => b
        case t => c.abort(t.pos, "`wildcard` parameter must be literal boolean")
      }

      lazy val retExprs0 = vars.map(varTree => reify {
        def mkRet = CypherFragment.Return.Expr[Any](_: Known[CypherFragment.Expr[Any]], as = None).known
        def mkVarRet = CypherFragment.Expr.Var.apply[Any] _ andThen (_.known) andThen mkRet
        varTree.splice match {
          case With.Var.Expr(expr) => mkRet(expr) :: Nil
          case With.Var.ReturnExpr(expr) => expr.known :: Nil
          case With.Var.Name(name) => mkVarRet(name) :: Nil
          case With.Var.Names(names) => names.toSeq.map(mkVarRet)
        }
      })
      lazy val retExprs = c.Expr[Seq[Known[CypherFragment.Return.Expr[_]]]](q"_root_.scala.Seq(..$retExprs0).flatten")
      lazy val withExpr = reify { With[R](wildcard.splice, ops.splice, retExprs.splice, res.splice) }

      // // // check for unbound usage // // //
      lazy val wrapExprSymbol = typeOf[With.Var.type].decl(TermName("wrapExpr"))
      lazy val leftBound = vars.collect{
        case c.Expr(q"$func[$_]($expr)($_)") if func.symbol == wrapExprSymbol => expr.symbol
      }
      def checkForUnboundUsage(tree: Tree, bound: Set[Symbol]): Unit =
        if (!isWildcard && vars.nonEmpty) {
          val forbidden = bound diff leftBound.toSet[c.universe.Symbol]
          val t = new Traverser {
            override def traverse(tree: c.universe.Tree): Unit =
              if (forbidden contains tree.symbol) c.abort(tree.pos, s"Variable unbound by `with`: ${tree.symbol}")
              else super.traverse(tree)
          }
          t.traverse(tree)
        }
      syntax.Match.MacrosInternal.CheckForUnboundUsage +:= res.tree -> checkForUnboundUsage _

      // do not build empty `WITH`, return initial result instead
      if (!isWildcard && vars.isEmpty) res else withExpr
    }
  }
}
