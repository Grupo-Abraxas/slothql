package com.arkondata.slothql.newcypher.syntax

import scala.reflect.macros.blackbox

import com.arkondata.slothql.newcypher.{ CypherFragment => CF }

class CypherSyntaxWithMacros(override val c: blackbox.Context) extends CypherSyntaxPatternMacros(c) {
  import c.universe._

  def with1[T1: WeakTypeTag, R: WeakTypeTag](t1: c.Expr[CF.Expr[T1]])
           (query: c.Expr[CF.Expr[T1] => CF.Query.Query0[R]]): c.Expr[CF.Query.Query0[R]] =
    withImpl(query.tree, t1.tree -> weakTypeOf[T1])

  def with2[T1: WeakTypeTag, T2: WeakTypeTag, R: WeakTypeTag](t1: c.Expr[CF.Expr[T1]], t2: c.Expr[CF.Expr[T2]])
           (query: c.Expr[(CF.Expr[T1], CF.Expr[T2] )=> CF.Query.Query0[R]]): c.Expr[CF.Query.Query0[R]] =
    withImpl(query.tree, t1.tree -> weakTypeOf[T1], t2.tree -> weakTypeOf[T2])

  protected type ExprWithInnerType = (Tree, Type)
  protected def withImpl[R](queryFunc: Tree, expr0: ExprWithInnerType, exprs0: ExprWithInnerType*): c.Expr[CF.Query.Query0[R]] = {
    val exprs = expr0 +: exprs0
    val Function(args, body) = queryFunc
    val argNames = args.map(_.name.toString)
    val (binds, returns) = exprs.zip(argNames).map {
      case ((tree, tpe), name) =>
        val bindName = TermName(name)
        val bind = q"val $bindName = _root_.com.arkondata.slothql.newcypher.CypherFragment.Expr.Alias[$tpe]($name)"
        val ret = q"_root_.com.arkondata.slothql.newcypher.CypherFragment.Return.Expr($tree, _root_.scala.Some($bindName))"
        bind -> ret
    }.unzip
    val returnTree = returns match {
      case Seq(single) => single
      case seq => q"_root_.com.arkondata.slothql.newcypher.CypherFragment.Return.Tuple(_root_.scala.List(..$seq))"
    }
    val newBody = transformBody(argNames, body)
    c.Expr[CF.Query.Query0[R]](
      q"""
        ..$binds
        _root_.com.arkondata.slothql.newcypher.CypherFragment.Query.Clause(
          _root_.com.arkondata.slothql.newcypher.CypherFragment.Clause.With(
            $returnTree,
            _root_.scala.None
          ),
          $newBody
        )
      """
    )
  }
}
