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
    val argNames0 = args.map(_.name.toString)
    val (argNames, binds, returns) = exprs.zip(argNames0).map {
      case ((tree, tpe), nme) =>
        val name = c.freshName(nme)
        val bindName = TermName(name)
        val bind = q"val $bindName = _root_.com.arkondata.slothql.newcypher.CypherFragment.Expr.Alias[$tpe]($nme)"
        val ret = q"_root_.com.arkondata.slothql.newcypher.CypherFragment.Return.Expr($tree, _root_.scala.Some($bindName))"
        ((nme, name), bind, ret)
    }.unzip3
    val returnTree = returns match {
      case Seq(single) => single
      case seq => q"_root_.com.arkondata.slothql.newcypher.CypherFragment.Return.Tuple(_root_.scala.List(..$seq))"
    }
    val newBody = transformBody(argNames.toMap, body)
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
