package com.arkondata.slothql.cypher.syntax

import scala.reflect.macros.whitebox

class CypherSyntaxCallMacros(override val c: whitebox.Context) extends CypherSyntaxPatternMacros(c){
  import c.universe._

  def void[R: WeakTypeTag](res: Tree): Tree =
    yieldImpl[R](res)
  def yield1[A1: WeakTypeTag, R: WeakTypeTag](yields1: Tree)(res: Tree): Tree =
    yieldImpl[R](res, yields1 -> weakTypeOf[A1])
  def yield2[A1: WeakTypeTag, A2: WeakTypeTag, R: WeakTypeTag](yields1: Tree, yields2: Tree)(res: Tree): Tree =
    yieldImpl[R](res, yields1 -> weakTypeOf[A1], yields2 -> weakTypeOf[A2])
  def yield3[A1: WeakTypeTag, A2: WeakTypeTag, A3: WeakTypeTag, R: WeakTypeTag](yields1: Tree, yields2: Tree, yields3: Tree)(res: Tree): Tree =
    yieldImpl[R](res, yields1 -> weakTypeOf[A1], yields2 -> weakTypeOf[A2], yields3 -> weakTypeOf[A3])

  protected def yieldImpl[R: WeakTypeTag](func: Tree, yieldsT: (Tree, Type)*): Tree = {
    val (procedure, params) = c.prefix.tree match {
      case q"$obj.apply($proc, ..$args)" if obj.symbol == CallSymbol => proc -> args
    }
    val (binds, yields, next) = func match {
      case Function(yieldsV, body) if yieldsT.nonEmpty =>
        val (rebind, binds, rets) = yieldsT.zip(yieldsV).map {
          case ((nme, tpe), ValDef(_, TermName(alias), _, _)) =>
            val name = c.freshName(alias)
            val bind = q"val ${TermName(name)} = _root_.com.arkondata.slothql.cypher.CypherFragment.Expr.Alias[$tpe]($alias)"
            val ret  =
              q"""
                _root_.com.arkondata.slothql.cypher.CypherFragment.Return.Expr[$tpe](
                  expr = _root_.com.arkondata.slothql.cypher.CypherFragment.Expr.Alias.Fixed($nme),
                  as = _root_.scala.Some(${TermName(name)})
                )
              """
            (alias -> name, bind, ret)
        }.unzip3
        val newBody = transformBody(rebind.toMap, body)
        val yields = q"_root_.scala.Some(_root_.com.arkondata.slothql.cypher.CypherFragment.Return.Tuple(_root_.scala.List(..$rets)))"
        (binds, yields, newBody)
      case body if yieldsT.isEmpty =>
        (Nil, q"_root_.scala.None", body)
    }
    q"""
    ..$binds
    _root_.com.arkondata.slothql.cypher.CypherFragment.Query.Clause(
      _root_.com.arkondata.slothql.cypher.CypherFragment.Clause.Call(
        procedure = $procedure,
        params = _root_.scala.List(..$params),
        yields = $yields,
        where = _root_.scala.None
      ),
      $next
    )
  """
  }

  private val CallSymbol = rootMirror.staticModule("com.arkondata.slothql.cypher.syntax.Call")
}
