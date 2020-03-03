package com.abraxas.slothql.newcypher.syntax

import scala.reflect.macros.whitebox

import com.abraxas.slothql.newcypher.{ CypherFragment => CF }

class CypherSyntaxMacros(val c: whitebox.Context) {
  import c.universe._

  def returnTuple[T: WeakTypeTag]: Tree = {
    val T = weakTypeOf[T]

    val tupleTypes = (T: @unchecked) match {
      case TypeRef(ScalaPackageType, s, tuple) if s.name.toString matches """Tuple\d+""" => tuple
    }
    val arg = c.internal.reificationSupport.freshTermName("t")
    def sel(i: Int) = q"$arg.${TermName(s"_${i + 1}")}"
    val (outTypes, returns) = tupleTypes.zipWithIndex.map {
      case (TypeRef(_, ExprSymb, List(t)), i)       => t -> returnExprTree(t, sel(i))
      case (TypeRef(_, ReturnExprSymb, List(t)), i) => t -> sel(i)
      case other => c.abort(c.enclosingPosition, s"Returning type $other is not supported")
    }.unzip
    q"""
      new _root_.com.abraxas.slothql.newcypher.syntax.CypherSyntaxReturnTuple[$T] {
        type Out = ${mkTupleType(outTypes)}
        def apply($arg: $T): _root_.com.abraxas.slothql.newcypher.CypherFragment.Return[Out] =
          _root_.com.abraxas.slothql.newcypher.CypherFragment.Return.Tuple[Out](_root_.scala.List(..$returns))
      }
     """
  }

  private def returnExprTree(tpe: Type, expr: Tree) =
    q"_root_.com.abraxas.slothql.newcypher.CypherFragment.Return.Expr[$tpe]($expr, as = _root_.scala.None)"

  protected lazy val ScalaPackageType = c.internal.thisType(rootMirror.staticPackage("scala").moduleClass)

  protected lazy val ExprSymb       = symbolOf[CF.Expr[_]]
  protected lazy val ReturnExprSymb = symbolOf[CF.Return.Expr[_]]

  protected def mkTupleType(types: List[Type]): Type = {
    val n = types.length
    val tupleSymb = ScalaPackageType.decl(TypeName(s"Tuple$n")).asClass
    c.internal.typeRef(ScalaPackageType, tupleSymb, types)
  }
}
