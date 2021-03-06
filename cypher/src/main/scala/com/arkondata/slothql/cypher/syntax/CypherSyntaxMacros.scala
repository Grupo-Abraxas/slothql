package com.arkondata.slothql.cypher.syntax

import scala.reflect.macros.whitebox

import com.arkondata.slothql.cypher.{ CypherFragment => CF }

class CypherSyntaxMacros(val c: whitebox.Context) {
  import c.universe._

  def returnTuple[T: WeakTypeTag]: Tree = {
    val T = weakTypeOf[T]

    val tupleTypes = (T: @unchecked) match {
      case TypeRef(ScalaPackageType, s, tuple) if s.name.toString matches """Tuple\d+""" => tuple
    }
    val arg         = c.internal.reificationSupport.freshTermName("t")
    def sel(i: Int) = q"$arg.${TermName(s"_${i + 1}")}"
    val (outTypes, returns) = tupleTypes.zipWithIndex.map {
      case (TypeRef(_, ExprSymb, List(t)), i)       => t -> returnExprTree(t, sel(i))
      case (TypeRef(_, ReturnExprSymb, List(t)), i) => t -> sel(i)
      case (t, i) if t <:< typeOf[CF.Expr[_]] =>
        val List(arg) = t.baseType(ExprSymb).typeArgs
        arg -> returnExprTree(arg, sel(i))
      case (other, _) =>
        c.abort(c.enclosingPosition, s"Returning type $other is not supported")
    }.unzip
    q"""
      new _root_.com.arkondata.slothql.cypher.syntax.CypherSyntaxReturnTuple[$T] {
        type Out = ${mkTupleType(outTypes)}
        def apply($arg: $T): _root_.com.arkondata.slothql.cypher.CypherFragment.Return.Return0[Out] =
          _root_.com.arkondata.slothql.cypher.CypherFragment.Return.Tuple[Out](_root_.scala.List(..$returns))
      }
     """
  }

  private def returnExprTree(tpe: Type, expr: Tree) =
    q"_root_.com.arkondata.slothql.cypher.CypherFragment.Return.Expr[$tpe]($expr, as = _root_.scala.None)"

  protected lazy val ScalaPackageType = c.internal.thisType(rootMirror.staticPackage("scala").moduleClass)

  protected lazy val ExprSymb       = symbolOf[CF.Expr[_]]
  protected lazy val ReturnExprSymb = symbolOf[CF.Return.Expr[_]]

  protected def mkTupleType(types: List[Type]): Type = {
    val n         = types.length
    val tupleSymb = ScalaPackageType.decl(TypeName(s"Tuple$n")).asClass
    c.internal.typeRef(ScalaPackageType, tupleSymb, types)
  }
}
