package com.arkondata.slothql.arrow.util

import scala.annotation.implicitNotFound
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

import com.arkondata.slothql.arrow.Arrow


@implicitNotFound("${A} is not an Arrow.Id")
trait IsId[A <: Arrow]
object IsId {
  implicit def evidence[A <: Arrow]: IsId[A] = macro instanceImpl[A]

  @inline def mkInstance[A <: Arrow]: IsId[A] = _instance.asInstanceOf[IsId[A]]
  private lazy val _instance = new IsId[Arrow] {}

  def instanceImpl[A <: Arrow: c.WeakTypeTag](c: blackbox.Context): c.Expr[IsId[A]] = {
    import c.universe._

    val A = weakTypeOf[A]
    if (!A.baseClasses.contains(symbolOf[Arrow.Id[_]])) c.abort(c.enclosingPosition, s"$A is not an Arrow.Id")
    else reify(mkInstance[A])
  }
}


@implicitNotFound("${A} is an Arrow.Id")
trait NotId[A <: Arrow]
object NotId {
  implicit def evidence[A <: Arrow]: NotId[A] = macro instanceImpl[A]

  @inline def mkInstance[A <: Arrow]: NotId[A] = _instance.asInstanceOf[NotId[A]]
  private lazy val _instance = new NotId[Arrow] {}

  def instanceImpl[A <: Arrow: c.WeakTypeTag](c: blackbox.Context): c.Expr[NotId[A]] = {
    import c.universe._

    val A = weakTypeOf[A]
    if (A.baseClasses contains symbolOf[Arrow.Id[_]]) c.abort(c.enclosingPosition, s"$A is an Arrow.Id")
    else reify(mkInstance[A])
  }
}
