package com.abraxas.slothql.util

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

import shapeless.DepFn1
import shapeless.labelled.KeyTag
import shapeless.tag.Tagged

trait ShapelessUntag[T] extends DepFn1[T]
object ShapelessUntag {
  type Aux[T, R] = ShapelessUntag[T] { type Out = R }


  implicit def untag[T <: Tagged[_]]: ShapelessUntag[T] = macro untagImpl[T]

  lazy val instance = new ShapelessUntag[Any] {
    type Out = Any
    @inline def apply(t: Any): Any = t
  }

  def untagImpl[T <: Tagged[_]: c.WeakTypeTag](c: whitebox.Context): c.Tree = {
    import c.universe._
    val T = weakTypeOf[T]

    val T0 =
      if (T <:< typeOf[Tagged[_]] || T <:< typeOf[KeyTag[_, _]])
        T.baseClasses
          .filterNot(_.asType.toType <:< typeOf[Tagged[_]])
          .filterNot(_.asType.toType <:< typeOf[KeyTag[_, _]])
          .map(sym => tq"$sym")
          .reduceLeft((acc, t) => tq"$acc with $t")
      else tq"$T"

    q"_root_.com.abraxas.slothql.util.ShapelessUntag.instance.asInstanceOf[_root_.com.abraxas.slothql.util.ShapelessUntag.Aux[$T, $T0]]"
  }
}
