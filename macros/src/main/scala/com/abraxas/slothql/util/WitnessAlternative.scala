package com.abraxas.slothql.util

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

object WitnessAlternative {
  implicit def forRefinedString[T]: shapeless.Witness.Aux[T] = macro forRefinedStringImpl[T]

  def forRefinedStringImpl[T: c.WeakTypeTag](c: whitebox.Context): c.Tree = {
    import c.universe._
    weakTypeOf[T] match {
      case t@RefinedType(List(ConstantType(Constant(s: String))), _) =>
        q"""
         new _root_.shapeless.Witness { type T = $t; val value: $t = $s }
       """
    }
  }
}
