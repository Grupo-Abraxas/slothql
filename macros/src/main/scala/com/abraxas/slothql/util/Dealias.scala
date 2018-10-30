package com.abraxas.slothql.util

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

class Dealias[T]
object Dealias {
  implicit def apply[T]: Dealias[T] = macro DealiasMacros.applyImpl[T]
}

class DealiasMacros(val c: blackbox.Context) {
  import c.universe._

  def applyImpl[T: WeakTypeTag]: Tree = q"new _root_.com.abraxas.slothql.util.Dealias[${weakTypeOf[T].dealias}]"
}
