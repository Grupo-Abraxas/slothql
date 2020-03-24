package com.arkondata.slothql.util

import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox

class raiseCompilationError(message: String) extends StaticAnnotation

object raiseCompilationError {
  def apply(c: whitebox.Context)(tree: c.Tree): c.Tree = {
    import c.universe._
    tree.symbol.annotations.foreach {
      case a if a.tree.tpe =:= typeOf[raiseCompilationError] =>
        a.tree.children.tail match {
          case List(Literal(Constant(msg: String))) => c.abort(a.tree.pos, msg)
          case _ => c.abort(a.tree.pos, "@raiseCompilationError's message should be a literal string")
        }
      case _ =>
    }
    EmptyTree
  }
}
