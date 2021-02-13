package com.arkondata.slothql.cypher.syntax

import scala.annotation.compileTimeOnly
import scala.language.implicitConversions

import com.arkondata.slothql.cypher.{ CypherFragment => CF }

trait CypherSyntaxLowPriorityImplicits {

  @compileTimeOnly("should have been replaced by cypher syntax macro")
  implicit def booleanCypherExprToBooleanForIfGuard(expr: CF.Expr[Boolean]): Boolean = ???

  @compileTimeOnly("should have been replaced by cypher syntax macro")
  implicit def optionalBooleanCypherExprToBooleanForIfGuard(expr: Option[CF.Expr[Boolean]]): Boolean = ???
}
