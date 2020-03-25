package com.arkondata.slothql.arrow.util.show

import com.arkondata.slothql.ScalaExprTest

object PrintExample1 extends App {
  val expr = ScalaExprTest.chooseBookReview
  val dot = ScalaExprToDot(expr)
  println(dot)
}
