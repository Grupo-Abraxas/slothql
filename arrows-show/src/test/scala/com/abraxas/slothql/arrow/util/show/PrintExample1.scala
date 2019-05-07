package com.abraxas.slothql.arrow.util.show

import com.abraxas.slothql.ScalaExprTest

object PrintExample1 extends App {
  val expr = ScalaExprTest.chooseBookReview
  val dot = ScalaExprToDot(expr)
  println(dot)
}
