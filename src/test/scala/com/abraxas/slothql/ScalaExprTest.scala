package com.abraxas.slothql

import com.abraxas.slothql.arrow.ScalaExpr
import com.abraxas.slothql.test.models.Page

object ScalaExprTest {
  val filterPage1 = ScalaExpr[Page].text === "abc"
  //ScalaExpr.Binary.PartiallyAppliedRight[
  //  ScalaExpr.Compare.Eq[Page, String],
  //  ScalaExpr.Literal[String]
  //]{ type Source = Page; type Target = Boolean }
  //= PartiallyAppliedRight(Eq(), Literal(abc))

  val filterPage2 = ScalaExpr[Page].text =!= "cba"
  //ScalaExpr.Binary.PartiallyAppliedRight[
  //  ScalaExpr.Compare.Neq[Page, String],
  //  ScalaExpr.Literal[String]
  //]{ type Source = Page; type Target = Boolean }
  //= PartiallyAppliedRight(Neq(), Literal(cba))
}
