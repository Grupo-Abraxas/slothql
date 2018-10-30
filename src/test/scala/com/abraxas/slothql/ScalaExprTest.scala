package com.abraxas.slothql

import com.abraxas.slothql.arrow.ScalaExpr
import com.abraxas.slothql.test.models.{ Book, Page }

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

  val selPages = ScalaExpr[Book].pages

  val slicePages = selPages.slice(1, 10)
  //ScalaExpr.Composition[
  //  ScalaExpr.IterableSlice[List[Page]],
  //  ScalaExpr.SelectField[Book, pages, List[Page]]
  //]{ type Source = Book; type Target = List[Page] }
  //= IterableSlice(1,10)
  //∘ SelectField[Book, List[Page]](pages)

  val filterPages = selPages.filter(_.text === "")
  //ScalaExpr.Composition[
  //  ScalaExpr.IterableFilter[List,
  //    ScalaExpr.Binary.PartiallyAppliedRight[
  //      ScalaExpr.Compare.Eq[Page, String],
  //      ScalaExpr.Literal[String]
  //    ]{ type Source = Page; type Target = Boolean }
  //  ],
  //  ScalaExpr.SelectField[Book, pages, List[Page]]
  //]{ type Source = Book; type Target = List[Page] }
  //= IterableFilter(PartiallyAppliedRight(Eq(), Literal()))
  //∘ SelectField[Book, List[Page]](pages)

  val filterPages2 = selPages.filter(x => x.text === x.text)
  //ScalaExpr.Composition[
  //  ScalaExpr.IterableFilter[List,
  //    ScalaExpr.Binary.PartiallyAppliedRight[
  //      ScalaExpr.Compare.Eq[Page, Page],
  //      ScalaExpr.SelectField[Page, text, String]
  //    ]{ type Source = Page; type Target = Boolean }
  //  ],
  //  ScalaExpr.SelectField[Book, pages, List[Page]]
  //]{ type Source = Book; type Target = List[Page] }
  //= IterableFilter(PartiallyAppliedRight(Eq(), SelectField[Page, String](text)))
  //∘ SelectField[Book, List[Page]](pages)

}
