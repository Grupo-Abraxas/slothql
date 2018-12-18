package com.abraxas.slothql

import com.abraxas.slothql.arrow.ScalaExpr
import com.abraxas.slothql.test.models.{ Book, Page }

object ScalaExprTest {
  val filterPage1 = ScalaExpr[Page].text === "abc"
  //ScalaExpr.Composition[
  //  ScalaExpr.Binary.PartiallyAppliedRight[
  //      ScalaExpr.Compare.Eq[String, String],
  //      ScalaExpr.Literal[String]
  //    ]{ type Source = String; type Target = Boolean },
  //  ScalaExpr.SelectField[Page, text, String]
  //]{ type Source = Page; type Target = Boolean }
  //= PartiallyAppliedRight(Eq(), Literal(abc))
  //∘ SelectField[Page, String](text)

  val filterPage2 = ScalaExpr[Page].text =!= "cba"
  //ScalaExpr.Composition[
  //  ScalaExpr.Binary.PartiallyAppliedRight[
  //      ScalaExpr.Compare.Neq[String, String],
  //      ScalaExpr.Literal[String]
  //    ]{ type Source = String; type Target = Boolean },
  //  ScalaExpr.SelectField[Page, text, String]
  //]{ type Source = Page; type Target = Boolean }
  //= PartiallyAppliedRight(Neq(), Literal(cba))
  //∘ SelectField[Page, String](text)

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
  //      ScalaExpr.Composition[
  //        ScalaExpr.Binary.PartiallyAppliedRight[
  //            ScalaExpr.Compare.Eq[String, String],
  //            ScalaExpr.Literal[String]
  //          ]{ type Source = String; type Target = Boolean },
  //        ScalaExpr.SelectField[Page, text, String]
  //      ]{ type Source = Page; type Target = Boolean }
  //    ],
  //  ScalaExpr.SelectField[Book, pages, List[Page]]
  //]{ type Source = Book; type Target = List[Page] }
  //= IterableFilter(PartiallyAppliedRight(Eq(), Literal()) ∘ SelectField[Page, String](text))
  //∘ SelectField[Book, List[Page]](pages)

  val filterPages2 = selPages.filter(x => x.text === x.text)
  //ScalaExpr.Composition[
  //  ScalaExpr.IterableFilter[List,
  //      ScalaExpr.Composition[
  //        ScalaExpr.Binary.PartiallyAppliedRight[
  //            ScalaExpr.Compare.Eq[String, String],
  //            ScalaExpr.SelectField[Page, text, String]
  //          ]{ type Source = String; type Target = Boolean },
  //        ScalaExpr.SelectField[Page, text, String]
  //      ]{ type Source = Page; type Target = Boolean }
  //    ],
  //  ScalaExpr.SelectField[Book, pages, List[Page]]
  //]{ type Source = Book; type Target = List[Page] }
  //= IterableFilter(PartiallyAppliedRight(Eq(), SelectField[Page, String](text)) ∘ SelectField[Page, String](text))
  //∘ SelectField[Book, List[Page]](pages)

  val orderPagesA = selPages.orderBy(_.text)
  //ScalaExpr.Composition[
  //  ScalaExpr.IterableOrderBy[List, ScalaExpr.SelectField[Page, text, String], String],
  //  ScalaExpr.SelectField[Book, pages, List[Page]]
  //]{ type Source = Book; type Target = List[Page] }
  //= IterableOrderBy(SelectField[Page, String](text),Ascending)
  //∘ SelectField[Book, List[Page]](pages)

  val orderPagesD = selPages.orderBy(_.text, _.Descending)
  //ScalaExpr.Composition[
  //  ScalaExpr.IterableOrderBy[List, ScalaExpr.SelectField[Page, text, String], String],
  //  ScalaExpr.SelectField[Book, pages, List[Page]]
  //]{ type Source = Book; type Target = List[Page] }
  //= IterableOrderBy(SelectField[Page, String](text),Descending)
  //∘ SelectField[Book, List[Page]](pages)

}
