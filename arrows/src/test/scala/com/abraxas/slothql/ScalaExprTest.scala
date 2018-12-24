package com.abraxas.slothql

import cats.data.Ior
import shapeless.Witness
import org.scalatest.Assertions.{ assert, assertionsHelper }

import com.abraxas.slothql.arrow.ScalaExpr
import com.abraxas.slothql.test.models.{ Book, Page }

object RunScalaExprTest extends App { ScalaExprTest }
object ScalaExprTest {
  type text  = Witness.`"text"`.T
  type pages = Witness.`"pages"`.T


  val filterPage1 = ScalaExpr[Page].text === "abc"
  implicitly[filterPage1.type <:<
    ScalaExpr.Composition[
      ScalaExpr.Binary.PartiallyAppliedRight[
        ScalaExpr.Compare.Eq[String, String],
        ScalaExpr.Literal[String]
      ]{ type Source = String; type Target = Boolean },
      ScalaExpr.SelectField[Page, text, String]
    ]{ type Source = Page; type Target = Boolean }
  ]
  assert(filterPage1 == (
      ScalaExpr.Binary.PartialApplyRight(ScalaExpr.Compare.Eq[String, String](), ScalaExpr.Literal("abc"))
    ∘ ScalaExpr.SelectField[Page, text, String]("text")
  ))


  val filterPage2 = ScalaExpr[Page].text =!= "cba"
  implicitly[filterPage2.type <:<
    ScalaExpr.Composition[
      ScalaExpr.Binary.PartiallyAppliedRight[
        ScalaExpr.Compare.Neq[String, String],
        ScalaExpr.Literal[String]
      ]{ type Source = String; type Target = Boolean },
      ScalaExpr.SelectField[Page, text, String]
    ]{ type Source = Page; type Target = Boolean }
  ]
  assert(filterPage2 == (
      ScalaExpr.Binary.PartialApplyRight(ScalaExpr.Compare.Neq[String, String](), ScalaExpr.Literal("cba"))
    ∘ ScalaExpr.SelectField[Page, text, String]("text")
  ))



  val selPages = ScalaExpr[Book].pages


  val slicePages = selPages.slice(1, 10)
  implicitly[slicePages.type <:<
    ScalaExpr.Composition[
      ScalaExpr.IterableSlice[List[Page]],
      ScalaExpr.SelectField[Book, pages, List[Page]]
    ]{ type Source = Book; type Target = List[Page] }
  ]
  assert(slicePages == (
      ScalaExpr.IterableSlice[List[Page]](Ior.both(1, 10))
    ∘ ScalaExpr.SelectField[Book, pages, List[Page]]("pages")
  ))


  val filterPages = selPages.filter(_.text === "")
  implicitly[filterPages.type <:<
    ScalaExpr.Composition[
      ScalaExpr.IterableFilter[List,
        ScalaExpr.Composition[
          ScalaExpr.Binary.PartiallyAppliedRight[
            ScalaExpr.Compare.Eq[String, String],
            ScalaExpr.Literal[String]
          ]{ type Source = String; type Target = Boolean },
          ScalaExpr.SelectField[Page, text, String]
        ]{ type Source = Page; type Target = Boolean }
      ],
      ScalaExpr.SelectField[Book, pages, List[Page]]
    ]{ type Source = Book; type Target = List[Page] }
  ]
  assert(filterPages == {
    val filterExpr = (
        ScalaExpr.Binary.PartialApplyRight(ScalaExpr.Compare.Eq[String, String](), ScalaExpr.Literal(""))
      ∘ ScalaExpr.SelectField[Page, text, String]("text")
    )

    (   ScalaExpr.IterableFilter[List, filterExpr.type](filterExpr)
      ∘ ScalaExpr.SelectField[Book, pages, List[Page]]("pages")
    )
  })


  val filterPages2 = selPages.filter(x => x.text === x.text)
  implicitly[filterPages2.type <:<
    ScalaExpr.Composition[
      ScalaExpr.IterableFilter[List,
        ScalaExpr.Composition[
          ScalaExpr.Binary.PartiallyAppliedRight[
            ScalaExpr.Compare.Eq[String, String],
            ScalaExpr.SelectField[Page, text, String]
          ]{ type Source = String; type Target = Boolean },
          ScalaExpr.SelectField[Page, text, String]
        ]{ type Source = Page; type Target = Boolean }
      ],
      ScalaExpr.SelectField[Book, pages, List[Page]]
    ]{ type Source = Book; type Target = List[Page] }
  ]
  assert(filterPages2 == {
    val filterExpr = (
        ScalaExpr.Binary.PartialApplyRight(ScalaExpr.Compare.Eq[String, String](), ScalaExpr.SelectField[Page, text, String]("text"))
      ∘ ScalaExpr.SelectField[Page, text, String]("text")
    )

    (   ScalaExpr.IterableFilter[List, filterExpr.type](filterExpr)
      ∘ ScalaExpr.SelectField[Book, pages, List[Page]]("pages")
    )
  })


  type OrderByPageExpr =
    ScalaExpr.Composition[
      ScalaExpr.IterableOrderBy[List, ScalaExpr.SelectField[Page, text, String], String],
      ScalaExpr.SelectField[Book, pages, List[Page]]
    ]{ type Source = Book; type Target = List[Page] }
  def OrderByPageExpr(dir: ScalaExpr.OrderBy.Direction) = { // TODO: existential warning
    val orderBy = ScalaExpr.SelectField[Page, text, String]("text")

    (   ScalaExpr.IterableOrderBy[List, orderBy.type, String](orderBy, dir)
      ∘ ScalaExpr.SelectField[Book, pages, List[Page]]("pages")
    )
  }

  val orderPagesA = selPages.orderBy(_.text)
  implicitly[orderPagesA.type <:< OrderByPageExpr]
  assert(orderPagesA == OrderByPageExpr(ScalaExpr.OrderBy.Ascending))

  val orderPagesD = selPages.orderBy(_.text, _.Descending)
  implicitly[orderPagesD.type <:< OrderByPageExpr]
  assert(orderPagesD == OrderByPageExpr(ScalaExpr.OrderBy.Descending))
}
