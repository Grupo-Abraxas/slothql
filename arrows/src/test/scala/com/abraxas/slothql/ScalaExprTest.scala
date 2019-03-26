package com.abraxas.slothql

import cats.data.Ior
import cats.instances.list._
import cats.instances.option._
import org.scalatest.Assertions.{ assert, assertionsHelper }
import shapeless._

import com.abraxas.slothql.arrow.{ Arrow, ScalaExpr }
import com.abraxas.slothql.test.models._

object RunScalaExprTest extends App { ScalaExprTest }
object ScalaExprTest {
  type title   = Witness.`"title"`.T
  type author  = Witness.`"author"`.T
  type pages   = Witness.`"pages"`.T
  type meta    = Witness.`"meta"`.T
  type reviews = Witness.`"reviews"`.T
  type name    = Witness.`"name"`.T
  type text    = Witness.`"text"`.T
  type isbn    = Witness.`"isbn"`.T
  type user    = Witness.`"user"`.T
  type vote    = Witness.`"vote"`.T
  type review  = Witness.`"review"`.T
  type url     = Witness.`"url"`.T


  val splitBook = ScalaExpr[Book].split(_.title, _.author.map(_.name), _.meta.isbn)
  implicitly[splitBook.type <:<
    ScalaExpr.Split[
      ScalaExpr.SelectField[Book, title, String] ::
      ScalaExpr.Composition[
          ScalaExpr.FMap[Option, ScalaExpr.SelectField[Author, name, String]],
          ScalaExpr.SelectField[Book, author, Option[Author]]
        ]{ type Source = Book; type Target = Option[String] } ::
      ScalaExpr.Composition[
          ScalaExpr.SelectField[Meta, isbn, String],
          ScalaExpr.SelectField[Book, meta, Meta]
        ]{ type Source = Book; type Target = String } ::
      HNil
    ]{ type Source = Book; type Target = (String, Option[String], String) }
  ]
  assert(splitBook ==
    Arrow.Split(
      ScalaExpr.SelectField[Book, title, String]("title"),
      ScalaExpr.FMap.tpe[Option].expr(ScalaExpr.SelectField[Author, name, String]("name"))
        ∘ ScalaExpr.SelectField[Book, author, Option[Author]]("author"),
      ScalaExpr.SelectField[Meta, isbn, String]("isbn")
        ∘ ScalaExpr.SelectField[Book, meta, Meta]("meta")
    )
  )
  assert(splitBook.short == ".split(_.title, _.author.map(_.name), _.meta.isbn)")


  val chooseBookReview = ScalaExpr[Book].reviews.map(_.choose(
    _.on[UserReview].split(_.user, _.text, _.vote),
    _.on[AnonReview].text,
    _.on[CustomReview].choose(
      _.on[Reviews].reviews.map(_.choose(
        _.on[AnonReview].text
      )),
      _.on[ImageReviewAttachment].split(_.review.split(_.user, _.text), _.url)
    )
  ))
  implicitly[chooseBookReview.type <:<
    ScalaExpr.Composition[
      ScalaExpr.FMap[List,
        ScalaExpr.Choose[Review,
          ScalaExpr.Split[
              ScalaExpr.SelectField[UserReview, user, String] ::
              ScalaExpr.SelectField[UserReview, text, String] ::
              ScalaExpr.SelectField[UserReview, vote, Int] ::
              HNil
            ]{ type Source = UserReview; type Target = (String, String, Int) } ::
          ScalaExpr.SelectField[AnonReview, text, String] ::
          ScalaExpr.Choose[CustomReview,
              ScalaExpr.Composition[
                  ScalaExpr.FMap[List,
                      ScalaExpr.Choose[Review,
                          ScalaExpr.SelectField[AnonReview, text, String] ::
                          HNil
                        ]{ type Target = String :+: CNil }
                    ],
                  ScalaExpr.SelectField[Reviews, reviews, List[Review]]
                ]{ type Source = Reviews; type Target = List[String :+: CNil] } ::
              ScalaExpr.Split[
                  ScalaExpr.Composition[
                      ScalaExpr.Split[
                          ScalaExpr.SelectField[UserReview, user, String] ::
                          ScalaExpr.SelectField[UserReview, text, String] ::
                          HNil
                        ]{ type Source = UserReview; type Target = (String, String) },
                      ScalaExpr.SelectField[ImageReviewAttachment, review, UserReview]
                    ]{ type Source = ImageReviewAttachment; type Target = (String, String) } ::
                  ScalaExpr.SelectField[ImageReviewAttachment, url, String] ::
                  HNil
                ]{ type Source = ImageReviewAttachment; type Target = ((String, String), String)} ::
              HNil
            ]{ type Target = List[String :+: CNil] :+: ((String, String), String) :+: CNil } ::
          HNil
        ]{ type Target = (String, String, Int) :+: String :+: (List[String :+: CNil] :+: ((String, String), String) :+: CNil) :+: CNil }
      ],
      ScalaExpr.SelectField[Book, reviews, List[Review]]
    ]{ type Source = Book
       type Target = List[(String, String, Int) :+: String :+: (List[String :+: CNil] :+: ((String, String), String) :+: CNil) :+: CNil]
    }
  ]
  assert(chooseBookReview == (
    ScalaExpr.FMap.tpe[List].expr(
      Arrow.Choose[Review].from(
        Arrow.Split(
          ScalaExpr.SelectField[UserReview, user, String]("user"),
          ScalaExpr.SelectField[UserReview, text, String]("text"),
          ScalaExpr.SelectField[UserReview, vote, Int]("vote")
        ),
        ScalaExpr.SelectField[AnonReview, text, String]("text"),
        Arrow.Choose[CustomReview].from(
          ScalaExpr.FMap.tpe[List].expr(
              Arrow.Choose[Review].from(
                ScalaExpr.SelectField[AnonReview, text, String]("text")
              )
            ) ∘ ScalaExpr.SelectField[Reviews, reviews, List[Review]]("reviews"),
          Arrow.Split(
            Arrow.Split(
                ScalaExpr.SelectField[UserReview, user, String]("user"),
                ScalaExpr.SelectField[UserReview, text, String]("text")
              ) ∘ ScalaExpr.SelectField[ImageReviewAttachment, review, UserReview]("review"),
            ScalaExpr.SelectField[ImageReviewAttachment, url, String]("url")
          )
        )
      )
    ) ∘ ScalaExpr.SelectField[Book, reviews, List[Review]]("reviews")
  ))
  assert(chooseBookReview.short == List(
    ".reviews.map(_.choose(",
      "_.on[UserReview].split(_.user, _.text, _.vote), ",
      "_.on[AnonReview].text, ",
      "_.on[CustomReview].choose(",
        "_.on[Reviews].reviews.map(_.choose(",
          "_.on[AnonReview].text",
        ")), ",
        "_.on[ImageReviewAttachment].split(_.review.split(_.user, _.text), _.url)",
      ")",
    "))"
  ).mkString)

  val chooseReview = ScalaExpr[Review].choose(
    _.on[UserReview].split(_.user, _.text, _.vote),
    _.on[AnonReview].text
  )
  implicitly[chooseReview.type <:<
    ScalaExpr.Choose[Review,
      ScalaExpr.Split[
          ScalaExpr.SelectField[UserReview, user, String] ::
          ScalaExpr.SelectField[UserReview, text, String] ::
          ScalaExpr.SelectField[UserReview, vote, Int] ::
          HNil
        ]{ type Source = UserReview; type Target = (String, String, Int) } ::
      ScalaExpr.SelectField[AnonReview, text, String] ::
      HNil
    ]{ type Target = (String, String, Int) :+: String :+: CNil }
  ]
  assert(chooseReview ==
    Arrow.Choose[Review].from(
      Arrow.Split(
        ScalaExpr.SelectField[UserReview, user, String]("user"),
        ScalaExpr.SelectField[UserReview, text, String]("text"),
        ScalaExpr.SelectField[UserReview, vote, Int]("vote")
      ),
      ScalaExpr.SelectField[AnonReview, text, String]("text")
    )
  )
  assert(chooseReview.short == ".choose(_.on[UserReview].split(_.user, _.text, _.vote), _.on[AnonReview].text)")

  shapeless.test.illTyped(
    """ScalaExpr[Review].choose(
         _.on[UserReview].user,
         _.on[UserReview].split(_.text, _.vote)
       )
    """,
    "Source types of the arrows must be distinct"
  )


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
  assert(filterPage1.short == ".text == \"abc\"")


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
  assert(filterPage2.short == ".text != \"cba\"")



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
  assert(slicePages.short == ".pages.slice(1..10)")


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
  assert(filterPages.short == ".pages.filter(_.text == \"\")")


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
  assert(filterPages2.short == ".pages.filter(_.text == .text)")


  type OrderByPageExpr =
    ScalaExpr.Composition[
      ScalaExpr.IterableOrderBy[List, ScalaExpr.SelectField[Page, text, String]],
      ScalaExpr.SelectField[Book, pages, List[Page]]
    ]{ type Source = Book; type Target = List[Page] }
  def OrderByPageExpr(dir: ScalaExpr.OrderBy.Direction) = { // TODO: existential warning
    val orderBy = ScalaExpr.SelectField[Page, text, String]("text")

    (   ScalaExpr.IterableOrderBy[List, orderBy.type](orderBy, dir)
      ∘ ScalaExpr.SelectField[Book, pages, List[Page]]("pages")
    )
  }

  val orderPagesA = selPages.orderBy(_.text)
  implicitly[orderPagesA.type <:< OrderByPageExpr]
  assert(orderPagesA == OrderByPageExpr(ScalaExpr.OrderBy.Ascending))
  assert(orderPagesA.short == ".pages.orderBy(_.text)")

  val orderPagesD = selPages.orderBy(_.text, _.Descending)
  implicitly[orderPagesD.type <:< OrderByPageExpr]
  assert(orderPagesD == OrderByPageExpr(ScalaExpr.OrderBy.Descending))
  assert(orderPagesD.short == ".pages.orderBy[Inv](_.text)")
}
