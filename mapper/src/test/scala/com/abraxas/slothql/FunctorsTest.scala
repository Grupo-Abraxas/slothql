package com.abraxas.slothql

import scala.language.higherKinds

import shapeless.{ ::, HNil, Witness}
import org.scalatest.Assertions._

import com.abraxas.slothql.arrow.util.ToDot
import com.abraxas.slothql.arrow.{ Arrow, FuncArrow, Functor, ScalaExpr }
import com.abraxas.slothql.mapper._

object RunFunctorsTest extends App { FunctorsTest }
object FunctorsTest {
  import cats.instances.list._
  import cats.instances.option._

  import com.abraxas.slothql.mapper.MapScalaExprToGraphPath._
  import com.abraxas.slothql.test.models._

  type title  = Witness.`"title"`.T
  type author = Witness.`"author"`.T
  type pages  = Witness.`"pages"`.T
  type meta   = Witness.`"meta"`.T
  type name      = Witness.`"name"`.T
  type pseudonym = Witness.`"pseudonym"`.T
  type text = Witness.`"text"`.T
  type isbn = Witness.`"isbn"`.T


  lazy val book = Book("History of Rome", Some(author), Nil, meta)
  lazy val author = Author("Theodor Mommsen", pseudonym = None)
  lazy val meta = Meta(isbn = "9786610240531")
  lazy val somePage = Page("...")


  // TODO: test CypherQueryArrow
  val selPages = ScalaExpr[Book].pages
  implicitly[selPages.type <:< ScalaExpr.SelectField[Book, pages, List[Page]]]
  val selPagesF = FuncArrow.from(selPages)
  implicitly[selPagesF.type <:< FuncArrow.Aux[Book, List[Page]]]
  assert(selPagesF(book) == List())
  val pathPages = Functor.map(selPages).to[GraphPath]
  implicitly[pathPages.type <:<
    GraphPath.OutgoingRelation[
      Book.BookRepr.type,
      Book.PageListRepr.type
    ]
  ]
  assert(pathPages.node == Book.BookRepr)
  assert(pathPages.relation == Book.PageListRepr)


  val selText = ScalaExpr[Page].text
  implicitly[selText.type <:< ScalaExpr.SelectField[Page, text, String]]
  val selTextF = FuncArrow.from(selText)
  implicitly[selTextF.type <:< FuncArrow.Aux[Page, String]]
  assert(selTextF(somePage) == "...")
  val pathText = Functor.map(selText).to[GraphPath]
  implicitly[pathText.type <:<
    GraphPath.PropSelection[Page.PageRepr.type, GraphRepr.Property{ type Type = String }]
  ]
  assert(pathText == GraphPath.PropSelection(Page.PageRepr, "text", GraphRepr.Property[String]))
  val textCypherArrow = Functor.map(pathText).to[CypherQueryArrow]
  implicitly[textCypherArrow.type <:< CypherQueryArrow.Aux[Unit, String]]
  val textCypherQuery = textCypherArrow(())
  assert(textCypherQuery.toCypher == "MATCH (`n`:`Page`) RETURN `n`.`text`")


  // TODO: test mapping to GraphPath
  // TODO: test CypherQueryArrow
  val selTextFMap = ScalaExpr.FMap.mk[List](selText)
  implicitly[selTextFMap.type <:< ScalaExpr.FMap[List, ScalaExpr.SelectField[Page, text, String]]]
  val selTextFMapF = FuncArrow.from(selTextFMap)
  implicitly[selTextFMapF.type <:< FuncArrow.Aux[List[Page], List[String]]]
  assert(selTextFMapF(List(somePage)) == List("..."))


  // TODO: test mapping to GraphPath
  // TODO: test CypherQueryArrow
  val mapPagesText1_0 = selTextFMap ∘ selPages
  implicitly[mapPagesText1_0.type <:<
    ScalaExpr.Composition[
      ScalaExpr.FMap[List, ScalaExpr.SelectField[Page, text, String]],
      ScalaExpr.SelectField[Book, pages, List[Page]]
    ]{ type Source = Book; type Target = List[String] }
  ]
  val mapPagesText1_0F = selTextFMapF ∘ selPagesF
  implicitly[mapPagesText1_0F.type <:< FuncArrow.Aux[Book, List[String]]]
  assert(mapPagesText1_0F(book) == Nil)


  // TODO: test mapping to GraphPath
  // TODO: test CypherQueryArrow
  val mapPagesText1 = selPages.map(selText)
  implicitly[mapPagesText1.type <:<
    ScalaExpr.Composition[
      ScalaExpr.FMap[List, ScalaExpr.SelectField[Page, text, String]],
      ScalaExpr.SelectField[Book, pages, List[Page]]
    ]{ type Source = Book; type Target = List[String] }
  ]
  val mapPagesText1F = FuncArrow.from(mapPagesText1)
  implicitly[mapPagesText1F.type <:< FuncArrow.Aux[Book, List[String]]]
  assert(mapPagesText1F(book) == Nil)


  // TODO: test mapping to GraphPath
  // TODO: test CypherQueryArrow
  val mapPagesText2 = selPages.map(_.text)
  implicitly[mapPagesText2.type <:<
    ScalaExpr.Composition[
      ScalaExpr.FMap[List, ScalaExpr.SelectField[Page, text, String]],
      ScalaExpr.SelectField[Book, pages, List[Page]]
    ]{ type Source = Book; type Target = List[String] }
  ]
  val mapPagesText2F = FuncArrow.from(mapPagesText2)
  implicitly[mapPagesText2F.type <:< FuncArrow.Aux[Book, List[String]]]
  assert(mapPagesText2F(book) == Nil)


  val selAuthor = ScalaExpr[Book].selectDynamic("author") // same as `.author`
  implicitly[selAuthor.type <:< ScalaExpr.SelectField[Book, author, Option[Author]]]
  val selAuthorF = FuncArrow.from(selAuthor)
  implicitly[selAuthorF.type <:< FuncArrow.Aux[Book, Option[Author]]]
  assert(selAuthorF(book) == Some(Author("Theodor Mommsen", None)))
  val pathAuthor = Functor.map(selAuthor).to[GraphPath]
  implicitly[pathAuthor.type <:<
    Arrow.Composition[
      GraphPath.RelationTarget[
        GraphRepr.Relation.Empty[author, Book.BookRepr.type, GraphRepr.Node.Optional[Author.AuthorRepr.type]],
        GraphRepr.Node.Optional[Author.AuthorRepr.type]
      ],
      GraphPath.OutgoingRelation[
        Book.BookRepr.type,
        GraphRepr.Relation.Empty[author, Book.BookRepr.type, GraphRepr.Node.Optional[Author.AuthorRepr.type]]
      ]
    ]
  ]
  implicitly[pathAuthor.Source =:= Book.BookRepr.type]
  implicitly[pathAuthor.Target =:= GraphRepr.Node.Optional[Author.AuthorRepr.type]]
  assert(pathAuthor == (
      GraphPath.RelationTarget(GraphRepr.Relation.Empty("author", Book.BookRepr, GraphRepr.Node.Optional(Author.AuthorRepr)), GraphRepr.Node.Optional(Author.AuthorRepr))
    ∘ GraphPath.OutgoingRelation(Book.BookRepr, GraphRepr.Relation.Empty("author", Book.BookRepr, GraphRepr.Node.Optional(Author.AuthorRepr)))
  ))
  val authorCypherArrow = Functor.map(pathAuthor).to[CypherQueryArrow]
  implicitly[authorCypherArrow.type <:< CypherQueryArrow.Aux[Unit, Map[String, Any]]]
  val authorCypherQuery = authorCypherArrow(())
  assert(authorCypherQuery.toCypher == "MATCH (:`Book`) -[:`author`]-> (`n`:`Author`) RETURN `n`")


  // TODO: test mapping to GraphPath
  // TODO: test CypherQueryArrow
  val mapAuthorName = selAuthor.map(_.name)
  implicitly[mapAuthorName.type <:<
    ScalaExpr.Composition[
      ScalaExpr.FMap[Option, ScalaExpr.SelectField[Author, name, String]],
      ScalaExpr.SelectField[Book, author, Option[Author]]
    ]{ type Source = Book; type Target = Option[String] }
  ]
  val mapAuthorNameF = FuncArrow.from(mapAuthorName)
  implicitly[mapAuthorNameF.type <:< FuncArrow.Aux[Book, Option[String]]]
  assert(mapAuthorNameF(book) == Some("Theodor Mommsen"))


  // TODO: test mapping to GraphPath
  // TODO: test CypherQueryArrow
  val mapAuthorPseudonym = selAuthor.flatMap(_.pseudonym)
  implicitly[mapAuthorPseudonym.type <:<
    ScalaExpr.Composition[
      ScalaExpr.MBind[Option, ScalaExpr.SelectField[Author, pseudonym, Option[String]]],
      ScalaExpr.SelectField[Book, author, Option[Author]]
    ]{ type Source = Book; type Target = Option[String] }
  ]
  val mapAuthorPseudonymF = FuncArrow.from(mapAuthorPseudonym)
  implicitly[mapAuthorPseudonymF.type <:< FuncArrow.Aux[Book, Option[String]]]
  assert(mapAuthorPseudonymF(book) == None)


  val selIsbn = ScalaExpr[Book].meta.isbn
  implicitly[selIsbn.type <:<
    ScalaExpr.Composition[
      ScalaExpr.SelectField[Meta, isbn, String],
      ScalaExpr.SelectField[Book, meta, Meta]
    ]{ type Source = Book; type Target = String }
  ]
  val selIsbnF = FuncArrow.from(selIsbn)
  implicitly[selIsbnF.type <:< FuncArrow.Aux[Book, String]]
  assert(selIsbnF(book) == "9786610240531")
  val pathIsbn = Functor.map(selIsbn).to[GraphPath]
  implicitly[pathIsbn.type <:<
    Arrow.Composition[
      GraphPath.PropSelection[Meta.MetaRepr.type, GraphRepr.Property{ type Type = String }],
      Arrow.Composition[
        GraphPath.RelationTarget[GraphRepr.Relation.Empty[meta, Book.BookRepr.type, Meta.MetaRepr.type], Meta.MetaRepr.type],
        GraphPath.OutgoingRelation[Book.BookRepr.type, GraphRepr.Relation.Empty[meta, Book.BookRepr.type, Meta.MetaRepr.type]]
      ]{ type Source = Book.BookRepr.type; type Target = Meta.MetaRepr.type }
    ]
  ]
  implicitly[pathIsbn.Source =:= Book.BookRepr.type]
  implicitly[pathIsbn.Target =:= GraphRepr.Property{ type Type = String }]
  assert(pathIsbn == (
      GraphPath.PropSelection(Meta.MetaRepr, "isbn", GraphRepr.Property[String])
    ∘ GraphPath.RelationTarget(GraphRepr.Relation.Empty("meta", Book.BookRepr, Meta.MetaRepr), Meta.MetaRepr)
    ∘ GraphPath.OutgoingRelation(Book.BookRepr, GraphRepr.Relation.Empty("meta", Book.BookRepr, Meta.MetaRepr))
  ))
  val isbnCypherArrow = Functor.map(pathIsbn).to[CypherQueryArrow]
  implicitly[isbnCypherArrow.type <:< CypherQueryArrow.Aux[Unit, String]]
  val isbnCypherQuery = isbnCypherArrow(())
  assert(isbnCypherQuery.toCypher == "MATCH (:`Book`) -[:`meta`]-> (`n`:`Meta`) RETURN `n`.`isbn`")


  val bookId = ScalaExpr[Book]
  implicitly[bookId.type <:< ScalaExpr.Id[Book]]
  val bookIdF = FuncArrow.from(bookId)
  implicitly[bookIdF.type <:< FuncArrow.Aux[Book, Book]]
  assert(bookIdF(book) == book)
  val bookIdPath = Functor.map(bookId).to[GraphPath]
  implicitly[bookIdPath.type <:< GraphPath.Initial[Book.BookRepr.type]]
  assert(bookIdPath == GraphPath.Initial(Book.BookRepr))
  val bookIdCypherArrow = Functor.map(bookIdPath).to[CypherQueryArrow]
  implicitly[bookIdCypherArrow.type <:< CypherQueryArrow.Aux[Unit, Map[String, Any]]]
  val bookIdCypherQuery = bookIdCypherArrow(())
  assert(bookIdCypherQuery.toCypher == "MATCH (`n`:`Book`) RETURN `n`")


  val split1 = bookId >^> { book => Arrow.Split(book.title, book.author, book.meta.isbn) }
  implicitly[split1.type <:<
    ScalaExpr.Split[
      ScalaExpr.SelectField[Book, title, String] ::
      ScalaExpr.SelectField[Book, author, Option[Author]] ::
      ScalaExpr.Composition[
        ScalaExpr.SelectField[Meta, isbn, String],
        ScalaExpr.SelectField[Book, meta, Meta]
      ]{ type Source = Book; type Target = String } ::
      HNil
    ]
  ]
  implicitly[split1.Source =:= Book]
  implicitly[split1.Target =:= (String, Option[Author], String)]
  val split1F = FuncArrow.from(split1)
  implicitly[split1F.type <:< FuncArrow.Aux[Book, (String, Option[Author], String)]]
  assert(split1F(book) == ("History of Rome", Some(author), "9786610240531"))
  val split1Path = Functor.map(split1).to[GraphPath]
  implicitly[split1Path.type <:<
    Arrow.Split[
      GraphPath.PropSelection[Book.BookRepr.type, GraphRepr.Property{ type Type = String }] ::
      Arrow.Composition[
        GraphPath.RelationTarget[
          GraphRepr.Relation.Empty[author, Book.BookRepr.type, GraphRepr.Node.Optional[Author.AuthorRepr.type]],
          GraphRepr.Node.Optional[Author.AuthorRepr.type]
        ],
        GraphPath.OutgoingRelation[
          Book.BookRepr.type,
          GraphRepr.Relation.Empty[author, Book.BookRepr.type, GraphRepr.Node.Optional[Author.AuthorRepr.type]]
        ]
      ]{ type Source = Book.BookRepr.type; type Target = GraphRepr.Node.Optional[Author.AuthorRepr.type] } ::
      Arrow.Composition[
        GraphPath.PropSelection[Meta.MetaRepr.type, GraphRepr.Property{ type Type = String }],
        Arrow.Composition[
          GraphPath.RelationTarget[
            GraphRepr.Relation.Empty[meta, Book.BookRepr.type, Meta.MetaRepr.type],
            Meta.MetaRepr.type
          ],
          GraphPath.OutgoingRelation[
            Book.BookRepr.type,
            GraphRepr.Relation.Empty[meta, Book.BookRepr.type, Meta.MetaRepr.type]
          ]
        ]{ type Source = Book.BookRepr.type; type Target = Meta.MetaRepr.type }
      ]{ type Source = Book.BookRepr.type; type Target = GraphRepr.Property{ type Type = String } } ::
      HNil
    ]
  ]
  implicitly[split1Path.Source =:= Book.BookRepr.type]
  implicitly[split1Path.Target =:= (GraphRepr.Property{ type Type = String }, GraphRepr.Node.Optional[Author.AuthorRepr.type], GraphRepr.Property{ type Type = String })]
  assert(split1Path ==
    Arrow.Split(
      GraphPath.PropSelection(Book.BookRepr, "title", GraphRepr.Property[String]),
      (   GraphPath.RelationTarget(GraphRepr.Relation.Empty("author", Book.BookRepr, GraphRepr.Node.Optional(Author.AuthorRepr)), GraphRepr.Node.Optional(Author.AuthorRepr))
        ∘ GraphPath.OutgoingRelation(Book.BookRepr, GraphRepr.Relation.Empty("author", Book.BookRepr, GraphRepr.Node.Optional(Author.AuthorRepr)))
      ),
      (   GraphPath.PropSelection(Meta.MetaRepr, "isbn", GraphRepr.Property[String])
        ∘ GraphPath.RelationTarget(GraphRepr.Relation.Empty("meta", Book.BookRepr, Meta.MetaRepr), Meta.MetaRepr)
        ∘ GraphPath.OutgoingRelation(Book.BookRepr, GraphRepr.Relation.Empty("meta", Book.BookRepr, Meta.MetaRepr))
      )
    )
  )
  lazy val split1PathDot = ToDot(split1Path)
  // println(s"split1PathDot = $split1PathDot")
  val split1CypherArrow = Functor.map(split1Path).to[CypherQueryArrow]
  implicitly[split1CypherArrow.type <:< CypherQueryArrow.Aux[Unit, String :: Map[String,Any] :: String :: HNil]]
  val split1CypherQuery = split1CypherArrow(())
  assert(split1CypherQuery.toCypher == "MATCH (`n0`:`Book`), (:`Book`) -[:`author`]-> (`n1`:`Author`), (:`Book`) -[:`meta`]-> (`n2`:`Meta`) RETURN `n0`.`title`, `n1`, `n2`.`isbn`")

  // Tested with `populate-2`
  // -- // -- // -- // -- // -- // -- // -- // -- // -- // -- // -- // -- // -- // -- // -- // -- // -- // -- // -- //
  // Seq[(String, Map[String, Any], String)] = Vector(
  //  ("History of Rome", Map("name" -> "Theodor Mommsen"), "9786610240531"),
  //  ("Homotopy Type Theory", Map("name" -> "Theodor Mommsen"), "9786610240531"),
  //  ("History of Rome", Map("name" -> "Theodor Mommsen"), ""),
  //  ("Homotopy Type Theory", Map("name" -> "Theodor Mommsen"), "")
  // )
  // TODO: fail ========================================================================================================


  // TODO: test mapping to GraphPath
  // TODO: test CypherQueryArrow
  val split2 = bookId andThenF { book =>
    Arrow.Split(
      book.title,
      book.author.map(author => author.pseudonym),
      book.author.map(author => author.name),
      book.author.map(author => Arrow.Split(author.name)),
      book.author.map(author => Arrow.Split(author.name, author.pseudonym)),
      book.meta.isbn
    )
  }
  implicitly[split2.type <:<
    ScalaExpr.Split[
      ScalaExpr.SelectField[Book, title, String] ::
      ScalaExpr.Composition[
        ScalaExpr.FMap[Option,
          ScalaExpr.SelectField[Author, pseudonym, Option[String]]
        ],
        ScalaExpr.SelectField[Book, author, Option[Author]]
      ]{ type Source = Book; type Target = Option[Option[String]] } ::
      ScalaExpr.Composition[
        ScalaExpr.FMap[Option,
          ScalaExpr.SelectField[Author, name, String]
        ],
        ScalaExpr.SelectField[Book, author, Option[Author]]
      ]{ type Source = Book; type Target = Option[String] } ::
      ScalaExpr.Composition[
        ScalaExpr.FMap[Option,
          ScalaExpr.Split[
            ScalaExpr.SelectField[Author, name, String] ::
            HNil
          ]{ type Source = Author; type Target = Tuple1[String] }
        ],
        ScalaExpr.SelectField[Book, author, Option[Author]]
      ]{ type Source = Book; type Target = Option[Tuple1[String]] } ::
      ScalaExpr.Composition[
        ScalaExpr.FMap[Option,
          ScalaExpr.Split[
            ScalaExpr.SelectField[Author, name, String] ::
            ScalaExpr.SelectField[Author, pseudonym, Option[String]] ::
            HNil
          ]{ type Source = Author; type Target = (String, Option[String]) }
        ],
        ScalaExpr.SelectField[Book, author, Option[Author]]
      ]{ type Source = Book; type Target = Option[(String, Option[String])] } ::
      ScalaExpr.Composition[
        ScalaExpr.SelectField[Meta, isbn, String],
        ScalaExpr.SelectField[Book, meta, Meta]
      ]{ type Source = Book; type Target = String } ::
      HNil
    ]{
      type Source = Book
      type Target = (String, Option[Option[String]], Option[String], Option[Tuple1[String]], Option[(String, Option[String])], String)
    }
  ]


  // TODO: test mapping to GraphPath
  // TODO: test CypherQueryArrow
  val split3_0 = new Arrow.SplitOps(bookId).split(_.title, _.meta.isbn)
  val split3 = bookId.split(_.title, _.meta.isbn)
  implicitly[split3.type <:<
    ScalaExpr.Split[
      ScalaExpr.SelectField[Book, title, String] ::
      ScalaExpr.Composition[
        ScalaExpr.SelectField[Meta, isbn, String],
        ScalaExpr.SelectField[Book, meta, Meta]
      ]{ type Source = Book; type Target = String } ::
      HNil
    ]{ type Source = Book; type Target = (String, String) }
  ]


  // TODO: test mapping to GraphPath
  // TODO: test CypherQueryArrow
  val split4 = bookId.meta.split(_.isbn)
  implicitly[split4.type <:<
    ScalaExpr.Composition[
      ScalaExpr.Split[
        ScalaExpr.SelectField[Meta, isbn, String] ::
        HNil
      ]{ type Source = Meta; type Target = Tuple1[String] },
      ScalaExpr.SelectField[Book, meta, Meta]
    ]{ type Source = Book; type Target = Tuple1[String] }
  ]


//  val mapped0 = Functor.map(sel2 ∘ sel1).to[GraphPath]
//  val mapped1 = Functor.map(sel3 ∘ (sel2 ∘ sel1)).to[GraphPath]

//  val m  = Functor.map(sel3).to[GraphPath] ∘ Functor.map(sel2 ∘ sel1).to[GraphPath]
//  assert(mapped1 == m)
}