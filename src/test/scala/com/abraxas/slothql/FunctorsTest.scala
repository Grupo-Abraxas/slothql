package com.abraxas.slothql

import scala.language.higherKinds

import com.abraxas.slothql.mapper._


object FunctorsTest {
  import cats.instances.list._
  import cats.instances.option._

  import com.abraxas.slothql.mapper.MapScalaExprToGraphPath._
  import com.abraxas.slothql.test.models._

  lazy val book = Book("History of Rome", Some(author), Nil, meta)
  lazy val author = Author("Theodor Mommsen", pseudonym = None)
  lazy val meta = Meta(isbn = "9786610240531")

  lazy val somePage = Page("...")


  // TODO: test CypherQueryArrow
  val selPages = ScalaExpr[Book].pages
  // ScalaExpr.SelectField[Book, pages, List[Page]]
  // = SelectField("pages")
  val selPagesF = FuncArrow.from(selPages)
  // Func1Arrow[Book, List[Page]] = <function1>
  selPagesF(book)
  // List[Page] = List()
  val pathPages = Functor.map(selPages).to[GraphPath]
  // GraphPath.OutgoingRelation[
  //  Book.BookRepr.type,
  //  Book.PageListRepr.type
  // ]
  // = OutgoingRelation(
  //  Node(labels = Book; fields = title -> Property[String]; outgoing = ),
  //  Relation(type = pages; fields = index -> Property[long]; from = Node["Book"]; to = Node["Page"])
  // )


  val selText = ScalaExpr[Page].text
  // ScalaExpr.SelectField[Page, text, String]
  // = SelectField("text")
  val selTextF = FuncArrow.from(selText)
  // Func1Arrow[Page, String] = <function1>
  selTextF(somePage)
  // String = "..."
  val pathText = Functor.map(selText).to[GraphPath]
  // GraphPath.PropSelection[
  //  Page.PageRepr.type,
  //  GraphRepr.Property{type Type = String}
  // ]
  // = PropSelection(
  //  Node(labels = Page; fields = text -> Property[String]; outgoing = ),
  //  Property[String]
  // )
  val textCypherArrow = Functor.map(pathText).to[CypherQueryArrow]
  // CypherQueryArrow{type Source = Unit;type Result = String}
  // = <CypherQueryArrow>
  val textCypherQuery = textCypherArrow(())
  // textCypherArrow.Target
  // = KnownClause(KnownMatch(NonEmptyList(KnownNode(Some(n),List(Page),Map()){ (`n`:`Page`) }),false,None){ MATCH (`n`:`Page`) },KnownReturn(KnownExpr(KnownKey(KnownVar[scala.collection.immutable.Map[java.lang.String, Any]](n){ `n` },text){ `n`.`text` },None){ `n`.`text` }){ RETURN `n`.`text` })
  // { MATCH (`n`:`Page`) RETURN `n`.`text` }

  // TODO: test mapping to GraphPath
  // TODO: test CypherQueryArrow
  val selTextFMap = ScalaExpr.FMap.mk[List](selText)
  // ScalaExpr.FMap[List, ScalaExpr.SelectField[Page, text, String]]
  // = FMap(SelectField("text"))
  val selTextFMapF = FuncArrow.from(selTextFMap)
  // Func1Arrow[List[Page], List[String]] = <function1>
  selTextFMapF(List(somePage))
  // List[String] = List("...")

  // TODO: test mapping to GraphPath
  // TODO: test CypherQueryArrow
  val mapPagesText1_0 = selTextFMap ∘ selPages
  // Arrow.Composition[
  //  ScalaExpr.FMap[List[A], ScalaExpr.SelectField[Page, text, String]],
  //  ScalaExpr.SelectField[Book, pages, List[Page]]
  // ]{type Source = Book;type Target = List[String]}
  // = FMap(SelectField(text)) ∘ SelectField(pages)
  val mapPagesText1_0F = selTextFMapF ∘ selPagesF
  // Func1Arrow[Book, List[String]] = <function1>
  mapPagesText1_0F(book)
  // List[String] = List()

  // TODO: test mapping to GraphPath
  // TODO: test CypherQueryArrow
  val mapPagesText1 = selPages.map(selText)
  // Arrow.Composition[
  //  ScalaExpr.FMap[List, ScalaExpr.SelectField[Page, text, String]],
  //  ScalaExpr.SelectField[Book, pages, List[Page]]
  // ]{type Source = Book;type Target = List[String]}
  // = FMap(SelectField(text)) ∘ SelectField(pages)
  val mapPagesText1F = FuncArrow.from(mapPagesText1)
  // Func1Arrow[Book, List[String]] = <function1>
  mapPagesText1F(book)
  // List[String] = List()


  // TODO: test mapping to GraphPath
  // TODO: test CypherQueryArrow
  val mapPagesText2 = selPages.map(_.text)
  // Arrow.Composition[
  //  ScalaExpr.FMap[List, ScalaExpr.SelectField[Page, text, String]],
  //  ScalaExpr.SelectField[Book, pages, List[Page]]
  // ]{type Source = Book;type Target = List[String]}
  // = FMap(SelectField(text)) ∘ SelectField(pages)
  val mapPagesText2F = FuncArrow.from(mapPagesText2)
  // Func1Arrow[Book, List[String]] = <function1>
  mapPagesText2F(book)
  // List[String] = List()


  val selAuthor = ScalaExpr[Book].selectDynamic("author") // same as `.author`
  // ScalaExpr.SelectField[Book, author, Option[Author]]
  // = SelectField("author")
  val selAuthorF = FuncArrow.from(selAuthor)
  // Func1Arrow[Book, Option[Author]] = <function1>
  selAuthorF(book)
  // Option[Author] = Some(Author("Theodor Mommsen", None))
  val pathAuthor = Functor.map(selAuthor).to[GraphPath]
  // Arrow.Composition[
  //  GraphPath.RelationTarget[
  //    GraphRepr.Relation.Empty[author, Book.BookRepr.type, GraphRepr.Node.Optional[Author.AuthorRepr.type]],
  //    GraphRepr.Node.Optional[Author.AuthorRepr.type]
  //  ],
  //  GraphPath.OutgoingRelation[
  //    Book.BookRepr.type,
  //    GraphRepr.Relation.Empty[author, Book.BookRepr.type, GraphRepr.Node.Optional[Author.AuthorRepr.type]]
  //  ]
  // ]{type Source = Book.BookRepr.type;type Target = GraphRepr.Node.Optional[Author.AuthorRepr.type]}
  // =   RelationTarget(
  //       Relation(type = author; fields = ; from = Node["Book"]; to = Node["Author"]),
  //       Node(labels = Author; fields = name -> Property[String], pseudonym -> Property[Option[String]]; outgoing = )
  // ) ∘ OutgoingRelation(
  //       Node(labels = Book; fields = title -> Property[String]; outgoing = ),
  //       Relation(type = author; fields = ; from = Node["Book"]; to = Node["Author"])
  // )
  val authorCypherArrow = Functor.map(pathAuthor).to[CypherQueryArrow]
  // CypherQueryArrow{type Source = Unit;type Result = scala.collection.immutable.Map[String,Any]}
  // = <CypherQueryArrow>
  val authorCypherQuery = authorCypherArrow(())
  // authorCypherArrow.Target
  // = KnownClause(KnownMatch(NonEmptyList(KnownPath(KnownNode(None,List(Book),Map()){ (:`Book`) },KnownRel(None,List(author),Map(),None,Outgoing){ -[:`author`]-> },KnownNode(Some(n),List(Author),Map()){ (`n`:`Author`) }){ (:`Book`) -[:`author`]-> (`n`:`Author`) }),false,None){ MATCH (:`Book`) -[:`author`]-> (`n`:`Author`) },KnownReturn(KnownExpr(KnownVar[scala.collection.immutable.Map[java.lang.String, Any]](n){ `n` },None){ `n` }){ RETURN `n` })
  // { MATCH (:`Book`) -[:`author`]-> (`n`:`Author`) RETURN `n` }

  // TODO: test mapping to GraphPath
  // TODO: test CypherQueryArrow
  val mapAuthorName = selAuthor.map(_.name)
  // Arrow.Composition[
  //  ScalaExpr.FMap[Option, ScalaExpr.SelectField[Author, name, String]],
  //  ScalaExpr.SelectField[Book, author, Option[Author]]
  // ]{type Source = Book;type Target = Option[String]}
  // = FMap(SelectField(name)) ∘ SelectField(author)
  val mapAuthorNameF = FuncArrow.from(mapAuthorName)
  // Func1Arrow[Book, Option[String]] = <function1>
  mapAuthorNameF(book)
  // Option[String] = Some("Theodor Mommsen")

  // TODO: test mapping to GraphPath
  // TODO: test CypherQueryArrow
  val mapAuthorPseudonym = selAuthor.flatMap(_.pseudonym)
  // Arrow.Composition[
  //  ScalaExpr.MBind[Option, ScalaExpr.SelectField[Author, pseudonym, Option[String]]],
  //  ScalaExpr.SelectField[Book, author, Option[Author]]
  // ]{type Source = Book;type Target = Option[String]}
  // = MBind(SelectField(pseudonym)) ∘ SelectField(author)
  val mapAuthorPseudonymF = FuncArrow.from(mapAuthorPseudonym)
  // Func1Arrow[Book, Option[String]] = <function1>
  mapAuthorPseudonymF(book)
  // Option[String] = None


  val selIsbn = ScalaExpr[Book].meta.isbn
  // Arrow.Composition[
  //  ScalaExpr.SelectField[Meta, isbn, String],
  //  ScalaExpr.SelectField[Book, meta, Meta]
  // ]{type Source = Book;type Target = String}
  // = SelectField(isbn) ∘ SelectField(meta)
  val selIsbnF = FuncArrow.from(selIsbn)
  // Func1Arrow[Book, String] = <function1>
  selIsbnF(book)
  // String = "9786610240531"
  val pathIsbn = Functor.map(selIsbn).to[GraphPath]
  // Arrow.Composition[
  //  GraphPath.PropSelection[Meta.MetaRepr.type, GraphRepr.Property{type Type = String}],
  //  Arrow.Composition[
  //    GraphPath.RelationTarget[GraphRepr.Relation.Empty[meta, Book.BookRepr.type, Meta.MetaRepr.type], Meta.MetaRepr.type],
  //    GraphPath.OutgoingRelation[Book.BookRepr.type, GraphRepr.Relation.Empty[meta, Book.BookRepr.type, Meta.MetaRepr.type]]
  //  ]{type Source = Book.BookRepr.type;type Target = Meta.MetaRepr.type}
  // ]{type Source = Book.BookRepr.type;type Target = GraphRepr.Property{type Type = String}}
  // = PropSelection(Node(labels = Meta; fields = isbn -> Property[String]; outgoing = ),Property[String])
  // ∘ RelationTarget(Relation(type = meta; fields = ; from = Node["Book"]; to = Node["Meta"]),Node(labels = Meta; fields = isbn -> Property[String]; outgoing = ))
  // ∘ OutgoingRelation(Node(labels = Book; fields = title -> Property[String]; outgoing = ),Relation(type = meta; fields = ; from = Node["Book"]; to = Node["Meta"]))
  val isbnCypherArrow = Functor.map(pathIsbn).to[CypherQueryArrow]
  // CypherQueryArrow{type Source = Unit;type Result = String}
  // = <CypherQueryArrow>
  val isbnCypherQuery = isbnCypherArrow(())
  // isbnCypherArrow.Target
  // = KnownClause(KnownMatch(NonEmptyList(KnownPath(KnownNode(None,List(Book),Map()){ (:`Book`) },KnownRel(None,List(meta),Map(),None,Outgoing){ -[:`meta`]-> },KnownNode(Some(n),List(Meta),Map()){ (`n`:`Meta`) }){ (:`Book`) -[:`meta`]-> (`n`:`Meta`) }),false,None){ MATCH (:`Book`) -[:`meta`]-> (`n`:`Meta`) },KnownReturn(KnownExpr(KnownKey(KnownVar[scala.collection.immutable.Map[java.lang.String, Any]](n){ `n` },isbn){ `n`.`isbn` },None){ `n`.`isbn` }){ RETURN `n`.`isbn` })
  // { MATCH (:`Book`) -[:`meta`]-> (`n`:`Meta`) RETURN `n`.`isbn` }


  val bookId = ScalaExpr[Book]
  // ScalaExpr.Id[Book] = Id
  val bookIdF = FuncArrow.from(bookId)
  // Func1Arrow[Book, Book] = <function1>
  bookIdF(book)
  // Book = Book("History of Rome", Some(Author("Theodor Mommsen", None)), List(), Meta("9786610240531"))
  val bookIdPath = Functor.map(bookId).to[GraphPath]
  // GraphPath.Initial[Book.BookRepr.type]
  // = Initial(Node(labels = Book; fields = title -> Property[String]; outgoing = ))
  val bookIdCypherArrow = Functor.map(bookIdPath).to[CypherQueryArrow]
  // CypherQueryArrow{type Source = Unit;type Result = scala.collection.immutable.Map[String,Any]}
  // = <CypherQueryArrow>
  val bookIdCypherQuery = bookIdCypherArrow(())
  // bookIdCypherArrow.Target
  // = KnownClause(KnownMatch(NonEmptyList(KnownNode(Some(n),List(Book),Map()){ (`n`:`Book`) }),false,None){ MATCH (`n`:`Book`) },KnownReturn(KnownExpr(KnownVar[scala.collection.immutable.Map[java.lang.String, Any]](n){ `n` },None){ `n` }){ RETURN `n` })
  // { MATCH (`n`:`Book`) RETURN `n` }


  val split1 = bookId >>> { book => Arrow.Split(book.title, book.author, book.meta.isbn) }
  // Arrow.Split[
  //  ScalaExpr.SelectField[Book, title, String] ::
  //  ScalaExpr.SelectField[Book, author, Option[Author]] ::
  //  Arrow.Composition[
  //      ScalaExpr.SelectField[Meta, isbn, String],
  //      ScalaExpr.SelectField[Book, meta, Meta]
  //    ]{type Source = Book;type Target = String} ::
  //  HNil
  // ]{type Source = Book;type Target = (String, Option[Author], String)}
  // = Split(SelectField(title) :: SelectField(author) :: SelectField(isbn) ∘ SelectField(meta) :: HNil)
  val split1F = FuncArrow.from(split1)
  // Func1Arrow[Book, (String, Option[Author], String)] = <function1>
  split1F(book)
  // (String, Option[Author], String) = ("History of Rome", Some(Author("Theodor Mommsen", None)), "9786610240531")
  val split1Path = Functor.map(split1).to[GraphPath]
  // Arrow.Split[
  //  GraphPath.PropSelection[Book.BookRepr.type, GraphRepr.Property{type Type = String}] ::
  //  Arrow.Composition[
  //      GraphPath.RelationTarget[
  //        GraphRepr.Relation.Empty[author, Book.BookRepr.type, GraphRepr.Node.Optional[Author.AuthorRepr.type]],
  //        GraphRepr.Node.Optional[Author.AuthorRepr.type]
  //      ],
  //      GraphPath.OutgoingRelation[
  //        Book.BookRepr.type,
  //        GraphRepr.Relation.Empty[author, Book.BookRepr.type, GraphRepr.Node.Optional[Author.AuthorRepr.type]]
  //      ]
  //    ]{type Source = Book.BookRepr.type;type Target = GraphRepr.Node.Optional[Author.AuthorRepr.type]} ::
  //  Arrow.Composition[
  //      GraphPath.PropSelection[Meta.MetaRepr.type, GraphRepr.Property{type Type = String}],
  //      Arrow.Composition[
  //        GraphPath.RelationTarget[
  //          GraphRepr.Relation.Empty[meta, Book.BookRepr.type, Meta.MetaRepr.type],
  //          Meta.MetaRepr.type
  //        ],
  //        GraphPath.OutgoingRelation[
  //          Book.BookRepr.type,
  //          GraphRepr.Relation.Empty[meta, Book.BookRepr.type, Meta.MetaRepr.type]
  //        ]
  //      ]{type Source = Book.BookRepr.type;type Target = Meta.MetaRepr.type}
  //    ]{type Source = Book.BookRepr.type;type Target = GraphRepr.Property{type Type = String}} ::
  //  HNil
  // ]{
  //  type Source = Book.BookRepr.type
  //  type Target = (GraphRepr.Property{type Type = String}, GraphRepr.Node.Optional[Author.AuthorRepr.type], GraphRepr.Property{type Type = String})
  // }
  // = Split(
  //  PropSelection(Node(labels = Book; fields = title -> Property[String]; outgoing = ),title,Property[String]) ::
  //  RelationTarget(Relation(type = author; fields = ; from = Node["Book"]; to = Node["Author"]),Node(labels = Author; fields = name -> Property[String], pseudonym -> Property[Option[String]]; outgoing = ))
  //    ∘ OutgoingRelation(Node(labels = Book; fields = title -> Property[String]; outgoing = ),Relation(type = author; fields = ; from = Node["Book"]; to = Node["Author"])) ::
  //  PropSelection(Node(labels = Meta; fields = isbn -> Property[String]; outgoing = ),isbn,Property[String])
  //    ∘ RelationTarget(Relation(type = meta; fields = ; from = Node["Book"]; to = Node["Meta"]),Node(labels = Meta; fields = isbn -> Property[String]; outgoing = ))
  //    ∘ OutgoingRelation(Node(labels = Book; fields = title -> Property[String]; outgoing = ),Relation(type = meta; fields = ; from = Node["Book"]; to = Node["Meta"])) ::
  //  HNil
  // )
  val split1CypherArrow = Functor.map(split1Path).to[CypherQueryArrow]
  // CypherQueryArrow{type Source = Unit; type Result = String :: scala.collection.immutable.Map[String,Any] :: String :: HNil}
  // = <CypherQueryArrow>
  val split1CypherQuery = split1CypherArrow(())
  // split1CypherArrow.Target
  // = KnownClause(KnownMatch(NonEmptyList(KnownNode(Some(n0),List(Book),Map()){ (`n0`:`Book`) }, KnownPath(KnownNode(None,List(Book),Map()){ (:`Book`) },KnownRel(None,List(author),Map(),None,Outgoing){ -[:`author`]-> },KnownNode(Some(n1),List(Author),Map()){ (`n1`:`Author`) }){ (:`Book`) -[:`author`]-> (`n1`:`Author`) }, KnownPath(KnownNode(None,List(Book),Map()){ (:`Book`) },KnownRel(None,List(meta),Map(),None,Outgoing){ -[:`meta`]-> },KnownNode(Some(n2),List(Meta),Map()){ (`n2`:`Meta`) }){ (:`Book`) -[:`meta`]-> (`n2`:`Meta`) }),false,None){ MATCH (`n0`:`Book`), (:`Book`) -[:`author`]-> (`n1`:`Author`), (:`Book`) -[:`meta`]-> (`n2`:`Meta`) },KnownReturn(KnownRetList(KnownExpr(KnownKey(KnownVar[scala.collection.immutable.Map[java.lang.String, Any]](n0){ `n0` },title){ `n0`.`title` },None){ `n0`.`title` }, KnownExpr(KnownVar[scala.collection.immutable.Map[java.lang.String, Any]](n1){ `n1` },None){ `n1` }, KnownExpr(KnownKey(KnownVar[scala.collection.immutable.Map[java.lang.String, Any]](n2){ `n2` },isbn){ `n2`.`isbn` },None){ `n2`.`isbn` }){ `n0`.`title`, `n1`, `n2`.`isbn` }){ RETURN `n0`.`title`, `n1`, `n2`.`isbn` })
  // { MATCH (`n0`:`Book`), (:`Book`) -[:`author`]-> (`n1`:`Author`), (:`Book`) -[:`meta`]-> (`n2`:`Meta`) RETURN `n0`.`title`, `n1`, `n2`.`isbn` }

  // Tested with `populate-2`
  // -- // -- // -- // -- // -- // -- // -- // -- // -- // -- // -- // -- // -- // -- // -- // -- // -- // -- // -- //
  // Seq[(String, Map[String, Any], String)] = Vector(
  //  ("History of Rome", Map("name" -> "Theodor Mommsen"), "9786610240531"),
  //  ("Homotopy Type Theory", Map("name" -> "Theodor Mommsen"), "9786610240531"),
  //  ("History of Rome", Map("name" -> "Theodor Mommsen"), ""),
  //  ("Homotopy Type Theory", Map("name" -> "Theodor Mommsen"), "")
  // )
  // TODO: fail ========================================================================================================



//  val mapped0 = Functor.map(sel2 ∘ sel1).to[GraphPath]
//  val mapped1 = Functor.map(sel3 ∘ (sel2 ∘ sel1)).to[GraphPath]

//  val m  = Functor.map(sel3).to[GraphPath] ∘ Functor.map(sel2 ∘ sel1).to[GraphPath]
//  assert(mapped1 == m)
}