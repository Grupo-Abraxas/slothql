package com.abraxas.slothql.test.models

import shapeless._
import shapeless.syntax.singleton._

import com.abraxas.slothql.mapper.{ GraphRepr, Schema }

case class Book(title: String, author: Option[Author], pages: List[Page], meta: Meta)
case class Author(name: String, pseudonym: Option[String])
case class Page(text: String)
case class Meta(isbn: String)

object Book {
  object PageListRepr extends GraphRepr.Relation {
    type Type = Witness.`"pages"`.T
    type Fields = Witness.`'index`.Field[GraphRepr.Property.Aux[Long]] :: HNil
    type From = BookRepr.type
    type To = Page.PageRepr.type

    lazy val Type: Type = "pages"
    lazy val Fields: Fields = 'index ->> GraphRepr.Property[Long] :: HNil
    lazy val From: From = BookRepr
    lazy val To: To = Page.PageRepr

    lazy val tpe: String = "pages"
    lazy val fields: Map[String, GraphRepr.Property] = Map("index" -> GraphRepr.Property[Long])
    lazy val from: GraphRepr.Node = BookRepr
    lazy val to: GraphRepr.Node = Page.PageRepr
  }

  object BookRepr extends GraphRepr.Node {
    type Labels = Witness.`"Book"`.T :: HNil
    type Fields = Witness.`'title`.Field[GraphRepr.Property.Aux[String]] :: HNil
    type Outgoing = Witness.`'author`.Field[GraphRepr.Node.Optional[Author.AuthorRepr.type]] ::
                    Witness.`'pages` .Field[PageListRepr.type] ::
                    Witness.`'meta`  .Field[Meta.MetaRepr.type] :: HNil

    lazy val Labels: Labels = "Book".narrow :: HNil
    lazy val Fields: Fields = 'title ->> GraphRepr.Property[String] :: HNil
    lazy val Outgoing: Outgoing = 'author ->> GraphRepr.Node.Optional(Author.AuthorRepr) ::
                                  'pages  ->> PageListRepr ::
                                  'meta   ->> Meta.MetaRepr :: HNil

    lazy val labels: List[String] = List("Book")
    lazy val fields: Map[String, GraphRepr.Property] = Map("title" -> GraphRepr.Property[String])
    // TODO =========================================================================================================
    lazy val outgoing: Map[String, GraphRepr.Relation] = ??? /*Map(
      "author" -> GraphRepr.Node.Optional(Author.AuthorRepr),
      "pages"  -> PageListRepr,
      "meta"   -> Meta.MetaRepr
    )*/
  }

  implicit def pagesSchema: Schema.Aux[List[Page], PageListRepr.type] = Schema.defineFor[List[Page]](PageListRepr)
  implicit def bookSchema: Schema.Aux[Book, BookRepr.type] = Schema.defineFor[Book](BookRepr)
}

object Author {
  object AuthorRepr extends GraphRepr.Node {
    type Labels = Witness.`"Author"`.T :: HNil
    type Fields = Witness.`'name`     .Field[GraphRepr.Property.Aux[String]] ::
                  Witness.`'pseudonym`.Field[GraphRepr.Property.Aux[Option[String]]] :: HNil
    type Outgoing = HNil
    val Labels: Labels = "Author".narrow :: HNil
    val Fields: Fields = 'name      ->> GraphRepr.Property[String] ::
                         'pseudonym ->> GraphRepr.Property[Option[String]] :: HNil
    val Outgoing: HNil = HNil
    val labels: List[String] = List("Author")
    val fields: Map[String, GraphRepr.Property] = Map("name"      -> GraphRepr.Property[String],
                                                      "pseudonym" -> GraphRepr.Property[Option[String]])
    val outgoing: Map[String, GraphRepr.Relation] = Map()
  }
}

object Page {
  object PageRepr extends GraphRepr.Node {
    type Labels = Witness.`"Page"`.T :: HNil
    type Fields = Witness.`'text`.Field[GraphRepr.Property.Aux[String]] :: HNil
    type Outgoing = HNil

    lazy val Labels: Labels = "Page".narrow :: HNil
    lazy val Fields: Fields = 'text ->> GraphRepr.Property[String] :: HNil
    lazy val Outgoing: HNil = HNil

    lazy val labels: List[String] = List("Page")
    lazy val fields: Map[String, GraphRepr.Property] = Map("text" -> GraphRepr.Property[String])
    lazy val outgoing: Map[String, GraphRepr.Relation] = Map()
  }
  implicit def pageSchema: Schema.Aux[Page, PageRepr.type] = Schema.defineFor[Page](PageRepr)
}

object Meta {
  object MetaRepr extends GraphRepr.Node {
    type Labels = Witness.`"Meta"`.T :: HNil
    type Fields = Witness.`'isbn`.Field[GraphRepr.Property.Aux[String]] :: HNil
    type Outgoing = HNil
    val Labels: Labels = "Meta".narrow :: HNil
    val Fields: Fields = 'isbn ->> GraphRepr.Property[String] :: HNil
    val Outgoing: Outgoing = HNil
    val labels: List[String] = List("Meta")
    val fields: Map[String, GraphRepr.Property] = Map("isbn" -> GraphRepr.Property[String])
    val outgoing: Map[String, GraphRepr.Relation] = Map()
  }
}