package com.abraxas.slothql.test.models

import shapeless._
import shapeless.syntax.singleton._

import com.abraxas.slothql.mapper.{ GraphRepr, Schema }

trait Book {
  def author: String
  def pages: List[Page]
}

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
    type Fields = Witness.`'author`.Field[GraphRepr.Property.Aux[String]] :: HNil
    type Outgoing = Witness.`'pages`.Field[PageListRepr.type] :: HNil

    lazy val Labels: Labels = "Book".narrow :: HNil
    lazy val Fields: Fields = 'author ->> GraphRepr.Property[String] :: HNil
    lazy val Outgoing: Outgoing = 'pages ->> PageListRepr :: HNil

    lazy val labels: List[String] = List("Book")
    lazy val fields: Map[String, GraphRepr.Property] = Map("author" -> GraphRepr.Property[String])
    lazy val outgoing: Map[String, GraphRepr.Relation] = Map("pages" -> PageListRepr)
  }

  implicit def pagesSchema: Schema.Aux[List[Page], PageListRepr.type] = Schema.defineFor[List[Page]](PageListRepr)
  implicit def bookSchema: Schema.Aux[Book, BookRepr.type] = Schema.defineFor[Book](BookRepr)
}

trait Page {
  def text: String
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