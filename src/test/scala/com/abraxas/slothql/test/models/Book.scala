package com.abraxas.slothql.test.models

import shapeless._

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
    val tpe: String = "pages"
    val fields: Map[String, GraphRepr.Property] = Map("index" -> GraphRepr.Property[Long])
    val from: GraphRepr.Node = BookRepr
    val to: GraphRepr.Node = Page.PageRepr
  }

  object BookRepr extends GraphRepr.Node {
    type Labels = Witness.`"Book"`.T :: HNil
    type Fields = Witness.`'author`.Field[GraphRepr.Property.Aux[String]] :: HNil
    type Outgoing = Witness.`'pages`.Field[PageListRepr.type] :: HNil
    val labels: List[String] = List("Book")
    val fields: Map[String, GraphRepr.Property] = Map("author" -> GraphRepr.Property[String])
    val outgoing: Map[String, GraphRepr.Relation] = Map("pages" -> PageListRepr)
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
    val labels: List[String] = List("Page")
    val fields: Map[String, GraphRepr.Property] = Map("text" -> GraphRepr.Property[String])
    val outgoing: Map[String, GraphRepr.Relation] = Map()
  }
  implicit def pageSchema: Schema.Aux[Page, PageRepr.type] = Schema.defineFor[Page](PageRepr)
}