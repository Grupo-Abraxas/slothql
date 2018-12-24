package com.abraxas.slothql.test.models

case class Book(title: String, author: Option[Author], pages: List[Page], meta: Meta)
case class Author(name: String, pseudonym: Option[String])
case class Page(text: String)
case class Meta(isbn: String)
