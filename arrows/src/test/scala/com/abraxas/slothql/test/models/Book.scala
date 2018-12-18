package com.abraxas.slothql.test.models

case class Book(title: String, pages: List[Page])
case class Page(text: String)
