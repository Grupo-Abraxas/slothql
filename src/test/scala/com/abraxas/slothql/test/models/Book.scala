package com.abraxas.slothql.test.models

trait Book {
  def author: String
  def pages: List[Page]
}

trait Page {
  def text: String
}
