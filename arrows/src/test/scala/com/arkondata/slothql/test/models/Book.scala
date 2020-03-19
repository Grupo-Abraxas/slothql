package com.arkondata.slothql.test.models

case class Book(title: String, author: Option[Author], pages: List[Page], meta: Meta, reviews: List[Review])
case class Author(name: String, pseudonym: Option[String])
case class Page(text: String)
case class Meta(isbn: String)

sealed trait Review
case class AnonReview(text: String) extends Review
case class UserReview(user: String, text: String, vote: Int) extends Review
case object StubReview extends Review

trait CustomReview extends Review

case class Reviews(reviews: List[Review]) extends CustomReview
case class ImageReviewAttachment(review: UserReview, url: String) extends CustomReview
