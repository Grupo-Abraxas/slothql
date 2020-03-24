package com.arkondata.slothql

import cats.instances.option._
import org.scalatest.Assertions._

import com.arkondata.slothql.arrow.{ Arrow, ScalaExpr }
import com.arkondata.slothql.test.models.{ Author, Book, Meta }

object ArrowCompositionAssociativityTest extends App {
  case class DummyShelf(book: Book)

  val shelfBook  = ScalaExpr[DummyShelf].book
  val bookIdArr  = ScalaExpr[Book]
  val bookAuthor = ScalaExpr[Book].author
  val bookMeta   = ScalaExpr[Book].meta
  val metaIdArr  = ScalaExpr[Meta]
  val metaIsbn   = ScalaExpr[Meta].isbn
  val authorName = ScalaExpr[Author].name
  val mapAuthorName = ScalaExpr.FMap.tpe[Option].expr(authorName)


  case class SomeA()
  case class SomeB()
  case class SomeC()
  case class SomeD()

  val aIdArr = Arrow.Id[SomeA]
  val bIdArr = Arrow.Id[SomeB]
  case object AB extends Arrow { type Source = SomeA; type Target = SomeB }
  case object BC extends Arrow { type Source = SomeB; type Target = SomeC }
  case object CD extends Arrow { type Source = SomeC; type Target = SomeD }


  assert{ (bookIdArr     ∘ (bookIdArr  ∘ bookIdArr))  ==  ((bookIdArr     ∘ bookIdArr)  ∘ bookIdArr) }
  assert{ (metaIdArr     ∘ (bookMeta   ∘ bookIdArr))  ==  ((metaIdArr     ∘ bookMeta)   ∘ bookIdArr) }
  assert{ (metaIsbn      ∘ (bookMeta   ∘ bookIdArr))  ==  ((metaIsbn      ∘ bookMeta)   ∘ bookIdArr) }
  assert{ (metaIsbn      ∘ (bookMeta   ∘ shelfBook))  ==  ((metaIsbn      ∘ bookMeta)   ∘ shelfBook) }
  assert{ (mapAuthorName ∘ (bookAuthor ∘ shelfBook))  ==  ((mapAuthorName ∘ bookAuthor) ∘ shelfBook) }

  assert{ (aIdArr        ∘ (aIdArr     ∘ aIdArr))     ==  ((aIdArr        ∘ aIdArr)     ∘ aIdArr) }
  assert{ (bIdArr        ∘ (AB         ∘ aIdArr))     ==  ((bIdArr        ∘ AB)         ∘ aIdArr) }
  assert{ (CD            ∘ (BC         ∘ AB))         ==  ((CD            ∘ BC)         ∘ AB) }

}
