package com.abraxas.slothql

import shapeless._
import shapeless.labelled.KeyTag

import com.abraxas.slothql.arrow.{ Functor, ScalaExpr }
import com.abraxas.slothql.test.models.{ Book, Meta, Page }

object TargetToRecordTest {
  type pages = Witness.`"pages"`.T
  type isbn  = Witness.`"isbn"`.T
  type meta  = Witness.`"meta"`.T
  type title = Witness.`"title"`.T


  val selPages = ScalaExpr[Book].pages
  val selPagesRec = Functor.map(selPages).to[ScalaExpr.TargetToRecord]
  implicitly[selPagesRec.type <:<
    ScalaExpr.SelectField[Book, pages, (List[Page] with KeyTag[pages, List[Page]]) :: HNil]
  ]
  implicitly[selPagesRec.Source =:= Book]
  implicitly[selPagesRec.Target <:< (
    (List[Page] with KeyTag[pages, List[Page]]) ::
    HNil
  )]


  val selIsbn = ScalaExpr[Book].meta.isbn
  val selIsbnRec = Functor.map(selIsbn).to[ScalaExpr.TargetToRecord]
  implicitly[selIsbnRec.type <:<
    ScalaExpr.Composition[
      ScalaExpr.SelectField[Meta, isbn, String with KeyTag[isbn, String]],
      ScalaExpr.SelectField[Book, meta, Meta   with KeyTag[meta, Meta]]
    ]
  ]
  implicitly[selIsbnRec.Source =:= Book]
  implicitly[selIsbnRec.Target <:< (
    (String with KeyTag[isbn, String] :: HNil)
      with KeyTag[meta, String with KeyTag[isbn, String] :: HNil] ::
    HNil
  )]


  val splitBook = ScalaExpr[Book].split(_.title, _.meta.isbn)
  val splitBookRec = Functor.map(splitBook).to[ScalaExpr.TargetToRecord]
  implicitly[splitBookRec.type <:<
    ScalaExpr.Split[
      ScalaExpr.SelectField[Book, title, String with KeyTag[title, String]] ::
      ScalaExpr.Composition[
        ScalaExpr.SelectField[Meta, isbn, String with KeyTag[isbn, String]],
        ScalaExpr.SelectField[Book, meta, Meta   with KeyTag[meta, Meta]]
      ]{
        type Source = Book
        type Target = (String with KeyTag[isbn, String] :: HNil)
                        with KeyTag[meta, String with KeyTag[isbn, String] :: HNil]
      } ::
      HNil
    ]
  ]
  implicitly[splitBookRec.Source =:= Book]
  implicitly[splitBookRec.Target <:< (
    (String with KeyTag[title, String]) ::
    (String with KeyTag[isbn, String] :: HNil)
      with KeyTag[meta, String with KeyTag[isbn, String] :: HNil] ::
    HNil
  )]


  val splitMeta = ScalaExpr[Book].meta.split(_.isbn)
  val splitMetaRec = Functor.map(splitMeta).to[ScalaExpr.TargetToRecord]
  implicitly[splitMetaRec.type <:<
    ScalaExpr.Composition[
      ScalaExpr.Split[
        ScalaExpr.SelectField[Meta, isbn, String with KeyTag[isbn, String]] ::
        HNil
      ]{
        type Source = Meta
        type Target = String with KeyTag[isbn, String] :: HNil
      },
      ScalaExpr.SelectField[Book, meta, Meta with KeyTag[meta, Meta]]
    ]
  ]
  implicitly[splitMetaRec.Source =:= Book]
  implicitly[splitMetaRec.Target <:< (
    (String with KeyTag[isbn, String] :: HNil)
      with KeyTag[meta, String with KeyTag[isbn, String] :: HNil] ::
    HNil
  )]


}
