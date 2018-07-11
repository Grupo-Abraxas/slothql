package com.abraxas.slothql

import scala.language.higherKinds

import com.abraxas.slothql.mapper._




object Functors {
/*
  implicit def fieldFocusToDBArrowLeaf[
    A <: Arrow, Obj, K <: Symbol, V, Repr <: GraphRepr.Element, Fields <: HList, Field <: GraphRepr.Property, V0
  ](
    implicit
    arr: A <:< FieldFocus[Obj, K, V],
    schema: Schema.Aux[Obj, Repr],
    node: Repr <:< GraphRepr.Node.Aux[_, Fields, _],
    select: shapeless.ops.record.Selector.Aux[Fields, K, Field],
    prop: Field <:< GraphRepr.Property.Aux[V0],
    ev: V <:< V0
  ): Functor.Aux[A, GraphPath, PropSelection[Repr, Field]] =
    Functor.define[A, GraphPath](_ => PropSelection(schema.repr, select(schema.repr.Fields)))

  implicit def fieldAndSeqFocusToDBArrow[
    A <: Arrow, AField <: Arrow, ASeq <: Arrow,
    Obj, K <: Symbol, V, Repr <: GraphRepr.Node,
    CC[x] <: Seq[x], V0,
    Rels <: HList, Rel <: GraphRepr.Relation, RelFields <: HList, RelTarget <: GraphRepr.Node,
    IndexField, IndexProp <: GraphRepr.Property, Index
  ](
    implicit
    arr: A <:< Arrow.Composition[ASeq, AField],
    fieldArr: AField <:< FieldFocus[Obj, K, V],
    seqArr: ASeq <:< SeqFocus[CC, V0],
    seq: V <:< CC[V0],
    schema: Schema.Aux[Obj, Repr],
    node: Repr <:< GraphRepr.Node.Aux[_, _, Rels],
    select: shapeless.ops.record.Selector.Aux[Rels, K, Rel],
    outgoing: Rel <:< GraphRepr.Relation.Aux[_, RelFields, _, RelTarget],
    onlyIndex: shapeless.ops.hlist.IsHCons.Aux[RelFields, IndexField, HNil],
    indexProp: IndexField <:< Witness.`'index`.Field[IndexProp],
    index: IndexProp <:< GraphRepr.Property.Aux[Index],
    integralIndex: Integral[Index],
    compose: Compose[RelationTarget[Rel, RelTarget], OutgoingRelation[Repr, Rel]]
   ): Functor.Aux[A, GraphPath, compose.Out] =
    Functor.define[A, GraphPath]{ _ =>
      val rel = select(schema.repr.Outgoing.asInstanceOf[Rels])
      compose(RelationTarget(rel, rel.To.asInstanceOf[RelTarget]), OutgoingRelation(schema.repr, rel))
    }
*/
}



object FunctorsTest {
  import cats.instances.list._
  import cats.instances.option._

  import com.abraxas.slothql.test.models._

  lazy val book = Book("History of Rome", Some(author), Nil, meta)
  lazy val author = Author("Theodor Mommsen", pseudonym = None)
  lazy val meta = Meta(isbn = "9786610240531")

  lazy val somePage = Page("...")


  val selPages = ScalaExpr[Book].pages
  // ScalaExpr.SelectField[Book, pages, List[Page]]
  // = SelectField("pages")
  val selPagesF = FuncArrow(selPages)
  // Func1Arrow[Book, List[Page]] = <function1>
  selPagesF(book)
  // List[Page] = List()


  val selText  = ScalaExpr[Page].text
  // ScalaExpr.SelectField[Page, text, String]
  // = SelectField("text")
  val selTextF = FuncArrow(selText)
  // Func1Arrow[Page, String] = <function1>
  selTextF(somePage)
  // String = "..."

  val selTextFMap = ScalaExpr.FMap.mk[List](selText)
  // ScalaExpr.FMap[List, ScalaExpr.SelectField[Page, text, String]]
  // = FMap(SelectField("text"))
  val selTextFMapF = FuncArrow(selTextFMap)
  // Func1Arrow[List[Page], List[String]] = <function1>
  selTextFMapF(List(somePage))
  // List[String] = List("...")

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

  val mapPagesText1 = selPages.map(selText)
  // Arrow.Composition[
  //  ScalaExpr.FMap[List, ScalaExpr.SelectField[Page, text, String]],
  //  ScalaExpr.SelectField[Book, pages, List[Page]]
  // ]{type Source = Book;type Target = List[String]}
  // = FMap(SelectField(text)) ∘ SelectField(pages)
  val mapPagesText1F = FuncArrow(mapPagesText1)
  // Func1Arrow[Book, List[String]] = <function1>
  mapPagesText1F(book)
  // List[String] = List()


  val mapPagesText2 = selPages.map(_.text)
  // Arrow.Composition[
  //  ScalaExpr.FMap[List, ScalaExpr.SelectField[Page, text, String]],
  //  ScalaExpr.SelectField[Book, pages, List[Page]]
  // ]{type Source = Book;type Target = List[String]}
  // = FMap(SelectField(text)) ∘ SelectField(pages)
  val mapPagesText2F = FuncArrow(mapPagesText2)
  // Func1Arrow[Book, List[String]] = <function1>
  mapPagesText2F(book)
  // List[String] = List()

  val selAuthor = ScalaExpr[Book].selectDynamic("author") // same as `.author`
  // ScalaExpr.SelectField[Book, author, Option[Author]]
  // = SelectField("author")
  val selAuthorF = FuncArrow(selAuthor)
  // Func1Arrow[Book, Option[Author]] = <function1>
  selAuthorF(book)
  // Option[Author] = Some(Author("Theodor Mommsen", None))

  val mapAuthorName = selAuthor.map(_.name)
  // Arrow.Composition[
  //  ScalaExpr.FMap[Option, ScalaExpr.SelectField[Author, name, String]],
  //  ScalaExpr.SelectField[Book, author, Option[Author]]
  // ]{type Source = Book;type Target = Option[String]}
  // = FMap(SelectField(name)) ∘ SelectField(author)
  val mapAuthorNameF = FuncArrow(mapAuthorName)
  // Func1Arrow[Book, Option[String]] = <function1>
  mapAuthorNameF(book)
  // Option[String] = Some("Theodor Mommsen")

  val mapAuthorPseudonym = selAuthor.flatMap(_.pseudonym)
  // Arrow.Composition[
  //  ScalaExpr.MBind[Option, ScalaExpr.SelectField[Author, pseudonym, Option[String]]],
  //  ScalaExpr.SelectField[Book, author, Option[Author]]
  // ]{type Source = Book;type Target = Option[String]}
  // = MBind(SelectField(pseudonym)) ∘ SelectField(author)
  val mapAuthorPseudonymF = FuncArrow(mapAuthorPseudonym)
  // Func1Arrow[Book, Option[String]] = <function1>
  mapAuthorPseudonymF(book)
  // Option[String] = None

  val selIsbn = ScalaExpr[Book].meta.isbn
  // Arrow.Composition[
  //  ScalaExpr.SelectField[Meta, isbn, String],
  //  ScalaExpr.SelectField[Book, meta, Meta]
  // ]{type Source = Book;type Target = String}
  // = SelectField(isbn) ∘ SelectField(meta)
  val selIsbnF = FuncArrow(selIsbn)
  // Func1Arrow[Book, String] = <function1>
  selIsbnF(book)
  // String = "9786610240531"

  val bookId = ScalaExpr[Book]
  // ScalaExpr.Id[Book] = Id

  val bookIdF = FuncArrow(bookId)
  // Func1Arrow[Book, Book] = <function1>
  bookIdF(book)
  // Book = Book("History of Rome", Some(Author("Theodor Mommsen", None)), List(), Meta("9786610240531"))

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

  // TODO: import is required
  import FuncArrow.mapSplitToFunc1Arrow
  val split1F = FuncArrow(split1)
  // Func1Arrow[Book, (String, Option[Author], String)] = <function1>
  split1F(book)
  // (String, Option[Author], String) = ("History of Rome", Some(Author("Theodor Mommsen", None)), "9786610240531")

//  val mapped0 = Functor.map(sel2 ∘ sel1).to[GraphPath]
//  val mapped1 = Functor.map(sel3 ∘ (sel2 ∘ sel1)).to[GraphPath]

//  val m  = Functor.map(sel3).to[GraphPath] ∘ Functor.map(sel2 ∘ sel1).to[GraphPath]
//  assert(mapped1 == m)
}