package com.abraxas.slothql

import scala.language.higherKinds

import shapeless.tag.@@
import shapeless.{ HList, HNil, Witness }

import com.abraxas.slothql.mapper.Arrow.{ Compose, Types }
import com.abraxas.slothql.mapper.GraphPath._
import com.abraxas.slothql.mapper._


trait CodeArrow extends Arrow
object CodeArrow {
  /** Arrow representing object field selection. */
  trait FieldFocus[Obj, K <: Symbol, V] extends CodeArrow {
    type Source = Obj
    type Target = V
    val field: K
  }
  object FieldFocus {
    def stub[Obj, V](k: String)(implicit w: Witness.Aux[k.type]): FieldFocus[Obj, Symbol @@ w.T, V] =
      new FieldFocus[Obj, Symbol @@ w.T, V] { val field = Symbol(w.value).asInstanceOf[Symbol @@ w.T] }
  }

  // TODO
  /** Arrow representing ??? inside a sequence. */
  trait SeqFocus[CC[x] <: Seq[x], A] extends CodeArrow {
    type Source = CC[A]
    type Target = A
  }
  object SeqFocus {
    def stub[CC[x] <: Seq[x], A]: SeqFocus[CC, A] = new SeqFocus[CC, A] {}
  }

  /** Arrow representing mapping a sequence with arrow `F`. */
  trait SeqMapper[F <: Arrow, CC[x] <: Seq[x]] extends CodeArrow {
    type Source <: CC[_]
    type Target <: CC[_]
  }
  object SeqMapper {
    type Aux[F <: Arrow, CC[x] <: Seq[x], S, T] =
      SeqMapper[F, CC] { type Source = CC[S]; type Target = CC[T] }
    def stub[F <: Arrow, CC[x] <: Seq[x]](implicit t: Types[F]): Aux[F, CC, t.Source, t.Target] =
      new SeqMapper[F, CC] {
        type Source = CC[t.Source]
        type Target = CC[t.Target]
      }
  }
}


object Functors {
  import CodeArrow._

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
}



object FunctorsTest {
  import CodeArrow._
  import Functors._
  import com.abraxas.slothql.test.models.{ Book, Page }

  val sel1 = FieldFocus.stub[Book, List[Page]]("pages")
  val sel2 = SeqFocus.stub[List, Page]
  val sel3 = FieldFocus.stub[Page, String]("text")

  val mapped0 = Functor.map(sel2 ∘ sel1).to[GraphPath]
  // Arrow.Composition[
  //  GraphPath.RelationTarget[Book.PageListRepr.type, Page.PageRepr.type],
  //  GraphPath.OutgoingRelation[Book.BookRepr.type, Book.PageListRepr.type]
  // ]{type Source = Book.BookRepr.type;type Target = Page.PageRepr.type}

  val mapped1 = Functor.map(sel3 ∘ (sel2 ∘ sel1)).to[GraphPath]
  // Arrow.Composition[
  //  GraphPath.PropSelection[Page.PageRepr.type, GraphRepr.Property{type Type = String}],
  //  Arrow.Composition[
  //    GraphPath.RelationTarget[Book.PageListRepr.type, Page.PageRepr.type],
  //    GraphPath.OutgoingRelation[Book.BookRepr.type, Book.PageListRepr.type]
  //  ]{type Source = Book.BookRepr.type;type Target = Page.PageRepr.type}
  // ]{type Source = Book.BookRepr.type;type Target = GraphRepr.Property{type Type = String}}

  val m  = Functor.map(sel3).to[GraphPath] ∘ Functor.map(sel2 ∘ sel1).to[GraphPath]
  assert(mapped1 == m)
}