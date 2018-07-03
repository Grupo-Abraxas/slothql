package com.abraxas.slothql

import scala.language.higherKinds

import shapeless.tag.@@
import shapeless.{ HList, HNil, Witness }

import com.abraxas.slothql.DBArrow.{ OutgoingRelation, RelationTarget }
import com.abraxas.slothql.mapper.Arrow.{ Compose, Types }
import com.abraxas.slothql.mapper.{ Arrow, Functor, GraphRepr, Schema }


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





trait GraphElem
trait Node extends GraphElem
trait Edge extends GraphElem
trait Leaf extends GraphElem


trait DBArrow extends Arrow {
  type Source <: GraphElem
  type Target <: GraphElem
}
object DBArrow {
  case class InitialVertex(labels: String*) extends DBArrow {
    type Source = Node
    type Target = Node
  }

  case class NodePropSelection(name: String) extends DBArrow {
    type Source = Node
    type Target = Leaf
  }

  trait NodeRelationArrow extends DBArrow {
    type Source = Node
    type Target = Edge
  }

  case class OutgoingRelation(names: String*) extends NodeRelationArrow
  case class IncomingRelation(names: String*) extends NodeRelationArrow

  trait RelationArrow extends DBArrow {
    type Source = Edge
    type Target = Node
  }
  case object RelationSource extends RelationArrow
  case object RelationTarget extends RelationArrow

}


object Foo {
  lazy val outgoingNode = Compose[RelationTarget.type, OutgoingRelation]
}

object Functors {
  import CodeArrow._
  import DBArrow._

  implicit def fieldFocusToDBArrowLeaf[
    A <: Arrow, Obj, K <: Symbol, V, Repr <: GraphRepr, Fields <: HList, Field <: GraphRepr, V0
  ](
    implicit
    arr: A <:< FieldFocus[Obj, K, V],
    schema: Schema.Aux[Obj, Repr],
    node: Repr <:< GraphRepr.Node.Aux[_, Fields, _],
    select: shapeless.ops.record.Selector.Aux[Fields, K, Field],
    prop: Field <:< GraphRepr.Property.Aux[V0],
    ev: V <:< V0
  ): Functor.Aux[A, DBArrow, NodePropSelection] =
    Functor.define[A, DBArrow](t => NodePropSelection(t.field.name))

  implicit def fieldAndSeqFocusToDBArrow[
    A <: Arrow, AField <: Arrow, ASeq <: Arrow,
    Obj, K <: Symbol, V, Repr <: GraphRepr,
    CC[x] <: Seq[x], V0,
    Rels <: HList, Rel <: GraphRepr.Relation, RelFields <: HList,
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
    outgoing: Rel <:< GraphRepr.Relation.Aux[_, RelFields, _, _],
    onlyIndex: shapeless.ops.hlist.IsHCons.Aux[RelFields, IndexField, HNil],
    indexProp: IndexField <:< Witness.`'index`.Field[IndexProp],
    index: IndexProp <:< GraphRepr.Property.Aux[Index],
    integralIndex: Integral[Index]
   ): Functor.Aux[A, DBArrow, Foo.outgoingNode.Out] =
    Functor.define[A, DBArrow](t => Foo.outgoingNode(RelationTarget, OutgoingRelation(t.G.field.name)))

}



object FunctorsTest {
  import CodeArrow._
  import Functors._
  import com.abraxas.slothql.test.models.{ Book, Page }

  val sel1 = FieldFocus.stub[Book, List[Page]]("pages")
  val sel2 = SeqFocus.stub[List, Page]
  val sel3 = FieldFocus.stub[Page, String]("text")

  val mapped0 = Functor.map(sel2 ∘ sel1).to[DBArrow]
  // Arrow.Composition[
  //  DBArrow.RelationTarget.type,
  //  DBArrow.OutgoingRelation
  // ]{type Source = Node;type Target = Node}

  val mapped1 = Functor.map(sel3 ∘ (sel2 ∘ sel1)).to[DBArrow]
  // Arrow.Composition[
  //  DBArrow.NodePropSelection,
  //  Arrow.Composition[DBArrow.RelationTarget.type, DBArrow.OutgoingRelation]{type Source = Node;type Target = Node}
  // ]{type Source = Node;type Target = Leaf}

  val m  = Functor.map(sel3).to[DBArrow] ∘ Functor.map(sel2 ∘ sel1).to[DBArrow]
  assert(mapped1 == m)
}