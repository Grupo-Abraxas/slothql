package com.abraxas.slothql

import scala.language.higherKinds

import shapeless.Witness

import com.abraxas.slothql.util.{ Arrow, Not }
import com.abraxas.slothql.util.Arrow.{ Compose, Functor, Types }


trait CodeArrow extends Arrow
object CodeArrow {
  /** Arrow representing object field selection. */
  trait FieldFocus[Obj, K <: String, V] extends CodeArrow {
    type Source = Obj
    type Target = V
    val field: K
  }
  object FieldFocus {
    def stub[Obj, V](k: String)(implicit w: Witness.Aux[k.type]): FieldFocus[Obj, w.T, V] =
      new FieldFocus[Obj, w.T, V] { val field = w.value }
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


trait CanStore[A]
object CanStore {
  implicit lazy val canStoreString: CanStore[String] = new CanStore[String] {}
}




object Functors {
  import CodeArrow._
  import DBArrow._

  protected lazy val outgoingNode = Compose[RelationTarget.type, OutgoingRelation]

  implicit def mapFieldFocusToDBArrowNode[Obj, K <: String, V](
    implicit cannotStore: Not[CanStore[V]]
  ): Functor.Aux[FieldFocus[Obj, K, V], DBArrow, outgoingNode.Out] =
    Functor.define[FieldFocus[Obj, K, V], DBArrow](t => outgoingNode(RelationTarget, OutgoingRelation(t.field)))

  implicit def mapFieldFocusToDBArrowLeaf[Obj, K <: String, V](
    implicit canStore: CanStore[V]
  ): Functor.Aux[FieldFocus[Obj, K, V], DBArrow, NodePropSelection] =
    Functor.define[FieldFocus[Obj, K, V], DBArrow](t => NodePropSelection(t.field))

  implicit def mapSeqFocusToDBArrow[CC[x] <: Seq[x], A]: Functor.Aux[SeqFocus[CC, A], DBArrow, Arrow.Id[Node]] =
    Functor.define[SeqFocus[CC, A], DBArrow](_ => Arrow.Id[Node])

}



object FunctorsTest {
  import com.abraxas.slothql.test.models.{ Book, Page }
  import CodeArrow._

  val sel1 = FieldFocus.stub[Book, List[Page]]("pages")
  val sel2 = SeqFocus.stub[List, Page]
  val sel3 = FieldFocus.stub[Page, String]("text")

  import Functors._

  val mapped = Functor.map(sel3 ∘ sel2 ∘ sel1).to[DBArrow]
  // Arrow.Composition[
  //  DBArrow.NodePropSelection,
  //  Arrow.Composition[DBArrow.RelationTarget.type, DBArrow.OutgoingRelation]{type Source = Node;type Target = Node}
  // ]{type Source = Node;type Target = Leaf}
}