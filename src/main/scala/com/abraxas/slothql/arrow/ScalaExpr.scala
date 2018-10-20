package com.abraxas.slothql.arrow

import scala.language.{ dynamics, higherKinds }

import shapeless.tag.@@
import shapeless.{ <:!<, Cached, HList, LUBConstraint, LabelledGeneric, ops }

import com.abraxas.slothql.arrow.Arrow.Types
import com.abraxas.slothql.util.ShowManifest


sealed trait ScalaExpr extends Arrow with ScalaExpr.FieldSelectionOps {
  val src: Manifest[Source]
  val tgt: Manifest[Target]
}

object ScalaExpr {
  type Aux[-S, +T] = ScalaExpr { type Source >: S; type Target <: T }

  def apply[A: Manifest]: Id[A] = Id[A]


  /** Identity arrow. (self selection) */
  sealed trait Id[A] extends ScalaExpr with Arrow.Id[A] {
    override def toString: String = s"Id[${ShowManifest(src)}]"
  }
  object Id {
    def apply[A: Manifest]: Id[A] =
      new Id[A]{
        val src: Manifest[A] = manifest[A]
        val tgt: Manifest[A] = manifest[A]
      }
  }


  sealed trait Composition[F <: ScalaExpr, G <: ScalaExpr] extends ScalaExpr with Arrow.Composition[F, G]
  object Composition {
    type Aux[F <: ScalaExpr, G <: ScalaExpr, S, T] = Composition[F, G] { type Source = S; type Target = T }
  }

  implicit def composeScalaExprs[F <: ScalaExpr, G <: ScalaExpr, S, T](
    implicit
    typesCorrespond: Arrow.Compose.TypesCorrespond.Aux[F, G, S, T],
    fNotId: F <:!< Arrow.Id[_],
    gNotId: G <:!< Arrow.Id[_]
  ): Arrow.Compose.Aux[F, G, Composition.Aux[F, G, S, T]] = composeInstance.asInstanceOf[Arrow.Compose.Aux[F, G, Composition.Aux[F, G, S, T]]]

  private lazy val composeInstance = new Arrow.Compose[ScalaExpr, ScalaExpr] {
    type Out = Composition[ScalaExpr, ScalaExpr]
    def apply(f: ScalaExpr, g: ScalaExpr): Composition[ScalaExpr, ScalaExpr] =
      new Composition[ScalaExpr, ScalaExpr] {
        val F: ScalaExpr = f
        val G: ScalaExpr = g
        type Source = G.Source
        type Target = F.Target
        val src: Manifest[G.Source] = G.src
        val tgt: Manifest[F.Target] = F.tgt
      }
  }


  sealed trait Split[Arrows <: HList] extends ScalaExpr with Arrow.Split[Arrows]
  object Split {
    type Aux[Arrows <: HList, S, T] = Split[Arrows] { type Source = S; type Target = T }
  }

  implicit def splitScalaExpr[Arrows <: HList, S, Ts <: HList, T](
    implicit
    areScalaExprs: LUBConstraint[Arrows, ScalaExpr],
    canSplit: Arrow.Split.Splitter.CanSplit.Aux[Arrows, S, T],
    sourceMf: Manifest[S],
    targetMf: Manifest[T]
  ): Arrow.Split.Splitter.Aux[Arrows, Split.Aux[Arrows, S, T]] =
    new Arrow.Split.Splitter[Arrows] {
      type Out = Split.Aux[Arrows, S, T]
      def apply(t: Arrows): Split.Aux[Arrows, S, T] =
        new Split[Arrows] {
          val arrows: Arrows = t
          type Source = S
          type Target = T
          val src: Manifest[S] = sourceMf
          val tgt: Manifest[T] = targetMf
        }
    }


  /** Expression representing selection of a field of an ADT (case class). */
  case class SelectField[Obj: Manifest, K <: String, V: Manifest](field: K) extends ScalaExpr {
    type Source = Obj
    type Target = V
    val src: Manifest[Obj] = manifest[Obj]
    val tgt: Manifest[V] = manifest[V]

    override def toString: String = s"SelectField[${ShowManifest(src)}, ${ShowManifest(tgt)}]($field)"
  }

  /** Expression representing functor `map` operation. */
  case class FMap[F[_], E <: ScalaExpr](expr: E)(implicit val F: cats.Functor[F], mf: Manifest[F[_]]) extends ScalaExpr {
    type Source = F[expr.Source]
    type Target = F[expr.Target]
    val src: Manifest[F[expr.Source]] = classType(mf, expr.src)
    val tgt: Manifest[F[expr.Target]] = classType(mf, expr.tgt)

    override def toString: String = s"FMap[${ShowManifest(src)}, ${ShowManifest(tgt)}]($expr)"
  }
  object FMap {
    def mk[F[_]]: Builder[F] = Builder.asInstanceOf[Builder[F]]

    protected class Builder[F[_]] {
      def apply[E <: ScalaExpr](expr: E)(implicit F: cats.Functor[F], mf0: Manifest[F[_]]): FMap[F, E] = FMap[F, E](expr)
    }
    private object Builder extends Builder
  }
  
  /** Expression representing monadic bind / `flatMap` operation. */
  case class MBind[F[_], E <: ScalaExpr](expr: E)(implicit val M: cats.Monad[F], mf: Manifest[F[_]]) extends ScalaExpr {
    type Source = F[expr.Source]
    type Target = expr.Target
    val src: Manifest[F[expr.Source]] = classType(mf, expr.src)
    val tgt: Manifest[expr.Target] = expr.tgt

    override def toString: String = s"MBind[${ShowManifest(src)}, ${ShowManifest(tgt)}]($expr)"
  }
  object MBind {
    def mk[F[_]]: Builder[F] = Builder.asInstanceOf[Builder[F]]

    protected class Builder[F[_]] {
      def apply[E <: ScalaExpr](expr: E)(implicit M: cats.Monad[F], mf0: Manifest[F[_]]): MBind[F, E] = MBind[F, E](expr)
    }
    private object Builder extends Builder
  }

  // TODO
  case class SelectIn[F[_], Sel, V](sel: Sel)(implicit mf: Manifest[F[_]], mv: Manifest[V]) extends ScalaExpr {
    type Source = F[V]
    type Target = V
    val src: Manifest[F[V]] = classType(mf, mv)
    val tgt: Manifest[V] = mv

    override def toString: String = s"SelectIn[${ShowManifest(src)}, ${ShowManifest(tgt)}]($sel)"
  }

  private def classType[F[_], T](clazz: Manifest[_ <: F[_]], arg: Manifest[T]): Manifest[F[T]] =
    Manifest.classType(clazz.runtimeClass.asInstanceOf[Class[F[T]]], arg)


  // // // // // // Syntax Ops // // // // // //


  implicit class ScalaExprFMapOps[A <: ScalaExpr, F[_], TA, S0](a: A)(
    implicit
    targetA: Types.Aux[A, _, TA],
    source0: TA <:< F[S0],
    functor: cats.Functor[F],
    mf0: Manifest[F[_]]
  ) {

    def map[B <: ScalaExpr](b: B)(implicit compose: Arrow.Compose[FMap[F, B], A]): compose.Out = compose(new FMap[F, B](b), a)
    def map[B <: ScalaExpr](fb: Id[S0] => B)(implicit compose: Arrow.Compose[FMap[F, B], A], mf: Manifest[S0]): compose.Out = compose(new FMap[F, B](fb(Id[S0])), a)
  }

  implicit class ScalaExprMBindOps[A <: ScalaExpr, F[_], TA, S0](a: A)(
    implicit
    targetA: Types.Aux[A, _, TA],
    source0: TA <:< F[S0],
    monad: cats.Monad[F],
    mf0: Manifest[F[_]]
  ) {

    def flatMap[B <: ScalaExpr.Aux[_, F[_]]](b: B)(implicit compose: Arrow.Compose[MBind[F, B], A]): compose.Out = compose(new MBind[F, B](b), a)
    def flatMap[B <: ScalaExpr.Aux[_, F[_]]](fb: Id[S0] => B)(implicit compose: Arrow.Compose[MBind[F, B], A], mf: Manifest[S0]): compose.Out = compose(new MBind[F, B](fb(Id[S0])), a)
  }

  protected trait FieldSelectionOps extends Dynamic {
    expr: ScalaExpr =>

    def selectDynamic[V, A <: Arrow](k: String)(
      implicit
      ev0: Syntax.HasField.Aux[Target, Symbol @@ k.type, V],
      ev1: expr.type <:< A, // using `expr.type` directly in `compose` would require _existential types_
      compose: Arrow.Compose[SelectField[Target, k.type, V], A],
      sourceMf: Manifest[Target],
      targetMf: Manifest[V]
    ): compose.Out = compose(SelectField(k), expr)
  }

  object Syntax {
    /** Evidence that `Obj` has a field of type `V` with name `K` */
    @annotation.implicitNotFound(msg = "${Obj} doesn't have field ${K}")
    trait HasField[Obj, K] { type Value }
    object HasField {
      type Aux[Obj, K, V] = HasField[Obj, K] { type Value = V }
      implicit def evidence[Obj, K, V, Repr <: HList](
        implicit
        generic: Cached[LabelledGeneric.Aux[Obj, Repr]],
        select:  Cached[ops.record.Selector.Aux[Repr, K, V]]
      ): HasField.Aux[Obj, K, V] = instance.asInstanceOf[HasField.Aux[Obj, K, V]]
      private lazy val instance = new HasField[Any, Any]{}
    }

  }

}
