package com.abraxas.slothql.arrow

import scala.language.{ dynamics, higherKinds }

import shapeless.labelled.{ FieldType, KeyTag }
import shapeless.tag.@@
import shapeless.{ <:!<, Cached, HList, LUBConstraint, LabelledGeneric, Lazy, LowPriority, ops }

import com.abraxas.slothql.arrow.Arrow.Types
import com.abraxas.slothql.util.{ ShapelessUtils, ShowManifest }


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
    def unapply(arr: Arrow): Option[Manifest[_]] = PartialFunction.condOpt(arr) { case id: Id[_] => id.src }
  }

  implicit def scalaExprIdArrowBuilder[A <: ScalaExpr, T: Manifest]: Arrow.Id.Builder.Aux[A, T, Id[T]] =
    new Arrow.Id.Builder[A, T] {
      type Out = Id[T]
      def apply(): Id[T] = Id[T]
    }

  sealed trait Composition[F <: ScalaExpr, G <: ScalaExpr] extends ScalaExpr with Arrow.Composition[F, G]
  object Composition {
    type Aux[F <: ScalaExpr, G <: ScalaExpr, S, T] = Composition[F, G] { type Source = S; type Target = T }
    def unapply(arr: Arrow): Option[(ScalaExpr, ScalaExpr)] = PartialFunction.condOpt(arr) { case c: Composition[_, _] => c.F -> c.G }

    protected[ScalaExpr] def mkComposition(f: ScalaExpr, g: ScalaExpr) =
      new Composition[ScalaExpr, ScalaExpr] {
        val F: ScalaExpr = f
        val G: ScalaExpr = g
        type Source = G.Source
        type Target = F.Target
        val src: Manifest[G.Source] = G.src
        val tgt: Manifest[F.Target] = F.tgt
      }
  }

  implicit def composeScalaExprs[F <: ScalaExpr, G <: ScalaExpr, S, T](
    implicit
    typesCorrespond: Arrow.Compose.TypesCorrespond.Aux[F, G, S, T],
    fNotId: F <:!< Arrow.Id[_],
    gNotId: G <:!< Arrow.Id[_]
  ): Arrow.Compose.Aux[F, G, Composition.Aux[F, G, S, T]] = composeInstance.asInstanceOf[Arrow.Compose.Aux[F, G, Composition.Aux[F, G, S, T]]]

  private lazy val composeInstance = new Arrow.Compose[ScalaExpr, ScalaExpr] {
    type Out = Composition[ScalaExpr, ScalaExpr]
    def apply(f: ScalaExpr, g: ScalaExpr): Composition[ScalaExpr, ScalaExpr] = Composition.mkComposition(f, g)
  }


  sealed trait Split[Arrows <: HList] extends ScalaExpr with Arrow.Split[Arrows] {
    override val toList: List[ScalaExpr]
  }
  object Split {
    type Aux[Arrows <: HList, S, T] = Split[Arrows] { type Source = S; type Target = T }
    def unapply(arr: Arrow): Option[List[ScalaExpr]] = PartialFunction.condOpt(arr) { case split: Split[_] => split.toList }
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
          val toList: List[ScalaExpr] = ShapelessUtils.unsafeHListToList(t)
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

  sealed trait Tagged extends Arrow
  object Tagged {
    implicit def tagScalaExprFunctor[E <: ScalaExpr, Obj, K <: String, V](
      implicit
      selectField: E <:< SelectField[Obj, K, V]
    ): Functor.Aux[E, Tagged, SelectField[Obj, K, V @@ K]] =
      Functor.define[E, Tagged](_.asInstanceOf[SelectField[Obj, K, V @@ K]])
  }

  sealed trait Labelled extends Arrow
  object Labelled {

    implicit def labelScalaExprSelectFieldFunctor[E <: ScalaExpr, Obj, K <: String, V](
      implicit
      selectField: E <:< SelectField[Obj, K, V]
    ): Functor.Aux[E, Labelled, SelectField[Obj, K, FieldType[K, V]]] =
      Functor.define[E, Labelled](_.asInstanceOf[SelectField[Obj, K, FieldType[K, V]]])

    // uses `ComposeLabelled`
    implicit def labelScalaExprCompositionFunctor[
      From <: ScalaExpr, FromF <: ScalaExpr, ToF <: ScalaExpr, FromG <: ScalaExpr, ToG <: ScalaExpr
    ](
      implicit
      composition: From <:< Composition[FromF, FromG],
      fF: Lazy[Functor.Aux[FromF, Labelled, ToF]],
      fG: Lazy[Functor.Aux[FromG, Labelled, ToG]],
      compose: ComposeLabelled[ToF, ToG]
     ): Functor.Aux[From, Labelled, compose.Out] =
      Functor.define[From, Labelled](t => compose(fF.value(t.F), fG.value(t.G)))

    trait ComposeLabelled[F <: ScalaExpr, G <: ScalaExpr] extends Arrow.Compose[F, G] { type Out <: ScalaExpr }
    object ComposeLabelled {
      type Aux[F <: ScalaExpr, G <: ScalaExpr, Composition <: ScalaExpr] = ComposeLabelled[F, G] { type Out = Composition }

      implicit def composeLabelledScalaExprs[F <: ScalaExpr, G <: ScalaExpr, GT, K, S, T](
        implicit
        gTypes: Types.Aux[G, _, GT],
        gTargetIsLabelled: GT <:< KeyTag[K, _],
        typesCorrespond: Arrow.Compose.TypesCorrespond.Aux[F, G, S, T]
        // fNotId: F <:!< Arrow.Id[_],
        // gNotId: G <:!< Arrow.Id[_]
      ): ComposeLabelled.Aux[F, G, Composition.Aux[F, G, S, FieldType[K, T]]] =
        instance.asInstanceOf[ComposeLabelled.Aux[F, G, Composition.Aux[F, G, S, FieldType[K, T]]]]

      private lazy val instance = new ComposeLabelled[ScalaExpr, ScalaExpr] {
        type Out = Composition[ScalaExpr, ScalaExpr]
        def apply(f: ScalaExpr, g: ScalaExpr): Out = Composition.mkComposition(f, g)
      }

      implicit def composeNonLabelledScalaExprs[F <: ScalaExpr, G <: ScalaExpr, C <: ScalaExpr](
        implicit
        compose: Arrow.Compose.Aux[F, G, C],
        lowPriority: LowPriority
      ): ComposeLabelled.Aux[F, G, C] =
        new ComposeLabelled[F, G] {
          type Out = C
          def apply(f: F, g: G): C = compose(f, g)
        }

    }
  }
}
