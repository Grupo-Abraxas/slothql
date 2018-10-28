package com.abraxas.slothql.arrow

import scala.language.{ dynamics, higherKinds }

import shapeless.labelled.{ FieldType, KeyTag }
import shapeless.tag.@@
import shapeless._

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
    def unapply[A](arr: Id[A]): Option[Manifest[_]] = PartialFunction.condOpt(arr) { case id: Id[_] => id.src }
  }

  implicit def scalaExprIdArrowBuilder[A <: ScalaExpr, T: Manifest]: Arrow.Id.Builder.Aux[A, T, Id[T]] =
    new Arrow.Id.Builder[A, T] {
      type Out = Id[T]
      def apply(): Id[T] = Id[T]
    }

  sealed trait Composition[F <: ScalaExpr, G <: ScalaExpr] extends ScalaExpr with Arrow.Composition[F, G]
  object Composition {
    type Aux[F <: ScalaExpr, G <: ScalaExpr, S, T] = Composition[F, G] { type Source = S; type Target = T }
    def unapply[F <: ScalaExpr, G <: ScalaExpr](arr: Composition[F, G]): Option[(F, G)] =
      PartialFunction.condOpt(arr) { case c: Composition[_, _] => c.F -> c.G }

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
    def unapply[Arrows <: HList](arr: Split[Arrows]): Option[List[ScalaExpr]] = PartialFunction.condOpt(arr) { case split: Split[_] => split.toList }

    protected[ScalaExpr] def mkSplitter[Arrows <: HList, As, S, T](
      implicit
      sourceMf: Manifest[S],
      targetMf: Manifest[T]
    ): Arrow.Split.Splitter0.Aux[Arrows, As, Split.Aux[Arrows, S, T]] =
      new Arrow.Split.Splitter0[Arrows, As] {
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
  }

  implicit def splitScalaExprAsHList[Arrows <: HList, S, Ts <: HList](
    implicit
    areScalaExprs: LUBConstraint[Arrows, ScalaExpr],
    canSplit: Arrow.Split.Splitter.CanSplit.Aux[Arrows, S, Ts],
    sourceMf: Manifest[S],
    targetMf: Manifest[Ts]
  ): Arrow.Split.Splitter0.Aux[Arrows, HList, Split.Aux[Arrows, S, Ts]] = Split.mkSplitter


  implicit def splitScalaExprAsProduct[Arrows <: HList, Ts <: HList, S, T](
    implicit
    areScalaExprs: LUBConstraint[Arrows, ScalaExpr],
    canSplit: Arrow.Split.Splitter.CanSplit.Aux[Arrows, S, Ts],
    tupler: ops.hlist.Tupler.Aux[Ts, T],
    sourceMf: Manifest[S],
    targetMf: Manifest[T]
  ): Arrow.Split.Splitter0.Aux[Arrows, Product, Split.Aux[Arrows, S, T]] = Split.mkSplitter



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

  case class ToList[I, A]()(implicit iterable: I <:< Iterable[_], mi: Manifest[I], ma: Manifest[A]) extends ScalaExpr {
    type Source = I
    type Target = List[A]
    val src: Manifest[I] = mi
    val tgt: Manifest[List[A]] = classType(manifest[List[_]], ma)
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

  implicit class ScalaExprToListOps[A <: ScalaExpr, TA, S0](a: A)(
    implicit
    targetA: Types.Aux[A, _, TA],
    iterable: TA <:< Iterable[S0],
    mf: Manifest[TA]
  ) {
    def toList(implicit compose: Arrow.Compose[ToList[TA, S0], A], ma: Manifest[S0]): compose.Out = compose(new ToList[TA, S0], a)
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


  /** This arrow serves only as an identifier for Functor. */
  sealed trait TargetToRecord extends Arrow
  object TargetToRecord {

    implicit def scalaExprLabelledRootTargetToRecordFunctor[E <: ScalaExpr, EL <: ScalaExpr, S, T, R <: ScalaExpr](
      implicit
      label: Lazy[Functor.Aux[E, TargetToRecord, EL]],
      types: Types.Aux[EL, S, T],
      isLabelled: T <:< KeyTag[_, _],
      changeTargetType: Internal.UnsafeChangeTypes.Aux[EL, S, T :: HNil, R]
    ): Functor.Aux[Functor.Root[E], TargetToRecord, R] =
      Functor.define[Functor.Root[E], TargetToRecord](t => changeTargetType(label.value(t.arrow)))

    implicit def scalaExprSelectFieldTargetToRecordFunctor[E <: ScalaExpr, Obj, K <: String, V](
      implicit
      selectField: E <:< SelectField[Obj, K, V]
    ): Functor.Aux[E, TargetToRecord, SelectField[Obj, K, FieldType[K, V]]] =
      Functor.define[E, TargetToRecord](_.asInstanceOf[SelectField[Obj, K, FieldType[K, V]]])

    implicit def scalaExprSplitTargetToRecordFunctor[From <: ScalaExpr, Exprs <: HList, Mapped <: HList](
      implicit
      isSplit: From <:< Split[Exprs],
      fmap: Lazy[Functor.FMapHList.Aux[Exprs, TargetToRecord, Mapped]],
      split: Arrow.Split.Splitter0[Mapped, HList]
    ): Functor.Aux[From, TargetToRecord, split.Out] =
      Functor.define[From, TargetToRecord](t => split(fmap.value(t.arrows)))

    // uses `ComposeLabelled`
    implicit def scalaExprCompositionTargetToRecordFunctor[From <: ScalaExpr, FromF <: ScalaExpr, ToF <: ScalaExpr, FromG <: ScalaExpr, ToG <: ScalaExpr](
       implicit
       composition: From <:< Composition[FromF, FromG],
       fF: Lazy[Functor.Aux[FromF, TargetToRecord, ToF]],
       fG: Lazy[Functor.Aux[FromG, TargetToRecord, ToG]],
       compose: ComposeLabelled[ToF, ToG]
     ): Functor.Aux[From, TargetToRecord, compose.Out] =
      Functor.define[From, TargetToRecord](t => compose(fF.value(t.F), fG.value(t.G)))

    trait ComposeLabelled[F <: ScalaExpr, G <: ScalaExpr] extends Arrow.Compose[F, G] { type Out <: ScalaExpr }
    object ComposeLabelled {
      type Aux[F <: ScalaExpr, G <: ScalaExpr, Composition <: ScalaExpr] = ComposeLabelled[F, G] { type Out = Composition }

      implicit def composeLabelledScalaExprs[F <: ScalaExpr, G <: ScalaExpr, GT, K, S, T](
        implicit
        gTypes: Types.Aux[G, _, GT],
        gTargetIsLabelled: GT <:< KeyTag[K, _],
        typesCorrespond: Arrow.Compose.TypesCorrespond.Aux[F, G, S, T],
        ev: T <:< Product
        // fNotId: F <:!< Arrow.Id[_],
        // gNotId: G <:!< Arrow.Id[_]
      ): ComposeLabelled.Aux[F, G, Composition.Aux[F, G, S, FieldType[K, T]]] =
        instance.asInstanceOf[ComposeLabelled.Aux[F, G, Composition.Aux[F, G, S, FieldType[K, T]]]]

      implicit def composeLabelledNonTupleScalaExprs[F <: ScalaExpr, G <: ScalaExpr, GT, K, S, T](
        implicit
        gTypes: Types.Aux[G, _, GT],
        gTargetIsLabelled: GT <:< KeyTag[K, _],
        typesCorrespond: Arrow.Compose.TypesCorrespond.Aux[F, G, S, T],
        ev: T <:!< Product
        // fNotId: F <:!< Arrow.Id[_],
        // gNotId: G <:!< Arrow.Id[_]
      ): ComposeLabelled.Aux[F, G, Composition.Aux[F, G, S, FieldType[K, T :: HNil]]] =
        instance.asInstanceOf[ComposeLabelled.Aux[F, G, Composition.Aux[F, G, S, FieldType[K, T :: HNil]]]]

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

  protected object Internal {
    trait UnsafeChangeTypes[E <: ScalaExpr, S, T] {
      type Out <: ScalaExpr
      final def apply(expr: ScalaExpr): Out = expr.asInstanceOf[Out]
    }
    object UnsafeChangeTypes {
      type Aux[E <: ScalaExpr, S, T, R <: ScalaExpr] = UnsafeChangeTypes[E, S, T] { type Out = R }

      implicit def changeId[E <: ScalaExpr, T](implicit isId: E <:< Id[_]): Aux[E, T, T, Id[T]] = inst
      implicit def changeComposition[E <: ScalaExpr, F <: ScalaExpr, G <: ScalaExpr, S, T](implicit isComposition: E <:< Composition[F, G]): Aux[E, S, T, Composition.Aux[F, G, S, T]] = inst
      implicit def changeSplit[E <: ScalaExpr, Arrows <: HList, S, T](implicit isSplit: E <:< Split[Arrows]): Aux[E, S, T, Split.Aux[Arrows, S, T]] = inst
      implicit def changeSelectField[E <: ScalaExpr, S, T, K <: String](implicit isSelectField: E <:< SelectField[_, K, _]): Aux[E, S, T, SelectField[S, K, T]] = inst
      // implicit def changeFMap
      // implicit def changeMBind
      // implicit def changeSelectIn

      private def inst[E <: ScalaExpr, S, T, R <: ScalaExpr] = instance.asInstanceOf[UnsafeChangeTypes.Aux[E, S, T, R]]
      private lazy val instance = new UnsafeChangeTypes[ScalaExpr, Any, Any] {}
    }
  }

  object Unsafe {
    def unchainRev(expr0: ScalaExpr): UnchainedRev = {
      def inner(expr: ScalaExpr): List[ScalaExpr] = expr match {
        case c: Composition[_, _] => inner(c.G) ::: inner(c.F)
        case _ => expr :: Nil
      }
      val unchained = inner(expr0)
      UnchainedRev(unchained, unchained.head.src.asInstanceOf[Manifest[Any]], unchained.last.tgt.asInstanceOf[Manifest[Any]])
    }

    final case class UnchainedRev protected(toList: List[ScalaExpr], src: Manifest[Any], tgt: Manifest[Any]) extends ScalaExpr {
      type Source = Any
      type Target = Any

      def isEmpty: Boolean = toList.isEmpty
      def nonEmpty: Boolean = !isEmpty

      def headOption: Option[ScalaExpr] = toList.headOption
      def head: ScalaExpr = headOption.get

      def tail: UnchainedRev = mk(toList.tail)
      def uncons: Option[(ScalaExpr, UnchainedRev)] = headOption.map(_ -> tail)

      def filter(pred: ScalaExpr => Boolean): UnchainedRev = mk(toList.filter(pred))

      private def mk(l: List[ScalaExpr]) =
        if (l.nonEmpty) copy(l, l.head.src.asInstanceOf[Manifest[Any]], l.last.tgt.asInstanceOf[Manifest[Any]])
        else UnchainedRev(Nil, Manifest.Nothing.asInstanceOf[Manifest[Any]], Manifest.Nothing.asInstanceOf[Manifest[Any]])
    }
  }
}
