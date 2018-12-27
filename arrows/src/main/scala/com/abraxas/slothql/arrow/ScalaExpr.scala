package com.abraxas.slothql.arrow

import scala.annotation.implicitNotFound
import scala.language.{ dynamics, higherKinds }

import cats.data.Ior
import shapeless.labelled.{ FieldType, KeyTag }
import shapeless._

import com.abraxas.slothql.util.{ HasField, ShapelessUtils, ShowManifest }


trait ScalaExpr extends Arrow with ScalaExpr.FieldSelectionOps {
  val src: Manifest[Source]
  val tgt: Manifest[Target]
}

object ScalaExpr {
  type Aux[-S, +T] = ScalaExpr { type Source >: S; type Target <: T }

  def apply[A: Manifest]: Id[A] = Id[A]

  object Binary {
    // TODO: Left

    sealed trait PartiallyAppliedRight[E <: ScalaExpr, R <: ScalaExpr] extends ScalaExpr {
      val expr: E
      val right: R

      override def equals(any: Any): Boolean = PartialFunction.cond(any) {
        case that: PartiallyAppliedRight[_, _] => this.expr == that.expr && this.right == that.right
      }
      override def hashCode(): Int = (expr, right).##
      override def toString: String = s"PartiallyAppliedRight($expr, $right)"
    }
    object PartiallyAppliedRight {
      type Aux[E <: ScalaExpr, R <: ScalaExpr, SL, T] = PartiallyAppliedRight[E, R] { type Source = SL; type Target = T }
      def unapply[E <: ScalaExpr, R <: ScalaExpr](arg: PartiallyAppliedRight[E, R]): Option[(E, R)] = Some(arg.expr -> arg.right)
    }

    /** Builds [[PartiallyAppliedRight]]. */
    trait PartialApplyRight[E <: ScalaExpr, R <: ScalaExpr] extends DepFn2[E, R] { type Out <: ScalaExpr }
    object PartialApplyRight {
      type Aux[E <: ScalaExpr, R <: ScalaExpr, Applied <: ScalaExpr] = PartialApplyRight[E, R] { type Out = Applied }
      def apply[E <: ScalaExpr, R <: ScalaExpr](expr: E, right: R)(implicit papply: PartialApplyRight[E, R]): papply.Out = papply(expr, right)

      implicit def partialApplyBinaryRight[E <: ScalaExpr, R <: ScalaExpr, SL, SR](
        implicit
        binary: Unpack2[E#Source, Tuple2, SL, SR]
      ): PartialApplyRight.Aux[E, R, PartiallyAppliedRight.Aux[E, R, SL, E#Target]] =
        instance.asInstanceOf[PartialApplyRight.Aux[E, R, PartiallyAppliedRight.Aux[E, R, SL, E#Target]]]
      private lazy val instance = new PartialApplyRight[ScalaExpr, ScalaExpr] {
        type Out = PartiallyAppliedRight[_, _]
        def apply(t: ScalaExpr, u: ScalaExpr): PartiallyAppliedRight[_, _] =
          new PartiallyAppliedRight[ScalaExpr, ScalaExpr] {
            type Source = Any
            type Target = Any
            val expr: ScalaExpr = t
            val right: ScalaExpr = u
            val src: Manifest[Any] = t.src.typeArguments.head.asInstanceOf[Manifest[Any]]
            val tgt: Manifest[Any] = t.tgt.asInstanceOf[Manifest[Any]]
          }
      }
    }
  }


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


  case class Literal[A](lit: A)(implicit m: Manifest[A]) extends ScalaExpr {
    type Source = Unit
    type Target = A
    val src: Manifest[Unit] = Manifest.Unit
    val tgt: Manifest[A] = m
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

    implicit def fmapToDot[F[_], E <: ScalaExpr](
      implicit
      showSrc: util.ShowT[E#Source],
      toDot: util.ArrowToDot[E]
    ): util.ArrowToDot[FMap[F, E]] = dotCluster(_.expr, "map")
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

    implicit def mbindToDot[F[_], E <: ScalaExpr](
      implicit
      showSrc: util.ShowT[E#Source],
      toDot: util.ArrowToDot[E]
    ): util.ArrowToDot[MBind[F, E]] = dotCluster(_.expr, "flatMap")
  }

  private def dotCluster[A <: Arrow, S: util.ShowT, E <: ScalaExpr: util.ArrowToDot](getE: A => E, label: String) =
    util.ArrowToDot.define[A]{
      (a, srcId) =>
        val (sourceDot, source) = util.ArrowToDot.defaultNewTypeNode[S]
        val (dot, target) = implicitly[util.ArrowToDot[E]].apply(getE(a), source)
        s"""subgraph cluster_${scala.util.Random.nextInt(Int.MaxValue)}{
           |  $sourceDot
           |  $dot
           |}
           |edge [label="$label"];
           |$srcId -> $source;
           |""".stripMargin -> target
    }


  case class IterableFilter[F[_], E <: ScalaExpr](expr: E)(
      implicit
      iterable: F[_] <:< Iterable[_],
      mf: Manifest[F[_]],
      filterArrow: E#Target =:= Boolean
  ) extends ScalaExpr {
    type Source = F[expr.Source]
    type Target = Source
    val src: Manifest[Source] = classType(mf, expr.src)
    val tgt: Manifest[Target] = src
  }

  object OrderBy {
    sealed trait Direction
    case object Ascending  extends Direction
    case object Descending extends Direction
  }

  case class IterableOrderBy[F[_], E <: ScalaExpr](expr: E, dir: OrderBy.Direction)(
      implicit
      iterable: F[_] <:< Iterable[_],
      mf: Manifest[F[_]],
      targetOrdering: Ordering[E#Target] // TODO: require it?
  ) extends ScalaExpr {
    type Source = F[expr.Source]
    type Target = Source
    val src: Manifest[Source] = classType(mf, expr.src)
    val tgt: Manifest[Target] = src
  }

  case class IterableSlice[I](range: Ior[Int, Int])(implicit iterable: I <:< Iterable[_], mf: Manifest[I]) extends ScalaExpr {
    type Source = I
    type Target = I
    val src: Manifest[I] = mf
    val tgt: Manifest[I] = mf

    def from: Option[Int] = range.left
    def to: Option[Int] = range.right
  }
  object IterableSlice {
    def range[I](from: Int, to: Int)(implicit iterable: I <:< Iterable[_], mf: Manifest[I]): IterableSlice[I] = {
      require(from > 0, s"slice range 'from' cannot be negative, got $from")
      require(to > 0, s"slice range 'to' cannot be negative, got $to")
      IterableSlice(Ior.both(from, to))
    }
    def from[I](n: Int)(implicit iterable: I <:< Iterable[_], mf: Manifest[I]): IterableSlice[I] = {
      require(n > 0, s"slice range 'from' cannot be negative, got $n")
      IterableSlice(Ior.left(n))
    }
    def to[I](n: Int)(implicit iterable: I <:< Iterable[_], mf: Manifest[I]): IterableSlice[I] = {
      require(n > 0, s"slice range 'to' cannot be negative, got $n")
      IterableSlice(Ior.right(n))
    }
  }

  case class IterableToList[I, A]()(implicit iterable: I <:< Iterable[_], mi: Manifest[I], ma: Manifest[A]) extends ScalaExpr {
    type Source = I
    type Target = List[A]
    val src: Manifest[I] = mi
    val tgt: Manifest[List[A]] = classType(manifest[List[_]], ma)
  }


  sealed trait Compare[A, B] extends ScalaExpr { type Source = (A, B); type Target = Boolean; val tgt = Manifest.Boolean }
  object Compare {
    case class Eq [A, B]()(implicit val src: Manifest[(A, B)]) extends Compare[A, B]
    case class Neq[A, B]()(implicit val src: Manifest[(A, B)]) extends Compare[A, B]
    // TODO: more

    trait CreateInstance[Cmp[_, _] <: Compare[_, _]] { def apply[L, R](implicit m: Manifest[(L, R)]): Cmp[L, R] }
    object CreateInstance {
      implicit lazy val createEqInstance: CreateInstance[Eq] = new CreateInstance[Eq]{
        def apply[L, R](implicit m: Manifest[(L, R)]): Eq[L, R] = Eq[L, R]()
      }
      implicit lazy val createNeqInstance: CreateInstance[Neq] = new CreateInstance[Neq]{
        def apply[L, R](implicit m: Manifest[(L, R)]): Neq[L, R] = Neq[L, R]()
      }
    }
  }

  private def classType[F[_], T](clazz: Manifest[_ <: F[_]], arg: Manifest[T]): Manifest[F[T]] =
    Manifest.classType(clazz.runtimeClass.asInstanceOf[Class[F[T]]], arg)
  private def classType[F[_, _], T1, T2](clazz: Manifest[_ <: F[_, _]], arg1: Manifest[T1], arg2: Manifest[T2]): Manifest[F[T1, T2]] =
    Manifest.classType(clazz.runtimeClass.asInstanceOf[Class[F[T1, T2]]], arg1, arg2)


  // // // // // // Syntax Ops // // // // // //


  implicit class ScalaExprFMapOps[A <: ScalaExpr, F[_], S0](a: A)(
    implicit
    source0: A#Target <:< F[S0],
    functor: cats.Functor[F],
    mf0: Manifest[F[_]]
  ) {

    def map[B <: ScalaExpr](b: B)           (implicit compose: Arrow.Compose[FMap[F, B], A]                  ): compose.Out = compose(new FMap[F, B](b), a)
    def map[B <: ScalaExpr](fb: Id[S0] => B)(implicit compose: Arrow.Compose[FMap[F, B], A], mf: Manifest[S0]): compose.Out = compose(new FMap[F, B](fb(Id[S0])), a)
  }

  implicit class ScalaExprMBindOps[A <: ScalaExpr, F[_], S0](a: A)(
    implicit
    source0: A#Target <:< F[S0],
    monad: cats.Monad[F],
    mf0: Manifest[F[_]]
  ) {

    def flatMap[B <: ScalaExpr.Aux[_, F[_]]](b: B)           (implicit compose: Arrow.Compose[MBind[F, B], A]                  ): compose.Out = compose(new MBind[F, B](b), a)
    def flatMap[B <: ScalaExpr.Aux[_, F[_]]](fb: Id[S0] => B)(implicit compose: Arrow.Compose[MBind[F, B], A], mf: Manifest[S0]): compose.Out = compose(new MBind[F, B](fb(Id[S0])), a)
  }

  implicit class ScalaExprIterableOps[A <: ScalaExpr, S0, F[_]](a: A)(
    implicit
    fType: A#Target <:< F[S0],
    iterableT: A#Target <:< Iterable[_],    // TODO
    iterableF: F[_] <:< Iterable[_],  // TODO
    mf: Manifest[F[_]]  // TODO
  ) {
    private implicit def aTargetManifest = a.tgt.asInstanceOf[Manifest[A#Target]]

    def toList(implicit compose: Arrow.Compose[IterableToList[A#Target, S0], A], ms0: Manifest[S0]): compose.Out = compose(new IterableToList[A#Target, S0], a)

    def drop(n: Int)(implicit compose: Arrow.Compose[IterableSlice[A#Target], A]): compose.Out = compose(IterableSlice.from(n), a)
    def take(n: Int)(implicit compose: Arrow.Compose[IterableSlice[A#Target], A]): compose.Out = compose(IterableSlice.to(n), a)
    def slice(from: Int, to: Int)(implicit compose: Arrow.Compose[IterableSlice[A#Target], A]): compose.Out = compose(IterableSlice.range(from, to), a)

    def filter[B <: ScalaExpr](expr: B)            (implicit compose: Arrow.Compose[IterableFilter[F, B], A], filterArrow: B#Target =:= Boolean                   ): compose.Out = compose(IterableFilter[F, B](expr), a)
    def filter[B <: ScalaExpr](mkExpr: Id[S0] => B)(implicit compose: Arrow.Compose[IterableFilter[F, B], A], filterArrow: B#Target =:= Boolean, ms0: Manifest[S0]): compose.Out = compose(IterableFilter[F, B](mkExpr(Id[S0])), a)

    def orderBy[B <: ScalaExpr](mkExpr: Id[S0] => B, dir: OrderBy.type => OrderBy.Direction = null)
                               (implicit targetOrdering: Ordering[B#Target], compose: Strict[Arrow.Compose[IterableOrderBy[F, B], A]], ms0: Manifest[S0]): compose.value.Out =
      compose.value(IterableOrderBy(mkExpr(Id[S0]), Option(dir).map(_(OrderBy)).getOrElse(OrderBy.Ascending)), a)
  }

  implicit class CompareOps[L <: ScalaExpr](left: L) {
    def ===[R <: ScalaExpr](right: R)(implicit build: CompareOps.BuildCmpRight[L, Compare.Eq, R]): build.Out = build(left, right)
    def ===[R: Manifest]   (right: R)(implicit build: CompareOps.BuildCmpRight[L, Compare.Eq, Literal[R]]): build.Out = build(left, Literal(right))

    def =!=[R <: ScalaExpr](right: R)(implicit build: CompareOps.BuildCmpRight[L, Compare.Neq, R]): build.Out = build(left, right)
    def =!=[R: Manifest]   (right: R)(implicit build: CompareOps.BuildCmpRight[L, Compare.Neq, Literal[R]]): build.Out = build(left, Literal(right))

    def exprEq[R <: ScalaExpr](right: R)(implicit build: CompareOps.BuildCmpRight[L, Compare.Eq, R]): build.Out = build(left, right)
    def exprEq[R: Manifest]   (right: R)(implicit build: CompareOps.BuildCmpRight[L, Compare.Eq, Literal[R]]): build.Out = build(left, Literal(right))

    def exprNeq[R <: ScalaExpr](right: R)(implicit build: CompareOps.BuildCmpRight[L, Compare.Neq, R]): build.Out = build(left, right)
    def exprNeq[R: Manifest]   (right: R)(implicit build: CompareOps.BuildCmpRight[L, Compare.Neq, Literal[R]]): build.Out = build(left, Literal(right))
  }
  object CompareOps {
    @implicitNotFound("Cannot build compare expression: ${L} ${Cmp} ${R}")
    trait BuildCmpRight[L <: ScalaExpr, Cmp[_, _] <: Compare[_, _], R <: ScalaExpr] {
      type Out <: ScalaExpr
      def apply(left: L, right: R): Out
    }
    object BuildCmpRight {
      type Aux[L <: ScalaExpr, Cmp[_, _] <: Compare[_, _], R <: ScalaExpr, Out0 <: ScalaExpr] = BuildCmpRight[L, Cmp, R] { type Out = Out0 }

      implicit def buildCmpRight[L <: ScalaExpr, Cmp[_, _] <: Compare[_, _], R <: ScalaExpr, PAR <: ScalaExpr, Out <: ScalaExpr](
        implicit
        papplyR: Binary.PartialApplyRight.Aux[Cmp[L#Target, R#Target], R, PAR],
        cmpInstance: Compare.CreateInstance[Cmp],
        compose: Arrow.Compose.Aux[PAR, L, Out]
      ): BuildCmpRight.Aux[L, Cmp, R, Out] =
        new BuildCmpRight[L, Cmp, R] {
          type Out = compose.Out
          def apply(left: L, right: R): compose.Out = {
            val cmp = cmpInstance(classType(
              Tuple2Manifest,
              left.tgt.asInstanceOf[Manifest[L#Target]],
              right.tgt.asInstanceOf[Manifest[R#Target]]
            ))
            compose(papplyR(cmp, right), left)
          }
        }
      private lazy val Tuple2Manifest = manifest[(_, _)]
    }
  }

  protected trait FieldSelectionOps extends Dynamic {
    expr: ScalaExpr =>

    def selectDynamic[V, A <: Arrow](k: String)(
      implicit
      ev0: HasField.Aux[Target, k.type, V],
      ev1: expr.type <:< A, // using `expr.type` directly in `compose` would require _existential types_
      compose: Arrow.Compose[SelectField[Target, k.type, V], A],
      sourceMf: Manifest[Target],
      targetMf: Manifest[V]
    ): compose.Out = compose(SelectField(k), expr)
  }


  object Unsafe {
    def unchainRev(expr0: ScalaExpr): UnchainedRev = {
      def inner(expr: ScalaExpr): List[ScalaExpr] = expr match {
        case c: Composition[_, _] => inner(c.G) ::: inner(c.F)
        case _ => expr :: Nil
      }
      unchainedRev(inner(expr0): _*)
    }

    def unchainedRev(unchained: ScalaExpr*): UnchainedRev =
      UnchainedRev(unchained.toList, unchained.head.src.asInstanceOf[Manifest[Any]], unchained.last.tgt.asInstanceOf[Manifest[Any]])

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
