package com.abraxas.slothql.arrow

import scala.annotation.implicitNotFound
import scala.language.{ dynamics, higherKinds }
import scala.reflect.runtime.{ universe => ru }

import cats.data.Ior
import shapeless._

import com.abraxas.slothql.arrow.util.{ ArrowToDot, NotId, ShowT }
import com.abraxas.slothql.util.{ HasField, ShapelessUtils, TypeUtils }


trait ScalaExpr extends Arrow with ScalaExpr.FieldSelectionOps {
  val src: ru.TypeTag[Source]
  val tgt: ru.TypeTag[Target]

  def short: String
}

object ScalaExpr {
  type Aux[-S, +T] = ScalaExpr { type Source >: S; type Target <: T }

  def apply[A: ru.TypeTag]: Id[A] = Id[A]

  object Binary {
    // TODO: Left

    sealed trait PartiallyAppliedRight[E <: ScalaExpr, R <: ScalaExpr] extends ScalaExpr {
      val expr: E
      val right: R

      override def equals(any: Any): Boolean = PartialFunction.cond(any) {
        case that: PartiallyAppliedRight[_, _] => this.expr == that.expr && this.right == that.right
      }
      override def hashCode(): Int = (expr, right).##
      override def toString: String = s"PartiallyAppliedRight[${TypeUtils.Show(src)}, ${TypeUtils.Show(tgt)}]($expr, $right)"
      def short: String = s" ${expr.short} ${right.short}"
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
            val src: ru.TypeTag[Any] = mkTypeTag(t.src.tpe.typeArgs.head)
            val tgt: ru.TypeTag[Any] = t.tgt.asInstanceOf[ru.TypeTag[Any]]
          }
      }
    }
  }


  /** Identity arrow. (self selection) */
  sealed trait Id[A] extends ScalaExpr with Arrow.Id[A] {
    override def toString: String = s"Id[${TypeUtils.Show(src)}]"

    override def hashCode(): Int = src.##
    override def equals(obj: Any): Boolean = PartialFunction.cond(obj) {
      case that: Id[_] => this.src.tpe =:= that.src.tpe
    }

    def self: Id[A] = this
    def const[B: ru.TypeTag](b: B): Const[A, Literal[B]] = Const[A](Literal(b))(implicitly[Unit =:= Unit], this.src)
  }
  object Id {
    def apply[A: ru.TypeTag]: Id[A] =
      new Id[A]{
        val src: ru.TypeTag[A] = ru.typeTag[A]
        val tgt: ru.TypeTag[A] = ru.typeTag[A]
        def short: String = ""
      }
    def unapply[A](arr: Id[A]): Option[ru.TypeTag[_]] = PartialFunction.condOpt(arr) { case id: Id[_] => id.src }
  }

  implicit def scalaExprIdArrowBuilder[A <: ScalaExpr, T: ru.TypeTag]: Arrow.Id.Builder.Aux[A, T, Id[T]] =
    new Arrow.Id.Builder[A, T] {
      type Out = Id[T]
      def apply(): Id[T] = Id[T]
    }

  sealed trait Const[S, E <: ScalaExpr] extends ScalaExpr with Arrow.Const[S, E] {
    override val arrow: E

    override def hashCode(): Int = src.hashCode() + arrow.hashCode()
    override def equals(obj: Any): Boolean = PartialFunction.cond(obj) {
      case that: Const[_, _] => that.arrow == this.arrow && that.src.tpe =:= this.src.tpe
    }
  }

  object Const {
    def apply[S]: Builder[S] = Builder.asInstanceOf[Builder[S]]
    protected class Builder[S] {
      def apply[E <: ScalaExpr](expr: E)(implicit unitSrc: E#Source =:= Unit, tagS: ru.TypeTag[S]): Const[S, E] =
        new Const[S, E] {
          val src: ru.TypeTag[S] = ru.typeTag[S]
          val tgt: ru.TypeTag[Target] = expr.tgt.asInstanceOf[ru.TypeTag[Target]]
          val arrow: E = expr
          def short: String = s"=> ${arrow.short}"
        }
    }
    private object Builder extends Builder[Any]

    implicit def constShowT[A <: Arrow, S, E <: ScalaExpr](implicit isConst: A <:< Const[S, E], show: ShowT[E]): ShowT[A] =
      ShowT.define(s"Const${show()}", s"Const${show.simple}", "Const", infix = false)
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
        val src: ru.TypeTag[G.Source] = G.src
        val tgt: ru.TypeTag[F.Target] = F.tgt
        def short: String = s"${g.short}${f.short}"
      }
  }

  implicit def composeScalaExprs[F <: ScalaExpr, G <: ScalaExpr, S, T](
    implicit
    typesCorrespond: Arrow.Compose.TypesCorrespond.Aux[F, G, S, T],
    fNotId: NotId[F],
    gNotId: NotId[G]
  ): Arrow.Compose.Aux[F, G, Composition.Aux[F, G, S, T]] = composeInstance.asInstanceOf[Arrow.Compose.Aux[F, G, Composition.Aux[F, G, S, T]]]

  private lazy val composeInstance = new Arrow.Compose[ScalaExpr, ScalaExpr] {
    type Out = Composition[ScalaExpr, ScalaExpr]
    def apply(f: ScalaExpr, g: ScalaExpr): Composition[ScalaExpr, ScalaExpr] = Composition.mkComposition(f, g)
  }


  sealed trait Split[Arrows <: HList] extends ScalaExpr with Arrow.Split[Arrows] {
    override val toList: List[ScalaExpr]
    def short: String = s".split(${toList.map("_" + _.short).mkString(", ")})"
  }
  object Split {
    type Aux[Arrows <: HList, S, T] = Split[Arrows] { type Source = S; type Target = T }
    def unapply[Arrows <: HList](arr: Split[Arrows]): Option[List[ScalaExpr]] = PartialFunction.condOpt(arr) { case split: Split[_] => split.toList }

    protected[ScalaExpr] def mkSplitter[Arrows <: HList, As, S, T](
      implicit
      sourceTag: ru.TypeTag[S],
      targetTag: ru.TypeTag[T]
    ): Arrow.Split.Splitter0.Aux[Arrows, As, Split.Aux[Arrows, S, T]] =
      new Arrow.Split.Splitter0[Arrows, As] {
        type Out = Split.Aux[Arrows, S, T]
        def apply(t: Arrows): Split.Aux[Arrows, S, T] =
          new Split[Arrows] {
            val arrows: Arrows = t
            type Source = S
            type Target = T
            val src: ru.TypeTag[S] = sourceTag
            val tgt: ru.TypeTag[T] = targetTag
            val toList: List[ScalaExpr] = ShapelessUtils.unsafeHListToList(t)
          }
      }
  }

  implicit def splitScalaExprAsHList[Arrows <: HList, S, Ts <: HList](
    implicit
    areScalaExprs: LUBConstraint[Arrows, ScalaExpr],
    canSplit: Arrow.Split.Splitter.CanSplit.Aux[Arrows, S, Ts],
    sourceTag: ru.TypeTag[S],
    targetTag: ru.TypeTag[Ts]
  ): Arrow.Split.Splitter0.Aux[Arrows, HList, Split.Aux[Arrows, S, Ts]] = Split.mkSplitter


  implicit def splitScalaExprAsProduct[Arrows <: HList, Ts <: HList, S, T](
    implicit
    areScalaExprs: LUBConstraint[Arrows, ScalaExpr],
    canSplit: Arrow.Split.Splitter.CanSplit.Aux[Arrows, S, Ts],
    tupler: ops.hlist.Tupler.Aux[Ts, T],
    sourceTag: ru.TypeTag[S],
    targetTag: ru.TypeTag[T]
  ): Arrow.Split.Splitter0.Aux[Arrows, Product, Split.Aux[Arrows, S, T]] = Split.mkSplitter


  sealed trait Choose[From, Arrows <: HList] extends ScalaExpr with Arrow.Choose[From, Arrows] {
    override val toList: List[ScalaExpr]
    def short: String = s".choose(${toList.map(shortChoose).mkString(", ")})"
    private def shortChoose(expr: ScalaExpr) = s"_.on[${TypeUtils.Show(expr.src)}]${expr.short}"
  }
  object Choose {
    type Aux[From, Arrows <: HList, T] = Choose[From, Arrows] { type Target = T }
    def unapply[From, Arrows <: HList](arr: Choose[From, Arrows]): Option[List[ScalaExpr]] =
      PartialFunction.condOpt(arr) { case choose: Choose[_, _] => choose.toList }
  }

  implicit def chooseScalaExpr[Arrows <: HList, S, C <: Coproduct](
    implicit
    areScalaExprs: LUBConstraint[Arrows, ScalaExpr],
    canChoose: Arrow.Choose.CanChoose.Aux[S, Arrows, C],
    sourceTag: ru.TypeTag[S]
  ): Arrow.Choose.Chooser.Aux[S, Arrows, Choose.Aux[S, Arrows, C]] =
    new Arrow.Choose.Chooser[S, Arrows] {
      type Out = Choose.Aux[S, Arrows, C]
      def apply(t: Arrows): Choose.Aux[S, Arrows, C] =
        new Choose[S, Arrows] {
          val arrows: Arrows = t
          type Target = C
          val src: ru.TypeTag[S] = sourceTag
          lazy val tgt: ru.TypeTag[C] = TypeUtils.Shapeless.tagCoproduct(toList.map(_.tgt))
          lazy val toList: List[ScalaExpr] = ShapelessUtils.unsafeHListToList(t)
        }
    }


  case class Literal[A](lit: A)(implicit tag: ru.TypeTag[A]) extends ScalaExpr {
    type Source = Unit
    type Target = A
    val src: ru.TypeTag[Unit] = ru.TypeTag.Unit
    val tgt: ru.TypeTag[A] = tag

    override def toString: String = s"Literal[${TypeUtils.Show(tgt)}]($lit)"
    def short: String = lit match {
      case s: String => s""""$s""""
      case _ => lit.toString
    }
  }
  object Literal {
    implicit def literalShowT[A: ru.TypeTag]: ShowT[Literal[A]] =
      ShowT.define(s"Lit[${TypeUtils.Show(ru.typeTag[A])}]", "Lit", infix = false)
  }


  /** Expression representing selection of a field of an ADT (case class). */
  case class SelectField[Obj: ru.TypeTag, K <: String, V: ru.TypeTag](field: K) extends ScalaExpr {
    type Source = Obj
    type Target = V
    val src: ru.TypeTag[Obj] = ru.typeTag[Obj]
    val tgt: ru.TypeTag[V] = ru.typeTag[V]

    override def toString: String = s"SelectField[${TypeUtils.Show(src)}, ${TypeUtils.Show(tgt)}]($field)"
    def short: String = s".$field"
  }
  object SelectField {
    implicit def showSelectFieldType[A <: Arrow, Obj, K <: String, V](
      implicit
      isSelectField: A <:< SelectField[Obj, K, V],
      showSrc: ShowT[Obj],
      showTgt: ShowT[V],
      kWitness: Witness.Aux[K]
    ): ShowT[A] =
      ShowT.define(
        show         = s"SelectField[${showSrc.simple}, ${kWitness.value}, ${showTgt.simple}]",
        showSimple   = kWitness.value,
        showSimplest = "SelectField",
        infix = false
      )
  }

  /** Expression representing functor `map` operation. */
  case class FMap[F[_], E <: ScalaExpr](expr: E)(implicit val F: cats.Functor[F], fTag: ru.TypeTag[F[_]]) extends ScalaExpr {
    type Source = F[expr.Source]
    type Target = F[expr.Target]
    val src: ru.TypeTag[F[expr.Source]] = appliedTypeTag(fTag, expr.src)
    val tgt: ru.TypeTag[F[expr.Target]] = appliedTypeTag(fTag, expr.tgt)

    override def toString: String = s"FMap[${TypeUtils.Show(src)}, ${TypeUtils.Show(tgt)}]($expr)"
    def short: String = s".map(_${expr.short})"
  }
  object FMap {
    def tpe[F[_]](implicit F: cats.Functor[F], fTag: ru.TypeTag[F[_]]): Builder[F] = new Builder[F]

    protected class Builder[F[_]](implicit F: cats.Functor[F], fTag: ru.TypeTag[F[_]]) {
      def expr[E <: ScalaExpr](expr: E): FMap[F, E] = FMap[F, E](expr)
    }

    implicit def fmapToDot[F[_], E <: ScalaExpr](
      implicit
      showSrc: ShowT[E#Source],
      toDot: ArrowToDot[E]
    ): ArrowToDot[FMap[F, E]] = dotCluster(_.expr, "map")
  }

  /** Expression representing monadic bind / `flatMap` operation. */
  case class MBind[F[_], E <: ScalaExpr](expr: E)(implicit val M: cats.Monad[F], fTag: ru.TypeTag[F[_]]) extends ScalaExpr {
    type Source = F[expr.Source]
    type Target = expr.Target
    val src: ru.TypeTag[F[expr.Source]] = appliedTypeTag(fTag, expr.src)
    val tgt: ru.TypeTag[expr.Target] = expr.tgt

    override def toString: String = s"MBind[${TypeUtils.Show(src)}, ${TypeUtils.Show(tgt)}]($expr)"
    def short: String = s".flatMap(_${expr.short})"
  }
  object MBind {
    def tpe[F[_]](implicit M: cats.Monad[F], fTag: ru.TypeTag[F[_]]): Builder[F] = new Builder[F]

    protected class Builder[F[_]](implicit M: cats.Monad[F], fTag: ru.TypeTag[F[_]]) {
      def apply[E <: ScalaExpr](expr: E): MBind[F, E] = MBind[F, E](expr)
    }

    implicit def mbindToDot[F[_], E <: ScalaExpr](
      implicit
      showSrc: ShowT[E#Source],
      toDot: ArrowToDot[E]
    ): ArrowToDot[MBind[F, E]] = dotCluster(_.expr, "flatMap")
  }

  private def dotCluster[A <: Arrow, S: ShowT, E <: ScalaExpr: ArrowToDot](getE: A => E, label: String) =
    ArrowToDot.define[A]{
      (a, srcId) =>
        val (sourceDot, source) = ArrowToDot.defaultNewTypeNode[S]()
        val (dot, target) = implicitly[ArrowToDot[E]].apply(getE(a), source)
        s"""subgraph cluster_${scala.util.Random.nextInt(Int.MaxValue)}{
           |  $sourceDot
           |  $dot
           |}
           |edge [label=<<b>$label</b>>,arrowtail=inv,dir=both];
           |$srcId -> $source;
           |edge [dir=forward]; // reset
           |""".stripMargin -> target
    }


  case class IterableFilter[F[_], E <: ScalaExpr](expr: E)(
      implicit
      iterable: F[_] <:< Iterable[_],
      fTag: ru.TypeTag[F[_]],
      filterArrow: E#Target =:= Boolean
  ) extends ScalaExpr {
    type Source = F[expr.Source]
    type Target = Source
    val src: ru.TypeTag[Source] = appliedTypeTag(fTag, expr.src)
    val tgt: ru.TypeTag[Target] = src

    override def toString: String = s"IterableFilter[${TypeUtils.Show(src)}]($expr)"
    def short: String = s".filter(_${expr.short})"
  }

  object OrderBy {
    sealed trait Direction
    case object Ascending  extends Direction
    case object Descending extends Direction
  }

  case class IterableOrderBy[F[_], E <: ScalaExpr](expr: E, dir: OrderBy.Direction)(
      implicit
      iterable: F[_] <:< Iterable[_],
      fTag: ru.TypeTag[F[_]],
      targetOrdering: Ordering[E#Target] // TODO: require it?
  ) extends ScalaExpr {
    type Source = F[expr.Source]
    type Target = Source
    val src: ru.TypeTag[Source] = appliedTypeTag(fTag, expr.src)
    val tgt: ru.TypeTag[Target] = src

    override def toString: String = s"IterableOrderBy[${TypeUtils.Show(src)}]($expr, $dir)"
    def short: String = s".${if (dir == OrderBy.Ascending) "orderBy" else "orderBy[Inv]"}(_${expr.short})"
  }

  case class IterableSlice[I](range: Ior[Int, Int])(implicit iterable: I <:< Iterable[_], tag: ru.TypeTag[I]) extends ScalaExpr {
    type Source = I
    type Target = I
    val src: ru.TypeTag[I] = tag
    val tgt: ru.TypeTag[I] = tag

    def from: Option[Int] = range.left
    def to: Option[Int] = range.right

    override def toString: String = s"IterableSlice[${TypeUtils.Show(src)}]($range)"
    def short: String = s".slice(${range.fold(
      from => s"$from..",
      to => s"..$to",
      (from, to) => s"$from..$to"
    )})"
  }
  object IterableSlice {
    def range[I](from: Int, to: Int)(implicit iterable: I <:< Iterable[_], tag: ru.TypeTag[I]): IterableSlice[I] = {
      require(from > 0, s"slice range 'from' cannot be negative, got $from")
      require(to > 0, s"slice range 'to' cannot be negative, got $to")
      IterableSlice(Ior.both(from, to))
    }
    def from[I](n: Int)(implicit iterable: I <:< Iterable[_], tag: ru.TypeTag[I]): IterableSlice[I] = {
      require(n > 0, s"slice range 'from' cannot be negative, got $n")
      IterableSlice(Ior.left(n))
    }
    def to[I](n: Int)(implicit iterable: I <:< Iterable[_], tag: ru.TypeTag[I]): IterableSlice[I] = {
      require(n > 0, s"slice range 'to' cannot be negative, got $n")
      IterableSlice(Ior.right(n))
    }
  }

  case class IterableToList[I, A]()(implicit iterable: I <:< Iterable[_], iTag: ru.TypeTag[I], aTag: ru.TypeTag[A]) extends ScalaExpr {
    type Source = I
    type Target = List[A]
    val src: ru.TypeTag[I] = iTag
    val tgt: ru.TypeTag[List[A]] = appliedTypeTag(ru.typeTag[List[_]], aTag)

    override def toString: String = s"IterableToList[${TypeUtils.Show(src)}, ${TypeUtils.Show(tgt)}]"

    def short: String = ".toList"
  }


  sealed trait Compare[A, B] extends ScalaExpr { type Source = (A, B); type Target = Boolean; val tgt = ru.TypeTag.Boolean }
  object Compare {
    case class Eq [A, B]()(implicit val src: ru.TypeTag[(A, B)]) extends Compare[A, B] {
      override def toString: String = s"Eq[${TypeUtils.Show(src)}]"
      def short: String = "=="
    }
    case class Neq[A, B]()(implicit val src: ru.TypeTag[(A, B)]) extends Compare[A, B] {
      override def toString: String = s"Neq[${TypeUtils.Show(src)}]"
      def short: String = "!="
    }
    // TODO: more

    trait CreateInstance[Cmp[_, _] <: Compare[_, _]] { def apply[L, R](implicit tag: ru.TypeTag[(L, R)]): Cmp[L, R] }
    object CreateInstance {
      implicit lazy val createEqInstance: CreateInstance[Eq] = new CreateInstance[Eq]{
        def apply[L, R](implicit tag: ru.TypeTag[(L, R)]): Eq[L, R] = Eq[L, R]()
      }
      implicit lazy val createNeqInstance: CreateInstance[Neq] = new CreateInstance[Neq]{
        def apply[L, R](implicit tag: ru.TypeTag[(L, R)]): Neq[L, R] = Neq[L, R]()
      }
    }
  }

  private def mkTypeTag[T](tpe: ru.Type): ru.TypeTag[T] = TypeUtils.tagType(tpe)
  private def appliedTypeTag[F[_], T](tag: ru.TypeTag[_ <: F[_]], arg: ru.TypeTag[T]): ru.TypeTag[F[T]] =
    TypeUtils.tagTypeRef(tag, List(arg))
  private def appliedTypeTag[F[_, _], T1, T2](tag: ru.TypeTag[_ <: F[_, _]], arg1: ru.TypeTag[T1], arg2: ru.TypeTag[T2]): ru.TypeTag[F[T1, T2]] =
    TypeUtils.tagTypeRef(tag, List(arg1, arg2))


  // // // // // // Syntax Ops // // // // // //


  implicit class ScalaExprFMapOps[A <: ScalaExpr, F[_], S0](a: A)(
    implicit
    source0: A#Target <:< F[S0],
    functor: cats.Functor[F],
    fTag: ru.TypeTag[F[_]]
  ) {

    def map[B <: ScalaExpr](b: B)           (implicit compose: Arrow.Compose[FMap[F, B], A]                   ): compose.Out = compose(new FMap[F, B](b), a)
    def map[B <: ScalaExpr](fb: Id[S0] => B)(implicit compose: Arrow.Compose[FMap[F, B], A], t: ru.TypeTag[S0]): compose.Out = compose(new FMap[F, B](fb(Id[S0])), a)
  }

  implicit class ScalaExprMBindOps[A <: ScalaExpr, F[_], S0](a: A)(
    implicit
    source0: A#Target <:< F[S0],
    monad: cats.Monad[F],
    fTag: ru.TypeTag[F[_]]
  ) {

    def flatMap[B <: ScalaExpr.Aux[_, F[_]]](b: B)           (implicit compose: Arrow.Compose[MBind[F, B], A]                   ): compose.Out = compose(new MBind[F, B](b), a)
    def flatMap[B <: ScalaExpr.Aux[_, F[_]]](fb: Id[S0] => B)(implicit compose: Arrow.Compose[MBind[F, B], A], t: ru.TypeTag[S0]): compose.Out = compose(new MBind[F, B](fb(Id[S0])), a)
  }

  implicit class ScalaExprIterableOps[A <: ScalaExpr, S0, F[_]](a: A)(
    implicit
    fType: A#Target <:< F[S0],
    iterableT: A#Target <:< Iterable[_],    // TODO
    iterableF: F[_] <:< Iterable[_],  // TODO
    mf: ru.TypeTag[F[_]]
  ) {
    private implicit def aTargetTypeTag = a.tgt.asInstanceOf[ru.TypeTag[A#Target]]

    def toList(implicit compose: Arrow.Compose[IterableToList[A#Target, S0], A], t: ru.TypeTag[S0]): compose.Out = compose(new IterableToList[A#Target, S0], a)

    def drop(n: Int)(implicit compose: Arrow.Compose[IterableSlice[A#Target], A]): compose.Out = compose(IterableSlice.from(n), a)
    def take(n: Int)(implicit compose: Arrow.Compose[IterableSlice[A#Target], A]): compose.Out = compose(IterableSlice.to(n), a)
    def slice(from: Int, to: Int)(implicit compose: Arrow.Compose[IterableSlice[A#Target], A]): compose.Out = compose(IterableSlice.range(from, to), a)

    def filter[B <: ScalaExpr](expr: B)            (implicit compose: Arrow.Compose[IterableFilter[F, B], A], filterArrow: B#Target =:= Boolean                   ): compose.Out = compose(IterableFilter[F, B](expr), a)
    def filter[B <: ScalaExpr](mkExpr: Id[S0] => B)(implicit compose: Arrow.Compose[IterableFilter[F, B], A], filterArrow: B#Target =:= Boolean, t: ru.TypeTag[S0]): compose.Out = compose(IterableFilter[F, B](mkExpr(Id[S0])), a)

    def orderBy[B <: ScalaExpr](mkExpr: Id[S0] => B, dir: OrderBy.type => OrderBy.Direction = null)
                               (implicit targetOrdering: Ordering[B#Target], compose: Strict[Arrow.Compose[IterableOrderBy[F, B], A]], t: ru.TypeTag[S0]): compose.value.Out =
      compose.value(IterableOrderBy(mkExpr(Id[S0]), Option(dir).map(_(OrderBy)).getOrElse(OrderBy.Ascending)), a)
  }

  implicit class CompareOps[L <: ScalaExpr](left: L) {
    def ===[R <: ScalaExpr](right: R)(implicit build: CompareOps.BuildCmpRight[L, Compare.Eq, R]): build.Out = build(left, right)
    def ===[R: ru.TypeTag] (right: R)(implicit build: CompareOps.BuildCmpRight[L, Compare.Eq, Literal[R]]): build.Out = build(left, Literal(right))

    def =!=[R <: ScalaExpr](right: R)(implicit build: CompareOps.BuildCmpRight[L, Compare.Neq, R]): build.Out = build(left, right)
    def =!=[R: ru.TypeTag] (right: R)(implicit build: CompareOps.BuildCmpRight[L, Compare.Neq, Literal[R]]): build.Out = build(left, Literal(right))

    def exprEq[R <: ScalaExpr](right: R)(implicit build: CompareOps.BuildCmpRight[L, Compare.Eq, R]): build.Out = build(left, right)
    def exprEq[R: ru.TypeTag] (right: R)(implicit build: CompareOps.BuildCmpRight[L, Compare.Eq, Literal[R]]): build.Out = build(left, Literal(right))

    def exprNeq[R <: ScalaExpr](right: R)(implicit build: CompareOps.BuildCmpRight[L, Compare.Neq, R]): build.Out = build(left, right)
    def exprNeq[R: ru.TypeTag] (right: R)(implicit build: CompareOps.BuildCmpRight[L, Compare.Neq, Literal[R]]): build.Out = build(left, Literal(right))
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
            val cmp = cmpInstance(appliedTypeTag(
              Tuple2TypeTag,
              left.tgt.asInstanceOf[ru.TypeTag[L#Target]],
              right.tgt.asInstanceOf[ru.TypeTag[R#Target]]
            ))
            compose(papplyR(cmp, right), left)
          }
        }
      private lazy val Tuple2TypeTag = ru.typeTag[(_, _)]
    }
  }

  protected trait FieldSelectionOps extends Dynamic {
    expr: ScalaExpr =>

    def selectDynamic[V, A <: Arrow](k: String)(
      implicit
      ev0: HasField.Aux[Target, k.type, V],
      ev1: expr.type <:< A, // using `expr.type` directly in `compose` would require _existential types_
      compose: Arrow.Compose[SelectField[Target, k.type, V], A]
    ): compose.Out = compose(SelectField[Target, k.type, V](k)(tgt, ev0.tag), expr)
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
      UnchainedRev(unchained.toList, unchained.head.src.asInstanceOf[ru.TypeTag[Any]], unchained.last.tgt.asInstanceOf[ru.TypeTag[Any]])

    final case class UnchainedRev protected(toList: List[ScalaExpr], src: ru.TypeTag[Any], tgt: ru.TypeTag[Any]) extends ScalaExpr {
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
        if (l.nonEmpty) copy(l, l.head.src.asInstanceOf[ru.TypeTag[Any]], l.last.tgt.asInstanceOf[ru.TypeTag[Any]])
        else UnchainedRev(Nil, ru.TypeTag.Nothing.asInstanceOf[ru.TypeTag[Any]], ru.TypeTag.Nothing.asInstanceOf[ru.TypeTag[Any]])

      def short: String = toList.map(_.short).mkString(".")
    }
  }
}
