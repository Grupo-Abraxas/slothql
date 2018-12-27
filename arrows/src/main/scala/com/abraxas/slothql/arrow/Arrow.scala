package com.abraxas.slothql.arrow

import scala.annotation.implicitNotFound
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

import shapeless._

import com.abraxas.slothql.util.ShapelessUtils

trait Arrow {
  type Source
  type Target
}

object Arrow {
  type Aux[-S, +T] = Arrow { type Source >: S; type Target <: T }

  // TODO: `Arrow.Id[Int] == Arrow.Id[String]` complies
  trait Id[A] extends Arrow {
    type Source = A
    type Target = A

    override def toString: String = "Id"
  }
  object Id {
    def apply[A]: Id[A] = instance.asInstanceOf[Id[A]]
    protected[Id] lazy val instance = new Id[Any] {}

    def unapply(arr: Arrow): Boolean = arr.isInstanceOf[Id[_]]

    trait Builder[A <: Arrow, T] extends DepFn0 { type Out <: Arrow }
    object Builder {
      type Aux[A <: Arrow, T, Id <: Arrow] = Builder[A, T] { type Out = Id }

      implicit def defaultIdArrowBuilder[A <: Arrow, T](implicit lowPriority: LowPriority): Builder.Aux[A, T, Arrow.Id[T]] =
        instance.asInstanceOf[Builder.Aux[A, T, Arrow.Id[T]]]
      private lazy val instance = new Builder[Arrow, Any] {
        type Out = Arrow.Id[Any]
        def apply(): Arrow.Id[Any] = Arrow.Id.instance
      }
    }
  }


  // case class Obj[A ](get: A) extends Arrow { type Source = A;    type Target =  A }
  // case class Lit[+A](get: A) extends Arrow { type Source = Unit; type Target <: A }

  trait Split[Arrows <: HList] extends Arrow {
    val arrows: Arrows
    val toList: List[Arrow]

    override def equals(obj: Any): Boolean = PartialFunction.cond(obj) {
      case that: Split[_] => this.arrows == that.arrows
    }
    override def hashCode(): Int = arrows.##
    override def toString: String = s"Split($arrows)"
  }
  object Split extends ProductArgs {
    type Aux[Arrows <: HList, S, T] = Split[Arrows] { type Source = S; type Target = T }

    def applyProduct[Arrows <: HList](arrows: Arrows)(implicit split: Splitter[Arrows]): split.Out = split(arrows)

    def unapply(arr: Arrow): Option[List[Arrow]] = PartialFunction.condOpt(arr) { case split: Split[_] => split.toList }

    type Splitter[Arrows <: HList] = Splitter0[Arrows, Product]
    object Splitter {
      type Aux[Arrows <: HList, Ts <: Split[_]] = Splitter[Arrows] { type Out = Ts }

      trait CanSplit[Arrows <: HList] {
        type Source
        type Targets <: HList
      }
      object CanSplit{
        type Aux[Arrows <: HList, S, Ts <: HList] = CanSplit[Arrows] { type Source = S; type Target = Ts }

        implicit def proveCanSplit[Arrows <: HList, S, Ts <: HList](
          implicit
          source: CommonSource.Aux[Arrows, S],
          targets: Targets.Aux[Arrows, Ts]
        ): CanSplit.Aux[Arrows, S, Ts] = instance.asInstanceOf[CanSplit.Aux[Arrows, S, Ts]]

        private lazy val instance = new CanSplit[HList] {}
      }
    }

    trait Splitter0[Arrows <: HList, As] extends DepFn1[Arrows] { type Out <: Split[Arrows] }
    object Splitter0 {
      type Aux[Arrows <: HList, As, Ts <: Split[_]] = Splitter0[Arrows, As] { type Out = Ts }

      implicit def arrowAsHListSplitter[Arrows <: HList, S, Ts <: HList](
        implicit
        canSplit: Splitter.CanSplit.Aux[Arrows, S, Ts],
        lowPriority: LowPriority
      ): Splitter0.Aux[Arrows, HList, Split.Aux[Arrows, S, Ts]] =
        instance.asInstanceOf[Splitter0.Aux[Arrows, HList, Split.Aux[Arrows, S, Ts]]]

      implicit def arrowAsTupleSplitter[Arrows <: HList, S, Ts <: HList, T <: Product](
        implicit
        canSplit: Splitter.CanSplit.Aux[Arrows, S, Ts],
        tupler: ops.hlist.Tupler.Aux[Ts, T],
        lowPriority: LowPriority
      ): Splitter0.Aux[Arrows, Product, Split.Aux[Arrows, S, T]] =
        instance.asInstanceOf[Splitter0.Aux[Arrows, Product, Split.Aux[Arrows, S, T]]]

      private lazy val instance = new Splitter0[HList, Any] {
        type Out = Split.Aux[HList, Any, Any]
        def apply(t: HList): Out =
          new Split[HList] {
            type Source = Any
            type Target = Any
            val arrows: HList = t
            val toList: List[Arrow] = ShapelessUtils.unsafeHListToList(t)
          }
      }
    }
  }

  implicit class SplitOps[F <: Arrow, IdArr <: Arrow](f: F)(implicit idArr: Id.Builder.Aux[F, F#Target, IdArr]) {
    /**
     *  Warning: any macro application error in the arguments (like incorrect field name) will be swallowed by this macro,
     *  showing just `exception during macro expansion` error. Looks like [[https://github.com/scala/bug/issues/9889]].
     */
    def split(arrows: (IdArr => Arrow)*): Arrow = macro SplitOps.splitImpl[F]
  }
  object SplitOps {
    def splitImpl[F: c.WeakTypeTag](c: whitebox.Context)(arrows: c.Tree*): c.Tree = {
      import c.universe._

      val (f, idBuilder) = c.prefix.tree match {
        case q"new $clazz($param)($id)" if clazz.tpe <:< typeOf[SplitOps[_,  _]] => param -> id
        case q"arrow.this.Arrow.SplitOps[..$_]($param)($id)" => param -> id
        case other => c.abort(c.prefix.tree.pos, s"Unexpected: $other")
      }
      q"""
        _root_.com.abraxas.slothql.arrow.Arrow.Split(..${arrows.map(t => q"$t($idBuilder())")}) ∘ $f
      """
    }
  }


  /** An arrow that represents arrows composition. */
  trait Composition[F <: Arrow, G <: Arrow] extends Arrow {
    val F: F
    val G: G
    type Source >: G.Source
    type Target <: F.Target

    override def hashCode(): Int = Unchain(this).##
    override def equals(o: scala.Any): Boolean = PartialFunction.cond(o) {
      case that: Composition[_, _] => Unchain(this) == Unchain(that)
    }

    override def toString: String = s"$F ∘ $G"
  }
  object Composition {
    type Aux[F <: Arrow, G <: Arrow, S, T] = Composition[F, G] { type Source = S; type Target = T }
    def apply[F <: Arrow, G <: Arrow](f: F, g: G)(implicit compose: Compose[F, G]): compose.Out = compose(f, g)
    def unapply(arr: Arrow): Option[(Arrow, Arrow)] = PartialFunction.condOpt(arr) { case c: Composition[_, _] => c.F -> c.G }
  }

  /** Syntax sugar for arrows composition. */
  implicit class ComposeOps[F <: Arrow](f: F) {
    def compose[G <: Arrow](g: G)(implicit compose: Compose[F, G]): compose.Out = compose(f, g)
    def ∘      [G <: Arrow](g: G)(implicit compose: Compose[F, G]): compose.Out = compose(f, g)
    def <<<    [G <: Arrow](g: G)(implicit compose: Compose[F, G]): compose.Out = compose(f, g)

    def andThen[G <: Arrow](g: G)(implicit compose: Compose[G, F]): compose.Out = compose(g, f)
    def >>>    [G <: Arrow](g: G)(implicit compose: Compose[G, F]): compose.Out = compose(g, f)
  }

  /** Syntax sugar for arrows composition. */
  implicit class ComposeOpsIdArr[F <: Arrow, IdArr <: Arrow](f: F)(implicit idArr: Id.Builder.Aux[F, F#Target, IdArr]) {
    def andThenF[G <: Arrow](fg: IdArr => G)(implicit compose: Compose[G, F]): compose.Out = compose(fg(idArr()), f)
    def >^>     [G <: Arrow](fg: IdArr => G)(implicit compose: Compose[G, F]): compose.Out = compose(fg(idArr()), f)
  }

  /** Syntax sugar for unchaining composed arrows. */
  implicit class UnchainOps[F <: Arrow](f: F) {
    def unchain(implicit ev: Unchain[F]): ev.Out = ev(f)
    def unchainRev[L <: HList](implicit ev: Unchain.Aux[F, L], reverse: ops.hlist.Reverse[L]): reverse.Out = reverse(ev(f))
  }


  /** Typeclass witnessing that arrows `F` and `G` can be composed. */
  trait Compose[F <: Arrow, G <: Arrow] extends DepFn2[F, G] { type Out <: Arrow }
  object Compose {
    type Aux[F <: Arrow, G <: Arrow, Composition <: Arrow] = Compose[F, G] { type Out = Composition }
    def apply[F <: Arrow, G <: Arrow](implicit compose: Compose[F, G]): Aux[F, G, compose.Out] = compose

    implicit def composeIdLeft[F <: Arrow, G <: Arrow, T](
      implicit
      idF: F <:< Arrow.Id[G#Target],
      notIdG: G <:!< Arrow.Id[_]
    ): Compose.Aux[F, G, G] =
      composeIdL.asInstanceOf[Compose.Aux[F, G, G]]
    private lazy val composeIdL = new Compose[Arrow.Id[Any], Arrow] {
      type Out = Arrow
      def apply(f: Arrow.Id[Any], g: Arrow): Arrow = g
    }

    implicit def composeIdRight[F <: Arrow, G <: Arrow, S](
      implicit
      idG: G <:< Arrow.Id[F#Source],
      notIdF: F <:!< Arrow.Id[_]
    ): Compose.Aux[F, G, F] =
      composeIdR.asInstanceOf[Compose.Aux[F, G, F]]
    private lazy val composeIdR = new Compose[Arrow, Arrow.Id[Any]] {
      type Out = Arrow
      def apply(f: Arrow, g: Arrow.Id[Any]): Arrow = f
    }

    implicit def composeIdBoth[F <: Arrow, G <: Arrow, S](implicit idF: F <:< Arrow.Id[S], idEq: F =:= G): Compose.Aux[F, G, F] =
      composeIdR.asInstanceOf[Compose.Aux[F, G, F]]

    implicit def canCompose[F <: Arrow, G <: Arrow, S, T](
      implicit
      typesCorrespond: TypesCorrespond.Aux[F, G, S, T],
      lowPriority: LowPriority
    ): Compose.Aux[F, G, Composition.Aux[F, G, S, T]] = instance.asInstanceOf[Aux[F, G, Composition.Aux[F, G, S, T]]]
    private lazy val instance = new Compose[Arrow, Arrow] {
      type Out = Composition[Arrow, Arrow]
      def apply(f: Arrow, g: Arrow) = new Composition[Arrow, Arrow] { val F = f; val G = g }
    }

    @implicitNotFound("Types of ${F} and ${G} do not correspond for composition")
    trait TypesCorrespond[F <: Arrow, G <: Arrow] {
      type Source
      type Target
    }
    object TypesCorrespond {
      type Aux[F <: Arrow, G <: Arrow, S, T] = TypesCorrespond[F, G] { type Source = S; type Target = T }

      implicit def proveTypesCorrespond[F <: Arrow, G <: Arrow](
        implicit
        typesCorrespond: F#Source <:< G#Target
      ): TypesCorrespond.Aux[F, G, G#Source, F#Target] = instance.asInstanceOf[TypesCorrespond.Aux[F, G, G#Source, F#Target]]
      private lazy val instance = new TypesCorrespond[Arrow, Arrow] {}
    }
  }


  /** Typeclass supporting extraction of composed arrows as an HList. */
  trait Unchain[F <: Arrow] extends DepFn1[F] { type Out <: HList }
  object Unchain {
    type Aux[F <: Arrow, Arrows <: HList] = Unchain[F] { type Out = Arrows }
    def apply[F <: Arrow](implicit unchain: Unchain[F]): Aux[F, unchain.Out] = unchain
    def apply(arr: Arrow): List[Arrow] = arr match {
      case c: Composition[_, _] => apply(c.F) ::: apply(c.G)
      case _ => arr :: Nil
    }

    implicit def unchainComposition[C <: Composition[_, _], F <: Arrow, G <: Arrow, ChF <: HList, ChG <: HList](
      implicit
      ev: C <:< Composition[F, G],
      unchainF: Lazy[Unchain.Aux[F, ChF]],
      unchainG: Lazy[Unchain.Aux[G, ChG]],
      concat: ops.hlist.Prepend[ChF, ChG]
    ): Unchain.Aux[C, concat.Out] =
      new Unchain[C] {
        type Out = concat.Out
        def apply(c: C): Out =
          concat(unchainF.value(c.F.asInstanceOf[F]), unchainG.value(c.G.asInstanceOf[G]))
      }

    implicit def notChained[F <: Arrow](implicit ev: F <:!< Composition[_, _]): Unchain.Aux[F, F :: HNil] =
      _notChained.asInstanceOf[Unchain.Aux[F, F :: HNil]]
    private lazy val _notChained = new Unchain[Arrow] {
      type Out = Arrow :: HNil
      def apply(t: Arrow): Out = t :: HNil
    }
  }

  @implicitNotFound("Arrows ${Arrows} have different sources")
  trait CommonSource[Arrows <: HList] { type Source }
  object CommonSource {
    type Aux[Arrows <: HList, S] = CommonSource[Arrows] { type Source = S }
    def apply[Arrows <: HList](implicit cs: CommonSource[Arrows]): Aux[Arrows, cs.Source] = cs

    implicit def singleCommonSource[H <: Arrow]: CommonSource.Aux[H :: HNil, H#Source] =
      instance.asInstanceOf[CommonSource.Aux[H :: HNil, H#Source]]

    implicit def multipleCommonSource[H <: Arrow, T <: HList, TS, S](
      implicit
      notSingle: T <:!< HNil,
      tSource: CommonSource.Aux[T, TS],
      lub: Lub[H#Source, TS, S]
    ): CommonSource.Aux[H :: T, S] = instance.asInstanceOf[CommonSource.Aux[H :: T, S]]

    private lazy val instance = new CommonSource[HList]{}
  }

  trait Targets[Arrows <: HList] { type Targets <: HList }
  object Targets {
    type Aux[Arrows <: HList, Ts <: HList] = Targets[Arrows] { type Targets = Ts }
    def apply[Arrows <: HList](implicit t: Targets[Arrows]): Aux[Arrows, t.Targets] = t

    implicit def singleTarget[H <: Arrow]: Targets.Aux[H :: HNil, H#Target :: HNil] =
      instance.asInstanceOf[Targets.Aux[H :: HNil, H#Target :: HNil]]
    implicit def multipleTargets[H <: Arrow, T <: HList](
      implicit
      notSingle: T =:!= HNil,
      t: Targets[T]
    ): Targets.Aux[H :: T, H#Target :: t.Targets] = instance.asInstanceOf[Targets.Aux[H :: T, H#Target :: t.Targets]]

    private lazy val instance = new Targets[HList] {}
  }
}
