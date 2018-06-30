package com.abraxas.slothql.util

import shapeless._

trait Arrow {
  type Source
  type Target
}

object Arrow {
  type Aux[-S, +T] = Arrow { type Source >: S; type Target <: T }
  type Inv[ S,  T] = Arrow { type Source  = S; type Target  = T }


  trait Id[A] extends Arrow { type Source = A; type Target = A }
  private object Id extends Id[Any]
  def Id[A]: Id[A] = Id.asInstanceOf[Id[A]]


  // case class Obj[A ](get: A) extends Arrow { type Source = A;    type Target =  A }
  // case class Lit[+A](get: A) extends Arrow { type Source = Unit; type Target <: A }


  /** An arrow that represents arrows composition. */
  trait Composition[F <: Arrow, G <: Arrow] extends Arrow {
    val F: F
    val G: G
    type Source >: G.Source
    type Target <: F.Target
  }
  object Composition {
    type Aux[F <: Arrow, G <: Arrow, S, T] = Composition[F, G] { type Source = S; type Target = T }
    def apply[F <: Arrow, G <: Arrow](f: F, g: G)(implicit compose: Compose[F, G]): compose.Out = compose(f, g)
  }

  /** Syntax sugar for arrows composition. */
  implicit class ComposeOps[F <: Arrow](f: F) {
    def compose[G <: Arrow](g: G)(implicit compose: Compose[F, G]): compose.Out = compose(f, g)
    def ∘      [G <: Arrow](g: G)(implicit compose: Compose[F, G]): compose.Out = compose(f, g)
  }



  /** Typeclass witnessing `Source` and `Target` of an [[Arrow]]. */
  trait Types[F <: Arrow]  { type Source; type Target }
  object Types {
    type Aux[F <: Arrow, S, T] = Types[F] { type Source = S; type Target = T }
    def apply[F <: Arrow](implicit t: Types[F]): Aux[F, t.Source, t.Target] = t

    implicit def arrowTypes[F <: Arrow, S, T](implicit ev: F <:< Arrow.Inv[S, T]): Aux[F, S, T] = instance.asInstanceOf[Aux[F, S, T]]
    private lazy val instance = new Types[Arrow] {}
  }

  /** Typeclass witnessing that arrows `F` and `G` can be composed. */
  trait Compose[F <: Arrow, G <: Arrow] extends DepFn2[F, G] { type Out <: Arrow }
  object Compose {
    type Aux[F <: Arrow, G <: Arrow, Composition <: Arrow] = Compose[F, G] { type Out = Composition }
    def apply[F <: Arrow, G <: Arrow](implicit compose: Compose[F, G]): Aux[F, G, compose.Out] = compose

    implicit def composeIdLeft[F <: Arrow, G <: Arrow, T](
      implicit
      types: Types.Aux[G, _, T],
      idF: F <:< Arrow.Id[T]
    ): Compose.Aux[F, G, G] =
      composeIdL.asInstanceOf[Compose.Aux[F, G, G]]
    private lazy val composeIdL = new Compose[Arrow.Id[Any], Arrow] {
      type Out = Arrow
      def apply(f: Arrow.Id[Any], g: Arrow): Arrow = g
    }

    implicit def composeIdRight[F <: Arrow, G <: Arrow, S](
      implicit
      types: Types.Aux[F, S, _],
      idG: G <:< Arrow.Id[S]
    ): Compose.Aux[F, G, F] =
      composeIdR.asInstanceOf[Compose.Aux[F, G, F]]
    private lazy val composeIdR = new Compose[Arrow, Arrow.Id[Any]] {
      type Out = Arrow
      def apply(f: Arrow, g: Arrow.Id[Any]): Arrow = f
    }


    implicit def canCompose[F <: Arrow, G <: Arrow, FS, FT, GS, GT](
      implicit
      fTypes: Types.Aux[F, FS, FT],
      gTypes: Types.Aux[G, GS, GT],
      typesCorrespond: FS <:< GT,
      lowPriority: LowPriority
    ): Aux[F, G, Composition.Aux[F, G, GS, FT]] = instance.asInstanceOf[Aux[F, G, Composition.Aux[F, G, GS, FT]]]
    private lazy val instance = new Compose[Arrow, Arrow] {
      type Out = Composition[Arrow, Arrow]
      def apply(f: Arrow, g: Arrow) = new Composition[Arrow, Arrow] { val F = f; val G = g }
    }
  }


  /** Typeclass supporting extraction of composed arrows as an HList. */
  trait Unchain[F <: Arrow] extends DepFn1[F] { type Out <: HList }
  object Unchain {
    type Aux[F <: Arrow, Arrows <: HList] = Unchain[F] { type Out = Arrows }
    def apply[F <: Arrow](implicit unchain: Unchain[F]): Aux[F, unchain.Out] = unchain

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

    implicit def notChained[F <: Arrow](implicit lowPriority: LowPriority): Unchain.Aux[F, F :: HNil] =
      _notChained.asInstanceOf[Unchain.Aux[F, F :: HNil]]
    private lazy val _notChained = new Unchain[Arrow] {
      type Out = Arrow :: HNil
      def apply(t: Arrow): Out = t :: HNil
    }
  }



  // TODO: is it a functor? should I rename it?
  /** A typeclass supporting ???. */
  trait Functor[From <: Arrow, To <: Arrow] extends DepFn1[From] { type Out <: Arrow }
  object Functor {
    type Aux[From <: Arrow, To <: Arrow, Out0 <: Arrow] = Functor[From, To] { type Out = Out0 }
    def apply[From <: Arrow, To <: Arrow](implicit functor: Functor[From, To]): Functor.Aux[From, To, functor.Out] = functor
    def map[From <: Arrow](from: From): PartialApplication[From] = new PartialApplication(from)

    def define[From <: Arrow, To <: Arrow]: DefinitionBuilder[From, To] = DefinitionBuilder.asInstanceOf[DefinitionBuilder[From, To]]

    protected class DefinitionBuilder[From <: Arrow, To <: Arrow] {
      def apply[R <: Arrow](map: From => R): Functor.Aux[From, To, R] =
        new Functor[From, To] {
          type Out = R
          def apply(t: From): R = map(t)
        }
    }
    private object DefinitionBuilder extends DefinitionBuilder[Arrow, Arrow]

    protected class PartialApplication[From <: Arrow](from: From) {
      def to[To <: Arrow](implicit functor: Functor[From, To]): functor.Out = functor(from)
    }

    implicit def compositionFunctor[From  <: Arrow, To  <: Arrow, FromF <: Arrow, ToF <: Arrow, FromG <: Arrow, ToG <: Arrow](
      implicit
      composition: From <:< Composition[FromF, FromG],
      fF: Lazy[Functor.Aux[FromF, To, ToF]],
      fG: Lazy[Functor.Aux[FromG, To, ToG]],
      compose: Compose[ToF, ToG],
      lowPriority: LowPriority
     ): Functor.Aux[From, To, compose.Out] =
      define[From, To](t => compose(fF.value(t.F), fG.value(t.G)))
  }

}