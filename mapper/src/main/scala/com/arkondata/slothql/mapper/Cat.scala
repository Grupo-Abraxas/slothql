package com.arkondata.slothql.mapper

trait Cat extends Cat.ArrowsComposition {

  /**
   * Category object.
   */
  type Obj

  /**
   * Category morphism.
   */
  type Arr <: {
    type Src <: Obj
    type Tgt <: Obj
  }

  type ArrAux[-T <: Obj, +R <: Obj] = Arr { type Src >: T; type Tgt <: R }
  type ~>[-T <: Obj, +R <: Obj] = ArrAux[T, R]


  /**
   * Identity morphism.
   */
  type Id[A <: Obj] <: A ~> A

  def id[A <: Obj]: Id[A]

  /**
   * Morphism composition.
   */
  type Compose[F <: Arr, G <: Arr] <: G#Src ~> F#Tgt

  type ∘[F <: Arr, G <: Arr] = Compose[F, G]

  def compose[F <: Arr, G <: Arr](f: F, g: G)(implicit c: CanCompose[F, G]): c.Out = c(f, g)

  protected def unsafeCompose[F <: Arr, G <: Arr](f: F, g: G): F ∘ G

  /** Abstract definitions of [[Id]] and [[Compose]]. */
  protected object Abstract {
    trait Id[A <: Obj] { this: Arr =>
      type Src = A
      type Tgt = A
    }
    trait Compose[F <: Arr, G <: Arr] { this: Arr =>
      type Src = G#Src
      type Tgt = F#Tgt
    }
  }
}

object Cat {

  type Aux[O, A[-_ <: O, +_ <: O]] = Cat { type Obj = O; type Arr[-T <: O, +R <: O] = A[T, R] }

  /**
   * Better identity arrows composition.
   */
  protected trait ArrowsComposition { this: Cat =>

    trait CanCompose[F <: Arr, G <: Arr] {
      type Out <: Arr
      def apply(f: F, g: G): Out
    }

    object CanCompose extends CanComposeLowPriorityImplicits{
      type Aux[F <: Arr, G <: Arr, C <: Arr] = CanCompose[F, G] { type Out = C }

      implicit def canComposeIdBoth[A <: Obj]: CanCompose.Aux[Id[A], Id[A], Id[A]] =
        leftInstance.asInstanceOf[CanCompose.Aux[Id[A], Id[A], Id[A]]]
    }

    protected trait CanComposeLowPriorityImplicits extends CanComposeLowestPriorityImplicits{
      implicit def canComposeIdLeft[A <: Obj, G <: Arr](implicit ev: A <:< G#Tgt): CanCompose.Aux[Id[A], G, G] =
        rightInstance.asInstanceOf[CanCompose.Aux[Id[A], G, G]]

      implicit def canComposeIdRight[A <: Obj, F <: Arr](implicit ev: F#Src <:< A): CanCompose.Aux[F, Id[A], F] =
        leftInstance.asInstanceOf[CanCompose.Aux[F, Id[A], F]]

      protected[this] lazy val leftInstance = new CanCompose[Arr, Id[Obj]] {
        type Out = Arr
        @inline def apply(f: Arr, g: Id[Obj]): Arr = f
      }
      protected[this] lazy val rightInstance = new CanCompose[Id[Obj], Arr] {
        type Out = Arr
        @inline def apply(f: Id[Obj], g: Arr): Arr = g
      }
    }

    protected trait CanComposeLowestPriorityImplicits {
      implicit def canCompose[F <: Arr, G <: Arr](implicit ev: F#Src <:< G#Tgt): CanCompose.Aux[F, G, F ∘ G] =
        instance.asInstanceOf[CanCompose.Aux[F, G, F ∘ G]]

      private lazy val instance = new CanCompose[Arr, Arr] {
        type Out = Arr ∘ Arr
        def apply(f: Arr, g: Arr): Arr ∘ Arr = unsafeCompose(f, g)
      }
    }
  }

  implicit object ScalaCat extends Cat {
    type Obj = Any

    sealed trait Arr {
      type Src
      type Tgt
      val func: Src => Tgt
    }

    final class Id[A] protected[ScalaCat]() extends Arr with Abstract.Id[A] {
      val func: A => A = locally[A]
    }

    final class Compose[F <: Arr, G <: Arr] protected[ScalaCat](f: F, g: G) extends Arr with Abstract.Compose[F, G] {
      // unsafe
      val func: G#Src => F#Tgt = f.func.asInstanceOf[Any => F#Tgt] compose g.func.asInstanceOf[G#Src => Any]
    }

    def id[A <: ScalaCat.Obj]: Id[A] = new Id[A]
    protected def unsafeCompose[F <: Arr, G <: Arr](f: F, g: G): F ∘ G = new Compose(f, g)
  }

}

trait CatPipe { self: Cat =>

  //  type Fanout[Arrs <: HList] <: Arr

  //  type First[Arrs <: HList, A <: Arr] <: Arr
}

trait Functor[From <: Cat, To <: Cat] {
  type MapObject[O <: From#Obj] <: {
    type Out <: To#Obj
  }
  type MapArrow[A <: From#Arr] <: {
    type Out <: To#Arr
  }
}