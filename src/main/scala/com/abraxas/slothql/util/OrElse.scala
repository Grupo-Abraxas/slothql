package com.abraxas.slothql.util

import scala.language.higherKinds

import shapeless._

/**
 * A typeclass supporting alternative selection in case `A` is typed with `Nothing`.
 */
trait OrElse[A, B] extends DepFn2[A, B]
object OrElse {
  type Aux[A, B, Out0] = OrElse[A, B] { type Out = Out0 }
  def apply[A, B](implicit ev: A OrElse B): Aux[A, B, ev.Out] = ev

  implicit def orElseA[A, B](implicit defined: A =:!= Nothing): Aux[A, B, A] = a.asInstanceOf[Aux[A, B, A]]
  private lazy val a = new OrElse[Any, Any] {
    type Out = Any
    def apply(a: Any, b: Any): Any = a
  }

  implicit def orElseB[A, B]: Aux[Nothing, B, B] = b.asInstanceOf[Aux[Nothing, B, B]]
  private lazy val b = new OrElse[Any, Any] {
    type Out = Any
    def apply(a: Any, b: Any): Any = b
  }
}

/**
 * A typeclass supporting alternative selection in case `A` is typed with `Nothing` or mapping `A` otherwise.
 */
trait MapOrElse[HF <: Poly1, A, B] extends DepFn2[A, B]
object MapOrElse {
  type Aux[HF <: Poly1, A, B, Out0] = MapOrElse[HF, A, B] { type Out = Out0 }
  def apply[HF <: Poly1, A, B](implicit moe: MapOrElse[HF, A, B]): Aux[HF, A, B, moe.Out] = moe

  implicit def mapOrElseA[HF <: Poly1, A, B, A0](
    implicit
    defined: A =:!= Nothing,
    hf: poly.Case1.Aux[HF, A0, A]
  ): Aux[HF, A0, B, A] =
    new MapOrElse[HF, A0, B] {
      type Out = A
      def apply(a0: A0, b: B): A = hf(a0)
    }

  implicit def mapOrElseB[HF <: Poly1, A, B](implicit undefined: A =:= Nothing): Aux[HF, A, B, B] = b.asInstanceOf[Aux[HF, A, B, B]]
  private lazy val b = new MapOrElse[Poly1, Any, Any] {
    type Out = Any
    def apply(a0: Any, b: Any): Any = b
  }

}

/**
 * A typeclass supporting alternative selection in case `A` is typed with `Nothing`
 * or wrapping `A` in type constructor `F` otherwise.
 */
trait MappedOrElse[F[_], A, B] { type Out }
object MappedOrElse {
  type Aux[F[_], A, B, Out0] = MappedOrElse[F, A, B] { type Out = Out0 }
  def apply[F[_], A, B](implicit moe: MappedOrElse[F, A, B]): Aux[F, A, B, moe.Out] = moe

  implicit def mapOrElseA[F[_], A, B](implicit defined: A =:!= Nothing): Aux[F, A, B, F[A]] = instance.asInstanceOf[Aux[F, A, B, F[A]]]
  implicit def mapOrElseB[F[_], A, B]: Aux[F, Nothing, B, B] = instance.asInstanceOf[Aux[F, Nothing, B, B]]
  private lazy val instance = new MappedOrElse[Id, Any, Any] { type Out = Any }
}
