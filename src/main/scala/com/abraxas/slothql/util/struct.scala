package com.abraxas.slothql.util

import shapeless._

/**
 * A typeclass witnessing that `A` can be constructed given arguments `Args`.
 */
trait Construct[A, Args <: HList] extends DepFn1[Args] { type Out <: A }
object Construct {
  type Aux[A, Args <: HList, Out0 <: A] = Construct[A, Args] { type Out = Out0 }

  def apply[A]: Builder[A] = Builder.asInstanceOf[Builder[A]]

  protected class Builder[A] extends ProductArgs {
    def applyProduct[Args <: HList](args: Args)(implicit build: Construct[A, Args]): Aux[A, Args, build.Out] = build
  }
  private object Builder extends Builder[Any]
}

/**
 * A typeclass witnessing that `A` can be recreated with other constructor arguments.
 * The changes are specified as shapeless records ([[shapeless.labelled.FieldType]]).
 */
trait Copy[A, Changes <: HList] extends DepFn2[A, Changes]
object Copy {
  type Aux[A, Changes <: HList, R] = Copy[A, Changes] { type Out = R }

  def apply[A](a: A): Builder[A] = new Builder[A](a)

  protected class Builder[A](a: A) extends RecordArgs {
    def getRecord[Changes <: HList](changes: Changes)(implicit copy: Copy[A, Changes]): Aux[A, Changes, copy.Out] = copy
    def applyRecord[Changes <: HList](changes: Changes)(implicit copy: Copy[A, Changes]): copy.Out = copy(a, changes)
  }
}

/**
 * A typeclass supporting transformation of `A`'s structure with polymorphic function `HF`.
 * `HF` would immediately transform `A` if defined at.
 * In any case it should proceed transforming the children recursively until reaching tree's bottom.
 */
sealed trait Transform[A, HF <: Poly1] extends DepFn1[A]
object Transform {
  type Aux[A, HF <: Poly1, R] = Transform[A, HF] { type Out = R }

  def get[A, HF <: Poly1](a: A, hf: HF)(implicit transform: Transform[A, HF]): Aux[A, HF, transform.Out] = transform
  def apply[A, HF <: Poly1](a: A, hf: HF)(implicit transform: Transform[A, HF]): transform.Out = transform(a)

  /**
   * Should apply `Transform` to each child. Must not apply `HF` to `A` even is defined at.
   */
  trait Children[A, HF <: Poly1] extends DepFn1[A]
  object Children {
    type Aux[A, HF <: Poly1, R] = Transform.Children[A, HF] { type Out = R }

    def apply[A, HF <: Poly1](a: A, hf: HF)(implicit transform: Transform.Children[A, HF]): Aux[A, HF, transform.Out] = transform

    def none[A, HF <: Poly1]: Children.Aux[A, HF, A] = noChildren.asInstanceOf[Children.Aux[A, HF, A]]
    private lazy val noChildren = new Children[Any, Poly1] {
      type Out = Any
      @inline def apply(t: Any): Any = t
    }
  }

  implicit def transformThis[A, HF <: Poly1, R0](
    implicit
    hf: poly.Case1.Aux[HF, A, R0],
    next: Transform.Children[R0, HF]
  ): Transform.Aux[A, HF, next.Out] =
    new Transform[A, HF] {
      type Out = next.Out
      def apply(t: A): next.Out = next(hf(t))
    }

  implicit def transformNext[A, HF <: Poly1](
    implicit
    hf: Not[poly.Case1[HF, A]],
    next: Transform.Children[A, HF]
  ): Transform.Aux[A, HF, next.Out] =
    new Transform[A, HF] {
      type Out = next.Out
      def apply(t: A): next.Out = next(t)
    }

  implicit def transformHCons[H0, T0 <: HList, H, T <: HList, HF <: Poly1](
    implicit
    h: Transform.Aux[H0, HF, H],
    t: Transform.Aux[T0, HF, T]
  ): Transform.Aux[H0 :: T0, HF, H :: T] =
    new Transform[H0 :: T0, HF] {
      type Out = H :: T
      def apply(l: H0 :: T0): H :: T = h(l.head) :: t(l.tail)
    }

  implicit def transformHNil[HF <: Poly1]: Transform.Aux[HNil, HF, HNil] = tHNil.asInstanceOf[Transform.Aux[HNil, HF, HNil]]
  private lazy val tHNil = new Transform[HNil, Poly1] {
    type Out = HNil
    @inline def apply(t: HNil): HNil = HNil
  }

}
