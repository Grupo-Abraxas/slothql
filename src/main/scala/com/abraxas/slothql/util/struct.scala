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
    def applyRecord[Changes <: HList](changes: Changes)(implicit copy: Copy[A, Changes]): Aux[A, Changes, copy.Out] = copy
  }

  implicit def copyIdentity[A]: Copy.Aux[A, HNil, A] = _copyIdentity.asInstanceOf[Copy.Aux[A, HNil, A]]
  private lazy val _copyIdentity = new Copy[Any, HNil] {
    type Out = Any
    def apply(t: Any, u: HNil): Any = t
  }
}

/**
 * A typeclass supporting transformation of `A`'s structure with polymorphic function `HF`.
 * `HF` would immediately transform `A` if defined at.
 * In any case it should proceed transforming the children recursively until reaching tree's bottom.
 */
trait Transform[A, HF <: Poly1] extends DepFn1[A]
object Transform {
  type Aux[A, HF <: Poly1, R] = Transform[A, HF] { type Out = R }

  def apply[A, HF <: Poly1](a: A, hf: HF)(implicit transform: Transform[A, HF]): Aux[A, HF, transform.Out] = transform

  implicit def transformThis[A, HF <: Poly1, R0](
    implicit
    hf: poly.Case1.Aux[HF, A, R0],
    next: Transform[R0, HF]
  ): Aux[A, HF, next.Out] =
    new Transform[A, HF] {
      type Out = next.Out
      def apply(t: A): next.Out = next(hf(t))
    }

}
