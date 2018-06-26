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
