package com.abraxas.slothql.util

import scala.language.higherKinds

import shapeless._

/**
 * Type class witnessing that the result of stripping type constructors `F0 <: F` off each element of `HList` `L` is `Out`.
 */
trait ComappedCov[L <: HList, F[_]] extends Serializable {
  type Out <: HList
}

object ComappedCov {
  type Aux[L <: HList, F[_], Out0 <: HList] = ComappedCov[L, F] { type Out = Out0 }
  def apply[L <: HList, F[_]](implicit ev: ComappedCov[L, F]): Aux[L, F, ev.Out] = ev

  implicit def comappedCovHNil[F[_]]: Aux[HNil, F, HNil] = instance.asInstanceOf[Aux[HNil, F, HNil]]

  implicit def comappedCovHCons[F[_], H, T <: HList, FH, Next <: HList](
    implicit mt : ComappedCov.Aux[T, F, Next], ev: FH <:< F[H]
  ): Aux[FH :: T, F, H :: Next] = instance.asInstanceOf[Aux[FH :: T, F, H :: Next]]

  private lazy val instance = new ComappedCov[HList, Id] { type Out = HList }
}