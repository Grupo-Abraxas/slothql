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

  implicit def hnil[F[_]]: Aux[HNil, F, HNil] = new ComappedCov[HNil, F] { type Out = HNil }

  implicit def hcons[F[_], H, T <: HList, FH, Next <: HList](
    implicit mt : ComappedCov.Aux[T, F, Next], ev: FH <:< F[H]
  ): Aux[FH :: T, F, H :: Next] =
    new ComappedCov[FH :: T, F] { type Out = H :: Next }

}