package com.abraxas.slothql.util

import shapeless._

/** Type class supporting removal of elements of `L` that are subtypes of `U`. */
trait FilterNotCov[L <: HList, U] extends DepFn1[L] { type Out <: HList }
object FilterNotCov {
  type Aux[L <: HList, U, Out0 <: HList] = FilterNotCov[L, U] { type Out = Out0 }
  def apply[L <: HList, U](implicit ev: FilterNotCov[L, U]): Aux[L, U, ev.Out] = ev

  implicit def filterNotCovHNil[U]: Aux[HNil, U, HNil] = instanceId.asInstanceOf[Aux[HNil, U, HNil]]
  private lazy val instanceId = new FilterNotCov[HList, Any] {
    type Out = HList
    @inline def apply(t: HList): HList = t
  }

  implicit def filterNotCovHConsLeave[U, H, T <: HList](
    implicit
    headIsNotSubtypeOfU: H <:!< U,
    tailFilterNotCov: FilterNotCov[T, U]
  ): Aux[H :: T, U, H :: tailFilterNotCov.Out] =
    new FilterNotCov[H :: T, U] {
      type Out = H :: tailFilterNotCov.Out
      def apply(t: H :: T): H :: tailFilterNotCov.Out = t.head :: tailFilterNotCov(t.tail)
    }

  implicit def filterNotCovHConsDrop[U, H, T <: HList](
    implicit
    headIsSubtypeOfU: H <:< U,
    tailFilterNotCov: FilterNotCov[T, U]
  ): Aux[H :: T, U, tailFilterNotCov.Out] =
    new FilterNotCov[H :: T, U] {
      type Out = tailFilterNotCov.Out
      def apply(t: H :: T): tailFilterNotCov.Out = tailFilterNotCov(t.tail)
    }
}
