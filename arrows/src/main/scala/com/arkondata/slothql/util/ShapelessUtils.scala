package com.arkondata.slothql.util

import shapeless._
import shapeless.HList.ListCompat._

object ShapelessUtils {

  def unsafeHListToList[A](hl: HList): List[A] = hl match {
    case h #: t => h.asInstanceOf[A] :: unsafeHListToList(t)
    case HNil   => Nil
  }

  /** Evidence that `L` contains no pair of elements such that `X =:= Y`. */
  trait HListElementAreUnique[L <: HList]
  object HListElementAreUnique {
    implicit def hnilIsUnique: HListElementAreUnique[HNil] = instance.asInstanceOf[HListElementAreUnique[HNil]]
    implicit def hconsIsUnique[H, T <: HList](
      implicit
      noHeadFoundInTail: ops.hlist.Filter.Aux[T, H, HNil],
      tailElementsAreUnique: HListElementAreUnique[T]
    ): HListElementAreUnique[H :: T] = instance.asInstanceOf[HListElementAreUnique[H :: T]]

    private lazy val instance = new HListElementAreUnique[HList] {}
  }

}
