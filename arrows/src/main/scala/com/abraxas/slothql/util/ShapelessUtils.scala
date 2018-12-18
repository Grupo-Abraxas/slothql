package com.abraxas.slothql.util

import shapeless.{ HList, HNil }
import shapeless.HList.ListCompat._

object ShapelessUtils {

  def unsafeHListToList[A](hl: HList): List[A] = hl match {
    case h #: t => h.asInstanceOf[A] :: unsafeHListToList(t)
    case HNil   => Nil
  }

}
