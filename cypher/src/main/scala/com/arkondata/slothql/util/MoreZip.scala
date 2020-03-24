package com.arkondata.slothql.util

import scala.collection.{ GenTraversable, mutable }
import scala.language.higherKinds

object MoreZip {
  def zip2[A, B, CC[X] <: GenTraversable[X]](ca: CC[A], cb: CC[B]): CC[(A, B)] = {
    val builder = ca.companion.newBuilder[(A, B)].asInstanceOf[mutable.Builder[(A, B), CC[(A, B)]]]
    val ia = ca.toIterator
    val ib = cb.toIterator
    while (ia.hasNext && ib.hasNext) builder += ((ia.next(), ib.next()))
    builder.result()
  }

  def zip3[A, B, C, CC[X] <: GenTraversable[X]](ca: CC[A], cb: CC[B], cc: CC[C]): CC[(A, B, C)] = {
    val builder = ca.companion.newBuilder[(A, B, C)].asInstanceOf[mutable.Builder[(A, B, C), CC[(A, B, C)]]]
    val ia = ca.toIterator
    val ib = cb.toIterator
    val ic = cc.toIterator
    while (ia.hasNext && ib.hasNext && ic.hasNext) builder += ((ia.next(), ib.next(), ic.next()))
    builder.result()
  }
}
