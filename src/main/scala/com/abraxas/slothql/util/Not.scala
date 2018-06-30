package com.abraxas.slothql.util

import shapeless.Witness

@annotation.implicitNotFound("${T} implicit exists")
sealed trait Not[T]
object Not {
  private lazy val notEv = new Not[Any] {}
  implicit def evidence[T, Answer <: Boolean](implicit ev: Exists[T, Answer], ne: Answer =:= Bool.False): Not[T] =
    notEv.asInstanceOf[Not[T]]
}

object Bool {
  type True  = Witness.`true`.T
  type False = Witness.`false`.T
}

trait Exists[T, Answer <: Boolean]
object Exists {
  private lazy val existsEv = new Exists[Any, Boolean] {}
  implicit def exists[T](implicit a: T): Exists[T, Bool.True] = existsEv.asInstanceOf[Exists[T, Bool.True]]
  implicit def notExists[T]: Exists[T, Bool.False] = existsEv.asInstanceOf[Exists[T, Bool.False]]
}
