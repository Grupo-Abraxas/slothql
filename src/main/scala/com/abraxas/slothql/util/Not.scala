package com.abraxas.slothql.util


/** Inspired by [[http://hacking-scala.org/post/73854628325/advanced-type-constraints-with-type-classes]] */

@annotation.implicitNotFound("${T} implicit exists")
sealed trait Not[T]
object Not {
  implicit def evidence[T, Answer <: Boolean](implicit ev: Exists[T, Answer], ne: Answer =:= Bool.False): Not[T] = instance.asInstanceOf[Not[T]]
  private lazy val instance = new Not[Any] {}
}

sealed trait Exists[T, Answer <: Boolean]
object Exists {
  implicit def exists[T](implicit a: T): Exists[T, Bool.True] = instance.asInstanceOf[Exists[T, Bool.True]]
  implicit def notExists[T]: Exists[T, Bool.False] = instance.asInstanceOf[Exists[T, Bool.False]]
  private lazy val instance = new Exists[Any, Boolean] {}
}
