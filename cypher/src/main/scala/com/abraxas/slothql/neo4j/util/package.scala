package com.abraxas.slothql.neo4j

import cats.{ Monad, StackSafeMonad }
import cats.effect.IO
import cats.instances.vector._
import cats.syntax.traverse._


package object util {
  implicit lazy val IOVectorMonad: Monad[λ[A => IO[Vector[A]]]] = new StackSafeMonad[λ[A => IO[Vector[A]]]] {
    def pure[A](x: A): IO[Vector[A]] = IO.pure(Vector(x))
    def flatMap[A, B](fa: IO[Vector[A]])(f: A => IO[Vector[B]]): IO[Vector[B]] = fa.flatMap(_.flatTraverse(f))
  }
}
