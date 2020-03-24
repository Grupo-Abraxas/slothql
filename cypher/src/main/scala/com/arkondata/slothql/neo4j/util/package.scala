package com.arkondata.slothql.neo4j

import scala.language.higherKinds

import cats.{ Monad, StackSafeMonad, Traverse }
import cats.effect.IO
import cats.syntax.applicative._
import cats.syntax.traverse._


package object util {
  implicit def ioTraverseMonadIsMonad[F[_]: Monad: Traverse]: Monad[λ[A => IO[F[A]]]] = new StackSafeMonad[λ[A => IO[F[A]]]] {
    def pure[A](x: A): IO[F[A]] = IO.pure(x.pure[F])
    def flatMap[A, B](fa: IO[F[A]])(f: A => IO[F[B]]): IO[F[B]] = fa.flatMap(_.flatTraverse(f))
  }
}
