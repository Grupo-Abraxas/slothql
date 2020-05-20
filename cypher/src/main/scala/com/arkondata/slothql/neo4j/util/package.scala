package com.arkondata.slothql.neo4j

import java.util.stream.{ Stream => JStream }

import scala.jdk.CollectionConverters._

import cats.effect.{ Blocker, ContextShift, Resource, Sync }
import cats.{ Applicative, Apply, Monad }
import cats.syntax.applicative._

import com.arkondata.slothql.cypher.CypherTransactor.{ TxC => TxC0, _ }


package object util {

  implicit def fs2StreamTxCMonad[F[_], Src](
    implicit
    A: Applicative[F],
    compiler: fs2.Stream.Compiler[F, F]
  ): Monad[TxC0[F, Src, fs2.Stream[F, *], *]] =
    new Monad[TxC0[F, Src, fs2.Stream[F, *], *]] {
      type TxC[A] = TxC0[F, Src, fs2.Stream[F, *], A]

      def pure[A](x: A): TxC[A] = const[F, Src, fs2.Stream[F, *]].apply(fs2.Stream.emit(x))

      override def map[A, B](fa: TxC[A])(f: A => B): TxC[B] = fa.map(_.map(f))

      def flatMap[A, B](fa: TxC[A])(f: A => TxC[B]): TxC[B] =
        fa.flatMap { sa =>
          val chained = sa.compile.fold(const[F, Src, fs2.Stream[F, *]].apply(fs2.Stream.empty): TxC[B])(
            (acc, a) =>
              Apply[Tx[F, Src, fs2.Stream[F, *], *]].product(acc, f(a)).map{ case (l, r) => l append r }
          )
          liftTx[F, Src, fs2.Stream[F, *]].apply(chained)
        }

      def tailRecM[A, B](a: A)(f: A => TxC[Either[A, B]]): TxC[B] =
        flatMap(f(a)) {
          case Left(value)  => tailRecM(value)(f)
          case Right(value) => liftF[F, Src, fs2.Stream[F, *]].apply(fs2.Stream.emit[F, B](value).pure[F])
        }
    }

  def javaStreamToFs2[F[_]: Sync: ContextShift, A](blocker: Blocker, fj: F[JStream[A]]): fs2.Stream[F, A] =
    fs2.Stream
      .resource(Resource.fromAutoCloseable(fj))
      .flatMap(jStream => fs2.Stream.fromBlockingIterator[F](blocker, jStream.iterator().asScala))

}
