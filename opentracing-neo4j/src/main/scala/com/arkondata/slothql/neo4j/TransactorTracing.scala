package com.arkondata.slothql.neo4j

import scala.concurrent.duration.{ DurationInt, FiniteDuration }

import cats.effect.kernel.{ Async, Deferred }
import cats.effect.std.Dispatcher
import cats.syntax.functor._
import natchez.Span
import org.neo4j.driver.Driver

class TransactorTracing[F[_]](
  val transactor: Neo4jCypherTransactor[F]
) {
  import transactor.{ Out, Tx }

  lazy val syntax  = transactor
  lazy val readers = transactor.readers

  def runRead[R](tx: Tx[R])(implicit span: Span[F]): Out[R] =
    apply(tx, transactor.defaultTimeout, write = false)

  def runWrite[R](tx: Tx[R])(implicit span: Span[F]): Out[R] =
    apply(tx, transactor.defaultTimeout, write = true)

  def runRead[R](tx: Tx[R], timeout: FiniteDuration)(implicit span: Span[F]): Out[R] =
    apply(tx, timeout, write = false)

  def runWrite[R](tx: Tx[R], timeout: FiniteDuration)(implicit span: Span[F]): Out[R] =
    apply(tx, timeout, write = true)

  def apply[R](tx: Tx[R], timeout: FiniteDuration, write: Boolean)(implicit span: Span[F]): Out[R] =
    transactor.apply(tx, timeout, write)
}

object TransactorTracing {

  def apply[F[_]: Async](
    driver: Driver,
    defaultTimeout: FiniteDuration = 10.seconds,
    chunkSize: Int = 1024
  )(implicit
    dispatcher: Dispatcher[F]
  ): F[(TransactorTracing[F], Deferred[F, Unit])] =
    Neo4jCypherTransactor(driver, defaultTimeout, chunkSize).map { case (transactor, complete) =>
      (new TransactorTracing[F](transactor), complete)
    }
}
