package com.arkondata.slothql.neo4j

import java.io.{ PrintWriter, StringWriter }
import java.util.concurrent.TimeUnit

import scala.concurrent.duration.{ DurationInt, FiniteDuration }

import cats.effect.kernel.{ Async, Deferred, MonadCancel }
import cats.effect.std.Dispatcher
import cats.effect.{ Outcome, Resource }
import cats.syntax.apply._
import cats.syntax.functor._
import cats.~>
import fs2.interop.reactivestreams.PublisherOps
import natchez.{ Span, Tags }
import org.neo4j.driver.reactive.{ RxSession, RxTransaction }
import org.neo4j.driver.{ Driver, Record }

import com.arkondata.slothql.cypher.{ CypherStatement, CypherTransactor }

class TransactorTracing[F[_]: MonadCancel[*[_], Throwable]](
  driver: Driver,
  completion: Deferred[F, Unit],
  defaultTimeout: FiniteDuration,
  chunkSize: Int
)(implicit
  dispatcher: Dispatcher[F],
  F: Async[F]
) extends Neo4jCypherTransactor.Syntax[F] {

  type Out[R]         = fs2.Stream[F, R]
  override type Tx[R] = CypherTransactor.Tx[F, Record, Out, R]

  object readers extends Neo4jCypherTransactor.Readers

  private def resolveErrorTrace(err: Throwable): Resource[F, String] = for {
    sw <- Resource.fromAutoCloseable(F.delay(new StringWriter()))
    pw <- Resource.fromAutoCloseable(F.delay(new PrintWriter(sw)))
    _  <- Resource.eval(F.delay(err.printStackTrace(pw)))
  } yield sw.toString

  def proxy(qSpan: Span[F]): Neo4jCypherTransactor[F] =
    new Neo4jCypherTransactor[F](driver, completion, defaultTimeout, chunkSize) {

      override protected def unwind[A](out: fs2.Stream[F, A]): fs2.Stream[F, A] =
        fs2.Stream.resource(qSpan.span("transaction-unwind")).flatMap(s => super.unwind(out))

      override protected def query[A](
        transactor: RxTransaction,
        query: CypherStatement.Prepared[A],
        read: CypherTransactor.Reader[Record, A]
      ): fs2.Stream[F, A] =
        fs2.Stream
          .resource(qSpan.span("transaction-query"))
          .flatMap { span =>
            val (res, rx) = super.queryWithSummary(transactor, query, read)
            res.onFinalizeCase(out =>
              rx.consume()
                .toStreamBuffered(chunkSize)
                .evalMap { rs =>
                  span.put(
                    "exit-case"                     -> out.toString,
                    "query-type"                    -> rs.queryType().toString,
                    "query"                         -> rs.query().text(),
                    "parameters"                    -> rs.query().parameters().toString,
                    "available"                     -> rs.resultAvailableAfter(TimeUnit.MILLISECONDS).toString,
                    "consumed"                      -> rs.resultConsumedAfter(TimeUnit.MILLISECONDS).toString,
                    "server-processor"              -> rs.server().address(),
                    "counter-contains-updates"      -> rs.counters().containsUpdates(),
                    "counter-nodes-created"         -> rs.counters().nodesCreated(),
                    "counter-nodes-deleted"         -> rs.counters().nodesDeleted(),
                    "counter-relationships-created" -> rs.counters().relationshipsCreated(),
                    "counter-relationships-deleted" -> rs.counters().relationshipsDeleted(),
                    "counter-properties-set"        -> rs.counters().propertiesSet(),
                    "counter-labels-added"          -> rs.counters().labelsAdded(),
                    "counter-labels-removed"        -> rs.counters().labelsRemoved()
                  )
                }
                .compile
                .drain
            )
          }

      override protected def gather[U, A](
        runOp: OpS ~> Out,
        value: CypherTransactor.Operation[Record, Out, U],
        fn: fs2.Stream[F, U] => A
      ): fs2.Stream[F, A] =
        fs2.Stream.resource(qSpan.span("transaction-gather")).flatMap(_ => super.gather(runOp, value, fn))

      override protected def sessionResource: Resource[F, RxSession] =
        super.sessionResource.guaranteeCase {
          case Outcome.Canceled() =>
            Resource.eval(qSpan.put(Tags.error(true), "canceled" -> true, "error-detail" -> "Was canceled"))
          case Outcome.Errored(err) =>
            resolveErrorTrace(err).evalMap(detail => qSpan.put(Tags.error(true), "error-detail" -> detail))
          case Outcome.Succeeded(_) => Resource.eval(qSpan.put("status" -> "success"))
        }
    }

  def runRead[R](tx: Tx[R])(implicit span: Span[F]): Out[R] =
    apply(tx, defaultTimeout, write = false)

  def runWrite[R](tx: Tx[R])(implicit span: Span[F]): Out[R] =
    apply(tx, defaultTimeout, write = true)

  def runRead[R](tx: Tx[R], timeout: FiniteDuration)(implicit span: Span[F]): Out[R] =
    apply(tx, timeout, write = false)

  def runWrite[R](tx: Tx[R], timeout: FiniteDuration)(implicit span: Span[F]): Out[R] =
    apply(tx, timeout, write = true)

  def apply[R](tx: Tx[R], timeout: FiniteDuration, write: Boolean)(implicit span: Span[F]): Out[R] =
    fs2.Stream.resource(span.span(s"session-${if (write) "write" else "read"}")).flatMap { qSpan =>
      fs2.Stream.eval(qSpan.put("registered-timeout" -> timeout.toString(), Tags.component("persistence"))) *>
      proxy(qSpan)(tx, timeout, write)
    }

}

object TransactorTracing {

  def apply[F[_]: Async](
    driver: Driver,
    defaultTimeout: FiniteDuration = 10.seconds,
    chunkSize: Int = 1024
  )(implicit
    dispatcher: Dispatcher[F]
  ): F[(TransactorTracing[F], Deferred[F, Unit])] =
    Deferred[F, Unit].map(defer => (new TransactorTracing(driver, defer, defaultTimeout, chunkSize), defer))
}
