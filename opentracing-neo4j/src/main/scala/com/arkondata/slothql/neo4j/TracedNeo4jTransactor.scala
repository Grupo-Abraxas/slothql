package com.arkondata.slothql.neo4j

import java.util.concurrent.TimeUnit

import scala.jdk.FutureConverters._

import cats.effect.{ Async, Blocker, ConcurrentEffect, ContextShift, Resource }
import cats.effect.syntax.effect._
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import org.neo4j.driver.{ Driver, Result, Session, Transaction, TransactionWork }
import io.opentracing.tag.Tags
import org.neo4j.driver.async.ResultCursor
import org.neo4j.driver.summary.ResultSummary

import com.github.fehu.opentracing.v2.{ Traced, Traced2 }
import com.github.fehu.opentracing.v2.fs2._
import com.github.fehu.opentracing.v2.syntax._
import com.arkondata.slothql.cypher.CypherStatement

class TracedNeo4jTransactor[T[_[*], *], F[_]: ConcurrentEffect](session: T[F, Session])
                           (implicit ce: ConcurrentEffect[T[F, *]], cs: ContextShift[T[F, *]], traced: Traced2[T, F])
  extends Neo4jCypherTransactor[T[F, *]](session)
{

  override protected def executeSync: T[F, T[F, Unit] => Unit] =
    for {
      p  <- traced.currentRunParams
      f0 = (f: T[F, Unit]) => f.runTraced(p).toIO.unsafeRunSync()
    } yield f0


  override def runRead[A](tx: Tx[A]): fs2.Stream[T[F, *], A] = super.runRead(tx).traced("runRead")
    .tracedElemLog

  override def runWrite[A](tx: Tx[A]): fs2.Stream[T[F, *], A] = super.runWrite(tx).traced("runWrite")

  // override protected def sessionResource: Resource[T[F, *], Session] =
  //   super.sessionResource.tracedLifetime("session")

  override protected def transactionResource(run: TransactionWork[Unit] => Unit): Resource[T[F, *], Transaction] =
    super.transactionResource(run).tracedLifetime("transaction")
                                  .evalTap(tx => Traced.currentSpan.setTag("tx" -> tx.##))

  override protected def commitTransaction(tx: Transaction): T[F, Unit] =
    Traced.currentSpan.log("commit transaction") *> super.commitTransaction(tx)

  override protected def rollbackTransaction(tx: Transaction): T[F, Unit] =
    Traced.currentSpan.log("rollback transaction") *> super.rollbackTransaction(tx)

  override protected def closeTransaction(tx: Transaction): T[F, Unit] =
    Traced.currentSpan.log("close transaction") *> super.closeTransaction(tx)

  override protected def runUnwind[A](out: fs2.Stream[T[F, *], A])(tx: Transaction): fs2.Stream[T[F, *], A] =
    super.runUnwind(out)(tx).traced("runUnwind")

  override protected def runGather[A, B](blocker: Blocker, op: Op[B], gather: fs2.Stream[T[F, *], B] => A)
                                        (tx: Transaction): fs2.Stream[T[F, *], A] =
    super.runGather(blocker, op, gather)(tx).traced("runGather")

  override protected def runQuery[A](blocker: Blocker, q: CypherStatement.Prepared[A], reader: Reader[A])(tx: Transaction): fs2.Stream[T[F, *], A] =
    super.runQuery(blocker, q, reader)(tx).traced("runQuery", Tags.DB_TYPE -> "cypher/neo4j",
                                                              Tags.DB_STATEMENT -> q.template)

  override protected def execQuery(tx: Transaction, q: CypherStatement.Prepared[_]): T[F, Result] =
    super.execQuery(tx, q) <* traced.currentSpan.log("execute")

  override protected def runningQuery[A](result: Result, stream: fs2.Stream[T[F, *], A]): T[F, fs2.Stream[T[F, *], A]] =
    ce.pure {
      stream.onFinalize {
        for {
          cursor  <- internalResultCursor(result)
          summary <- Async.fromFuture(ce.delay{ cursor.consumeAsync().asScala })
          _       <- reportSummary(summary)
        } yield ()
      }
    }

  protected def reportSummary(summary: ResultSummary): T[F, Unit] = Traced.currentSpan.log(
    "event"                                 -> "Result fully consumed.",
    "summary.queryType"                     -> summary.queryType,
    "summary.notifications"                 -> summary.notifications,
    "summary.resultAvailableAfter"          -> summary.resultAvailableAfter(TimeUnit.MILLISECONDS),
    "summary.resultAvailableAfter.unit"     -> "milliseconds",
    "summary.resultConsumedAfter"           -> summary.resultConsumedAfter(TimeUnit.MILLISECONDS),
    "summary.resultConsumedAfter.unit"      -> "milliseconds",
    "summary.server.address"                -> summary.server.address,
    "summary.server.version"                -> summary.server.version,
    "summary.database.name"                 -> summary.database.name,
    "summary.counters.containsUpdates"      -> summary.counters.containsUpdates,
    "summary.counters.nodesCreated"         -> summary.counters.nodesCreated,
    "summary.counters.nodesDeleted"         -> summary.counters.nodesDeleted,
    "summary.counters.relationshipsCreated" -> summary.counters.relationshipsCreated,
    "summary.counters.relationshipsDeleted" -> summary.counters.relationshipsDeleted,
    "summary.counters.propertiesSet"        -> summary.counters.propertiesSet,
    "summary.counters.labelsAdded"          -> summary.counters.labelsAdded,
    "summary.counters.labelsRemoved"        -> summary.counters.labelsRemoved,
    "summary.counters.indexesAdded"         -> summary.counters.indexesAdded,
    "summary.counters.indexesRemoved"       -> summary.counters.indexesRemoved,
    "summary.counters.constraintsAdded"     -> summary.counters.constraintsAdded,
    "summary.counters.constraintsRemoved"   -> summary.counters.constraintsRemoved,
    "summary.counters.containsSystemUpdates" -> summary.counters.containsSystemUpdates,
    "summary.counters.systemUpdates"        -> summary.counters.systemUpdates
  )

  // Use java reflection for getting access to private field
  private def internalResultCursor(result: Result): T[F, ResultCursor] = for {
    clazz  <- ce.pure(result.getClass)
    field  <- ce.catchNonFatal{ clazz.getDeclaredField("cursor") }
    _      <- ce.catchNonFatal{ field.setAccessible(true) }
    cursor <- ce.catchNonFatal{ field.get(result).asInstanceOf[ResultCursor] }
  } yield cursor

}

object TracedNeo4jTransactor {
  def apply[T[_[*], *], F[_]: ConcurrentEffect](driver: Driver)
                                               (implicit ce: ConcurrentEffect[T[F, *]],
                                                         cs: ContextShift[T[F, *]],
                                                         traced: Traced2[T, F]
                                               ): TracedNeo4jTransactor[T, F] =
    new TracedNeo4jTransactor(ce.delay{ driver.session() })
}