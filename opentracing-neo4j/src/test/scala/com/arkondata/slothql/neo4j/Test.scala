package com.arkondata.slothql.neo4j

import java.util.concurrent.TimeUnit

import cats.effect.{ Blocker, ConcurrentEffect, ContextShift }
import cats.instances.option._
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.~>
import com.github.fehu.opentracing.SpanOps
import com.github.fehu.opentracing.effect.{ ResourceTracingOps, activeSpan }
import com.github.fehu.opentracing.util.TraceBundle
import org.neo4j.driver.summary.ResultSummary
import org.neo4j.driver.{ Session, Transaction }

@TransactorTracing
class Test[F[_]: ContextShift](session: F[Session])(implicit tracingBundle: TraceBundle.Endo[F], ce: ConcurrentEffect[F])
  extends Neo4jCypherTransactor[F](session)
{
  import tracingBundle.Implicits._

  import TransactorTracing._
  val helper = new TracingHelper[F]
  import helper._

  def transactorTracingSetup = TransactorTracing.setup[F](
    runRead  = traceStreamK("Transactor#runRead"),
    runWrite = traceStreamK("Transactor#runWrite"),
    run      = traceStreamK("Transactor#run"),
    blockerResource = traceResource[Blocker](_.traceUsage("Transactor#blocker")),
    sessionResource = traceResource[Session](_.traceUsage("Transactor#session")),
    transactionResource = traceResource[Transaction](_.traceCreation("Transactor#transaction:build")
                                                      .traceUsage("Transactor#transaction:usage")),
    backgroundWorkResource   = traceResource[F[Unit]](_.traceUsage("Transactor#backgroundWork")),
    runInsideTxWork          = (_: EagerTracingInterface)("TransactionWork"),
    commitTransaction        = traceLog(_.debug("commit")),
    rollbackTransaction      = traceLog(_.debug("rollback")),
    closeTransaction         = traceLog(_.debug("close")),
    runUnwind = traceStreamK("Transactor#unwind"),
    runGather = traceStreamK("Transactor#gather"),
    runQuery  = traceStreamK("Transactor#query"),
    readRecord = trace((span, _) => read => λ[F ~> F](_ <* span.traverse_(s => ce.delay(s.debug("Read value", "value" -> read.toString))))),
    reportSummary = (summary: ResultSummary) => activeSpan.flatMap(_.traverse_(s =>
                    ce.delay(s.debug(
                      "Result fully consumed.",
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
                    ))
                  ))
  )
}


