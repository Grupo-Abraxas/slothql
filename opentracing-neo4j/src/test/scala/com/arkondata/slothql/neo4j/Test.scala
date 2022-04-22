package com.arkondata.slothql.neo4j

import scala.concurrent.duration.DurationInt

import cats.effect.kernel.Async
import cats.effect.std.Dispatcher
import org.neo4j.driver.Driver

import com.arkondata.opentracing.util.TraceBundle
import com.arkondata.slothql.neo4j.Neo4jCypherTransactor

@TransactorTracing
class Test[F[_]](driver: Driver)(implicit
  tracingBundle: TraceBundle.Endo[F],
  ce: Async[F],
  dispatcher: Dispatcher[F]
) extends Neo4jCypherTransactor[F](driver, defaultTimeout = 10.seconds) {

  import TransactorTracing._
  val helper = new TracingHelper[F]
  import helper._

  def transactorTracingSetup = TransactorTracing.setup[F](
    runRead  = traceStreamK("Transactor#runRead"),
    runWrite = traceStreamK("Transactor#runWrite"),
    apply = traceStreamK("Transactor#apply") andThen
      logStreamK((log, e) => log.debug("Next stream element", "element" -> e))
  )
}
