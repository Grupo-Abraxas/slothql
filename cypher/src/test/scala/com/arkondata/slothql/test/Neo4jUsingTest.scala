package com.arkondata.slothql.test

import scala.concurrent.duration.DurationInt

import cats.effect.std.Dispatcher
import cats.effect.{ unsafe, IO }
import org.scalatest.{ BeforeAndAfterAll, Suite }

import com.arkondata.slothql.Connection
import com.arkondata.slothql.neo4j.Neo4jCypherTransactor

trait Neo4jUsingTest extends BeforeAndAfterAll {
  this: Suite =>

  implicit val runtime = unsafe.implicits.global

  private lazy val (dispatcher0, release) = Dispatcher[IO].allocated.unsafeRunSync()
  implicit val dispatcher                 = dispatcher0

  lazy val tx = new Neo4jCypherTransactor[IO](Connection.driver, 10.seconds)

  override protected def afterAll(): Unit = {
    release.unsafeRunSync()
    Connection.driver.close()
    super.afterAll()
  }

}
