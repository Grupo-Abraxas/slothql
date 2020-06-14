package com.arkondata.slothql.test

import scala.concurrent.ExecutionContext

import cats.effect.IO
import org.scalatest.{ BeforeAndAfterAll, Suite }

import com.arkondata.slothql.Connection
import com.arkondata.slothql.neo4j.Neo4jCypherTransactor

trait Neo4jUsingTest extends BeforeAndAfterAll {
  this: Suite =>

  implicit val cs = IO.contextShift(ExecutionContext.global)

  lazy val tx = Neo4jCypherTransactor[IO](Connection.driver)

  override protected def afterAll(): Unit = {
    Connection.driver.close()
    super.afterAll()
  }

}
