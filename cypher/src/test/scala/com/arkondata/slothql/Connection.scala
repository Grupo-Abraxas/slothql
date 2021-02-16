package com.arkondata.slothql

import org.neo4j.driver.{ AuthTokens, GraphDatabase }

object Connection {
  lazy val host = sys.env.getOrElse("NEO4J_HOST", "neo4j")
  lazy val port = 7687

  private val pattern                     = "(.+)/(.+)".r
  private val pattern(username, password) = sys.env.getOrElse("NEO4J_AUTH", "neo4j/test123")

  lazy val auth = AuthTokens.basic(username, password)

  def driver = GraphDatabase.driver(s"bolt://$host:$port", auth)
}
