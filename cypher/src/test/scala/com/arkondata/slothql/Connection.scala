package com.arkondata.slothql

import org.neo4j.driver.{ AuthTokens, GraphDatabase }

object Connection {
  lazy val host = "localhost"
  lazy val port = 7687
  lazy val auth = AuthTokens.basic( "neo4j", "test" )

  def driver = GraphDatabase.driver(s"bolt://$host:$port", auth)
}
