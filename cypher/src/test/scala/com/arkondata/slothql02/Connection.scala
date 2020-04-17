package com.arkondata.slothql02

import org.neo4j.driver.v1.{ AuthTokens, GraphDatabase }

object Connection {
  lazy val host = "localhost"
  lazy val port = 7687
  lazy val auth = AuthTokens.basic( "neo4j", "neo4j" )

  def driver = GraphDatabase.driver(s"bolt://$host:$port", auth)
}
