package com.arkondata.slothql.apoc

import org.neo4j.driver.exceptions.ClientException

class ApocException(message: String) extends Exception(message)

object ApocException {

  def adapt: PartialFunction[Throwable, ApocException] = {
    case ce: ClientException if ce.getMessage matches regex =>
      new ApocException(regex.r.findFirstMatchIn(ce.getMessage).get.group(1))
  }

  def unapply(err: Throwable): Option[ApocException] = PartialFunction.condOpt(err)(adapt)

  private val regex = """^Failed to invoke procedure `apoc.*`: Caused by: java\.lang\.RuntimeException: (.*)$"""
}
