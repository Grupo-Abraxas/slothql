package com.arkondata.slothql.apoc

import org.neo4j.driver.exceptions.ClientException

class ApocException(message: String) extends Exception(message)

object ApocException {

  def adapt: PartialFunction[Throwable, ApocException] =
    readapt{ case err => new ApocException(err) }

  def readapt[E <: Exception](pf : PartialFunction[String, E]): PartialFunction[Throwable, E] = {
    val readapt0: PartialFunction[Throwable, String] = {
      case ce: ClientException if ce.getMessage matches regex =>
        regex.r.findFirstMatchIn(ce.getMessage).get.group(1)
    }
    readapt0 andThen pf
  }

  def unapply(err: Throwable): Option[ApocException] = PartialFunction.condOpt(err)(adapt)

  private val regex = """^Failed to invoke procedure `apoc.*`: Caused by: java\.lang\.RuntimeException: (.*)$"""
}
