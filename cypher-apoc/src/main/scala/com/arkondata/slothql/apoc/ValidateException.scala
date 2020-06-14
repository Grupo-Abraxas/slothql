package com.arkondata.slothql.apoc

import org.neo4j.driver.exceptions.ClientException

class ValidateException(message: String) extends Exception(message)

object ValidateException {

  def adapt: PartialFunction[Throwable, ValidateException] = {
    case ce: ClientException if ce.getMessage startsWith prefix => new ValidateException(ce.getMessage.drop(prefixLength))
  }

  def unapply(err: Throwable): Option[ValidateException] = PartialFunction.condOpt(err)(adapt)

  private val prefix = "Failed to invoke procedure `apoc.util.validate`: Caused by: java.lang.RuntimeException: "
  private val prefixLength = prefix.length
}
