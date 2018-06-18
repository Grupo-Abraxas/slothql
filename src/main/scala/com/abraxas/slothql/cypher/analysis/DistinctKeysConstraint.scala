package com.abraxas.slothql.cypher.analysis

import scala.annotation.implicitNotFound

import shapeless._

/**
 * Type class witnessing that every element of `L` is of the form `FieldType[K, V]` where all the `K` have distinct types.
 */
@implicitNotFound("Implicit not found: DistinctKeysConstraint[${L}]. Some keys have the same type.")
trait DistinctKeysConstraint[L <: HList] extends Serializable
object DistinctKeysConstraint {
  def apply[L <: HList](implicit c: DistinctKeysConstraint[L]): DistinctKeysConstraint[L] = c

  implicit def impl[L <: HList, K <: HList](
    implicit keys: Keys.Aux[L, K], distinct: IsDistinctConstraint[K]
  ): DistinctKeysConstraint[L] = instance

  private def instance[L <: HList]: DistinctKeysConstraint[L] = _instance.asInstanceOf[DistinctKeysConstraint[L]]
  private lazy val _instance = new DistinctKeysConstraint[HList] {}
}