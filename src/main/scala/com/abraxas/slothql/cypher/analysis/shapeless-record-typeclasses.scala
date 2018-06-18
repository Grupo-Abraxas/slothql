package com.abraxas.slothql.cypher.analysis

import shapeless._
import shapeless.labelled.FieldType


/**
 * Alternative implementation of [[ops.record.Values]].
 * Uses [[Values.FieldTypeInv]] type alias for selection of a concrete value type.
 * Some typeclasses in this project require this implementation of `Values`.
 */
trait Values[L <: HList] extends DepFn1[L] with Serializable { type Out <: HList }
object Values {
  def apply[L <: HList](implicit values: Values[L]): Aux[L, values.Out] = values

  type Aux[L <: HList, Out0 <: HList] = Values[L] { type Out = Out0 }

  implicit def hnilValues: Aux[HNil, HNil]  = _hnilValues
  private lazy val _hnilValues = new Values[HNil] {
    type Out = HNil
    def apply(t: HNil): HNil = HNil
  }

  // Because `FieldType` is covariant in its 2nd argument, `H <:< FieldType[_, V]` resolves `V` as `Any`.
  // This type alias allows to select concrete type (not sure why).
  type FieldTypeInv[K, V] = FieldType[K, V]

  implicit def hconsValues[H, V, T <: HList](
    implicit ev0: H <:< FieldTypeInv[_, V], next: Values[T]
  ): Aux[H :: T, V :: next.Out] =
    new Values[H :: T] {
      type Out = V :: next.Out
      def apply(t: H :: T): V :: next.Out = t.head :: next(t.tail)
    }
}


/**
 * ~Contravariant~ version of [[ops.record.Keys]].
 * The difference is that head element is `H <: FieldType[K, _]` instead of `FieldType[K, _]`.
 * The same can be achieved by copying the original implementation and changing trait declaration: `trait Keys[-L <: HList]`.
 * Some typeclasses in this project require this implementation of `Keys`.
 */
trait Keys[L <: HList] extends DepFn0 with Serializable { type Out <: HList }
object Keys {
  def apply[L <: HList](implicit keys: Keys[L]): Aux[L, keys.Out] = keys

  type Aux[L <: HList, Out0 <: HList] = Keys[L] { type Out = Out0 }

  implicit def hnilKeys: Aux[HNil, HNil] = _hnilKeys
  private lazy val _hnilKeys = new Keys[HNil] {
    type Out = HNil
    def apply(): HNil = HNil
  }

  implicit def hconsKeys[H, K, T <: HList](
    implicit ev: H <:< FieldType[K, _], w: Witness.Aux[K], next: Keys[T]
  ): Aux[H :: T, K :: next.Out] =
    new Keys[H :: T] {
      type Out = K :: next.Out
      def apply(): K :: next.Out = w.value :: next()
    }
}
