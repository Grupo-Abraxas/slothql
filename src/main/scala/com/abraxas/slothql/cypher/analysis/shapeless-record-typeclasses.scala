package com.abraxas.slothql.cypher.analysis

import shapeless._
import shapeless.labelled.FieldType

import com.abraxas.slothql.util.Not


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


/**
 * Alternative implementation of [[ops.record.Renamer]].
 */
@annotation.implicitNotFound("Field ${K1} cannot be renamed to ${K2}. Either key ${K1} doesn't exist or ${K2} is already present in ${L}.")
trait Renamer[L <: HList, K1, K2] extends DepFn1[L] with Serializable { type Out <: HList }
object Renamer {
  type Aux[L <: HList, K1, K2, Out0 <: HList] = Renamer[L, K1, K2] { type Out = Out0 }
  def apply[L <: HList, K1, K2](implicit renamer: Renamer[L, K1, K2]): Aux[L, K1, K2, renamer.Out] = renamer

  implicit def hlistRenamer[L <: HList, K1, K2](
    implicit
    aliasesAreDistinct: DistinctKeysConstraint[L],
    newNameNotUsed: Not[ops.record.Selector[L, K2]],
    renamer: Renamer0[L, K1, K2]
  ): Aux[L, K1, K2, renamer.Out] =
    new Renamer[L, K1, K2] {
      type Out = renamer.Out
      def apply(t: L): renamer.Out = renamer(t)
    }

  @annotation.implicitNotFound("No field ${K1} in record ${L}")
  trait Renamer0[L <: HList, K1, K2] extends DepFn1[L] with Serializable { type Out <: HList }
  object Renamer0 {
    type Aux[L <: HList, K1, K2, Out0 <: HList] = Renamer0[L, K1, K2] { type Out = Out0 }

    implicit def hlistRenamer0This[H, T <: HList, V, K1, K2](implicit ev: H <:< FieldType[K1, V]): Aux[H :: T, K1, K2, FieldType[K2, V] :: T] =
      new Renamer0[H :: T, K1, K2] {
        type Out = FieldType[K2, V] :: T
        def apply(t: H :: T): FieldType[K2, V] :: T = labelled.field[K2](t.head: V) :: t.tail
      }
    implicit def hlistRenamer0Next[H, T <: HList, K1, K2](implicit renamer: Renamer0[T, K1, K2]): Aux[H :: T, K1, K2, H :: renamer.Out] =
      new Renamer0[H :: T, K1, K2] {
        type Out = H :: renamer.Out
        def apply(t: H :: T): ::[H, renamer.Out] = t.head :: renamer(t.tail)
      }
  }
}
