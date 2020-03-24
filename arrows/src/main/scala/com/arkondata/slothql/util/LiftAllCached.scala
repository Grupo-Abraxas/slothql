package com.arkondata.slothql.util

import scala.language.higherKinds

import shapeless._

import com.arkondata.slothql.util

/**
 * Typeclass witnessing that all the elements of an HList have instances of the given typeclass.
 * Uses [[shapeless.Cached]].
 * Since [[shapeless.ops.hlist.LiftAll]] is `sealed` I have to create new typeclass instead of providing other instance.
 */

sealed trait LiftAllCachedHList[F[_], In <: HList] {
  type Out <: HList
  def instances: Out
}

object LiftAllCachedHList {
  type Aux[F[_], In0 <: HList, Out0 <: HList] = LiftAllCachedHList[F, In0] { type Out = Out0 }

  implicit def hnil[F[_]]: LiftAllCachedHList.Aux[F, HNil, HNil] = hnilInst.asInstanceOf[LiftAllCachedHList.Aux[F, HNil, HNil]]
  private lazy val hnilInst = new LiftAllCachedHList[cats.Id, HNil] {
    type Out = HNil
    def instances = HNil
  }

  implicit def hcons[F[_], H, T <: HList, TI <: HList](implicit
                                                       headInstance: Cached[F[H]],
                                                       tailInstances: LiftAllCachedHList.Aux[F, T, TI]
                                                      ): LiftAllCachedHList.Aux[F, H :: T, F[H] :: TI] =
    new LiftAllCachedHList[F, H :: T] {
      type Out = F[H] :: TI
      def instances = headInstance.value :: tailInstances.instances
    }

}


/**
 * Typeclass witnessing that all the constructor of a Coproduct have instances of the given typeclass.
 * Uses [[shapeless.Cached]].
 * Since [[shapeless.ops.coproduct.LiftAll]] is `sealed` I have to create new typeclass instead of providing other instance.
 */

sealed trait LiftAllCachedCoproduct[F[_], In <: Coproduct] {
  type Out <: HList
  def instances: Out
}

object LiftAllCachedCoproduct {
  type Aux[F[_], In0 <: Coproduct, Out0 <: HList] = LiftAllCachedCoproduct[F, In0] { type Out = Out0 }

  implicit def hnil[F[_]]: LiftAllCachedCoproduct.Aux[F, CNil, HNil] = hnilInst.asInstanceOf[LiftAllCachedCoproduct.Aux[F, CNil, HNil]]
  private lazy val hnilInst = new LiftAllCachedCoproduct[cats.Id, CNil] {
    type Out = HNil
    def instances = HNil
  }

  implicit def hcons[F[_], H, T <: Coproduct, TI <: HList](implicit
                                                           headInstance: Cached[F[H]],
                                                           tailInstances: LiftAllCachedCoproduct.Aux[F, T, TI]
                                                          ): LiftAllCachedCoproduct.Aux[F, H :+: T, F[H] :: TI] =
    new LiftAllCachedCoproduct[F, H :+: T] {
      type Out = F[H] :: TI
      def instances = headInstance.value :: tailInstances.instances
    }

}
