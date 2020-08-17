package com.arkondata.slothql.mapper

import shapeless._
import shapeless.labelled.FieldType
import shapeless.tag.@@

object Test {

  type ->>[K <: String, V] = FieldType[Symbol @@ K, V]

  type A1 = ScalaType.Atomic.Aux[Int]
  type A2 = ScalaType.Atomic.Aux[String]

  type P1 = ScalaType.Product.Aux["P1", ("x" ->> A1) :: ("y" ->> A2) :: HNil]
  type P2 = ScalaType.Product.Aux["P2", ("z" ->> A1) :: ("p" ->> P1) :: HNil]

  type EP1x = ScalaExpr.FieldSelection[P1, "x", A1]
  type EP1y = ScalaExpr.FieldSelection[P1, "y", A2]
  type EP2z = ScalaExpr.FieldSelection[P2, "z", A1]
  type EP2p = ScalaExpr.FieldSelection[P2, "p", P1]

  type EP2px = ScalaExpr.Compose[EP1x, EP2p]
  type EP2py = ScalaExpr.Compose[EP1y, EP2p]

  val mapA1 = ScalaTypeToGraphFunctor.MapObject[A1]
  val mapA2 = ScalaTypeToGraphFunctor.MapObject[A2]
  val mapP1 = ScalaTypeToGraphFunctor.MapObject[P1]
  val mapP2 = ScalaTypeToGraphFunctor.MapObject[P2]

  val mapEP1x = ScalaTypeToGraphFunctor.MapArrow[EP1x]
  val mapEP1y = ScalaTypeToGraphFunctor.MapArrow[EP1y]
  val mapEP2z = ScalaTypeToGraphFunctor.MapArrow[EP2z]
  val mapEP2p = ScalaTypeToGraphFunctor.MapArrow[EP2p]

  // TODO: support composition
  // val mapEP2px = ScalaTypeToGraphFunctor.MapArrow[EP2px]
  // val mapEP2py = ScalaTypeToGraphFunctor.MapArrow[EP2py]

}
