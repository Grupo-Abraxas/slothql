package com.arkondata.slothql.mapper

import shapeless.HList

sealed trait ScalaType

object ScalaType {
  sealed trait Atomic extends ScalaType {
    type T
  }
  object Atomic {
    type Aux[T0] = Atomic { type T = T0 }
  }

  sealed trait Product extends ScalaType {
    type Name <: String
    type Fields <: HList
  }
  object Product {
    type Aux[Nme <: String, Fs <: HList] = Product{ type Name = Nme; type Fields = Fs }
  }

  sealed trait Coproduct extends ScalaType {
    type Name <: String
    type Alternatives <: HList
  }
  object Coproduct {
    type Aux[Nme <: String, Alts <: HList] = Coproduct{ type Name = Nme; type Alternatives = Alts }
  }

  sealed trait Optional[T <: ScalaType] extends ScalaType

//  sealed trait AtomicOpt extends Optional[Atomic] with Atomic
//  object AtomicOpt {
//    type Aux[T0] = AtomicOpt{ type T = T0 }
//  }
//
//  sealed trait ProductOpt extends Optional[Product] with Product
//  object ProductOpt {
//    type Aux[Nme <: String, Fs <: HList] = ProductOpt{ type Name = Nme; type Fields = Fs }
//  }
//
//  sealed trait CoproductOpt extends Optional[Coproduct] with Coproduct
//  object CoproductOpt {
//    type Aux[Nme <: String, Alts <: HList] = CoproductOpt{ type Name = Nme; type Alternatives = Alts }
//  }

//  sealed trait Atomic[T] extends ScalaType
//  sealed trait Product[Name <: String, Fields <: HList] extends ScalaType
//  sealed trait Coproduct[Name <: String, Alternatives <: HList] extends ScalaType
//
//  sealed trait Optional[T <: ScalaType] extends ScalaType
//  sealed trait AtomicOpt[T] extends Optional[Atomic[T]]
//  sealed trait ProductOpt[Name <: String, Fields <: HList] extends Optional[Product[Name, Fields]]
//  sealed trait CoproductOpt[Name <: String, Alternatives <: HList] extends Optional[Coproduct[Name, Alternatives]]

//  case class Atomic(clazz: Class[_]) extends ScalaType
//  case class Product(name: String, fields: ???) extends ScalaType
//  case class Coproduct(name: String, alternatives: ???) extends ScalaType
//  case class Optional(tpe: ScalaType) extends ScalaType

  type ScalaTypeCat = ScalaTypeCat.type
  implicit object ScalaTypeCat extends Cat {
    type Obj = ScalaType
    type Arr = ScalaExpr

    type Id[A <: ScalaType] = ScalaExpr.Id[A]
    type Compose[F <: ScalaExpr, G <: ScalaExpr] = ScalaExpr.Compose[F, G]

    def id[A <: ScalaType]: Id[A] = new ScalaExpr.Id[A]
    protected def unsafeCompose[F <: ScalaExpr, G <: ScalaExpr](f: F, g: G): F ∘ G = ScalaExpr.Compose(f, g)
  }
}

trait ScalaExpr {
  type Src <: ScalaType
  type Tgt <: ScalaType
}
object ScalaExpr {
  import ScalaType._

  type Aux[-T <: ScalaType, +R <: ScalaType] = ScalaExpr { type Src >: T; type Tgt <: R }

  final class Id[A <: ScalaType] extends ScalaExpr { type Src = A; type Tgt = A }
  final case class Compose[F <: ScalaExpr, G <: ScalaExpr](f: F, g: G) extends ScalaExpr {
    type Src = G#Src
    type Tgt = F#Tgt
  }

  sealed trait FieldSelection[P <: Product, K <: String, R <: ScalaType] extends ScalaExpr { type Src = P; type Tgt = R }
  sealed trait FieldSelectionExt[P <: Product, K <: String, R <: ScalaType, Ext] extends ScalaExpr { type Src = P; type Tgt = R }
  sealed trait AlternativeSelection[C <: Coproduct, K <: String, R <: Product] extends ScalaExpr { type Src = C; type Tgt = Optional[R] }
  sealed trait MapOptional[A <: ScalaType, B <: ScalaType, M <: ScalaExpr.Aux[A, B]] extends ScalaExpr { type Src = Optional[A]; type Tgt = Optional[B] }
}
