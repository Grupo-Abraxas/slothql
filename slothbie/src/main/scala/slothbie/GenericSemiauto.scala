package slothbie

import scala.reflect.runtime.{ universe => u }

import shapeless._
import shapeless.labelled.FieldType
import shapeless.ops.record.{ Keys, ToMap }

import com.arkondata.slothql.cypher.CypherStatement.{ LiftValue, LiftedValue }
import com.arkondata.slothql.cypher.syntax.lit

object GenericSemiauto {

  trait GenericRepr[T] {
    def apply: LiftValue[T]
  }

  object GenericRepr {
    def apply[A](implicit genR: GenericRepr[A]): GenericRepr[A] = genR

    implicit def genericCaseClassRepr[A <: Product, H <: HList](implicit
      gen: LabelledGeneric.Aux[A, H],
      lifter: LiftValue[Map[String, LiftedValue]],
      toMap: LiftMapEntry[H]
    ): GenericRepr[A] = new GenericRepr[A] {
      LiftValue.liftStringMapLiftedValues

      override def apply: LiftValue[A] = new LiftValue[A] {

        override def asParam(a: A): AnyRef =
          lifter.asParam(toMap(gen.to(a)))

        override def asLiteral(a: A): String =
          lifter.asLiteral(toMap(gen.to(a)))
      }
    }

  }

  trait LiftMapEntry[A] {
    def apply(a: A): Map[String, LiftedValue]
  }

  implicit def lifterHList[K <: Symbol, V: LiftValue, L <: HList](implicit
    witness: Witness.Aux[K],
    lifter: LiftMapEntry[L]
  ): LiftMapEntry[FieldType[K, V] :: L] =
    new LiftMapEntry[FieldType[K, V] :: L] {
      private lazy val name: String = witness.value.name

      override def apply(a: FieldType[K, V] :: L): Map[String, LiftedValue] = Map(
        `name` -> lit(a.head.asInstanceOf[V])
      ) ++ lifter(a.tail)
    }

  implicit def lifterOneAndHNil[K <: Symbol, V: LiftValue](implicit
    witness: Witness.Aux[K]
  ): LiftMapEntry[FieldType[K, V] :: HNil] =
    new LiftMapEntry[FieldType[K, V] :: HNil] {
      private lazy val name: String = witness.value.name

      override def apply(a: FieldType[K, V] :: HNil): Map[String, LiftedValue] = Map(
        `name` -> lit(a.head.asInstanceOf[V])
      )
    }

}
