package com.arkondata.slothql.mapper.util

object TypeBool {
  trait Or[A <: Boolean, B <: Boolean] {
    type Out <: Boolean
  }
  object Or extends OrLowPriorityImplicits {
    type Aux[A <: Boolean, B <: Boolean, R <: Boolean] = Or[A, B] { type Out = R }

    implicit lazy val orFalse: Or.Aux[false, false, false] = instance.asInstanceOf[Or.Aux[false, false, false]]

    protected[this] lazy val instance = new Or[Boolean, Boolean] {}
  }

  protected trait OrLowPriorityImplicits { this: Or.type =>

    implicit def orTrue[A <: Boolean, B <: Boolean]: Or.Aux[A, B, true] = instance.asInstanceOf[Or.Aux[A, B, true]]
  }
}
