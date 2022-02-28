package slothbie

import scala.language.implicitConversions

import com.arkondata.slothql.cypher.CypherStatement.LiftValue

case class SingleFragment[A](value: A, lift: LiftValue[A]) {
  def toParam: AnyRef = lift.asParam(value)
}

case object SingleFragment {

  implicit def fromLiftValue[A](value: A)(implicit lift: LiftValue[A]): SingleFragment[A] =
    SingleFragment[A](value, lift)
}
