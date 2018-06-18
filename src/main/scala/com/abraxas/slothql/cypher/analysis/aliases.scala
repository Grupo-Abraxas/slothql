package com.abraxas.slothql.cypher.analysis

import scala.annotation.implicitNotFound

import shapeless._
import shapeless.labelled.FieldType

import com.abraxas.slothql.cypher.CypherFragment
import com.abraxas.slothql.cypher.CypherFragment.{ Clause, Pattern, PatternTuple, Query }

@implicitNotFound("Alias \"${A}\" was not found in ${T}")
trait HasAlias[T, A <: String] {
  type Aliased <: Pattern
}

object HasAlias {
  type Aux[T, A <: String, Aliased0 <: Pattern] = HasAlias[T, A] { type Aliased = Aliased0 }
  def apply[T](a: String, t: T)(implicit has: HasAlias[T, a.type]): HasAlias.Aux[T, a.type, has.Aliased] = has

  implicit def impl[T, A <: String, Aliases <: HList, Aliased0 <: Pattern](
    implicit
    aliases: AllAliases.Aux[T, Aliases],
    aliasesAreDistinct: DistinctKeysConstraint[Aliases],
    select: ops.record.Selector.Aux[Aliases, A, Aliased0]
  ): Aux[T, A, Aliased0] = instance.asInstanceOf[Aux[T, A, Aliased0]]

  private lazy val instance = new HasAlias[Any, String] { type Aliased = Pattern }
}


@implicitNotFound("No aliases can be defined for ${T}")
trait AllAliases[-T] {
  type Aliases <: HList
}

object AllAliases {
  type Aux[-T, Aliases0 <: HList] = AllAliases[T] { type Aliases = Aliases0 }
  def apply[T](implicit all: AllAliases[T]): AllAliases.Aux[T, all.Aliases] = all
  def apply[T](t: T)(implicit all: AllAliases[T]): AllAliases.Aux[T, all.Aliases] = all

  implicit def nodeWithAlias[A <: String, N <: Pattern.Node](implicit ev: N <:< Pattern.Node.Aux[Some[A], _, _]): AllAliases.Aux[N, FieldType[A, N] :: HNil] = instance
  implicit def nodeWithoutAlias[N <: Pattern.Node](implicit ev: N <:< Pattern.Node.Aux[None.type, _, _]): AllAliases.Aux[N, HNil] = instance

  implicit def relWithAlias[A <: String, R <: Pattern.Rel](implicit ev: R <:< Pattern.Rel.Aux[Some[A], _, _, _, _]): AllAliases.Aux[R, FieldType[A, R] :: HNil] = instance
  implicit def relWithoutAlias[R <: Pattern.Rel](implicit ev: R <:< Pattern.Rel.Aux[None.type, _, _, _, _]): AllAliases.Aux[R, HNil] = instance

  implicit def path[H <: Pattern.Node, R <: Pattern.Rel, T <: Pattern.Pattern0, HA <: HList, RA <: HList, TA <: HList, Concat0 <: HList](
    implicit
    hAliases: AllAliases.Aux[H, HA], rAliases: AllAliases.Aux[R, RA], tAliases: AllAliases.Aux[T, TA],
    concat0: ops.hlist.Prepend.Aux[HA, RA, Concat0], concat1: ops.hlist.Prepend[Concat0, TA]
  ): AllAliases.Aux[Pattern.Path[H, R, T], concat1.Out] = instance

  implicit def patternsHNil: AllAliases.Aux[PatternTuple.Aux[HNil], HNil] = instance
  implicit def patternsHCons[Ps <: HList, H, T <: HList, HA <: HList, TA <: HList](
    implicit
    hcons: ops.hlist.IsHCons.Aux[Ps, H, T],
    hAliases: AllAliases.Aux[H, HA],
    tAliases: AllAliases.Aux[PatternTuple.Aux[T], TA],
    concat: ops.hlist.Prepend[HA, TA]
  ): AllAliases.Aux[PatternTuple.Aux[Ps], concat.Out] = instance

  implicit def matchPattern[PT <: PatternTuple, M](
    implicit ev: M <:< Clause.Match.Aux[PT, _, _], aliases: AllAliases[PT] // TODO: may there be aliases en `where`?
  ): AllAliases.Aux[M, aliases.Aliases] = instance

  implicit def queryClause[A, Clause0 <: CypherFragment.Clause, Q <: CypherFragment.Query.Query0[A]](
    implicit clauseAliases: AllAliases[Clause0] // TODO: qAliases: AllAliases[Q]
  ): AllAliases.Aux[Query.Clause.Aux[A, Clause0, Q], clauseAliases.Aliases] = instance

  private def instance[T, Aliases0 <: HList]: AllAliases.Aux[T, Aliases0] = _instance.asInstanceOf[AllAliases.Aux[T, Aliases0]]
  private lazy val _instance = new AllAliases[Any]{ type Aliases = HList }
}
