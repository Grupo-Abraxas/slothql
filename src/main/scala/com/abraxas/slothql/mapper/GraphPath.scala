package com.abraxas.slothql.mapper


sealed trait GraphPath extends Arrow {
  type Source <: GraphRepr
  type Target <: GraphRepr
}

object GraphPath {
  case class Initial[N <: GraphRepr.Node](node: N)
    extends GraphPath { type Source = N; type Target = N }
  case class InitialUnique[N <: GraphRepr.Node with GraphRepr.Identifiable.Aux[Id], Id](node: N, id: Id)
    extends GraphPath { type Source = N; type Target = N }

  case class PropSelection[E <: GraphRepr.Element, P <: GraphRepr.Property](elem: E, prop: P)
    extends GraphPath { type Source = E; type Target = P }

  sealed trait NodeRelationArrow[N <: GraphRepr.Node, R <: GraphRepr.Relation] extends GraphPath {
    type Source = N
    type Target = R

    val node: N
    val relation: R
  }
  case class OutgoingRelation[N <: GraphRepr.Node, R <: GraphRepr.Relation](node: N, relation: R) extends NodeRelationArrow[N, R]
  case class IncomingRelation[N <: GraphRepr.Node, R <: GraphRepr.Relation](node: N, relation: R) extends NodeRelationArrow[N, R]

  sealed trait RelationArrow[R <: GraphRepr.Relation, N <: GraphRepr.Node] extends GraphPath {
    type Source = R
    type Target = N

    val relation: R
    val node: N
  }
  case class RelationSource[R <: GraphRepr.Relation, N <: GraphRepr.Node](relation: R, node: N) extends RelationArrow[R, N]
  case class RelationTarget[R <: GraphRepr.Relation, N <: GraphRepr.Node](relation: R, node: N) extends RelationArrow[R, N]

}