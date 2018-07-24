package com.abraxas.slothql.mapper

import shapeless.{ ops, HList }
import shapeless.tag.@@

object MapScalaExprToGraphPath {

  implicit def mapScalaExprIdToGraphPath[A, N <: GraphRepr.Node](
    implicit
    schema: Schema.Aux[A, N]
  ): Functor.Aux[ScalaExpr.Id[A], GraphPath, GraphPath.Initial[N]] =
    Functor.define[ScalaExpr.Id[A], GraphPath](_ => GraphPath.Initial(schema.repr))

  implicit def mapScalaExprSelectFieldToGraphNodePath[
    A, K <: String, V, NA <: GraphRepr.Node, Outgoing <: HList, NV <: GraphRepr.Node
  ](
    implicit
    schemaA: Schema.Aux[A, NA],
    nodeA: NA <:< GraphRepr.Node.Aux[_, _, Outgoing],
    selectV: ops.record.Selector.Aux[Outgoing, Symbol @@ K, NV],
    compose: Arrow.Compose[
      GraphPath.RelationTarget[GraphRepr.Relation.Empty[K, NA, NV], NV],
      GraphPath.OutgoingRelation[NA, GraphRepr.Relation.Empty[K, NA, NV]]
      ]
  ): Functor.Aux[ScalaExpr.SelectField[A, K, V], GraphPath, compose.Out] =
    Functor.define[ScalaExpr.SelectField[A, K, V], GraphPath]{ t =>
      val reprV = selectV(nodeA(schemaA.repr).Outgoing)
      val rel = GraphRepr.Relation.Empty[K, NA, NV](t.field, schemaA.repr, reprV)
      compose(
        GraphPath.RelationTarget(rel, reprV),
        GraphPath.OutgoingRelation(schemaA.repr, rel)
      )
    }

  implicit def mapScalaExprSelectFieldToGraphRelationPath[
    A, K <: String, V, N <: GraphRepr.Node, Outgoing <: HList, R <: GraphRepr.Relation
  ](
    implicit
    schemaA: Schema.Aux[A, N],
    nodeA: N <:< GraphRepr.Node.Aux[_, _, Outgoing],
    selectR: ops.record.Selector.Aux[Outgoing, Symbol @@ K, R]
  ): Functor.Aux[ScalaExpr.SelectField[A, K, V], GraphPath, GraphPath.OutgoingRelation[N, R]] =
    Functor.define[ScalaExpr.SelectField[A, K, V], GraphPath](_ =>
      GraphPath.OutgoingRelation(schemaA.repr, selectR(nodeA(schemaA.repr).Outgoing))
    )

  implicit def mapScalaExprSelectFieldToGraphPropertyPath[
    A, K <: String, V, N <: GraphRepr.Node, Props <: HList, P <: GraphRepr.Property
  ](
    implicit
    schemaA: Schema.Aux[A, N],
    nodeA: N <:< GraphRepr.Node.Aux[_, Props, _],
    selectP: ops.record.Selector.Aux[Props, Symbol @@ K, P]
  ): Functor.Aux[ScalaExpr.SelectField[A, K, V], GraphPath, GraphPath.PropSelection[N, P]] =
    Functor.define[ScalaExpr.SelectField[A, K, V], GraphPath](sel =>
      GraphPath.PropSelection(schemaA.repr, sel.field, selectP(nodeA(schemaA.repr).Fields))
    )


//  implicit def mapScalaExpr?ToGraphPath(
//    implicit
//    dummyImplicit: DummyImplicit
//  ): Functor.Aux[ScalaExpr.?, GraphPath, GraphPath.?] = ???

}
