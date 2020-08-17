package com.arkondata.slothql.mapper

import shapeless._

import com.arkondata.slothql.mapper.util.TypeBool

object ScalaTypeToGraphFunctor extends Functor[ScalaType.ScalaTypeCat, Graph.GraphCat] {

  // // // // // // // // // // Map Objects // // // // // // // // // //

  sealed trait MapObject[O <: ScalaType] {
    type Out <: Graph
  }
  object MapObject {
    type Aux[O <: ScalaType, G <: Graph] = MapObject[O] { type Out = G }
    def apply[O <: ScalaType](implicit map: MapObject[O]): MapObject.Aux[O, map.Out] = map

    /**
     * Maps [[ScalaType.Atomic]] to [[Graph.Prop]].
     */
    implicit def mapAtomic[A <: ScalaType.Atomic, T](
      implicit ev: A <:< ScalaType.Atomic.Aux[T]
    ): MapObject.Aux[A, Graph.Prop.Aux[T, false]] =
      instance.asInstanceOf[MapObject.Aux[A, Graph.Prop.Aux[T, false]]]

    /**
     * Maps [[ScalaType.Product]] to [[Graph.Node]].
     */
    implicit def mapProduct[P <: ScalaType.Product, Name <: String](
      implicit name: P <:< ScalaType.Product.Aux[Name, _]
    ): MapObject.Aux[P, Graph.Node.Aux[Name :: HNil, false]] =
      instance.asInstanceOf[MapObject.Aux[P, Graph.Node.Aux[Name :: HNil, false]]]

    /**
     * Maps [[ScalaType.Coproduct]] to [[Graph.Node]].
     */
    implicit def mapCoproduct[C <: ScalaType.Coproduct, Name <: String](
      implicit name: C <:< ScalaType.Product.Aux[Name, _]
    ): MapObject.Aux[C, Graph.Node.Aux[Name :: HNil, false]] =
      instance.asInstanceOf[MapObject.Aux[C, Graph.Node.Aux[Name :: HNil, false]]]

    /**
     * Maps [[ScalaType.Optional]] [[ScalaType.Atomic]] to [[Graph.Prop]].
     */
    implicit def mapOptionalAtomic[A <: ScalaType.Atomic, P <: Graph.Prop, T](
        implicit
        map: MapObject.Aux[A, P],
        ev: P <:< Graph.Prop.Aux[T, _]
    ): MapObject.Aux[ScalaType.Optional[A], Graph.Prop.Aux[T, true]] =
      instance.asInstanceOf[MapObject.Aux[ScalaType.Optional[A], Graph.Prop.Aux[T, true]]]

    /**
     * Maps [[ScalaType.Optional]] [[ScalaType.Product]] to [[Graph.Node]].
     */
    implicit def mapOptionalProduct[P <: ScalaType.Product, N <: Graph.Node, Ls <: HList](
      implicit
      map: MapObject.Aux[P, N],
      ev: N <:< Graph.Node.Aux[Ls, _]
    ): MapObject.Aux[ScalaType.Optional[P], Graph.Node.Aux[Ls, true]] =
      instance.asInstanceOf[MapObject.Aux[ScalaType.Optional[P], Graph.Node.Aux[Ls, true]]]

    /**
     * Maps [[ScalaType.Optional]] [[ScalaType.Coproduct]] to [[Graph.Node]].
     */
    implicit def mapOptionalCoproduct[C <: ScalaType.Coproduct, N <: Graph.Node, Ls <: HList](
      implicit
      map: MapObject.Aux[C, N],
      ev: N <:< Graph.Node.Aux[Ls, _]
    ): MapObject.Aux[ScalaType.Optional[C], Graph.Node.Aux[Ls, true]] =
      instance.asInstanceOf[MapObject.Aux[ScalaType.Optional[C], Graph.Node.Aux[Ls, true]]]

    private lazy val instance = new MapObject[ScalaType] {}
  }

  // // // // // // // // // // Map Arrows // // // // // // // // // //

  sealed trait MapArrow[E <: ScalaExpr] {
    type Out <: Graph.GraphCat.Arr
  }

  object MapArrow {
    type Aux[E <: ScalaExpr, A <: Graph.GraphCat.Arr] = MapArrow[E] { type Out = A }

    def apply[E <: ScalaExpr](implicit map: MapArrow[E]): MapArrow.Aux[E, map.Out] = map

    /**
     * Maps [[ScalaExpr.FieldSelection]] of [[ScalaType.Atomic]] to [[Graph.GraphCat.NodeProp]].
     */
    implicit def mapFieldSelectionAtomic[
      P <: ScalaType.Product,
      K <: String,
      R <: ScalaType.Atomic,
      N <: Graph.Node,
      Pp <: Graph.Prop
    ](
      implicit
      mapP: MapObject.Aux[P, N],
      mapR: MapObject.Aux[R, Pp]
    ): MapArrow.Aux[ScalaExpr.FieldSelection[P, K, R], Graph.GraphCat.NodeProp[N, K, Pp]] =
      instance.asInstanceOf[MapArrow.Aux[ScalaExpr.FieldSelection[P, K, R], Graph.GraphCat.NodeProp[N, K, Pp]]]

    /**
     * Maps [[ScalaExpr.AlternativeSelection]] to [[Graph.GraphCat.NodeLabel]].
     */
    implicit def mapAlternativeSelection[C <: ScalaType.Coproduct, K <: String, R <: ScalaType.Product, N <: Graph.Node](
      implicit mapC: MapObject.Aux[C, N]
    ): MapArrow.Aux[ScalaExpr.AlternativeSelection[C, K, R], Graph.GraphCat.NodeLabel[N, C#Name]] =
      instance.asInstanceOf[MapArrow.Aux[ScalaExpr.AlternativeSelection[C, K, R], Graph.GraphCat.NodeLabel[N, C#Name]]]

    /**
     * Maps [[ScalaExpr.FieldSelection]] of [[ScalaType.Product]] to [[Graph.GraphCat.RelTgt]] {{{∘}}} [[Graph.GraphCat.OutRel]].
     */
    implicit def mapFieldSelectionProduct[
      P0 <: ScalaType.Product,
      K  <: String,
      P1 <: ScalaType.Product,
      N1 <: Graph.Node,
      N2 <: Graph.Node
    ](
       implicit
       mapP: MapObject.Aux[P0, N1],
       mapR: MapObject.Aux[P1, N2],
       isOpt: TypeBool.Or[N1#Optional, N2#Optional]
    ): MapArrow.Aux[
        ScalaExpr.FieldSelection[P0, K, P1],
        Graph.GraphCat.Compose[
          Graph.GraphCat.RelTgt[Graph.Rel.Aux[K, isOpt.Out], N2],
          Graph.GraphCat.OutRel[N1, Graph.Rel.Aux[K, isOpt.Out]]
        ]
      ] =
      instance.asInstanceOf[
        MapArrow.Aux[
          ScalaExpr.FieldSelection[P0, K, P1],
          Graph.GraphCat.Compose[
            Graph.GraphCat.RelTgt[Graph.Rel.Aux[K, isOpt.Out], N2],
            Graph.GraphCat.OutRel[N1, Graph.Rel.Aux[K, isOpt.Out]]
          ]
        ]
      ]

    private lazy val instance = new MapArrow[ScalaExpr] {}
  }
}
