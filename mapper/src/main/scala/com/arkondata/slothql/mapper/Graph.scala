package com.arkondata.slothql.mapper

import shapeless._

sealed trait Graph {
  type Optional <: Boolean
}

object Graph {
  type HStringList = HList

  sealed trait Node extends Graph {
    type Labels <: HStringList
  }
  object Node {
    type Aux[Ls <: HStringList, Opt <: Boolean] = Node { type Labels = Ls; type Optional = Opt }
  }
  
  sealed trait Prop extends Graph {
    // TODO
    type Type <: Any
  }
  object Prop {
    type Aux[T <: Any, Opt <: Boolean] = Prop { type Type = T; type Optional = Opt }
  }
  
  sealed trait Rel extends Graph {
    type Type <: String
  }
  object Rel {
    type Aux[T <: String, Opt <: Boolean] = Rel { type Type = T; type Optional = Opt }
  }

  type GraphCat = GraphCat.type
  implicit object GraphCat extends Cat {
    type Obj = Graph

    sealed trait Arr {
      type Src <: Graph
      type Tgt <: Graph
    }

    /** Identity arrow. */
    final class Id[A <: Graph] extends Arr with Abstract.Id[A]

    /** Arrows composition. */
    final case class Compose[F <: Arr, G <: Arr](f: F, g: G) extends Arr with Abstract.Compose[F, G]

    /** Outgoing relation selection: {{{ ((Node:A) -[Rel:B]->) }}}. */
    sealed trait OutRel[N <: Node, R <: Rel] extends Arr { type Src = N;  type Tgt = R }

    /** Relationship's target selection: {{{ (-[Rel:A]-> (Node:B)) }}}. */
    sealed trait RelTgt[R <: Rel, N <: Node] extends Arr { type Src = R; type Tgt = N }
    
    /** Node property selection: {{{ ((Node:A) .> (Prop::B)) }}}. */
    sealed trait NodeProp[N <: Node, K <: String, P <: Prop] extends Arr { type Src = N; type Tgt = P }
    
    /** Relationship's property selection: {{{ ([Rel:A] .> (Prop::B)) }}} */
    sealed trait RelProp[R <: Rel, K <: String, P <: Prop] extends Arr { type Src = R; type Tgt = P }

    /** Relationship's property selection: {{{ ([Rel:A] .> (Prop::B)) }}} */
    sealed trait NodeLabel[N <: Node, L <: String] extends Arr { type Src = N; type Tgt = Node.Aux[L :: N#Labels, N#Optional] }
//    sealed trait NodeLabels[N <: Node, Ls <: HStringList] extends Arr { type Src = N; type Tgt = P }

    def id[A <: Graph]: Id[A] = new Id[A]
    protected def unsafeCompose[F <: Arr, G <: Arr](f: F, g: G): F ∘ G = Compose(f, g)
  }
}

