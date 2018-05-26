package com.abraxas.slothql.cypher

import scala.language.implicitConversions

import com.abraxas.slothql.cypher.CypherFragment.{ Expr, Return }

package object syntax {

  sealed trait Graph

  sealed trait GraphElem extends Expr.Var[Expr.MapExpr0] {
    def prop[A]: GraphElem.PropBuilder[A, this.type] = new GraphElem.PropBuilder[A, this.type](this)
  }
  sealed trait Vertex extends GraphElem
  sealed trait Edge   extends GraphElem

  protected[syntax] object Graph extends Graph {
    def apply(): Graph = this
  }
  protected[syntax] object Vertex {
    def apply(): Vertex = apply("")
    def apply(nme: String): Vertex = new Vertex {
      type Name = nme.type
      val name: nme.type = nme
    }
  }
  protected[syntax] object Edge {
    def apply(): Edge = apply("")
    def apply(nme: String): Edge = new Edge {
      type Name = nme.type
      val name: nme.type = nme
    }
  }

  object GraphElem {
    class PropBuilder[A, E <: GraphElem](elem: E) {
      def apply(k: String): Expr.Key.Aux[A, k.type, E] = Expr.Key[A](elem, k)
    }
  }


  object -> {
    def unapply(g: Graph): Option[(Vertex, Vertex)] = Some(Vertex() -> Vertex())
    def unapply(v: Vertex): Option[(Vertex, Vertex)] = Some(Vertex() -> Vertex())
  }
  object `<-` {
    def unapply(g: Graph): Option[(Vertex, Vertex)] = Some(Vertex() -> Vertex())
    def unapply(v: Vertex): Option[(Vertex, Vertex)] = Some(Vertex() -> Vertex())
  }

  case class -[A, B](a: A, b: B)
//  object - {
//    def unapply(g: Graph): Option[(Vertex - Edge, Vertex)] = ???
//    def unapply(v: Vertex): Option[(Vertex - Edge, Vertex)] = ???
//  }

  object > {
    def unapply(g: Graph): Option[(Vertex - Edge, Vertex)] = Some(new -(Vertex(), Edge()) -> Vertex())
    def unapply(v: Vertex): Option[(Vertex - Edge, Vertex)] = Some(new -(Vertex(), Edge()) -> Vertex())

    def unapply(path: Vertex - Edge): Option[(Vertex - Edge, Vertex - Edge)] = Some(new -(Vertex(), Edge()) -> new -(Vertex(), Edge()))
  }
  object < {
    def unapply(g: Graph): Option[(Vertex, Edge - Vertex)] = Some(Vertex() -> new -(Edge(), Vertex()))
    def unapply(v: Vertex): Option[(Vertex, Edge - Vertex)] = Some(Vertex() -> new -(Edge(), Vertex()))
  }

  object -- {
    def unapply(g: Graph): Option[(Vertex, Vertex)] = Some(Vertex() -> Vertex())
    def unapply(v: Vertex): Option[(Vertex, Vertex)] = Some(Vertex() -> Vertex())
  }


  lazy val ⟶ : ->.type = ->
  lazy val ⟵ : `<-`.type = `<-`
  lazy val ⟷ : --.type = --



  implicit def returnExpr[E <: Expr[_], R <: Return.Expr[_]](e: E)(implicit build: Return.Expr.Build.Aux[E, None.type, R]): R = build(e, None)
}
