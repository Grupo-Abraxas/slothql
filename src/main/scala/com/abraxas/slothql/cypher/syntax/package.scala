package com.abraxas.slothql.cypher

import scala.language.implicitConversions

import shapeless.{ Generic, HList }

import com.abraxas.slothql.cypher.CypherFragment.Expr.MapExpr0
import com.abraxas.slothql.cypher.CypherFragment.{ Expr, Return }

package object syntax {

  sealed trait Graph

  sealed trait GraphElem {
    private[syntax] var _alias: String = _ // This `var` should be set only once by a macro
    def alias: String = _alias

    def prop[A]: GraphElem.PropBuilder[A, this.type] = new GraphElem.PropBuilder[A, this.type](this)
    def propOpt[A]: GraphElem.PropBuilder[Option[A], this.type] = new GraphElem.PropBuilder[Option[A], this.type](this)
  }
  sealed trait Vertex extends GraphElem
  sealed trait Edge   extends GraphElem

  private[syntax] object Graph extends Graph

  object Vertex {
    @inline private[syntax] def apply(): Vertex = new Vertex {}
    def unapplySeq(v: Vertex): Option[Seq[AnyRef]] = Some(???)
  }
  object Edge {
    @inline private[syntax] def apply(): Edge = new Edge {}
    def unapplySeq(v: Edge): Option[Seq[AnyRef]] = Some(???)
  }

  object GraphElem {
    class PropBuilder[A, E <: GraphElem](elem: E) {
      def apply(k: String)(implicit m: Manifest[A]): Expr.Key[A] =
        Expr.Key[A](Expr.Var[MapExpr0](elem.alias), k)
    }
  }

  object := {
    def unapply(arg: Any): Option[(String, Any)] = Some(???)
  }


  // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // //


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


  implicit def makeLit[A: Manifest](a: A): Expr.Lit[A] = Expr.Lit[A](a)


  implicit def returnExpr[A, E <: Expr[_]](e: E)(implicit ev: E <:< Expr[A], fragment: CypherFragment[E]): Return.Expr[A] =
    Return.Expr(CypherFragment.Known(e).widen, as = None)

  implicit def returnTuple[P <: Product, L <: HList](p: P)(
    implicit gen: Generic.Aux[P, L], build: Return.List.Build[L]
  ): build.Out = build(gen.to(p))

}
