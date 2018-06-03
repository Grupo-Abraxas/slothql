package com.abraxas.slothql.cypher

import scala.language.implicitConversions

import shapeless.{ <:!<, Generic, HList }

import com.abraxas.slothql.cypher.CypherFragment.{ Expr, Known, Return }

package object syntax {

  sealed trait Graph

  type GraphElem = Expr.Var[Map[String, Any]] with Graph.Elem
  type Vertex    = Expr.Var[Map[String, Any]] with Graph.Vertex
  type Edge      = Expr.Var[Map[String, Any]] with Graph.Edge

  object GraphElem {
    private[syntax] class Impl extends Expr.Var[Map[String, Any]] {
      private[syntax] var _alias: String = _ // This `var` should be set only once by a macro

      lazy val name: String = _alias
      lazy val m: Manifest[Map[String, Any]] = manifest[Map[String, Any]]
    }
  }

  final implicit class GraphElemOps(e: GraphElem) {
    /** Select vertex/edge property. */
    def prop[A: Manifest](k: String): Expr.Key[A] = Expr.Key[A](e, k)
    /** Select vertex/edge property as [[Option]]. */
    def propOpt[A: Manifest](k: String): Expr.Key[Option[A]] = prop[Option[A]](k)

    /** Alias for [[prop]]. */
    @deprecated("seems to break query type resolution", since = "03.06.18")
    def apply[A: Manifest](k: String): Expr.Key[A] = prop(k)
    /** Alias for [[propOpt]]. */
    def opt[A: Manifest](k: String): Expr.Key[Option[A]] = propOpt(k)

    /** Call built-in function `func` passing `this` expression as first argument. */
    def call[R: Manifest](func: String, args: Known[Expr[_]]*): Expr.Call[R] =
      Expr.Call(func, e.known :: args.toList)

    /** Call built-in `id` function. */
    def id: Expr.Call[Long] = call("id")
    /** Call built-in `count` function. */
    def count: Expr.Call[Long] = call("count")
    /** Call built-in `keys` function. */
    def keys: Expr.Call[List[String]] = call("keys")

  }

  final implicit class VertexOps(v: Vertex) {
    /** Call built-in `labels` function. */
    def labels: Expr.Call[List[String]] = v.call("labels")
  }

  final implicit class EdgeOps(e: Edge) {
    /** Call built-in `type` function. */
    def tpe: Expr.Call[String] = e.call("type")
    /** Call built-in `type` function. */
    def `type`: Expr.Call[String] = tpe
  }

  private[syntax] object Graph extends Graph {
    sealed trait Elem
    sealed trait Vertex extends Elem
    sealed trait Edge   extends Elem
  }

  object Vertex {
    @inline private[syntax] def apply(): Vertex = (new GraphElem.Impl).asInstanceOf[Vertex]
    def unapplySeq(v: Vertex): Option[Seq[AnyRef]] = Some(???)
  }
  object Edge {
    @inline private[syntax] def apply(): Edge = (new GraphElem.Impl).asInstanceOf[Edge]
    def unapplySeq(v: Edge): Option[Seq[AnyRef]] = Some(???)
  }


  object := {
    def unapply(arg: Any): Option[(String, Any)] = Some(???)
  }

  object *: {
    def unapply(edge: Edge): Option[(Expr.Var[List[Map[String, Any]]], -[Int, Int], Edge)] = Some(???)
  }

  // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // //

  implicit class CallExprOps(func: Symbol) {
    def call[R](args: Known[Expr[_]]*): Expr.Call[R] = Expr.Call(func.name, args.toList)
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
    Return.Expr(Known(e).widen, as = None)

  implicit def returnTuple[P <: Product, L <: HList](p: P)(
    implicit gen: Generic.Aux[P, L], build: Return.List.Build[L], ev: P <:!< Expr[_]
  ): build.Out = build(gen.to(p))

}
