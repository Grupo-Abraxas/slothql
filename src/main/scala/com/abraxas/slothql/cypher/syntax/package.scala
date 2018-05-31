package com.abraxas.slothql.cypher

import scala.language.implicitConversions

import shapeless.{ <:!<, Generic, HList }

import com.abraxas.slothql.cypher.CypherFragment.{ Expr, Known, Return }

package object syntax {

  sealed trait Graph

  sealed trait GraphElem extends Expr.Var[Map[String, Any]] {
    private[syntax] var _alias: String = _ // This `var` should be set only once by a macro

    lazy val name: String = _alias
    lazy val m: Manifest[Map[String, Any]] = manifest[Map[String, Any]]

    /** Select vertex/edge property. */
    def prop[A: Manifest](k: String): Expr.Key[A] = Expr.Key[A](this, k)
    /** Select vertex/edge property as [[Option]]. */
    def propOpt[A: Manifest](k: String): Expr.Key[Option[A]] = prop[Option[A]](k)

    /** Alias for [[prop]]. */
    def apply[A: Manifest](k: String): Expr.Key[A] = prop(k)
    /** Alias for [[propOpt]]. */
    def opt[A: Manifest](k: String): Expr.Key[Option[A]] = propOpt(k)

    /** Call built-in function `func` passing `this` expression as first argument. */
    def call[R: Manifest](func: String, args: Known[Expr[_]]*): Expr.Call[R] =
      Expr.Call(func, this.known :: args.toList)

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


  object := {
    def unapply(arg: Any): Option[(String, Any)] = Some(???)
  }

  object ** {
    def unapply(arg: Any): Option[(Int, Int)] = Some(???)
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
