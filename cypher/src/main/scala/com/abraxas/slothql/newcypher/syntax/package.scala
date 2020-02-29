package com.abraxas.slothql.newcypher

import scala.annotation.compileTimeOnly
import scala.language.experimental.macros

import com.abraxas.slothql.newcypher.{ CypherFragment => CF }

package object syntax {

  object Match {
    def apply[R]              (query: Node => CF.Query[R]): CF.Query[R] = macro CypherSyntaxMacros.match_[R]
//  def optional[R]           (query: Node => CF.Query[R]): CF.Query[R] = macro CypherSyntaxMacros.optional[R]
//  def maybe[R](opt: Boolean)(query: Node => CF.Query[R]): CF.Query[R] = macro CypherSyntaxMacros.maybe[R]
  }

  object With {
    // def apply[R](query: Node => CF.Query[R]): CF.Query[R] = macro CypherSyntaxMacros.with_[R]
  }

  object Unwind {
    // def apply[R](query: Node => CF.Query[R]): CF.Query[R] = macro CypherSyntaxMacros.unwind[R]
  }

  object Call {
    // def apply[R](query: Node => CF.Query[R]): CF.Query[R] = macro CypherSyntaxMacros.call[R]
  }

  object Create {
    // def apply[R](query: Node => CF.Query[R]): CF.Query[R] = macro CypherSyntaxMacros.create[R]
  }

  object Merge {
    // def apply[R](query: Node => CF.Query[R]): CF.Query[R] = macro CypherSyntaxMacros.merge[R]
  }

  object Delete {
    // def apply[R](query: Node => CF.Query[R]): CF.Query[R] = macro CypherSyntaxMacros.delete[R]
  }

  object Foreach {
    // def apply[R](query: Node => CF.Query[R]): CF.Query[R] = macro CypherSyntaxMacros.foreach[R]
  }

  sealed trait Node { val alias: String }
  sealed trait Rel  { val alias: String; type Dir <: Rel.Direction }

  object CypherSyntaxFromMacro {
    def mkNode(name: String): Node                          = new Node { val alias = name }
    def mkRel[D <: Rel.Direction](name: String): Rel.Aux[D] = new Rel  { val alias = name; type Dir = D }
  }

  // // // // // //
  // N = Node
  // R = Rel
  // N   => N < RN
  // NR  => N < RNX
  // N   => NR > N
  // NR  => NR > NR
  // NR  => N - R
  // RN  => R - N
  // RNX => RN - R
  // RNX => RN

  protected object CypherSyntaxInternal {
    sealed trait NR {
      type Dir <: Rel.Direction
    }
    sealed trait NRI extends NR { type Dir = Rel.Incoming }
    sealed trait NRO extends NR { type Dir = Rel.Outgoing }

    sealed trait RN{
      type Dir <: Rel.Direction
    }
    sealed trait RNI extends RN { type Dir = Rel.Incoming }
    sealed trait RNO extends RN { type Dir = Rel.Outgoing }

    sealed trait RNX extends RN {
      type Dir = DirRight
      type DirLeft  <: Rel.Direction
      type DirRight <: Rel.Direction
    }
    sealed trait RNXII { type DirLeft = Rel.Incoming; type DirRight = Rel.Incoming }
    sealed trait RNXIO { type DirLeft = Rel.Incoming; type DirRight = Rel.Outgoing }
//    sealed trait RNXOI { type DirLeft = Rel.Outgoing; type DirRight = Rel.Incoming }
//    sealed trait RNXOO { type DirLeft = Rel.Outgoing; type DirRight = Rel.Outgoing }
  }
  import CypherSyntaxInternal._

  @compileTimeOnly("Con only be used inside `Match` / `Create`")
  object < {
    def unapply(n: Node): Option[(Node, RNI)] = ???
    def unapply(n: NRI): Option[(Node, RNXII)] = ???
    def unapply(n: NRO): Option[(Node, RNXIO)] = ???
  }

  @compileTimeOnly("Con only be used inside `Match` / `Create`")
  object > {
    def unapply(n: Node): Option[(NRO, Node)] = ???
    def unapply(n: NRI): Option[(NRO, NRI)] = ???
    def unapply(n: NRO): Option[(NRO, NRO)] = ???
  }

  @compileTimeOnly("Con only be used inside `Match` / `Create`")
  object - {
    def unapply(r: NRI): Option[(Node, Rel.Aux[Rel.Incoming])] = ???
    def unapply(r: NRO): Option[(Node, Rel.Aux[Rel.Outgoing])] = ???
    def unapply(l: RNI): Option[(Rel.Aux[Rel.Incoming], Node)] = ???
    def unapply(l: RNO): Option[(Rel.Aux[Rel.Outgoing], Node)] = ???
    def unapply(l: RNXII): Option[(RNI, Rel.Aux[Rel.Incoming])] = ???
    def unapply(l: RNXIO): Option[(RNI, Rel.Aux[Rel.Outgoing])] = ???
//    def unapply(l: RNXOI): Option[(RNO, Rel[Rel.Incoming])] = ???
//    def unapply(l: RNXOO): Option[(RNO, Rel[Rel.Outgoing])] = ???
  }

  @compileTimeOnly("Con only be used inside `Match` / `Create`")
  object Node {
    /** Supported params: {{{String}}}, {{{Iterable[String]}}}, {{{:=[_]}}}. */
    def unapplySeq(n: Node): Option[Seq[Any]] = ???
  }
  @compileTimeOnly("Con only be used inside `Match` / `Create`")
  object Rel {
    type Aux[D <: Rel.Direction]  = Rel { type Dir = D }

    type Direction = CF.Pattern.Rel.Direction
    type Incoming  = CF.Pattern.Rel.Incoming.type
    type Outgoing  = CF.Pattern.Rel.Outgoing.type

    /** Supported params: {{{String}}}, {{{Iterable[String]}}}, {{{:=[_]}}}, {{{**}}}. */
    def unapplySeq(r: Rel): Option[Seq[Any]] = ???
  }

  @compileTimeOnly("Con only be used inside `Match` / `Create`")
  object := {
    def unapply[A](any: Any): Option[(String, CF.Expr[A])] = ???
  }
  @compileTimeOnly("Con only be used inside `Match` / `Create`")
  object ** {
    def unapply(any: Any): Option[(CF.Expr[Long], CF.Expr[Long])] = ???
  }

}
