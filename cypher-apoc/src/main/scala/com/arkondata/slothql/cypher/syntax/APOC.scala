package com.arkondata.slothql.cypher.syntax

import cats.data.NonEmptyList
import shapeless.{ HList, ProductArgs }

import com.arkondata.slothql.cypher.ParameterizedCypherQuery
import com.arkondata.slothql.cypher.syntax.apoc._

object APOC {
  def when[PT <: HList, PE <: HList, Ps <: HList, A](
    cond: Expr[Boolean],
    thenQuery: ParameterizedCypherQuery[PT, A],
    elseQuery: ParameterizedCypherQuery[PE, A],
    write: Boolean = false
  )(implicit b: When.Builder[PT, PE]): When.ParamsSyntax[b.Params, A] =
    new When.ParamsSyntax(cond, thenQuery, elseQuery, write)(b.toMap)

  object `case` extends ProductArgs {
    def applyProduct[Cases <: HList](cases: Cases)(implicit b: Case.Builder[Cases]): Case.OtherwiseSyntax[b.Params, b.Out] =
      new Case.OtherwiseSyntax(b.toList(cases), write = false)

    def writeProduct[Cases <: HList](cases: Cases)(implicit b: Case.Builder[Cases]): Case.OtherwiseSyntax[b.Params, b.Out] =
      new Case.OtherwiseSyntax(b.toList(cases), write = true)
  }

  def assertNot[R](pred: Expr[Boolean], msg: Expr[String], msgParams: Expr[Any]*)(res: Query[R]): Query[R] =
    Call("apoc.util.validate", pred, msg, list(msgParams: _*)).void(res)

  def assert[R](pred: Expr[Boolean], msg: Expr[String], msgParams: Expr[Any]*)(res: Query[R]): Query[R] =
    assertNot(!pred, msg, msgParams: _*)(res)

  object lock {
    object read {
      def nodes[R](node: Node, nodes: Node*)(res: Query[R]): Query[R] =
        this.nodes(NonEmptyList(node, nodes.toList))(res)

      def nodes[R](nodes: NonEmptyList[Node])(res: Query[R]): Query[R] =
        Call("apoc.lock.read.nodes", list(nodes.toList: _*)).void(res)

      def rels[R](rel: Rel, rels: Rel*)(res: Query[R]): Query[R] =
        this.rels(NonEmptyList(rel, rels.toList))(res)

      def rels[R](rels: NonEmptyList[Rel])(res: Query[R]): Query[R] =
        Call("apoc.lock.read.rels", list(rels.toList: _*)).void(res)
    }

    object write {
      def apply[R](nodes: NonEmptyList[Node], rels: NonEmptyList[Rel])(res: Query[R]): Query[R] =
        Call("apoc.lock.all", list(nodes.toList: _*), list(rels.toList: _*)).void(res)

      def nodes[R](node: Node, nodes: Node*)(res: Query[R]): Query[R] =
        this.nodes(NonEmptyList(node, nodes.toList))(res)

      def nodes[R](nodes: NonEmptyList[Node])(res: Query[R]): Query[R] =
        Call("apoc.lock.nodes", list(nodes.toList: _*)).void(res)

      def rels[R](rel: Rel, rels: Rel*)(res: Query[R]): Query[R] =
        this.rels(NonEmptyList(rel, rels.toList))(res)

      def rels[R](rels: NonEmptyList[Rel])(res: Query[R]): Query[R] =
        Call("apoc.lock.rels", list(rels.toList: _*)).void(res)
    }
  }
}
