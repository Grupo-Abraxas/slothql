package com.arkondata.slothql02.cypher.syntax

import com.arkondata.slothql02.cypher.CypherFragment.{ Expr, Known, Pattern }

package object pattern {

  implicit class VertexObjectPatternOps(v: Vertex.type) {
    def pattern( labels: Seq[String]                 = Nil,
                 props:  Map[String, Known[Expr[_]]] = Map(),
                 alias:  String                      = null
               ): Pattern.Node =
      Pattern.Node(Option(alias), labels.toList, props)
  }

  implicit class PatternPathBuilder(pat: Pattern.Pattern0) {
    def --( types:  Seq[String]                            = Nil,
            props:  Map[String, Known[Expr[_]]]            = Map(),
            length: Pattern.Rel.type => Pattern.Rel.Length = null,
            alias:  String                                 = null
          ): PatternRightPathBuilder =
      new PatternRightPathBuilder(pat, mkRel(alias, types, props, length, _))

    def `<-`( types:  Seq[String]                            = Nil,
              props:  Map[String, Known[Expr[_]]]            = Map(),
              length: Pattern.Rel.type => Pattern.Rel.Length = null,
              alias:  String                                 = null
            ): PatternLeftPathBuilder = new PatternLeftPathBuilder(pat, mkRel(alias, types, props, length, Pattern.Rel.Incoming))

    /** Unicode alias for [[<-]]. Has different operator precedence. */
    def ⟵( types:  Seq[String]                            = Nil,
             props:  Map[String, Known[Expr[_]]]            = Map(),
             length: Pattern.Rel.type => Pattern.Rel.Length = null,
             alias:  String                                 = null
           ): PatternLeftPathBuilder = `<-`(types, props, length, alias)

    private def mkRel(alias: String, types: Seq[String], props: Map[String, Known[Expr[_]]], length: Pattern.Rel.type => Pattern.Rel.Length, dir: Pattern.Rel.Direction) =
      Pattern.Rel(Option(alias), types.toList, props, Option(length).map(_(Pattern.Rel)), dir)
  }


  class PatternRightPathBuilder(left: Pattern.Pattern0, mkRel: Pattern.Rel.Direction => Pattern.Rel) {
    def --(node: Pattern.Node): Pattern.Path = mkPathBuilderPath(left, mkRel(Pattern.Rel.Any), node)
    def --( labels: Seq[String]                 = Nil,
            props:  Map[String, Known[Expr[_]]] = Map(),
            alias:  String                      = null
          ): Pattern.Path =
      this -- Pattern.Node(Option(alias), labels.toList, props)

    def ->(node: Pattern.Node): Pattern.Path = mkPathBuilderPath(left, mkRel(Pattern.Rel.Outgoing), node)
    def ->( labels: Seq[String]                 = Nil,
            props:  Map[String, Known[Expr[_]]] = Map(),
            alias:  String                      = null
          ): Pattern.Path =
      this -> Pattern.Node(Option(alias), labels.toList, props)
  }

  class PatternLeftPathBuilder(left: Pattern.Pattern0, rel: Pattern.Rel) {
    def --(node: Pattern.Node): Pattern.Path = mkPathBuilderPath(left, rel, node)
    def --( labels: Seq[String]                 = Nil,
            props:  Map[String, Known[Expr[_]]] = Map(),
            alias:  String                      = null
          ): Pattern.Path =
      this -- Pattern.Node(Option(alias), labels.toList, props)

    /** Unicode alias for [[--(labels:Seq[String]*]]. Has different operator precedence. */
    def ⤙( labels: Seq[String]                 = Nil,
            props:  Map[String, Known[Expr[_]]] = Map(),
            alias:  String                      = null
          ): Pattern.Path =
      this -- Pattern.Node(Option(alias), labels.toList, props)
  }

  private def mkPathBuilderPath(left: Pattern.Pattern0, rel: Pattern.Rel, right: Pattern.Node): Pattern.Path = left match {
    case node: Pattern.Node => Pattern.Path(node, rel, right)
    case Pattern.Path(left0, rel0, right0) => Pattern.Path(left0, rel0, mkPathBuilderPath(right0.fragment, rel, right))
  }


  implicit class VarExprBuilder(name: String) {
    def vertexAlias: Vertex = {
      val v = Vertex()
      v.asInstanceOf[Graph.Impl[_]]._alias = name
      v
    }
    def edgeAlias: Edge = {
      val e = Edge()
      e.asInstanceOf[Graph.Impl[_]]._alias = name
      e
    }
  }

}
