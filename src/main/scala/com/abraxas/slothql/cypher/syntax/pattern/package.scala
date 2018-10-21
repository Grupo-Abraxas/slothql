package com.abraxas.slothql.cypher.syntax

import com.abraxas.slothql.cypher.CypherFragment.{ Expr, Known, Pattern }

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
      new PatternRightPathBuilder(pat, Pattern.Rel(Option(alias), types.toList, props, Option(length).map(_(Pattern.Rel)), _))
  }


  class PatternRightPathBuilder(left: Pattern.Pattern0, mkRel: Pattern.Rel.Direction => Pattern.Rel) {
    def --(node: Pattern.Node): Pattern.Path = mkPath(left, mkRel(Pattern.Rel.Any), node)
    def --( labels: Seq[String]                 = Nil,
            props:  Map[String, Known[Expr[_]]] = Map(),
            alias:  String                      = null
          ): Pattern.Path =
      this -- Pattern.Node(Option(alias), labels.toList, props)

    def ->(node: Pattern.Node): Pattern.Path = mkPath(left, mkRel(Pattern.Rel.Outgoing), node)
    def ->( labels: Seq[String]                 = Nil,
            props:  Map[String, Known[Expr[_]]] = Map(),
            alias:  String                      = null
          ): Pattern.Path =
      this -> Pattern.Node(Option(alias), labels.toList, props)

    private def mkPath(left: Pattern.Pattern0, rel: Pattern.Rel, right: Pattern.Node): Pattern.Path = left match {
      case node: Pattern.Node => Pattern.Path(node, rel, right)
      case Pattern.Path(left0, rel0, right0) => Pattern.Path(left0, rel0, mkPath(right0.fragment, rel, right))
    }
  }


  implicit class VarExprBuilder(name: String) {
    def vertexAlias: Vertex = {
      val v = Vertex()
      v.asInstanceOf[GraphElem.Impl]._alias = name
      v
    }
    def edgeAlias: Edge = {
      val e = Edge()
      e.asInstanceOf[GraphElem.Impl]._alias = name
      e
    }
  }

}
