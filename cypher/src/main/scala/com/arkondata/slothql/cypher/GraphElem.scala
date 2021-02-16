package com.arkondata.slothql.cypher

sealed trait GraphElem

object GraphElem {
  final case class Node(id: Long, labels: List[String], props: Map[String, Any]) extends GraphElem

  final case class Rel(id: Long, tpe: String, startNodeId: Long, endNodeId: Long, props: Map[String, Any])
      extends GraphElem
}

final case class GraphPath(nodes: List[GraphElem.Node], rels: List[GraphElem.Rel])
