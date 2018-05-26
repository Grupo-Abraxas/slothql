package com.abraxas.slothql.cypher.syntax

import com.abraxas.slothql.cypher.CypherFragment.Query


object SyntaxTest {

  (??? : Graph) match {
    case a -> b =>
    case a `<-` b =>
    case a -(b)> c =>
    case a <(b)- c =>

    case a -> b -> c =>
    case a `<-` b `<-` c =>

    case  a -(b)> c -(d)> e =>
    case  a -(b)> c -(d)> e -(f)> g -(h)> i =>
    case  a <(b)- c <(d)- e  =>
    case  a -(b)> c -(d)> e <(f)- g =>

    case a -- b =>
    case a -- b -- c =>

//    case a -(b)- c -(d)- e =>
//    case a -(b)> c -(d)- e <(f)- g =>
  }

}

object SyntaxTest2 extends App {
  val query: Query[Int] = Match {
    case a -(b)> c -(d)> e <(f)- g => a.prop[Int]("count")
  }

  println(query)
  println(query.known.toCypher)

  // QClause(KnownMatch(NonEmptyList(KnownPath(KnownNode(Some(a),List(),Map()){ (`a`) },KnownRel(Some(b),List(),Map(),None,Outgoing){ -[`b`]-> },KnownPath(KnownNode(Some(c),List(),Map()){ (`c`) },KnownRel(Some(d),List(),Map(),None,Outgoing){ -[`d`]-> },KnownPath(KnownNode(Some(e),List(),Map()){ (`e`) },KnownRel(Some(f),List(),Map(),None,Incoming){ <-[`f`]- },KnownNode(Some(g),List(),Map()){ (`g`) }){ (`e`) <-[`f`]- (`g`) }){ (`c`) -[`d`]-> (`e`) <-[`f`]- (`g`) }){ (`a`) -[`b`]-> (`c`) -[`d`]-> (`e`) <-[`f`]- (`g`) }),false,None){ MATCH (`a`) -[`b`]-> (`c`) -[`d`]-> (`e`) <-[`f`]- (`g`) }, KnownQReturn(KnownRetExpr(KnownVar().count{ ``.`count` }){ ``.`count` }){ RETURN ``.`count` })
  // MATCH (`a`) -[`b`]-> (`c`) -[`d`]-> (`e`) <-[`f`]- (`g`) RETURN ``.`count`
}
