package com.abraxas.slothql.cypher.syntax

import shapeless.HNil

import com.abraxas.slothql.cypher.CypherFragment.{ Query, Return }


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
  val query = Match {
    case a -(b)> c -(d)> e <(f)- g =>
      Return.List(
        Return.Expr(a.prop[Int]("count"), as = None),
        Return.Expr(f.prop[String]("name"), as = None) :: HNil
      )
  }

  println(query)
  println(query.known.toCypher)

  // Clause(KnownMatch(NonEmptyList(KnownPath(KnownNode(Some(a),List(),Map()){ (`a`) },KnownRel(Some(b),List(),Map(),None,Outgoing){ -[`b`]-> },KnownPath(KnownNode(Some(c),List(),Map()){ (`c`) },KnownRel(Some(d),List(),Map(),None,Outgoing){ -[`d`]-> },KnownPath(KnownNode(Some(e),List(),Map()){ (`e`) },KnownRel(Some(f),List(),Map(),None,Incoming){ <-[`f`]- },KnownNode(Some(g),List(),Map()){ (`g`) }){ (`e`) <-[`f`]- (`g`) }){ (`c`) -[`d`]-> (`e`) <-[`f`]- (`g`) }){ (`a`) -[`b`]-> (`c`) -[`d`]-> (`e`) <-[`f`]- (`g`) }),false,None){ MATCH (`a`) -[`b`]-> (`c`) -[`d`]-> (`e`) <-[`f`]- (`g`) },KnownReturn(KnownRetList(KnownExpr(KnownKey(KnownVar(a){ `a` },count){ `a`.`count` },None){ `a`.`count` }, KnownExpr(KnownKey(KnownVar(f){ `f` },name){ `f`.`name` },None){ `f`.`name` }){ `a`.`count`, `f`.`name` }){ RETURN `a`.`count`, `f`.`name` })
  // MATCH (`a`) -[`b`]-> (`c`) -[`d`]-> (`e`) <-[`f`]- (`g`) RETURN `a`.`count`, `f`.`name`
}
