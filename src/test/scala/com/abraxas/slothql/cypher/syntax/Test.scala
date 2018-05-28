package com.abraxas.slothql.cypher.syntax

import shapeless.HNil

import com.abraxas.slothql.Connection
import com.abraxas.slothql.cypher.CypherFragment.{ Query, Return }
import com.abraxas.slothql.neo4j.CypherTransactor


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

object SyntaxTest3 extends App {
  val query = Match {
    case user < _ - _ < _ - group =>
      Return.List(
        Return.Expr(user.propOpt[String]("email"), as = None),
        Return.Expr(user.propOpt[String]("name"), as = None) ::
        Return.Expr(user.propOpt[Int]("age"), as = None) ::
        Return.Expr(user.propOpt[Boolean]("confirmed"), as = None) ::
        Return.Expr(group.prop[String]("name"), as = None) :: HNil
//        Return.Expr(role.prop[String]("name"), as = None) :: HNil TODO
      )
  }

  println(query)
  println(query.known.toCypher)

  // Clause(KnownMatch(NonEmptyList(KnownPath(KnownNode(Some(user),List(),Map()){ (`user`) },KnownRel(None,List(),Map(),None,Incoming){ <-[]- },KnownPath(KnownNode(None,List(),Map()){ () },KnownRel(None,List(),Map(),None,Incoming){ <-[]- },KnownNode(Some(group),List(),Map()){ (`group`) }){ () <-[]- (`group`) }){ (`user`) <-[]- () <-[]- (`group`) }),false,None){ MATCH (`user`) <-[]- () <-[]- (`group`) },KnownReturn(KnownRetList(KnownExpr(KnownKey(KnownVar(user){ `user` },email){ `user`.`email` },None){ `user`.`email` }, KnownExpr(KnownKey(KnownVar(user){ `user` },name){ `user`.`name` },None){ `user`.`name` }, KnownExpr(KnownKey(KnownVar(user){ `user` },age){ `user`.`age` },None){ `user`.`age` }, KnownExpr(KnownKey(KnownVar(user){ `user` },confirmed){ `user`.`confirmed` },None){ `user`.`confirmed` }, KnownExpr(KnownKey(KnownVar(group){ `group` },name){ `group`.`name` },None){ `group`.`name` }){ `user`.`email`, `user`.`name`, `user`.`age`, `user`.`confirmed`, `group`.`name` }){ RETURN `user`.`email`, `user`.`name`, `user`.`age`, `user`.`confirmed`, `group`.`name` })
  // MATCH (`user`) <-[]- () <-[]- (`group`) RETURN `user`.`email`, `user`.`name`, `user`.`age`, `user`.`confirmed`, `group`.`name`

  val driver = Connection.driver
  val tx = CypherTransactor.Default(driver.session())

  import com.abraxas.slothql.neo4j.CypherTransactor.RecordReader._
  import com.abraxas.slothql.neo4j.CypherTransactor.ValueReader._

  val io = tx.read(query)
  val result: Seq[(Option[String], Option[String], Option[Int], Option[Boolean], String)] = io.unsafeRunSync()

  println("result = " + result)

  driver.close()
  sys.exit()

}



/*


*/