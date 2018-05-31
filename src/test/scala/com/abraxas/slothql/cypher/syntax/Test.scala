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
    case a -(b)> c -(d)> e <(f)- g => a.prop[Int]("count")
  }

  println(query)
  println(query.known.toCypher)

  // Clause(KnownMatch(NonEmptyList(KnownPath(KnownNode(Some(a),List(),Map()){ (`a`) },KnownRel(Some(b),List(),Map(),None,Outgoing){ -[`b`]-> },KnownPath(KnownNode(Some(c),List(),Map()){ (`c`) },KnownRel(Some(d),List(),Map(),None,Outgoing){ -[`d`]-> },KnownPath(KnownNode(Some(e),List(),Map()){ (`e`) },KnownRel(Some(f),List(),Map(),None,Incoming){ <-[`f`]- },KnownNode(Some(g),List(),Map()){ (`g`) }){ (`e`) <-[`f`]- (`g`) }){ (`c`) -[`d`]-> (`e`) <-[`f`]- (`g`) }){ (`a`) -[`b`]-> (`c`) -[`d`]-> (`e`) <-[`f`]- (`g`) }),false,None){ MATCH (`a`) -[`b`]-> (`c`) -[`d`]-> (`e`) <-[`f`]- (`g`) },KnownReturn(KnownRetList(KnownExpr(KnownKey(KnownVar(a){ `a` },count){ `a`.`count` },None){ `a`.`count` }, KnownExpr(KnownKey(KnownVar(f){ `f` },name){ `f`.`name` },None){ `f`.`name` }){ `a`.`count`, `f`.`name` }){ RETURN `a`.`count`, `f`.`name` })
  // MATCH (`a`) -[`b`]-> (`c`) -[`d`]-> (`e`) <-[`f`]- (`g`) RETURN `a`.`count`, `f`.`name`
}

object SyntaxTest3 extends App {
  val query = Match {
    case user < _ - _ < _ - group =>
      (
        user.propOpt[String]("email"),
        user.propOpt[String]("name"),
        user.propOpt[Int]("age"),
        user.propOpt[Boolean]("confirmed"),
        group.prop[String]("name")
      )
//        role.prop[String]("name") TODO
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

object SyntaxTest4 extends App {
  val id = "u1"
  val query = Match {
    case (u@Vertex("User", "id" := `id`)) < _ - Vertex("Members") < _ - group =>
      (
        u.prop[String]("email"),
        u.prop[String]("name"),
        u.prop[Int]("age"),
        u.prop[Boolean]("confirmed"),
        group.prop[String]("name")
      )
//        role.prop[String]("name") TODO
  }

  println(query)
  println(query.known.toCypher)

  // Clause(KnownMatch(NonEmptyList(KnownPath(KnownNode(Some(u),List(User),Map(id -> KnownLit(u1){ "u1" })){ (`u`:`User`{ `id`: "u1" }) },KnownRel(None,List(),Map(),None,Incoming){ <-[]- },KnownPath(KnownNode(None,List(Members),Map()){ (:`Members`) },KnownRel(None,List(),Map(),None,Incoming){ <-[]- },KnownNode(Some(group),List(),Map()){ (`group`) }){ (:`Members`) <-[]- (`group`) }){ (`u`:`User`{ `id`: "u1" }) <-[]- (:`Members`) <-[]- (`group`) }),false,None){ MATCH (`u`:`User`{ `id`: "u1" }) <-[]- (:`Members`) <-[]- (`group`) },KnownReturn(KnownRetList(KnownExpr(KnownKey(KnownVar(u){ `u` },email){ `u`.`email` },None){ `u`.`email` }, KnownExpr(KnownKey(KnownVar(u){ `u` },name){ `u`.`name` },None){ `u`.`name` }, KnownExpr(KnownKey(KnownVar(u){ `u` },age){ `u`.`age` },None){ `u`.`age` }, KnownExpr(KnownKey(KnownVar(u){ `u` },confirmed){ `u`.`confirmed` },None){ `u`.`confirmed` }, KnownExpr(KnownKey(KnownVar(group){ `group` },name){ `group`.`name` },None){ `group`.`name` }){ `u`.`email`, `u`.`name`, `u`.`age`, `u`.`confirmed`, `group`.`name` }){ RETURN `u`.`email`, `u`.`name`, `u`.`age`, `u`.`confirmed`, `group`.`name` })
  // MATCH (`u`:`User`{ `id`: "u1" }) <-[]- (:`Members`) <-[]- (`group`) RETURN `u`.`email`, `u`.`name`, `u`.`age`, `u`.`confirmed`, `group`.`name`

  val driver = Connection.driver
  val tx = CypherTransactor.Default(driver.session())

  import com.abraxas.slothql.neo4j.CypherTransactor.RecordReader._
  import com.abraxas.slothql.neo4j.CypherTransactor.ValueReader._

  val io = tx.read(query)
  val result: Seq[(String, String, Int, Boolean, String)] = io.unsafeRunSync()

  println("result = " + result)

  driver.close()
  sys.exit()

}

object SyntaxTest5 extends App {
  val id = "g1"
  val query = Match {
    case Vertex("Group", "id" := "g1") < Edge("parent", 0 ** _) - (g@Vertex("Group")) => g.prop[String]("name")
  }

  println(query)
  println(query.known.toCypher)

  // Clause(KnownMatch(NonEmptyList(KnownPath(KnownNode(None,List(Group),Map(id -> KnownLit(g1){ "g1" })){ (:`Group`{ `id`: "g1" }) },KnownRel(None,List(parent),Map(),Some(Range(Left(0))),Incoming){ <-[:`parent`*0..]- },KnownNode(Some(g),List(Group),Map()){ (`g`:`Group`) }){ (:`Group`{ `id`: "g1" }) <-[:`parent`*0..]- (`g`:`Group`) }),false,None){ MATCH (:`Group`{ `id`: "g1" }) <-[:`parent`*0..]- (`g`:`Group`) },KnownReturn(KnownExpr(KnownKey(KnownVar(g){ `g` },name){ `g`.`name` },None){ `g`.`name` }){ RETURN `g`.`name` })
  // MATCH (:`Group`{ `id`: "g1" }) <-[:`parent`*0..]- (`g`:`Group`) RETURN `g`.`name`

  val driver = Connection.driver
  val tx = CypherTransactor.Default(driver.session())

  import com.abraxas.slothql.neo4j.CypherTransactor.RecordReader._
  import com.abraxas.slothql.neo4j.CypherTransactor.ValueReader._

  val io = tx.read(query)
  val result: Seq[String] = io.unsafeRunSync()

  println("result = " + result)

  driver.close()
  sys.exit()

}

object SyntaxTest6 extends App {
  val id = "g1"
  val query = Match {
    case Vertex("Group", "id" := "g1") < Edge("parent", **) - (g@Vertex("Group")) => g.prop[String]("name")
  }

  println(query)
  println(query.known.toCypher)

  // Clause(KnownMatch(NonEmptyList(KnownPath(KnownNode(None,List(Group),Map(id -> KnownLit(g1){ "g1" })){ (:`Group`{ `id`: "g1" }) },KnownRel(None,List(parent),Map(),Some(All),Incoming){ <-[:`parent`*]- },KnownNode(Some(g),List(Group),Map()){ (`g`:`Group`) }){ (:`Group`{ `id`: "g1" }) <-[:`parent`*]- (`g`:`Group`) }),false,None){ MATCH (:`Group`{ `id`: "g1" }) <-[:`parent`*]- (`g`:`Group`) },KnownReturn(KnownExpr(KnownKey(KnownVar(g){ `g` },name){ `g`.`name` },None){ `g`.`name` }){ RETURN `g`.`name` })
  // MATCH (:`Group`{ `id`: "g1" }) <-[:`parent`*]- (`g`:`Group`) RETURN `g`.`name`

  val driver = Connection.driver
  val tx = CypherTransactor.Default(driver.session())

  import com.abraxas.slothql.neo4j.CypherTransactor.RecordReader._
  import com.abraxas.slothql.neo4j.CypherTransactor.ValueReader._

  val io = tx.read(query)
  val result: Seq[String] = io.unsafeRunSync()

  println("result = " + result)

  driver.close()
  sys.exit()

}



/*


*/