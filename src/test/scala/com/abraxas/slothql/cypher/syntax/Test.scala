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

  // MATCH (`user`) <-[]- () <-[]- (`group`) RETURN `user`.`email`, `user`.`name`, `user`.`age`, `user`.`confirmed`, `group`.`name`
  // result = Buffer((Some(john@example.com),Some(John),Some(28),Some(true),Root Group), (None,None,None,None,Sub Group), (Some(john@example.com),Some(John),Some(28),Some(true),Sub Group))

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

  // MATCH (`u`:`User`{ `id`: "u1" }) <-[]- (:`Members`) <-[]- (`group`) RETURN `u`.`email`, `u`.`name`, `u`.`age`, `u`.`confirmed`, `group`.`name`
  // result = Buffer((john@example.com,John,28,true,Sub Group), (john@example.com,John,28,true,Root Group))

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
    case Vertex("Group", "id" := `id`) < Edge("parent", 0 ** _) - (g@Vertex("Group")) => g[String]("name")
  }

  println(query)
  println(query.known.toCypher)

  // MATCH (:`Group`{ `id`: "g1" }) <-[:`parent`*0..]- (`g`:`Group`) RETURN `g`.`name`
  // result = Buffer(Root Group, Sub Group)

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
    case Vertex("Group", "id" := `id`) < Edge("parent", **) - (g@Vertex("Group")) =>
      (g, g.call[Map[String, Any]]("properties"), 'pi.call[Double]())
  }

  println(query)
  println(query.known.toCypher)

  // MATCH (:`Group`{ `id`: "g1" }) <-[:`parent`*]- (`g`:`Group`) RETURN `g`, `properties`(`g`), `pi`()
  // result = Buffer((Map(name -> Sub Group, id -> g2),Map(name -> Sub Group, id -> g2),3.141592653589793))

  val driver = Connection.driver
  val tx = CypherTransactor.Default(driver.session())

  import com.abraxas.slothql.neo4j.CypherTransactor.RecordReader._
  import com.abraxas.slothql.neo4j.CypherTransactor.ValueReader._

  val io = tx.read(query)
  val result: Seq[(Map[String, Any], Map[String, Any], Double)] = io.unsafeRunSync()

  println("result = " + result)

  driver.close()
  sys.exit()

}

object SyntaxTest7 extends App {
  val id = "g1"
  val query = Match {
    case Vertex("Group", "id" := `id`) < (e@Edge("parent")) - (g@Vertex("Group")) => (
      g.id,
      g.count,
      g.keys,
      g.labels,
      e.id,
      e.count,
      e.keys,
      e.tpe
    )
  }

  println(query)
  println(query.known.toCypher)

  // MATCH (:`Group`{ `id`: "g1" }) <-[`e`:`parent`]- (`g`:`Group`) RETURN `id`(`g`), `count`(`g`), `keys`(`g`), `labels`(`g`), `id`(`e`), `count`(`e`), `keys`(`e`), `type`(`e`)
  // result = Buffer((3,1,List(name, id),List(Group),2,1,List(),parent))

  val driver = Connection.driver
  val tx = CypherTransactor.Default(driver.session())

  import com.abraxas.slothql.neo4j.CypherTransactor.RecordReader._
  import com.abraxas.slothql.neo4j.CypherTransactor.ValueReader._

  val io = tx.read(query)
  val result: Seq[(Long, Long, List[String], List[String], Long, Long, List[String], String)] = io.unsafeRunSync()

  println("result = " + result)

  driver.close()
  sys.exit()

}

// TODO: failing - Type mismatch: expected Node or Relationship but was List<Relationship> (line 1, column 133 (offset: 132))
object SyntaxTest8 extends App {
  val id = "g1"
  val query = Match {
    case Vertex("Group", "id" := `id`) < (e@Edge("parent", 0 ** _)) - (g@Vertex("Group")) => (
      g.id,
      g.count,
      g.keys,
      g.labels,
      e.id,
      e.count,
      e.keys,
      e.tpe
    )
  }

  println(query)
  println(query.known.toCypher)

  // MATCH (:`Group`{ `id`: "g1" }) <-[:`parent`*0..]- (`g`:`Group`) RETURN `g`.`name`
  // result = Buffer(Root Group, Sub Group)

  val driver = Connection.driver
  val tx = CypherTransactor.Default(driver.session())

  import com.abraxas.slothql.neo4j.CypherTransactor.RecordReader._
  import com.abraxas.slothql.neo4j.CypherTransactor.ValueReader._

  val io = tx.read(query)
  val result: Seq[(Long, Long, List[String], List[String], Long, Long, List[String], String)] = io.unsafeRunSync()

  println("result = " + result)

  driver.close()
  sys.exit()

}


/*


*/