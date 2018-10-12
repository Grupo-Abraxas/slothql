package com.abraxas.slothql.cypher.syntax

import shapeless.HNil

import com.abraxas.slothql.Connection
import com.abraxas.slothql.cypher.CypherFragment.{ Query, Return }
import com.abraxas.slothql.neo4j.Neo4jCypherTransactor


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

  // MATCH (`a`) -[`b`]-> (`c`) -[`d`]-> (`e`) <-[`f`]- (`g`) RETURN `a`.`count`
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
  }

  println(query)
  println(query.known.toCypher)

  // MATCH (`user`) <-[]- () <-[]- (`group`) RETURN `user`.`email`, `user`.`name`, `user`.`age`, `user`.`confirmed`, `group`.`name`
  // result = Buffer((Some(john@example.com),Some(John),Some(28),Some(true),Root Group), (None,None,None,None,Sub Group), (Some(john@example.com),Some(John),Some(28),Some(true),Sub Group))

  val driver = Connection.driver
  val tx = Neo4jCypherTransactor(driver.session())

  val io = tx.readIO(query)
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
  }

  println(query)
  println(query.known.toCypher)

  // MATCH (`u`:`User`{ `id`: "u1" }) <-[]- (:`Members`) <-[]- (`group`) RETURN `u`.`email`, `u`.`name`, `u`.`age`, `u`.`confirmed`, `group`.`name`
  // result = Buffer((john@example.com,John,28,true,Sub Group), (john@example.com,John,28,true,Root Group))

  val driver = Connection.driver
  val tx = Neo4jCypherTransactor(driver.session())

  val io = tx.readIO(query)
  val result: Seq[(String, String, Int, Boolean, String)] = io.unsafeRunSync()

  println("result = " + result)

  driver.close()
  sys.exit()

}

object SyntaxTest5 extends App {
  val id = "g1"
  val query = Match {
    case Vertex("Group", "id" := `id`) < _ *:(0 - _, Edge("parent")) - (g@Vertex("Group")) => g.prop[String]("name")
  }

  println(query)
  println(query.known.toCypher)

  // MATCH (:`Group`{ `id`: "g1" }) <-[:`parent`*0..]- (`g`:`Group`) RETURN `g`.`name`
  // result = Buffer(Root Group, Sub Group)

  val driver = Connection.driver
  val tx = Neo4jCypherTransactor(driver.session())

  val io = tx.readIO(query)
  val result: Seq[String] = io.unsafeRunSync()

  println("result = " + result)

  driver.close()
  sys.exit()

}

object SyntaxTest6 extends App {
  val id = "g1"
  val query = Match {
    case Vertex("Group", "id" := `id`) < _ *:(_, Edge("parent")) - (g@Vertex("Group")) =>
      (g, g.call[Map[String, Any]]("properties"), 'pi.call[Double]())
  }

  println(query)
  println(query.known.toCypher)

  // MATCH (:`Group`{ `id`: "g1" }) <-[:`parent`*]- (`g`:`Group`) RETURN `g`, `properties`(`g`), `pi`()
  // result = Buffer((Map(name -> Sub Group, id -> g2),Map(name -> Sub Group, id -> g2),3.141592653589793))

  val driver = Connection.driver
  val tx = Neo4jCypherTransactor(driver.session())

  val io = tx.readIO(query)
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
  val tx = Neo4jCypherTransactor(driver.session())

  val io = tx.readIO(query)
  val result: Seq[(Long, Long, List[String], List[String], Long, Long, List[String], String)] = io.unsafeRunSync()

  println("result = " + result)

  driver.close()
  sys.exit()

}

object SyntaxTest8 extends App {
  val id = "g1"
  val query = Match {
    case Vertex("Group", "id" := `id`) < es*:(0 - _, Edge("parent")) - (g@Vertex("Group")) => (
      g.id,
      g.count,
      g.keys,
      g.labels,
      es
    )
  }

  println(query)
  println(query.known.toCypher)

  // MATCH (:`Group`{ `id`: "g1" }) <-[`es`:`parent`*0..]- (`g`:`Group`) RETURN `id`(`g`), `count`(`g`), `keys`(`g`), `labels`(`g`), `es`
  // result = Buffer((3,1,List(name, id),List(Group),List(Map())), (2,1,List(name, id),List(Group),List()))

  val driver = Connection.driver
  val tx = Neo4jCypherTransactor(driver.session())

  val io = tx.readIO(query)
  val result: Seq[(Long, Long, List[String], List[String], List[Map[String, Any]])] = io.unsafeRunSync()

  println("result = " + result)

  driver.close()
  sys.exit()

}

object SyntaxTest9 extends App {
  val query = Match {
    case v < _ - _ => (
      v.id,
      v.id === lit(2),
      v.id === lit("QWERTY"),
      v.id > lit(2L),
      !(v.id > lit(2L)),
      v.id > lit(2L) && v.id <= lit(4L),
      (v.id > lit(2L)) xor (v.id <= lit(4L)),
      v.isNull
    )
  }

  println(query)
  println(query.known.toCypher)

  // MATCH (`v`) <-[]- () RETURN `id`(`v`), `id`(`v`) = 2, `id`(`v`) = "QWERTY", `id`(`v`) > 2, NOT `id`(`v`) > 2, `id`(`v`) > 2 AND `id`(`v`) <= 4, `id`(`v`) > 2 XOR `id`(`v`) <= 4, `v` IS NULL
  // result = Buffer(
  //    (0,false,false,false,true,false,true,false),
  //    (0,false,false,false,true,false,true,false),
  //    (1,false,false,false,true,false,true,false),
  //    (2,true, false,false,true,false,true,false),
  //    (4,false,false,true,false,true,false,false)
  //  )

  val driver = Connection.driver
  val tx = Neo4jCypherTransactor(driver.session())

  val io = tx.readIO(query)
  val result: Seq[(Long, Boolean, Boolean, Boolean, Boolean, Boolean, Boolean, Boolean)] = io.unsafeRunSync()

  println("result = " + result)

  driver.close()
  sys.exit()

}

object SyntaxTest10 extends App {
  val l = list("Admin", "Share", "Create")
  val query = Match {
    case v < e - _ => (
      list(v.id, e.id),
      l,
      v.labels ++ list(e.tpe),
      e.tpe in l,
      v.labels at lit(0),
      l at (lit(1), lit(2L)),
      l from lit(2),
      l to lit(2L)
    )
  }

  println(query)
  println(query.known.toCypher)

  // MATCH (`v`) <-[`e`]- ()
  // RETURN [ `id`(`v`), `id`(`e`) ],
  //        [ "Admin", "Share", "Create" ],
  //        `labels`(`v`) + [ `type`(`e`) ],
  //        `type`(`e`) IN [ "Admin", "Share", "Create" ],
  //        `labels`(`v`)[0],
  //        [ "Admin", "Share", "Create" ][1..2],
  //        [ "Admin", "Share", "Create" ][2..],
  //        [ "Admin", "Share", "Create" ][..2]
  // result = Buffer(
  //    (List(0, 0),List(Admin, Share, Create),List(User, Admin),     true, User,   List(Share),List(Create),List(Admin, Share)),
  //    (List(1, 1),List(Admin, Share, Create),List(Members, members),false,Members,List(Share),List(Create),List(Admin, Share)),
  //    (List(4, 4),List(Admin, Share, Create),List(Members, members),false,Members,List(Share),List(Create),List(Admin, Share)),
  //    (List(2, 2),List(Admin, Share, Create),List(Group, parent),   false,Group,  List(Share),List(Create),List(Admin, Share)),
  //    (List(0, 3),List(Admin, Share, Create),List(User, Edit),      false,User,   List(Share),List(Create),List(Admin, Share))
  //  )

  val driver = Connection.driver
  val tx = Neo4jCypherTransactor(driver.session())

  val io = tx.readIO(query)
  val result: Seq[(List[Long], List[String], List[String], Boolean, String, List[String], List[String], List[String])] = io.unsafeRunSync()

  println("result = " + result)

  driver.close()
  sys.exit()

}

object SyntaxTest11 extends App {
  val query = Match {
    case v < e - _ if e.tpe in list("Admin", "Share") => v
  }

  println(query)
  println(query.known.toCypher)

  // MATCH (`v`) <-[`e`]- () WHERE `type`(`e`) IN [ "Admin", "Share" ] RETURN `v`
  // result = Buffer(Map(name -> John, email -> john@example.com, confirmed -> true, age -> 28, id -> u1))

  val driver = Connection.driver
  val tx = Neo4jCypherTransactor(driver.session())

  val io = tx.readIO(query)
  val result: Seq[Map[String, Any]] = io.unsafeRunSync()

  println("result = " + result)

  driver.close()
  sys.exit()

}

object SyntaxTest12 extends App {
  val query = Match {
    case v1 -> v2 `<-` v3 => list(v1, v2, v3)
  }

  println(query)
  println(query.known.toCypher)

  // MATCH (`v1`) -[]-> (`v2`) <-[]- (`v3`) RETURN [ `v1`, `v2`, `v3` ]
  // result = Buffer(List(Map(), Map(name -> John, email -> john@example.com, confirmed -> true, age -> 28, id -> u1), Map()), List(Map(), Map(name -> John, email -> john@example.com, confirmed -> true, age -> 28, id -> u1), Map()))

  val driver = Connection.driver
  val tx = Neo4jCypherTransactor(driver.session())

  val io = tx.readIO(query)
  val result: Seq[List[Map[String, Any]]] = io.unsafeRunSync()

  println("result = " + result)

  driver.close()
  sys.exit()

}

object SyntaxTest13 extends App {
  val query = Match {
    case v1 -> v2 -(e)> v3 `<-` v4 => list(v1, v2, v3, v4) -> e.tpe
  }

  println(query)
  println(query.known.toCypher)

  // MATCH (`v1`) -[]-> (`v2`) -[`e`]-> (`v3`) <-[]- (`v4`) RETURN [ `v1`, `v2`, `v3`, `v4` ], `type`(`e`)
  // result = Buffer((List(Map(name -> Root Group, id -> g1), Map(), Map(name -> John, email -> john@example.com, confirmed -> true, age -> 28, id -> u1), Map()),Admin), (List(Map(name -> Sub Group, id -> g2), Map(), Map(name -> John, email -> john@example.com, confirmed -> true, age -> 28, id -> u1), Map()),Edit))

  val driver = Connection.driver
  val tx = Neo4jCypherTransactor(driver.session())

  val io = tx.readIO(query)
  val result: Seq[(List[Map[String, Any]], String)] = io.unsafeRunSync()

  println("result = " + result)

  driver.close()
  sys.exit()

}

object SyntaxTest14 extends App {
  val query = Match {
    case v1 -> v2 <(e)- v3 `<-` v4 => list(v1, v2, v3, v4) -> e.tpe
  }

  println(query)
  println(query.known.toCypher)

  // MATCH (`v1`) -[]-> (`v2`) <-[`e`]- (`v3`) <-[]- (`v4`) RETURN [ `v1`, `v2`, `v3`, `v4` ], `type`(`e`)
  // result = Buffer((List(Map(), Map(name -> John, email -> john@example.com, confirmed -> true, age -> 28, id -> u1), Map(), Map(name -> Root Group, id -> g1)),Admin), (List(Map(), Map(name -> John, email -> john@example.com, confirmed -> true, age -> 28, id -> u1), Map(), Map(name -> Sub Group, id -> g2)),Edit))

  val driver = Connection.driver
  val tx = Neo4jCypherTransactor(driver.session())

  val io = tx.readIO(query)
  val result: Seq[(List[Map[String, Any]], String)] = io.unsafeRunSync()

  println("result = " + result)

  driver.close()
  sys.exit()

}

object SyntaxTest15 extends App {
  val query = Match {
    case v@Vertex("Group") => (v.id, v.prop[String]("name"))
  }

  println(query)
  println(query.known.toCypher)

  // MATCH (`v`:`Group`) RETURN `id`(`v`), `v`.`name`
  // result = Buffer((2,Root Group), (3,Sub Group))

  val driver = Connection.driver
  val tx = Neo4jCypherTransactor(driver.session())

  val io = tx.readIO(query)
  val result: Seq[(Long, String)] = io.unsafeRunSync()

  println("result = " + result)

  driver.close()
  sys.exit()

}

object SyntaxTest16 extends App {
  val query = Match {
    case v@Vertex("Group") =>
      val name = v.prop[String]("name")
      (v.id, name)
        .orderBy(name, v.id)
  }

  println(query)
  println(query.known.toCypher)

  // MATCH (`v`:`Group`) RETURN `id`(`v`), `v`.`name` ORDER BY `v`.`name`, `id`(`v`)
  // result = Buffer((2,Root Group), (3,Sub Group))

  val driver = Connection.driver
  val tx = Neo4jCypherTransactor(driver.session())

  val io = tx.readIO(query)
  val result: Seq[(Long, String)] = io.unsafeRunSync()

  println("result = " + result)

  driver.close()
  sys.exit()

}

object SyntaxTest17 extends App {
  val query = Match {
    case v@Vertex("Group") =>
      val name = v.prop[String]("name")
      (v.id, name)
        .orderBy(name.desc)
        .orderBy(v.id)
  }

  println(query)
  println(query.known.toCypher)

  // MATCH (`v`:`Group`) RETURN `id`(`v`), `v`.`name` ORDER BY `v`.`name` DESC, `id`(`v`)
  // result = Buffer((3,Sub Group), (2,Root Group))

  val driver = Connection.driver
  val tx = Neo4jCypherTransactor(driver.session())

  val io = tx.readIO(query)
  val result: Seq[(Long, String)] = io.unsafeRunSync()

  println("result = " + result)

  driver.close()
  sys.exit()

}

object SyntaxTest18 extends App {
  val query = Match {
    case v@Vertex("Group") =>
      val name = v.prop[String]("name")
      (v.id, name)
        .orderBy(name.desc)
        .skip(1)
        .limit(1)
  }

  println(query)
  println(query.known.toCypher)

  // MATCH (`v`:`Group`) RETURN `id`(`v`), `v`.`name` ORDER BY `v`.`name` DESC SKIP 1 LIMIT 1
  // result = Buffer((2,Root Group))

  val driver = Connection.driver
  val tx = Neo4jCypherTransactor(driver.session())

  val io = tx.readIO(query)
  val result: Seq[(Long, String)] = io.unsafeRunSync()

  println("result = " + result)

  driver.close()
  sys.exit()

}

object SyntaxTest19 extends App {
  val query = Match {
    case v@Vertex("Group") =>
      val name = v.prop[String]("name")
      (v.id, name)
        .orderBy(name)
        .distinct

  }

  println(query)
  println(query.known.toCypher)

  // MATCH (`v`:`Group`) RETURN DISTINCT `id`(`v`), `v`.`name` ORDER BY `v`.`name`
  // result = Buffer((2,Root Group), (3,Sub Group))

  val driver = Connection.driver
  val tx = Neo4jCypherTransactor(driver.session())

  val io = tx.readIO(query)
  val result: Seq[(Long, String)] = io.unsafeRunSync()

  println("result = " + result)

  driver.close()
  sys.exit()

}


object SyntaxTest20 extends App {
  val id = Some("g1")
  val query = Match {
    case g@Vertex("Group", "id" :?= `id`) => g
  }

  println(query)
  println(query.known.toCypher)

  // MATCH (`g`:`Group`{ `id`: "g1" }) RETURN `g`
  // result = Vector(Map(name -> Root Group, id -> g1))

  val driver = Connection.driver
  val tx = Neo4jCypherTransactor(driver.session())

  val io = tx.readIO(query)
  val result: Seq[Map[String, Any]] = io.unsafeRunSync()

  println("result = " + result)

  driver.close()
  sys.exit()

}

object SyntaxTest21 extends App {
  val id = Option.empty[String]
  val query = Match {
    case g@Vertex("Group", "id" :?= `id`) => g
  }

  println(query)
  println(query.known.toCypher)

  // MATCH (`g`:`Group`) RETURN `g`
  // result = Vector(Map(name -> Root Group, id -> g1), Map(name -> Sub Group, id -> g2))

  val driver = Connection.driver
  val tx = Neo4jCypherTransactor(driver.session())

  val io = tx.readIO(query)
  val result: Seq[Map[String, Any]] = io.unsafeRunSync()

  println("result = " + result)

  driver.close()
  sys.exit()

}

object SyntaxTest22 extends App {
  val id = Option.empty[String]
  val query = Match {
    case u@Vertex("User", "id" :?= `id`) =>
      Match {
        case (g@Vertex("Group")) -> Vertex("Members") -Edge("Admin")> u2 if u === u2 => u.prop[String]("email") -> g.prop[String]("name")
        // case (g@Vertex("Group")) -> Vertex("Members") -Edge("Admin")> `u` => u.prop[String]("email") -> g.prop[String]("name") // TODO: syntax =============
      }
  }

  println(query)
  println(query.known.toCypher)

  // MATCH (`u`:`User`) MATCH (`g`:`Group`) -[]-> (:`Members`) -[:`Admin`]-> (`u2`) WHERE `u` = `u2` RETURN `u`.`email`, `g`.`name`
  // result = Vector((john@example.com,Root Group))

  val driver = Connection.driver
  val tx = Neo4jCypherTransactor(driver.session())

  val io = tx.readIO(query)
  val result: Seq[(String, String)] = io.unsafeRunSync()

  println("result = " + result)

  driver.close()
  sys.exit()

}

object SyntaxTest23 extends App {
  val query = Match {
    case u@Vertex("User") =>
      Match {
        case (g@Vertex("Group")) -> Vertex("Members") -Edge("FooBar")> u2 if u === u2 => u.prop[String]("email") -> g.prop[String]("name")
      }
  }

  val queryOpt = Match {
    case u@Vertex("User") =>
      Match.optional {
        case (g@Vertex("Group")) -> Vertex("Members") -Edge("FooBar")> u2 if u === u2 => u.prop[String]("email") -> g.prop[String]("name")
      }
  }

  println(query)
  println(query.known.toCypher)

  // MATCH (`u`:`User`) MATCH (`g`:`Group`) -[]-> (:`Members`) -[:`FooBar`]-> (`u2`) WHERE `u` = `u2` RETURN `u`.`email`, `g`.`name`
  // result = Vector()


  println(queryOpt)
  println(queryOpt.known.toCypher)

  // MATCH (`u`:`User`) OPTIONAL MATCH (`g`:`Group`) -[]-> (:`Members`) -[:`FooBar`]-> (`u2`) WHERE `u` = `u2` RETURN `u`.`email`, `g`.`name`
  // result = Vector((john@example.com,null))

  val driver = Connection.driver
  val tx = Neo4jCypherTransactor(driver.session())

  val io = tx.readIO(query)
  val result: Seq[(String, String)] = io.unsafeRunSync()

  println("result = " + result)

  val ioOpt = tx.readIO(queryOpt)
  val resultOpt: Seq[(String, String)] = ioOpt.unsafeRunSync()

  println("result = " + resultOpt)

  driver.close()
  sys.exit()

}

object SyntaxTest24 extends App {
  val query = Match {
    case u@Vertex("User") => dict(
      "id" -> u.prop[String]("id"),
      MapEntry.pairToMapEntry("personal" -> dict(
        "name" -> u.prop[String]("name"),
        "age"  -> u.prop[Int]("age")
      )),
      "account" -> dict(
        "email"     -> u.prop[String]("email"),
        "confirmed" -> u.prop[Boolean]("confirmed")
      )
    )
  }

  println(query)
  println(query.known.toCypher)

  // MATCH (`u`:`User`) RETURN { `id`: `u`.`id`, `personal`: { `name`: `u`.`name`, `age`: `u`.`age` }, `account`: { `email`: `u`.`email`, `confirmed`: `u`.`confirmed` } }
  // result = Vector(Map(personal -> Map(name -> John, age -> 28), id -> u1, account -> Map(confirmed -> true, email -> john@example.com)))

  val driver = Connection.driver
  val tx = Neo4jCypherTransactor(driver.session())

  val io = tx.readIO(query)
  val result = io.unsafeRunSync()

  println("result = " + result)

  driver.close()
  sys.exit()

}

/*
TODO: fails to compile =================================================================================================

object SyntaxTest22 extends App {
  val email = "user@example.com"
  val query = Match {
    case Vertex("User", "email" := `email`) -(e)> (g@Vertex("Group")) => g.prop[String]("id")
  }

  println(query)
  println(query.known.toCypher)

  // MATCH (`g`:`Group`) RETURN `g`
  // result = Vector(Map(name -> Root Group, id -> g1), Map(name -> Sub Group, id -> g2))

  val driver = Connection.driver
  val tx = Neo4jCypherTransactor(driver.session())

  val io = tx.readIO(query)
  val result: Seq[String] = io.unsafeRunSync()

  println("result = " + result)

  driver.close()
  sys.exit()

}
*/

/*


*/