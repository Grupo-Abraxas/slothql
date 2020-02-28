package com.abraxas.slothql.newcypher

import scala.annotation.unchecked.uncheckedVariance

import cats.data.{ Ior, NonEmptyList }
import cats.instances.list._
import cats.instances.option._
import shapeless.HList
import shapeless.LUBConstraint.<<:
import shapeless.ops.tuple.ToTraversable


trait CypherFragment {
  type Statement <: CypherStatement
  val toCypherF: CypherStatement.GenF[Statement]
  final def toCypher(gen: CypherStatement.Gen): (Statement, CypherStatement.Gen) = toCypherF(gen)
}

/** Cypher Query Language Fragments.
 *
 * From ''Cypher: An Evolving Query Language for Property Graphs''
 * [[http://homepages.inf.ed.ac.uk/pguaglia/papers/sigmod18.pdf]]:
 *
 * Syntax of expressions, queries, clauses and patterns of core Cypher.
 *
 *
 * `A` is a countable set of names.
 * `V` is the set of values, inductively defined as follows:
 *  - Identifiers (i.e., elements of `N` and `R`) are values;
 *  - Base types (elements of `Z` and `Σ∗`) are values;
 *  - `true`, `false` and `null `are values;
 *  - `list()` is a value (empty list), and if `v1, . . . ,vm` are values, for `m > 0`,
 *    then `list(v1, . . . ,vm)` is a value.
 *  - `map()` is a value (empty map), and if `k1, . . . , km` are distinct property keys and `v1, . . . ,vm` are values,
 *    for `m > 0`, then `map((k1,v1), . . . ,(km,vm))` is a value.
 *  - If `n` is a node identifier, then `path(n)` is a value. If `n1, . . . ,nm` are node ids and
 *    `r1, . . . ,rm−1` are relationship ids, for `m > 1`, then `path(n1,r1,n2, . . . ,nm−1,rm−1,nm)` is a value.
 * `F` are base functions.
 *
 * Expressions {{{
 * expr ::= v | a | f (expr_list)      where v ∈ V, a ∈ A, f ∈ F                                              values/variables
 *        | expr.k | {} | { prop_list }                                                                       maps
 *        | [ ] | [ expr_list ] | expr IN expr | expr[expr] | expr[expr..] | expr[..expr] | expr[expr..expr]  lists
 *        | expr STARTS WITH expr | expr ENDS WITH expr | expr CONTAINS expr                                  strings
 *        | expr OR expr | expr AND expr | expr XOR expr | NOT expr | expr IS NULL | expr IS NOT NULL         logic
 *        | expr < expr | expr <= expr | expr >= expr | expr > expr | expr = expr | expr <> expr              inequalities
 * }}}
 *
 * expr_list ::= expr | expr, expr_list                                    expression lists
 *
 * Queries {{{
 * query ::= query◦ | query UNION query | query UNION ALL query            unions
 * query◦ ::= RETURN ret | clause query◦                                   sequences of clauses
 * ret0 ::=  expr [AS a] | ret0, expr [AS a]                               return lists
 * ret  ::= ∗ | ret0                                                       return lists
 * }}}
 * Clauses {{{
 * clause ::= [OPTIONAL] MATCH pattern_tuple [WHERE expr]                  matching clauses
 *          | WITH ret [WHERE expr] | UNWIND expr AS a     where a ∈ A     relational clauses
 *          | CALL ... [YIELD ret0]
 * pattern_tuple ::= pattern | pattern, pattern_tuple                      tuples of patterns
 * }}}
 *
 * Pattern {{{
 * pattern ::= pattern◦ | a = pattern◦
 * pattern◦ ::= node_pattern | node_pattern rel_pattern pattern◦
 * node_pattern ::= (a? label_list? map? )
 * rel_pattern ::= -[a? type_list? len? map? ]->
 *               | <-[a? type_list? len? map? ]-
 *               | -[a? type_list? len? map? ]-
 * label_list ::= :ℓ | :ℓ label_list
 * map ::= { prop_list }
 * prop_list ::= k:expr | k:expr, prop_list
 * type_list ::= :t | type_list | t
 * len ::= ∗ | ∗d | ∗d1.. | ∗..d2 | ∗d1..d2       where d,d1,d2 ∈ N
 *}}}
 */
object CypherFragment {
  import CypherStatement._

  type Aux[+S <: CypherStatement] = CypherFragment { type Statement <: S }

  // // // // // // // // // // // // // //
  // // // // // Expressions // // // // //
  // // // // // // // // // // // // // //

  sealed trait Expr[+T] extends CypherFragment {
    type Statement = Part
    final lazy val toCypherF: GenF[Part] = Expr.toCypher(this).f
  }
  object Expr {

    // // // Values and Variables // // //
    sealed trait Input[A] extends Expr[A] {
      def lift: LiftValue[A]
    }
    final case class Param[A](name: String, lift: LiftValue[A]) extends Input[A]
    final case class Lit[A](value: A, lift: LiftValue[A]) extends Input[A]

    final case object Null extends Expr[Nothing]

    final case class Var[+A](name: String) extends Expr[A]

    final case class Func[+A](func: String, params: List[Expr[_]]) extends Expr[A]

    // // // Maps // // //
    sealed trait MapExpr[+A] extends Expr[A]
    final case class MapDef[+A](get: Map[String, Expr[A]]) extends MapExpr[Map[String, A]]
    final case class MapKey[+A](map: Expr[Map[String, A]], key: String) extends MapExpr[A]
    final case class MapDynKey[+A](map: Expr[Map[String, A]], key: Expr[String]) extends MapExpr[A]
    final case class MapAdd[+A](map: Expr[Map[String, A]], values: Map[String, Expr[A]]) extends MapExpr[Map[String, A]]


    // // // Lists // // //
    sealed trait ListExpr[+A] extends Expr[A]
    final case class ListDef[+A](vals: List[Expr[A]]) extends ListExpr[List[A]]
    final case class InList[+A] (list: Expr[List[A]], elem: Expr[A]) extends ListExpr[Boolean]
    final case class AtIndex[+A](list: Expr[List[A]], index: Expr[Long]) extends ListExpr[A]
    final case class AtRange[+A](list: Expr[List[A]], limits: Ior[Expr[Long], Expr[Long]]) extends ListExpr[List[A]]
    final case class Concat[+A] (pref: Expr[List[A]], suff: Expr[List[A]]) extends ListExpr[List[A]]

    final case class Reduce[A, B](
      list: Expr[List[A]],
      elemAlias: String,
      initial: Expr[B],
      accAlias: String,
      reduce: (Var[B], Var[A]) => Expr[B]
    ) extends ListExpr[B]

    final case class ListComprehension[A, B](
      list: Expr[List[A]],
      elemAlias: String,
      filter: Option[Var[A] => Expr[Boolean]],
      map: Option[Var[A] => Expr[B]]
    ) extends ListExpr[List[B]]

    final case class ListPredicate[A](
      list: Expr[List[A]],
      elemAlias: String,
      predicate: ListPredicate.Predicate,
      cond: Var[A] => Expr[Boolean]
    ) extends ListExpr[Boolean]

    object ListPredicate {
      sealed abstract class Predicate(protected[Expr] val cypher: String)

      case object All     extends Predicate("all")
      case object Any     extends Predicate("any")
      case object None    extends Predicate("none")
      case object Single  extends Predicate("single")
    }

    def toCypher(expr: Expr[_]): GenS[Part] = expr match {
      case Param(name0, lift)  => GenS.newParam(name0, lift).map(name => s"$$${escapeName(name)}")
      case Lit(value, lift)    => GenS.part(lift.asLiteral(value))
      case Null                => GenS.part("null")
      case Var(name)           => GenS.part(escapeName(name))
      case Func(func, params)  => funcLikePart(func, params)
      case MapDef(entries)     => mapPart(entries)
      case MapKey(map, key)    => GenS.part(map).map(m => s"$m.${escapeName(key)}")
      case MapDynKey(map, key) => (GenS.part(map), GenS.part(key)).map2((m, k) => s"$m[$k]")
      case MapAdd(map, values) => (GenS.part(map), mapPart(values, _.mkString(", "))).map2((m, add) => s"$m{.*, $add}")
      case ListDef(values)     => GenS.partsSequence(values).map(_.mkString("[ ", ", ", " ]"))
      case InList(list, elem)  => (GenS.part(list), GenS.part(elem)).map2((l, e) => s"$e IN $l")
      case AtIndex(list, idx)  => (GenS.part(list), GenS.part(idx)).map2((l, i) => s"$l[$i]")
      case AtRange(list, lim)  => (GenS.part(list), rangePart(lim)).map2((l, r) => s"$l[$r]")
      case Concat(pref, suff)  => (GenS.part(pref), GenS.part(suff)).map2((p, s) => s"$p + $s")
      case Reduce(list, elemAlias, initial, accAlias, reduce) =>
        for {
          lst  <- GenS.part(list)
          elem <- GenS.newAlias(elemAlias)
          init <- GenS.part(initial)
          acc  <- GenS.newAlias(accAlias)
          expr <- GenS.part(reduce(Var(acc), Var(elem)))
        } yield s"reduce(${escapeName(acc)} = $init, ${escapeName(elem)} IN $lst | $expr)"
      case ListComprehension(list, _, None, None) =>
        GenS.part(list)
      case ListComprehension(list, elemAlias, filter, map) =>
        for {
          lst  <- GenS.part(list)
          elem <- GenS.newAlias(elemAlias)
          fOpt <- GenS.partsSequence(filter.map(_(Var(elem))))
          mOpt <- GenS.partsSequence(map.map(_(Var(elem))))
          f    <- GenS.part(fOpt.map(f => s" WHERE $f").getOrElse(""))
          m    <- GenS.part(mOpt.map(m => s" | $m").getOrElse(""))
        } yield s"[${escapeName(elem)} IN $lst$f$m]"
      case ListPredicate(list, elemAlias, predicate, cond) =>
        for {
          lst  <- GenS.part(list)
          elem <- GenS.newAlias(elemAlias)
          expr <- GenS.part(cond(Var(elem)))
        } yield s"${predicate.cypher}(${escapeName(elem)} IN $lst WHERE $expr)"
    }
  }

  // // // // // // // // // // // // // //
  // // // // // / Queries / // // // // //
  // // // // // // // // // // // // // //

  sealed trait Query[+A] extends CypherFragment {
    type Statement = Complete[A @ uncheckedVariance]
    final lazy val toCypherF: GenF[Statement] = Query.toCypher(this).f
  }

  object Query {
    final case class Union[+A](left: Query[A], right: Query[A], all: Boolean) extends Query[A]

    sealed trait Query0[+A] extends Query[A]
    final case class Return[+A](ret: CypherFragment.Return[A]) extends Query0[A]
    final case class Clause[+A](clause: CypherFragment.Clause, query: Query0[A]) extends Query0[A]

    def toCypher[A](query: Query[A]): GenS[Complete[A]] = query match {
      case Union(left, right, all) =>
        (GenS.complete(left), GenS.complete(right)).map2((l, r) => s"$l UNION${if (all) " ALL" else ""} $r")
      case Clause(clause, query) =>
        (GenS.part(clause).toComplete[A], GenS.complete(query)).map2((c, q) => s"$c $q")
      case Return(ret) =>
        GenS.part(ret).toComplete[A].map(r => s"RETURN $r")
    }
  }

  // // // // // // // // // // // // // //
  // // // // // / Return // // // // // //
  // // // // // // // // // // // // // //

  sealed trait Return[+A] extends CypherFragment {
    type Statement = Part
    final lazy val toCypherF: GenF[Statement] = Return.toCypher(this).f
  }
  object Return {
    case object All extends Return[Map[String, Any]]

    final case class Expr[+A](expr: CypherFragment.Expr[A], as: Option[String]) extends Return[A]
    final case class Tuple[T <: HList: <<:[Expr[_]]#λ](tuple: T)(implicit list: ToTraversable.Aux[T, List, Expr[_]]) extends Return[T] {
      def toList: List[Expr[_]] = list(tuple)
    }

    sealed trait Order
    object Order {
      case object Ascending extends Order
      case object Descending extends Order
    }

    type OrderBy = List[(CypherFragment.Expr[_], Order)]

    final case class Options[+A](
      ret: Return[A],
      distinct: Boolean,
      orderBy: OrderBy,
      skip: Option[CypherFragment.Expr[Long]],
      limit: Option[CypherFragment.Expr[Long]]
    ) extends Return[A]


    def toCypher(ret: Return[_]): GenS[Part] = ret match {
      case All            => GenS.part("*")
      case t@Tuple(_)     => GenS.partsSequence(t.toList).map(_.mkString(", "))
      case Expr(expr, as) =>
        val r = GenS.part(expr)
        as.map(alias => r.map(t => s"$t AS ${escapeName(alias)}")).getOrElse(r)
      case Options(ret0, distinct0, orderBy0, skip0, limit0) =>
        val (ordEs, ordOs)  = orderBy0.unzip
        for {
          ret      <- GenS.part(ret0)
          ordList  <- GenS.partsSequence(ordEs)
          skipOpt  <- GenS.partsSequence(skip0)
          limOpt   <- GenS.partsSequence(limit0)
          distinct <- GenS.part(if (distinct0) "DISTINCT " else "")
          ord      <- GenS.part(ordList.zip(ordOs).map{ case (s, o) => s" $s $o" }.mkString(", "))
          skip     <- GenS.part(skipOpt.map(s => s" $s").getOrElse(""))
          lim      <- GenS.part(limOpt .map(s => s" $s").getOrElse(""))
        } yield s"$distinct$ret$ord$skip$lim"
    }
  }

  // // // // // // // // // // // // // //
  // // // // // / Clause // // // // // //
  // // // // // // // // // // // // // //

  sealed trait Clause extends CypherFragment {
    type Statement = Part
    final lazy val toCypherF: GenF[Part] = Clause.toCypher(this).f
  }
  object Clause {
    sealed trait Read extends Clause
    sealed trait Write extends Clause
    sealed trait ReadWrite extends Clause

    case class Match(pattern: PatternTuple, optional: Boolean, where: Option[Expr[Boolean]]) extends Read
    case class With(ret: Return[_], where: Option[Expr[Boolean]]) extends Read
    case class Unwind(expr: Expr[Seq[_]], as: String) extends Read
    case class Call(
      procedure: String,
      params: List[Expr[_]],
      yields: Option[Return[_]],
      where: Option[Expr[Boolean]]
    ) extends ReadWrite

    case class Create(pattern: PatternTuple) extends Write
    case class Delete(elems: NonEmptyList[Expr[_]]) extends Write

    case class SetProps(set: NonEmptyList[SetProps.One]) extends Write
    object SetProps {
      case class One(to: Expr[Map[String, _]], key: String, value: Expr[_]) extends CypherFragment {
        type Statement = Part
        val toCypherF: GenF[Part] = (
          for {
            tgt <- GenS.part(to)
            v   <- GenS.part(value)
          } yield s"$tgt.${escapeName(key)} = $v"
        ).f
      }
    }

    def toCypher(clause: Clause): GenS[Part] = clause match {
      case Match(pattern, optional, where) =>
        for {
          ps  <- GenS.partsSequence(pattern.toList)
          wh0 <- GenS.partsSequence(where)
          opt <- GenS.part{ if (optional) "OPTIONAL " else "" }
          wh  <- GenS.part{ wh0.map(w => s"WHERE $w").getOrElse("") }
        } yield s"${opt}MATCH ${ps.mkString(", ")}$wh"
      case Unwind(expr, as) =>
        GenS.part(expr).map(e => s"$e AS ${escapeName(as)}")
      case With(ret0, where0) =>
        for {
          ret      <- GenS.part(ret0)
          whereOpt <- GenS.partsSequence(where0)
          where    <- GenS.part(whereOpt.map(w => s" WHERE $w").getOrElse(""))
        } yield s"WITH $ret$where"
      case Create(pattern) => GenS.partsSequence(pattern.toList).map(ps => s"CREATE ${ps.mkString(", ")}")
      case Delete(elems)   => GenS.partsSequence(elems.toList)  .map(es => s"DELETE ${es.mkString(", ")}")
      case SetProps(set)   => GenS.partsSequence(set.toList)    .map(ss => s"SET ${ss.mkString(", ")}")
      case Call(procedure, params, yields0, where0) =>
        for {
          call     <- funcLikePart(procedure, params)
          yieldOpt <- GenS.partsSequence(yields0)
          whereOpt <- GenS.partsSequence(where0)
          yields   <- GenS.part(yieldOpt.map(y => s" YIELD $y").getOrElse(""))
          where    <- GenS.part(whereOpt.map(w => s" WHERE $w").getOrElse(""))
        } yield s"CALL $call$yields$where"
    }
  }

  // // // // // // // // // // // // // //
  // // // // // / Pattern / // // // // //
  // // // // // // // // // // // // // //

  type PatternTuple = NonEmptyList[Pattern]

  sealed trait Pattern extends CypherFragment {
    type Statement = Part
    final lazy val toCypherF: GenF[Part] = Pattern.toCypher(this).f
  }
  object Pattern {
    case class Let(alias: String, pattern: Pattern0) extends Pattern

    sealed trait Pattern0 extends Pattern
    sealed trait PatternA extends Pattern {
      val alias: Option[String]
    }
    case class Node(alias: Option[String], labels: List[String], map: Map[String, Expr[_]]) extends Pattern0 with PatternA
    case class Path(left: Node, rel: Rel, right: Pattern0) extends Pattern0
    case class Rel(
      alias: Option[String],
      types: List[String],
      map: Map[String, Expr[_]],
      length: Option[Rel.Length],
      dir: Rel.Direction
    ) extends Pattern with PatternA

    object Rel {
      sealed trait Length
      case object All extends Length
      case class Range(limits: Ior[Long, Long]) extends Length

      sealed trait Direction
      case object Outgoing extends Direction
      case object Incoming extends Direction
      case object Any extends Direction
    }

    def toCypher(pattern: Pattern): GenS[Part] = pattern match {
      case Let(alias, pattern) =>
        for {
          pat <- GenS.part(pattern)
        } yield s"${escapeName(alias)} = $pat"
      case Path(left, relation, right) =>
        for {
          lhs <- GenS.part(left)
          rel <- GenS.part(relation)
          rhs <- GenS.part(right)
        } yield s"$lhs $rel $rhs"
      case Node(alias, labels, map) =>
        for {
          m <- mapPart(map)
        } yield s"(${aliasStr(alias)}${labelLikeStr(labels, ":")}$m)"
      case Rel(alias, types, map, length, dir) =>
        for {
          m   <- mapPart(map)
          len <- length match {
                   case None                   => GenS.part("")
                   case Some(Rel.All)          => GenS.part("*")
                   case Some(Rel.Range(range)) => rangePart(range.bimap(longLit, longLit)).map(r => s"* $r")
                 }
        } yield {
          val tpe    = labelLikeStr(types, "|")
          val params = s"[${aliasStr(alias)}$tpe$len$m]"
          dir match {
            case Rel.Outgoing => s"-$params->"
            case Rel.Incoming => s"<-$params-"
            case Rel.Any      => s"-$params-"
          }
        }
    }

    @deprecated // TODO ================================================================================================
    private def aliasStr(alias: Option[String]) = alias.map(escapeName).getOrElse("")
    private def labelLikeStr(xs: List[String], sep: String) = xs match {
      case Nil => ""
      case _ => xs.map(escapeName).mkString(":", sep, "")
    }
    private def longLit(long: Long) = Expr.Lit(long, LiftValue.liftLongValue)
  }

  // // // // // // // // // // // // // //
  // // // // // // Utils // // // // // //
  // // // // // // // // // // // // // //

  private def funcLikePart(func: String, params: List[Expr[_]]): GenS[Part] =
    GenS.partsSequence(params).map { ps =>
      s"${func.split('.').map(escapeName).mkString(".")}(${ps.mkString(", ")})"
    }

  private def mapPart(map: Map[String, Expr[_]], join: List[String] => String = _.mkString("{ ", ", ", " }")): GenS[Part] =
    mapEntryParts(map).map {
      join apply _.zip(map.keys).map{ case (t, k) => s"${escapeName(k)}: $t" }
    }

  private def rangePart(ior: Ior[Expr[_], Expr[_]]) = ior match {
    case Ior.Left(min)      => GenS.part(min).map(m => s"$m..")
    case Ior.Right(max)     => GenS.part(max).map(m => s"..$m")
    case Ior.Both(min, max) => (GenS.part(min), GenS.part(max)).map2((mn, mx) => s"$mn..$mx")
  }


  private def mapEntryParts(map: Map[String, Expr[_]]): GenS0[List, Part] =
    if (map.isEmpty) GenF.pure(List.empty[Part]).genS
    else GenS.partsSequence(map.values.toList)

  private def escapeName(name: String) = name match {
    case "_" => "_"
    case _ => escapeName0(name)
  }
  private def escapeName0(name: String) = "`" + name.replace("`", "``") + "`"

}
