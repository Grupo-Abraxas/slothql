package com.arkondata.slothql.cypher

import scala.annotation.unchecked.uncheckedVariance

import cats.data.{ Ior, NonEmptyList }
import cats.instances.list._
import cats.instances.option._

trait CypherFragment {
  type Statement <: CypherStatement

  def isPrecedenceSafe: Boolean = true

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
  * }}}
  */
object CypherFragment {
  import CypherStatement._
  import GenS._

  type Aux[+S <: CypherStatement] = CypherFragment { type Statement <: S }

  sealed trait PrecedenceUnsafe extends CypherFragment {
    override def isPrecedenceSafe: Boolean = false
  }

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

    final case class Param[A](name: String, lift: LiftValue[A]) extends Input[A] with CypherStatement.Param

    object Param {
      def make[A](name: String)(implicit lift: LiftValue[A]): Param[A] = Param(name, lift)
    }

    final case class Lit[A](value: A, lift: LiftValue[A]) extends Input[A] with LiftedValue { type Value = A }

    final case object Null extends Expr[Nothing]

    class Alias[+A](val name: String) extends Expr[A] with CypherStatement.Alias

    object Alias {
      def apply[A](name: String): Alias[A] = new Alias(name)

      final case class Fixed[+A](override val name: String) extends Alias[A](name) with CypherStatement.Alias.Fixed
    }

    final case class Func[+A](func: String, params: List[Expr[_]]) extends Expr[A]

    // // // Maps // // //
    sealed trait MapExpr[+A] extends Expr[A]
    final case class MapDef[+A](get: Map[String, Expr[A]]) extends MapExpr[Map[String, A]]
    final case class MapKey[+A](map: Expr[Map[String, Any]], key: String) extends MapExpr[A]
    final case class MapDynKey[+A](map: Expr[Map[String, Any]], key: Expr[String]) extends MapExpr[A]
    final case class MapAdd[+A](map: Expr[Map[String, A]], values: Map[String, Expr[A]]) extends MapExpr[Map[String, A]]

    def toCypher(expr: MapExpr[_]): GenS[Part] = expr match {
      case MapDef(entries)     => mapPart(entries)
      case MapKey(map, key)    => part(map).map(m => s"$m.${escapeName(key)}")
      case MapDynKey(map, key) => part2(map, key)((m, k) => s"$m[$k]")
      case MapAdd(map, values) => part2(map, mapPart(values, _.mkString(", ")))((m, add) => s"$m{.*, $add}")
    }

    // // // Lists // // //
    sealed trait ListExpr[+A] extends Expr[A]
    final case class ListDef[+A](vals: List[Expr[A]]) extends ListExpr[List[A]]
    final case class InList[+A](list: Expr[List[A]], elem: Expr[A]) extends ListExpr[Boolean] with PrecedenceUnsafe
    final case class AtIndex[+A](list: Expr[List[A]], index: Expr[Long]) extends ListExpr[A]
    final case class AtRange[+A](list: Expr[List[A]], limits: Ior[Expr[Long], Expr[Long]]) extends ListExpr[List[A]]

    final case class Concat[+A](pref: Expr[List[A]], suff: Expr[List[A]])
        extends ListExpr[List[A]]
        with PrecedenceUnsafe

    final case class Reduce[A, B](
      list: Expr[List[A]],
      initial: Expr[B],
      reduce: (Alias[B], Alias[A]) => Expr[B]
    ) extends ListExpr[B]

    final case class ListComprehension[A, B](
      list: Expr[List[A]],
      filter: Option[Alias[A] => Expr[Boolean]],
      map: Option[Alias[A] => Expr[B]]
    ) extends ListExpr[List[B]]

    final case class ListPredicate[A](
      list: Expr[List[A]],
      predicate: ListPredicate.Predicate,
      cond: Alias[A] => Expr[Boolean]
    ) extends ListExpr[Boolean]

    object ListPredicate {
      sealed abstract class Predicate(protected[Expr] val cypher: String)

      case object All extends Predicate("all")
      case object Any extends Predicate("any")
      case object None extends Predicate("none")
      case object Single extends Predicate("single")
    }

    def toCypher(expr: ListExpr[_]): GenS[Part] = expr match {
      case ListDef(values)    => partsSequence(values).map(_.mkString("[", ", ", "]"))
      case InList(list, elem) => part2op(elem, list, "IN")
      case AtIndex(list, idx) => part2(list, idx)((l, i) => s"$l[$i]")
      case AtRange(list, lim) => part2(list, rangePart(lim))((l, r) => s"$l[$r]")
      case Concat(pref, suff) => part2op(pref, suff, "+")
      case Reduce(list, initial, reduce) =>
        val elemAlias = defaultAlias
        val accAlias  = defaultAlias
        for {
          lst  <- part(list)
          acc  <- liftAlias(accAlias)
          elem <- liftAlias(elemAlias)
          init <- part(initial)
          expr <- part(reduce(accAlias, elemAlias))
        } yield s"reduce($acc = $init, $elem IN $lst | $expr)"
      case ListComprehension(list, None, None) =>
        part(list)
      case ListComprehension(list, filter, map) =>
        val elemAlias = defaultAlias
        for {
          lst  <- part(list)
          elem <- liftAlias(elemAlias)
          fOpt <- partsSequence(filter.map(_(elemAlias)))
          mOpt <- partsSequence(map.map(_(elemAlias)))
          f    <- part(fOpt.map(f => s" WHERE $f").getOrElse(""))
          m    <- part(mOpt.map(m => s" | $m").getOrElse(""))
        } yield s"[$elem IN $lst$f$m]"
      case ListPredicate(list, predicate, cond) =>
        val elemAlias = defaultAlias
        for {
          lst  <- part(list)
          elem <- liftAlias(elemAlias)
          expr <- part(cond(elemAlias))
        } yield s"${predicate.cypher}($elem IN $lst WHERE $expr)"
    }

    // // // Strings // // //
    case class StringExpr(left: Expr[String], right: Expr[String], op: StringExpr.Op)
        extends Expr[Boolean]
        with PrecedenceUnsafe

    object StringExpr {
      sealed abstract class Op(protected[Expr] val cypher: String)
      case object StartsWith extends Op("STARTS WITH")
      case object EndsWith extends Op("ENDS WITH")
      case object Contains extends Op("CONTAINS")
      case object Regex extends Op("=~")
    }

    def toCypher(expr: StringExpr): GenS[Part] = part2op(expr.left, expr.right, expr.op.cypher)

    // // // Logic // // //
    sealed trait LogicExpr extends Expr[Boolean] with PrecedenceUnsafe
    case class LogicUnaryExpr(expr: Expr[Boolean], op: LogicExpr.UnaryOp) extends LogicExpr
    case class LogicBinaryExpr(left: Expr[Boolean], right: Expr[Boolean], op: LogicExpr.BinaryOp) extends LogicExpr

    object LogicExpr {
      sealed trait UnaryOp
      case object Negate extends UnaryOp

      sealed abstract class BinaryOp(protected[Expr] val cypher: String)
      case object Or extends BinaryOp("OR")
      case object And extends BinaryOp("AND")
      case object Xor extends BinaryOp("XOR")
    }

    def toCypher(expr: LogicExpr): GenS[Part] = expr match {
      case LogicUnaryExpr(expr, LogicExpr.Negate) => part(expr).map(e => s"NOT ${parentheses(expr, e)}")
      case LogicBinaryExpr(left, right, op)       => part2op(left, right, op.cypher)
    }

    // // // Compare // // //
    sealed trait CompareExpr extends Expr[Boolean] with PrecedenceUnsafe
    case class CompareUnaryExpr(expr: Expr[Any], op: CompareExpr.UnaryOp) extends CompareExpr
    case class CompareBinaryExpr(left: Expr[_], right: Expr[_], op: CompareExpr.BinaryOp) extends CompareExpr

    object CompareExpr {
      sealed trait UnaryOp
      case object IsNull extends UnaryOp
      case object NotNull extends UnaryOp

      sealed abstract class BinaryOp(protected[Expr] val cypher: String)
      case object Eq extends BinaryOp("=")
      case object Neq extends BinaryOp("<>")
      case object Lt extends BinaryOp("<")
      case object Lte extends BinaryOp("<=")
      case object Gte extends BinaryOp(">=")
      case object Gt extends BinaryOp(">")
    }

    def toCypher(expr: CompareExpr): GenS[Part] = expr match {
      case CompareUnaryExpr(expr, CompareExpr.IsNull)  => part(expr).map(e => s"${parentheses(expr, e)} IS NULL")
      case CompareUnaryExpr(expr, CompareExpr.NotNull) => part(expr).map(e => s"${parentheses(expr, e)} IS NOT NULL")
      case CompareBinaryExpr(left, right, op)          => part2op(left, right, op.cypher)
    }

    // // // Mathematical // // //
    sealed trait MathematicalExpr[N] extends Expr[N] with PrecedenceUnsafe
    case class MathematicalUnaryExpr[N](expr: Expr[N], op: MathematicalExpr.UnaryOp) extends MathematicalExpr[N]

    case class MathematicalBinaryExpr[N](left: Expr[N], right: Expr[N], op: MathematicalExpr.BinaryOp)
        extends MathematicalExpr[N]

    object MathematicalExpr {
      sealed trait UnaryOp
      case object Negation extends UnaryOp

      sealed abstract class BinaryOp(protected[Expr] val cypher: String)
      case object Addition extends BinaryOp("+")
      case object Subtraction extends BinaryOp("-")
      case object Multiplication extends BinaryOp("*")
      case object Division extends BinaryOp("/")
      case object ModuloDivision extends BinaryOp("%")
      case object Exponentiation extends BinaryOp("^")
    }

    def toCypher(expr: MathematicalExpr[_]): GenS[Part] = expr match {
      case MathematicalUnaryExpr(expr, MathematicalExpr.Negation) => part(expr).map(e => s"-${parentheses(expr, e)}")
      case MathematicalBinaryExpr(left, right, op)                => part2op(left, right, op.cypher)
    }

    // // // Distinct // // //
    case class Distinct[A](expr: Expr[A]) extends Expr[A] with PrecedenceUnsafe

    // // // Pattern Predicate // // //
    case class Exists(pattern: Pattern) extends Expr[Boolean]

    // // // Cases // // //
    sealed trait CaseExpr[A] extends Expr[A]

    case class SimpleCaseExpr[V, A](value: Expr[V], cases: Map[Expr[V], Expr[A]], default: Option[Expr[A]])
        extends CaseExpr[A]
    case class GenericCaseExpr[A](cases: Map[Expr[Boolean], Expr[A]], default: Option[Expr[A]]) extends CaseExpr[A]

    def toCypher(expr: CaseExpr[_]): GenS[Part] = {
      def casesStr(cases: Seq[(String, String)]) = cases.map { case (c, t) => s"WHEN $c THEN $t" }
        .mkString(" ", " ", "")
      def casesPart(cases0: Map[_ <: Expr[_], Expr[_]]) = {
        val (conds0, thens0) = cases0.unzip
        for {
          conds <- partsSequence(conds0.toList)
          thens <- partsSequence(thens0.toList)
          cases <- part(casesStr(conds.zip(thens)))
        } yield cases
      }
      def defaultStr(opt: Option[String])   = opt.map(d => s" ELSE $d").getOrElse("")
      def defaultPart(opt: Option[Expr[_]]) = partsSequence(opt).map(defaultStr)

      expr match {
        case SimpleCaseExpr(value0, cases0, default0) =>
          for {
            value   <- part(value0)
            cases   <- casesPart(cases0)
            default <- defaultPart(default0)
          } yield s"CASE $value$cases$default END"
        case GenericCaseExpr(cases0, default0) =>
          for {
            cases   <- casesPart(cases0)
            default <- defaultPart(default0)
          } yield s"CASE$cases$default END"
      }
    }

    // // // Final // // //
    def toCypher(expr: Expr[_]): GenS[Part] = expr match {
      case e: MapExpr[_]          => toCypher(e)
      case e: ListExpr[_]         => toCypher(e)
      case e: StringExpr          => toCypher(e)
      case e: LogicExpr           => toCypher(e)
      case e: CompareExpr         => toCypher(e)
      case e: MathematicalExpr[_] => toCypher(e)
      case e: CaseExpr[_]         => toCypher(e)
      case param @ Param(_, lift) => liftParam(param, lift).map(name => s"$$${escapeName(name)}")
      case Lit(value, lift)       => part(lift.asLiteral(value))
      case Null                   => part("null")
      case alias: Alias[_]        => liftAlias(alias)
      case Func(func, params)     => funcLikePart(func, params)
      case Distinct(expr)         => part(expr).map(e => s"DISTINCT $e")
      case Exists(pattern)        => part(pattern).map(p => s"EXISTS ($p)")
    }

    private def defaultAlias[R] = Expr.Alias[R]("#")
  }

  // // // // // // // // // // // // // //
  // // // // // / Queries / // // // // //
  // // // // // // // // // // // // // //

  sealed trait Query[+A] extends CypherFragment {
    type Statement = Complete[A @uncheckedVariance]
    final lazy val toCypherF: GenF[Statement] = Query.toCypher(this).f
  }

  object Query {
    final case class Union[+A](left: Query[A], right: Query[A], all: Boolean) extends Query[A]

    sealed trait Query0[+A] extends Query[A]
    final case class Return[+A](ret: CypherFragment.Return[A]) extends Query0[A]
    final case object Nothing extends Query0[Nothing]

    final case class Clause[+A](clause: CypherFragment.Clause, query: Query0[A]) extends Query0[A]

    def toCypher[A](query: Query[A]): GenS[Complete[A]] = query match {
      case Union(left, right, all) =>
        (complete(left), complete(right)).map2((l, r) => s"$l UNION${if (all) " ALL" else ""} $r")
      case Clause(clause, query) =>
        (part(clause).toComplete[A], complete(query)).map2((c, q) => s"$c $q")
      case Return(ret) =>
        part(ret).toComplete[A].map(r => s"RETURN $r")
      case Nothing =>
        complete("")
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
    sealed trait Return0[+A] extends Return[A]
    case object Wildcard extends Return0[Any]
    final case class Expr[+A](expr: CypherFragment.Expr[A], as: Option[Alias]) extends Return0[A]
    final case class Tuple[T <: Product](exprs: List[Expr[_]]) extends Return0[T]
    final case class WildcardTuple(exprs: List[Expr[_]]) extends Return0[Any]

    sealed trait Order

    object Order {

      case object Ascending extends Order {
        override def toString: String = "ASC"
      }

      case object Descending extends Order {
        override def toString: String = "DESC"
      }
    }

    type OrderBy = List[(CypherFragment.Expr[_], Order)]

    final case class Options[+A](
      ret: Return0[A],
      distinct: Boolean,
      orderBy: OrderBy,
      skip: Option[CypherFragment.Expr[Long]],
      limit: Option[CypherFragment.Expr[Long]]
    ) extends Return[A]

    def toCypher(ret: Return[_]): GenS[Part] = ret match {
      case Wildcard             => part("*")
      case Tuple(exprs)         => partsSequence(exprs).map(_.mkString(", "))
      case WildcardTuple(exprs) => partsSequence(exprs).map(xs => s"*, ${xs.mkString(", ")}")
      case Expr(expr, as) =>
        as.map { alias =>
          for {
            alias <- liftAlias(alias)
            expr  <- part(expr)
          } yield s"$expr AS $alias"
        }.getOrElse(part(expr))
      case Options(ret0, distinct0, orderBy0, skip0, limit0) =>
        val (ordEs, ordOs) = orderBy0.unzip
        for {
          ret      <- part(ret0)
          ordList  <- partsSequence(ordEs)
          skipOpt  <- partsSequence(skip0)
          limOpt   <- partsSequence(limit0)
          distinct <- part(if (distinct0) "DISTINCT " else "")
          ord0     <- part(ordList.zip(ordOs).map { case (s, o) => s"$s $o" }.mkString(", "))
          ord      <- part(if (ordList.nonEmpty) s" ORDER BY $ord0" else "")
          skip     <- part(skipOpt.map(s => s" SKIP $s").getOrElse(""))
          lim      <- part(limOpt.map(s => s" LIMIT $s").getOrElse(""))
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
    case class Unwind(expr: Expr[Seq[_]], as: Alias) extends Read

    case class Merge(pattern: PatternTuple) extends Write

    case class Call(
      procedure: String,
      params: List[Expr[_]],
      yields: Option[Return[_]],
      where: Option[Expr[Boolean]]
    ) extends ReadWrite

    case class Create(pattern: PatternTuple) extends Write
    case class Delete(elems: NonEmptyList[Expr[_]], detach: Boolean) extends Write

    case class OnMatch(props: Write) extends Write

    case class SetProps(set: NonEmptyList[SetProps.One]) extends Write

    object SetProps {

      case class One(to: Expr[Map[String, _]], key: String, value: Expr[_]) extends CypherFragment {
        type Statement = Part

        val toCypherF: GenF[Part] = (
          for {
            tgt <- part(to)
            v   <- part(value)
          } yield s"$tgt.${escapeName(key)} = $v"
        ).f
      }
    }

    case class SetNode(to: Expr[Map[String, _]], props: Expr[Map[String, Expr[_]]]) extends Write

    case class ExtendNode(to: Expr[Map[String, _]], props: Expr[Map[String, Expr[_]]]) extends Write

    def toCypher(clause: Clause): GenS[Part] = clause match {
      case Match(pattern, optional, where) =>
        for {
          ps  <- partsSequence(pattern.toList)
          wh0 <- partsSequence(where)
          opt <- part(if (optional) "OPTIONAL " else "")
          wh  <- part(wh0.map(w => s" WHERE $w").getOrElse(""))
        } yield s"${opt}MATCH ${ps.mkString(", ")}$wh"
      case Merge(pattern) =>
        for {
          ps <- partsSequence(pattern.toList)
        } yield s"MERGE ${ps.mkString(", ")}"
      case OnMatch(props) =>
        for {
          ps <- part(props)
        } yield s"ON MATCH $ps"
      case Unwind(expr, as) =>
        for {
          expr  <- part(expr)
          alias <- liftAlias(as)
        } yield s"UNWIND $expr AS $alias"
      case With(ret0, where0) =>
        for {
          ret      <- part(ret0)
          whereOpt <- partsSequence(where0)
          where    <- part(whereOpt.map(w => s" WHERE $w").getOrElse(""))
        } yield s"WITH $ret$where"
      case Create(pattern) => partsSequence(pattern.toList).map(ps => s"CREATE ${ps.mkString(", ")}")
      case SetProps(set)   => partsSequence(set.toList).map(ss => s"SET ${ss.mkString(", ")}")
      case SetNode(n, props0) =>
        for {
          name  <- part(n)
          props <- part(props0)
          set   <- GenS.part(s"SET $name = $props")
        } yield set
      case ExtendNode(n, props0) =>
        for {
          name  <- part(n)
          props <- part(props0)
          set   <- GenS.part(s"SET $name += $props")
        } yield set
      case Delete(elems, detach) =>
        val detachStr = if (detach) "DETACH " else ""
        partsSequence(elems.toList).map(es => s"${detachStr}DELETE ${es.mkString(", ")}")
      case Call(procedure, params, yields0, where0) =>
        for {
          call     <- funcLikePart(procedure, params)
          yieldOpt <- partsSequence(yields0)
          whereOpt <- partsSequence(where0)
          yields   <- part(yieldOpt.map(y => s" YIELD $y").getOrElse(""))
          where    <- part(whereOpt.map(w => s" WHERE $w").getOrElse(""))
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
    case class Let(alias: CypherStatement.Alias, pattern: Pattern0) extends Pattern

    sealed trait Pattern0 extends Pattern

    sealed trait PatternA extends Pattern {
      val alias: Option[Alias]
    }

    case class Node(
      alias: Option[CypherStatement.Alias],
      labels: List[String],
      props: Either[Map[String, Expr[_]], Expr[Map[String, Any]]]
    ) extends Pattern0
        with PatternA
    case class Path(left: Node, rel: Rel, right: Pattern0) extends Pattern0

    case class Rel(
      alias: Option[CypherStatement.Alias],
      types: List[String],
      props: Either[Map[String, Expr[_]], Expr[Map[String, Any]]],
      length: Option[Rel.Length],
      dir: Rel.Direction
    ) extends Pattern
        with PatternA

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
          alias <- liftAlias(alias)
          pat   <- part(pattern)
        } yield s"$alias = $pat"
      case Path(left, relation, right) =>
        for {
          lhs <- part(left)
          rel <- part(relation)
          rhs <- part(right)
        } yield s"$lhs $rel $rhs"
      case Node(alias, labels, map) =>
        for {
          alias <- liftAlias(alias)
          m     <- mapEPart(map, joinPropsMap)
        } yield s"($alias${labelLikeStr(labels, ":")}$m)"
      case Rel(alias, types, map, length, dir) =>
        for {
          m <- mapEPart(map, joinPropsMap)
          len <- length match {
                   case None                   => part("")
                   case Some(Rel.All)          => part("*")
                   case Some(Rel.Range(range)) => rangePart(range.bimap(longLit, longLit)).map(r => s"*$r")
                 }
          alias <- liftAlias(alias)
        } yield {
          val tpe       = labelLikeStr(types, "|")
          val hasParams = alias.nonEmpty || tpe.nonEmpty || len.nonEmpty || m.nonEmpty
          val params    = if (hasParams) s"[$alias$tpe$len$m]" else ""
          dir match {
            case Rel.Outgoing => s"-$params->"
            case Rel.Incoming => s"<-$params-"
            case Rel.Any      => s"-$params-"
          }
        }
    }

    private def joinPropsMap: List[String] => String = {
      case Nil => ""
      case l   => l.mkString("{ ", ", ", " }")
    }

    private def labelLikeStr(xs: List[String], sep: String) = xs match {
      case Nil => ""
      case _   => xs.map(escapeName).mkString(":", sep, "")
    }
    private def longLit(long: Long) = Expr.Lit(long, LiftValue.liftLongValue)
  }

  // // // // // // // // // // // // // //
  // // // // // // Utils // // // // // //
  // // // // // // // // // // // // // //

  private def part2(left: CypherFragment.Aux[Part], right: CypherFragment.Aux[Part])(
    f: (String, String) => String
  ): GenS[Part] =
    (part(left), part(right)).map2(f)

  private def part2(left: CypherFragment.Aux[Part], right: GenS[Part])(f: (String, String) => String): GenS[Part] =
    (part(left), right).map2(f)

  private def part2(left: GenS[Part], right: CypherFragment.Aux[Part])(f: (String, String) => String): GenS[Part] =
    (left, part(right)).map2(f)

  private def part2op(left: CypherFragment.Aux[Part], right: CypherFragment.Aux[Part], op: String): GenS[Part] =
    part2(left, right)((l, r) => s"${parentheses(left, l)} $op ${parentheses(right, r)}")

  private def parentheses(fragment: CypherFragment, str: String): String =
    if (fragment.isPrecedenceSafe) str else s"($str)"

  private def funcLikePart(func: String, params: List[Expr[_]]): GenS[Part] =
    partsSequence(params).map { ps =>
      s"${func.split('.').map(escapeName).mkString(".")}(${ps.mkString(", ")})"
    }

  private def mapPart(
    map: Map[String, Expr[_]],
    join: List[String] => String = _.mkString("{ ", ", ", " }")
  ): GenS[Part] =
    mapEntryParts(map).map {
      join apply _.zip(map.keys).map { case (t, k) => s"${escapeName(k)}: $t" }
    }

  private def mapEPart(
    map: Either[Map[String, Expr[_]], Expr[Map[String, Any]]],
    join: List[String] => String = _.mkString("{ ", ", ", " }")
  ): GenS[Part] =
    map.fold(mapPart(_, join), part)

  private def rangePart(ior: Ior[Expr[_], Expr[_]]) = ior match {
    case Ior.Left(min)      => part(min).map(m => s"$m..")
    case Ior.Right(max)     => part(max).map(m => s"..$m")
    case Ior.Both(min, max) => part2(min, max)((mn, mx) => s"$mn..$mx")
  }

  private def mapEntryParts(map: Map[String, Expr[_]]): GenS0[List, Part] =
    if (map.isEmpty) GenF.pure(List.empty[Part]).genS
    else partsSequence(map.values.toList)

  protected[cypher] def escapeName(name: String): String = name match {
    case "_" => "_"
    case _   => escapeName0(name)
  }
  private def escapeName0(name: String) = s"`${name.replace("`", "``")}`"

}
