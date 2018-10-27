package com.abraxas.slothql.cypher

import scala.language.{ higherKinds, implicitConversions }

import cats.{ Bifunctor, Contravariant, Functor }
import cats.data.{ Ior, NonEmptyList }
import cats.syntax.bifunctor._
import cats.syntax.functor._
import shapeless.{ :: => #:, _ }

import com.abraxas.slothql.util.ComappedCov

trait CypherFragment[-A] {
  // TODO: (String, Params)
  def toCypher(f: A): String
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
 * ret ::= ∗ | expr [AS a] | ret , expr [AS a]                             return lists
 * }}}
 * Clauses {{{
 * clause ::= [OPTIONAL] MATCH pattern_tuple [WHERE expr]                  matching clauses
 *          | WITH ret [WHERE expr] | UNWIND expr AS a     where a ∈ A     relational clauses
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
  @inline def apply[A](f: A)(implicit fragment: CypherFragment[A]): String = fragment.toCypher(f)
  def define[A](toCypher0: A => String): CypherFragment[A] =
    new CypherFragment[A]{
      def toCypher(f: A): String = toCypher0(f)
    }

  implicit lazy val CypherFragmentIsContravariant: Contravariant[CypherFragment] =
    new Contravariant[CypherFragment] {
      def contramap[A, B](fa: CypherFragment[A])(f: B => A): CypherFragment[B] =
        define(fa.toCypher _ compose f)
    }

  sealed trait Known[+A] {
    self =>

    val fragment: A
    def toCypher: String

    def map[B](f: A => B): Known[B] = new Known[B] {
      val fragment: B = f(self.fragment)
      def toCypher: String = self.toCypher
    }

    def widen[B](implicit ev: A <:< B): Known[B] = this.asInstanceOf[Known[B]]

    override def toString: String = s"Known$fragment{ $toCypher }"
  }
  object Known {
    implicit def apply[A](f: A)(implicit cypherFragment: CypherFragment[A]): Known[A] =
      new Known[A] {
        val fragment: A = f
        lazy val toCypher: String = cypherFragment.toCypher(f)
      }
    implicit def functor[F[_]: Functor, A: CypherFragment](fa: F[A]): F[Known[A]] = fa.map(apply[A])
    implicit def bifunctor[F[_, _]: Bifunctor, A: CypherFragment, B: CypherFragment](fab: F[A, B]): F[Known[A], Known[B]] =
      fab.bimap(apply[A], apply[B])
  }
  // Explicit analog of implicit `Known.apply`
  implicit class KnownOps[A: CypherFragment](f: A) {
    @inline def known: Known[A] = Known(f)
  }


  sealed trait Expr[+T]
  object Expr {
    type Inv[T] = Expr[T]

    // // // Values and Variables // // //
    case class Lit[+A](value: A) extends Expr[A]
    trait Var[A] extends Expr[A] {
      val name: String
      val m: Manifest[A]

      override def toString: String = s"Var[$m]($name)"
    }
    case class Call[+A](func: String, params: scala.List[Known[Expr[_]]]) extends Expr[A]

    object Lit {
      implicit lazy val literalStringFragment: CypherFragment[Lit[String]] = define {
        case Lit(str) => "\"" + str.replaceAll("\"", "\\\"") + "\""
      }
      /** Warning: it does nothing to check whether the number can be represented in cypher (~Long~ equivalent). */
      implicit def literalNumericFragment[N: Numeric]: CypherFragment[Lit[N]] =
        literalToString.asInstanceOf[CypherFragment[Lit[N]]]
      implicit lazy val literalBooleanFragment: CypherFragment[Lit[Boolean]] =
        literalToString.asInstanceOf[CypherFragment[Lit[Boolean]]]

      private lazy val literalToString = define[Lit[_]](_.value.toString)
    }
    object Var {
      def apply[A: Manifest](nme: String): Var[A] = new Var[A] {
        val name: String = nme
        val m: Manifest[A] = manifest[A]
      }
      def unapply(expr: Expr[_]): Option[String] = PartialFunction.condOpt(expr) { case v: Var[_] => v.name }

      implicit def fragment[A]: CypherFragment[Var[A]] = instance.asInstanceOf[CypherFragment[Var[A]]]
      private lazy val instance = define[Var[_]](v => escapeName(v.name))
    }
    object Call {
      implicit def fragment[A]: CypherFragment[Call[A]] = instance.asInstanceOf[CypherFragment[Call[A]]]
      private lazy val instance = define[Call[_]] {
        case Call(func, params) => s"${escapeName(func)}(${params.map(_.toCypher).mkString(", ")})"
      }
    }

    // // // Maps // // //
    case class Map[+A](get: Predef.Map[String, Known[Expr[A]]]) extends Expr[Predef.Map[String, A]]
    case class Key[+A](map: Known[Expr[Predef.Map[String, _]]], key: String) extends Expr[A]

    object Map {
      implicit def fragment[A]: CypherFragment[Map[A]] = instance.asInstanceOf[CypherFragment[Map[A]]]
      lazy val instance: CypherFragment[Map[_]] = define(m => mapStr(m.get))
    }
    object Key {
      implicit def fragment[A]: CypherFragment[Key[A]] = instance.asInstanceOf[CypherFragment[Key[A]]]
      lazy val instance: CypherFragment[Key[_]] = define {
        case Key(m, k) => s"${m.toCypher}.${escapeName(k)}"
      }
    }

    // // // Lists // // //
    case class List[+A](get: scala.List[Known[Expr[A]]]) extends Expr[scala.List[A]]
    case class In[A](elem: Known[Expr[A]], list: Known[Expr[scala.List[A]]]) extends Expr[Boolean]
    case class AtIndex[A](list: Known[Expr[scala.List[A]]], index: Known[Expr[Long]]) extends Expr[A]
    case class AtRange[A](list: Known[Expr[scala.List[A]]], limits: Ior[Known[Expr[Long]], Known[Expr[Long]]]) extends Expr[scala.List[A]]
    case class Concat[A](list0: Known[Expr[scala.List[A]]], list1: Known[Expr[scala.List[A]]]) extends Expr[scala.List[A]]

    object List {
      implicit def fragment[A]: CypherFragment[List[A]] = instance.asInstanceOf[CypherFragment[List[A]]]
      private lazy val instance: CypherFragment[List[_]] = define {
        _.get.map(_.toCypher).mkString("[ ", ", ", " ]")
      }
    }
    object In {
      implicit def fragment[A]: CypherFragment[In[A]] = instance.asInstanceOf[CypherFragment[In[A]]]
      private lazy val instance: CypherFragment[In[_]] = define {
        case In(elem, list) => s"${elem.toCypher} IN ${list.toCypher}"
      }
    }
    private def atIndex(list: Known[Expr[scala.List[_]]], index: String) = s"${list.toCypher}[$index]"
    object AtIndex {
      implicit def fragment[A]: CypherFragment[AtIndex[A]] = instance.asInstanceOf[CypherFragment[AtIndex[A]]]
      private lazy val instance = define[AtIndex[_]] {
        case AtIndex(list, index) => atIndex(list, index.toCypher)
      }
    }
    object AtRange {
      implicit def fragment[A]: CypherFragment[AtRange[A]] = instance.asInstanceOf[CypherFragment[AtRange[A]]]
      private lazy val instance = define[AtRange[_]] {
        case AtRange(list, range) => atIndex(list, rangeStr(range))
      }
    }
    object Concat {
      implicit def fragment[A]: CypherFragment[Concat[A]] = instance.asInstanceOf[CypherFragment[Concat[A]]]
      private lazy val instance = define[Concat[_]] {
        case Concat(list0, list1) => s"${list0.toCypher} + ${list1.toCypher}"
      }
    }

    // // // Strings // // //
    case class StringExpr(left: Known[Expr[String]], right: Known[Expr[String]], op: StringExpr.Op) extends Expr[Boolean]
    object StringExpr {
      sealed trait Op
      case object StartsWith extends Op
      case object EndsWith   extends Op
      case object Contains   extends Op

      implicit lazy val fragment: CypherFragment[StringExpr] = define {
        case StringExpr(left, right, op) =>
          val opStr = op match {
            case StartsWith => "STARTS WITH"
            case EndsWith   => "ENDS WITH"
            case Contains   => "CONTAINS"
          }
          s"${left.toCypher} $opStr ${right.toCypher}"
      }
    }

    // // // Logic // // //
    sealed trait LogicExpr extends Expr[Boolean]
    case class LogicBinaryExpr(left: Known[Expr[Boolean]], right: Known[Expr[Boolean]], op: LogicExpr.BinaryOp) extends LogicExpr
    case class LogicNegationExpr(expr: Known[Expr[Boolean]]) extends LogicExpr
    object LogicExpr {
      sealed trait BinaryOp
      case object Or  extends BinaryOp
      case object And extends BinaryOp
      case object Xor extends BinaryOp

      implicit lazy val fragment: CypherFragment[LogicExpr] = define {
        case LogicNegationExpr(expr) => s"NOT ${expr.toCypher}"
        case LogicBinaryExpr(left, right, op) =>
          val opStr = op match {
            case Or  => "OR"
            case And => "AND"
            case Xor => "XOR"
          }
          s"${left.toCypher} $opStr ${right.toCypher}"
      }
    }

    // // // Compare // // //
    sealed trait CompareExpr extends Expr[Boolean]
    case class CompareBinaryExpr[A](left: Known[Expr[A]], right: Known[Expr[A]], op: CompareExpr.BinaryOp) extends CompareExpr
    case class CompareBinaryAnyExpr(left: Known[Expr[Any]], right: Known[Expr[Any]], op: CompareExpr.BinaryAnyOp) extends CompareExpr
    case class CompareUnaryExpr(expr: Known[Expr[Any]], op: CompareExpr.UnaryOp) extends CompareExpr

    object CompareExpr {
      sealed trait BinaryOp
      case object Lt  extends BinaryOp
      case object Lte extends BinaryOp
      case object Gte extends BinaryOp
      case object Gt  extends BinaryOp

      sealed trait BinaryAnyOp
      case object Eq  extends BinaryAnyOp
      case object Neq extends BinaryAnyOp

      sealed trait UnaryOp
      case object IsNull  extends UnaryOp
      case object NotNull extends UnaryOp

      implicit lazy val fragment: CypherFragment[CompareExpr] = define {
        case CompareBinaryExpr(left, right, op) =>
          val opStr = op match {
            case Lt  => "<"
            case Lte => "<="
            case Gte => ">="
            case Gt  => ">"
          }
          s"${left.toCypher} $opStr ${right.toCypher}"
        case CompareBinaryAnyExpr(left, right, op) =>
          val opStr = op match {
            case Eq  => "="
            case Neq => "<>"
          }
          s"${left.toCypher} $opStr ${right.toCypher}"
        case CompareUnaryExpr(expr, op) =>
          val opStr = op match {
            case IsNull  => "IS NULL"
            case NotNull => "IS NOT NULL"
          }
          s"${expr.toCypher} $opStr"
      }
    }

    case class Distinct[A](expr: Known[Expr[A]]) extends Expr[A]
    object Distinct {
      implicit def fragment[A]: CypherFragment[Distinct[A]] = define {
        case Distinct(expr) => s"DISTINCT ${expr.toCypher}"
      }
    }
  }

  sealed trait Query[+A]
  object Query {
    case class Union[+A, +B](left: Known[Query[A]], right: Known[Query[B]], all: Boolean) extends Query[(A, B)] // TODO

    sealed trait Query0[+A] extends Query[A]
    case class Return[+A](ret: Known[CypherFragment.Return[A]]) extends Query0[A]
    case class Clause[+A](clause: Known[CypherFragment.Clause], query: Known[Query0[A]]) extends Query0[A]

    implicit def fragment[A]: CypherFragment[Query[A]] = instance.asInstanceOf[CypherFragment[Query[A]]]
    private lazy val instance = define[Query[Any]] {
      case Union(left, right, all) => s"${left.toCypher} UNION ${if (all) "ALL " else ""}${right.toCypher}"
      case Clause(clause, query) => s"${clause.toCypher} ${query.toCypher}"
      case Return(ret) => s"RETURN ${ret.toCypher}"
    }
  }

  sealed trait Return[+A]
  object Return {

    sealed trait Return0[+A] extends Return[A]
    sealed trait Return1[+A] extends Return0[A]

    type Ascending = Boolean
    type Order = Map[Known[CypherFragment.Expr[_]], Ascending]

    trait Options[+A] extends Return[A] {
      val ret: Known[Return0[A]]
      val distinct: Boolean
      val order: Order
      val skip: Option[Long]
      val limit: Option[Long]
    }
    object Options {
      type Inv[A] = Options[A]

      def apply[A](ret0: Known[Return0[A]], distinct0: Boolean, order0: Order, skip0: Option[Long], limit0: Option[Long]): Options[A] =
        new Options[A] {
          val ret: Known[Return0[A]] = ret0
          val distinct: Boolean = distinct0
          val order: Order = order0
          val skip: Option[Long] = skip0
          val limit: Option[Long] = limit0
        }

      def unapply[A](ret: Return[A]): Option[(Known[Return0[A]], Boolean, Order, Option[Long], Option[Long])] = PartialFunction.condOpt(ret) {
        case ops: Options[A @unchecked] => (ops.ret, ops.distinct, ops.order, ops.skip, ops.limit)
      }
    }

    case object All extends Return1[Any] {
      def as[A]: Return1[A] = this.asInstanceOf[Return1[A]]
    }
    type All = All.type

    case class Expr[+A](expr: Known[CypherFragment.Expr[A]], as: Option[String]) extends Return1[A]


    /** Isn't completely compatible with cypher syntax since it doesn't permit to return ∗ as first element of a list. */
    sealed trait UntypedList extends Return0[scala.List[Any]] {
      val toList: scala.List[Known[Expr[_]]]

      override def toString: String = s"RetUntypedList(${toList.mkString(", ")})"
    }

    /** Isn't completely compatible with cypher syntax since it doesn't permit to return ∗ as first element of a list. */
    sealed trait List[L <: HList] extends Return0[L] {
      type Expressions <: HList
      val expressions: Expressions
      val toList: scala.List[Known[Expr[_]]]

      override def toString: String = s"RetList(${toList.mkString(", ")})"
    }
    object List extends ProductArgs {
      def untypedReturns(exprs: Known[Return.Expr[_]]*): UntypedList = new UntypedList { val toList: scala.List[Known[Expr[_]]] = exprs.toList }
      def untyped(exprs: Iterable[Known[CypherFragment.Expr[_]]]): UntypedList = new UntypedList {
        val toList: scala.List[Known[Expr[_]]] = exprs.toList.map(Return.Expr[Any](_, as = None).known)
      }

      type Aux[L <: HList, E <: HList] = List[L] { type Expressions = E }

      sealed trait Build[L <: HList] {
        type Ret  <: HList
        type Expr <: HList
        type Out = List.Aux[Ret, Expr]
        def apply(l: L): Out
      }
      object Build {
        type Aux[L <: HList, R <: HList, E <: HList] = Build[L] { type Ret = R; type Expr = E }

        object KnownHList extends Poly2 {
          implicit def impl[A, E <: CypherFragment.Expr[_]](
            implicit ev: E <:< CypherFragment.Expr[A], fragment: CypherFragment[E]
          ): Case.Aux[A, E, Known[Return.Expr[A]]] =
            at[A, E]((_, e) => Known(Return.Expr(Known(e).widen, None)))
        }

        object NullF extends Poly0 {
          implicit def impl[A]: Case0[A] = at[A](null.asInstanceOf[A])
        }

        implicit def listOfExpressions[L <: HList, RT <: HList, RE <: HList, RK <: HList](
          implicit
          nonEmpty: ops.hlist.IsHCons[L],
          stripExpr: ComappedCov.Aux[L, CypherFragment.Expr, RT],
          nulls: ops.hlist.FillWith[NullF.type, RT],
          known: ops.hlist.ZipWith.Aux[RT, L, KnownHList.type, RK],
          list: ops.hlist.ToTraversable.Aux[RK, scala.List, Known[Return.Expr[_]]]
        ): Aux[L, RT, RK] =
          new Build[L] {
            type Ret = RT
            type Expr = RK
            def apply(l: L): Out =
              new List[RT] {
                type Expressions = RK
                val expressions: RK = known(nulls(), l)
                val toList: scala.List[Known[Return.Expr[_]]] = expressions.toList
              }
          }
      }

      def applyProduct[L <: HList](l: L)(implicit build: Build[L]): build.Out = build(l)
      def fromHList[L <: HList](l: L)(implicit build: Build[L]): build.Out = build(l)

      def unapply(ret: Return[_]): Option[scala.List[Known[Expr[_]]]] = PartialFunction.condOpt(ret) {
        case list: List[_]     => list.toList
        case list: UntypedList => list.toList
      }
    }

    implicit def fragment[A]: CypherFragment[Return[A]] = instance.asInstanceOf[CypherFragment[Return[A]]]
    private lazy val instance = define[Return[Any]] {
      case All => "*"
      case Expr(expr, as) => expr.toCypher + asStr(as)
      case List(head :: Nil) => head.toCypher
      case List(head :: tail) => s"${head.toCypher}, ${tail.map(_.toCypher).mkString(", ")}"
      case Options(expr, distinct, order, skip, limit) =>
        val distinctFrag = if (distinct) "DISTINCT " else ""
        val orderFrag =
          if (order.isEmpty) ""
          else {
            val by = order.map{
              case (e, true)  => e.toCypher
              case (e, false) => s"${e.toCypher} DESC"
            }
            " ORDER BY " + by.mkString(", ")
          }
        val skipFrag = skip map (" SKIP " + _) getOrElse ""
        val limitFrag = limit map (" LIMIT " + _) getOrElse ""
        s"$distinctFrag${expr.toCypher}$orderFrag$skipFrag$limitFrag"
    }
  }

  sealed trait Clause
  object Clause {
    case class Match(pattern: PatternTuple, optional: Boolean, where: Option[Known[Expr[Boolean]]]) extends Clause
    case class With(ret: Known[Return[_]], where: Option[Known[Expr[Boolean]]]) extends Clause
    case class Unwind(expr: Known[Expr[List[_]]], as: String) extends Clause

    implicit lazy val fragment: CypherFragment[Clause] = define[Clause] {
      case Match(pattern, optional, where) =>
        val optionalStr = if (optional) "OPTIONAL " else ""
        val patternStr = pattern.toList.map(_.toCypher).mkString(", ")
        s"${optionalStr}MATCH $patternStr${whereStr(where)}"
      case With(ret, where) => s"WITH ${ret.toCypher}${whereStr(where)}"
      case Unwind(expr, as) => s"UNWIND ${expr.toCypher}${asStr(Option(as))}"
    }
  }


  type PatternTuple = NonEmptyList[Known[Pattern]]
  sealed trait Pattern
  object Pattern {
    case class Let[+A](alias: String, pattern: Known[Pattern0]) extends Pattern

    sealed trait Pattern0 extends Pattern
    case class Node(alias: Option[String], labels: List[String], map: Map[String, Known[Expr[_]]]) extends Pattern0
    case class Path(left: Known[Node], rel: Known[Rel], right: Known[Pattern0]) extends Pattern0
    case class Rel(alias: Option[String], types: List[String], map: Map[String, Known[Expr[_]]], length: Option[Rel.Length], dir: Rel.Direction) extends Pattern

    object Rel {
      sealed trait Length
      case object All extends Length
      case class Range(limits: Ior[Long, Long]) extends Length

      sealed trait Direction
      case object Outgoing extends Direction
      case object Incoming extends Direction
      case object Any extends Direction
    }

    implicit lazy val fragment: CypherFragment[Pattern] = define[Pattern] {
      case Let(alias, pattern) => s"${escapeName(alias)} = ${pattern.toCypher}"
      case Node(alias, labels, map) => s"(${aliasStr(alias)}${labelsStr(labels)}${mapStr(map)})"
      case Path(left, rel, right) => s"${left.toCypher} ${rel.toCypher} ${right.toCypher}"
      case Rel(alias, types, map, len, dir) =>
        val lenStr = len match {
          case None => ""
          case Some(Rel.All) => "*"
          case Some(Rel.Range(range)) => "*" + rangeStr(range.bimap(Expr.Lit(_), Expr.Lit(_)))
        }
        val params = s"[${aliasStr(alias)}${typesStr(types)}$lenStr${mapStr(map)}]"
        dir match {
          case Rel.Outgoing => s"-$params->"
          case Rel.Incoming => s"<-$params-"
          case Rel.Any      => s"-$params-"
        }
    }

    private def aliasStr(alias: Option[String]) = alias.map(escapeName).getOrElse("")
    private def labelsStr(labels: List[String]) = labelLikeStr(labels, ":")
    private def typesStr(types: List[String]) = labelLikeStr(types, "|")
    private def labelLikeStr(xs: List[String], sep: String) = xs match {
      case Nil => ""
      case _ => xs.map(escapeName).mkString(":", sep, "")
    }

    /** Typeclass building a pattern from HList of pattern elements. */
    trait HBuilder[Ps <: HList] extends DepFn1[Ps] { type Out = Pattern0 }
    object HBuilder {

      implicit def singleNodePatternBuilder: HBuilder[Node #: HNil] = instance0
      private lazy val instance0 = new HBuilder[Node #: HNil] {
        def apply(t: Node #: HNil): Node = t.head
      }

      implicit def pathPatternBuilder[T <: HList](implicit right: HBuilder[T]): HBuilder[Node #: Rel #: T] =
        new HBuilder[Node #: Rel #: T] {
          def apply(t: Node #: Rel #: T): Path = Path(t.head, t.tail.head, right(t.tail.tail))
        }
    }
  }

  private def escapeName(name: String) = "`" + name.replaceAll("`", "``") + "`"
  private def whereStr(where: Option[Known[Expr[Boolean]]]) = where.map(" WHERE " + _.toCypher).getOrElse("")
  private def asStr(as: Option[String]) = as.map(escapeName).map(" AS " + _).getOrElse("")
  private def mapStr(map: Map[String, Known[Expr[_]]]) =
    if (map.isEmpty) ""
    else map.map{ case (k, v) => s"${escapeName(k)}: ${v.toCypher}" }.mkString("{ ", ", ", " }")
  private def rangeStr(ior: Ior[Known[_], Known[_]]) = ior match {
      case Ior.Left(min)      => s"${min.toCypher}.."
      case Ior.Right(max)     => s"..${max.toCypher}"
      case Ior.Both(min, max) => s"${min.toCypher}..${max.toCypher}"
  }
}
