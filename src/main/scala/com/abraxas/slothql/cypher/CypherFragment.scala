package com.abraxas.slothql.cypher

import scala.language.{ higherKinds, implicitConversions }

import cats.arrow.Arrow
import cats.{ Bifunctor, Contravariant, Functor }
import cats.data.{ Ior, NonEmptyList }
import cats.instances.function._
import cats.syntax.bifunctor._
import cats.syntax.functor._
import shapeless.{ :: => #:, _ }
import shapeless.syntax.singleton._

import com.abraxas.slothql.util.ComappedCov
import com.abraxas.slothql.util.Bool.{ False, True }

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
    case class Lit[A](value: A)(implicit val m: Manifest[A]) extends Expr[A]
    trait Var[A] extends Expr[A] {
      val name: String
      val m: Manifest[A]

      override def toString: String = s"Var[$m]($name)"
    }
    case class Call[A](func: String, params: scala.List[Known[Expr[_]]]) extends Expr[A]

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
    case class Map[A](get: Predef.Map[String, Known[Expr[A]]]) extends Expr[Predef.Map[String, A]]
    case class Key[A](map: Known[Expr[Predef.Map[String, _]]], key: String)(implicit val m: Manifest[A]) extends Expr[A]

    object Map {
      implicit lazy val fragment: CypherFragment[Map[_]] = define(m => mapStr(m.get))
    }
    object Key {
      implicit def fragment[A]: CypherFragment[Key[A]] = instance.asInstanceOf[CypherFragment[Key[A]]]
      implicit lazy val instance: CypherFragment[Key[_]] = define {
        case Key(m, k) => s"${m.toCypher}.${escapeName(k)}"
      }
    }

    // // // Lists // // //
    case class List[A](get: scala.List[Known[Expr[A]]]) extends Expr[scala.List[A]]
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
  }

  sealed trait Query[+A]
  object Query {
    case class Union[+A, +B](left: Known[Query[A]], right: Known[Query[B]], all: Boolean) extends Query[(A, B)] // TODO

    sealed trait Query0[+A] extends Query[A]
    case class Return[+A](ret: Known[CypherFragment.Return[A]]) extends Query0[A]

    trait Clause[+A] extends Query0[A] {
      type Clause <: CypherFragment.Clause
      type Query  <: Query0[A]

      val clause: Known[CypherFragment.Clause]
      val query: Known[Query0[A]]
    }
    object Clause {
      type Aux[+A, Clause0 <: CypherFragment.Clause, Q <: Query0[A]] =
        Clause[A] { type Clause <: Clause0; type Query = Q }

      def apply[A](clause: Known[CypherFragment.Clause], query: Known[Query0[A]]): Clause[A] = {
        @inline def clause0 = clause
        @inline def query0 = query
        new Clause[A] {
          type Clause = Nothing
          type Query  = Nothing
          val clause: Known[CypherFragment.Clause] = clause0
          val query: Known[Query0[A]] = query0
        }
      }
      def unapply[A](q: Query[A]): Option[(Known[CypherFragment.Clause], Known[Query0[A]])] =
        PartialFunction.condOpt(q) { case c: Clause[A @unchecked] => c.clause -> c.query }

      def typed[A, Clause0 <: CypherFragment.Clause: CypherFragment, Q[x] <: Query0[x]](clause: Clause0, query: Q[A])(
        implicit qFrag: CypherFragment[Q[A]]
      ): Clause.Aux[A, Clause0, Q[A]] = {
        @inline def clause0 = clause
        @inline def query0 = query
        new Clause[A] {
          type Clause = Clause0
          type Query = Q[A]
          val clause: Known[CypherFragment.Clause] = clause0
          val query: Known[Q[A]] = Known(query0)(qFrag)
        }
      }
    }

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

      protected[slothql] object Internal {
        trait Ops[+A] extends Return[A] {
          protected[slothql] val options: Known[Options[A]]
        }
      }
    }

    case object All extends Return1[Any]
    type All = All.type

    case class Expr[+A](expr: Known[CypherFragment.Expr[A]], as: Option[String]) extends Return1[A]

    /** Isn't completely compatible with cypher syntax since it doesn't permit to return ∗ as first element of a list. */
    sealed trait List[L <: HList] extends Return0[L] {
      type Expressions <: HList
      val expressions: Expressions
      val toList: scala.List[Known[Expr[_]]]

      override def toString: String = s"RetList(${toList.mkString(", ")})"
    }
    object List extends ProductArgs {
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
        case list: List[_] => list.toList
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
      case ops: Options.Internal.Ops[_] => ops.options.toCypher
    }
  }

  sealed trait Clause
  object Clause {

    trait Match extends Clause {
      type Pattern  <: PatternTuple
      type Optional <: Boolean
      type Where    <: Option[Expr[Boolean]]

      val pattern: PatternTuple
      val optional: Boolean
      val where: Option[Known[Expr[Boolean]]]
    }
    case class With(ret: Known[Return[_]], where: Option[Known[Expr[Boolean]]]) extends Clause
    case class Unwind(expr: Known[Expr[List[_]]], as: String) extends Clause

    object Match {
      type Aux[Pattern0 <: PatternTuple, Optional0 <: Boolean, Where0 <: Option[Expr[Boolean]]] =
        Match { type Pattern = Pattern0; type Optional = Optional0; type Where = Where0 }

      def apply(pattern: PatternTuple, optional: Boolean, where: Option[Known[Expr[Boolean]]]): Match = {
        @inline val pattern0 = pattern
        @inline val optional0 = optional
        @inline val where0 = where
        new Match {
          type Pattern  = Nothing
          type Optional = Nothing
          type Where    = Nothing
          val pattern: PatternTuple = pattern0
          val optional: Boolean = optional0
          val where: Option[Known[Expr[Boolean]]] = where0
        }
      }
      def unapply(clause: Clause): Option[(PatternTuple, Boolean, Option[Known[Expr[Boolean]]])] =
        PartialFunction.condOpt(clause) { case m: Match => (m.pattern, m.optional, m.where) }

      def typed[PT <: PatternTuple, Opt <: Boolean: (True |∨| False)#λ](pattern: PT, optional: Opt): Match.Aux[PT, Opt, None.type] = {
        @inline def pattern0 = pattern
        @inline def optional0 = optional
        new Match {
          type Pattern  = PT
          type Optional = Opt
          type Where    = None.type
          val pattern: PatternTuple = pattern0
          val optional: Boolean = optional0
          val where: Option[Known[Expr[Boolean]]] = None
        }
      }
      def typed[PT <: PatternTuple, Opt <: Boolean: (True |∨| False)#λ, W <: Expr[Boolean]: CypherFragment](
        pattern: PT, optional: Opt, where: W
      ): Match.Aux[PT, Opt, Some[W]] = {
        @inline def pattern0 = pattern
        @inline def optional0 = optional
        @inline def where0 = where
        new Match {
          type Pattern  = PT
          type Optional = Opt
          type Where    = Some[W]
          val pattern: PatternTuple = pattern0
          val optional: Boolean = optional0
          val where: Option[Known[Expr[Boolean]]] = Some(where0.known.widen)
        }
      }

      def optional[PT <: PatternTuple](pattern: PT): Match.Aux[PT, True, None.type] =
        typed(pattern, true.narrow)
      def optional[PT <: PatternTuple, W <: Expr[Boolean]: CypherFragment](pattern: PT, where: W): Match.Aux[PT, True, Some[W]] =
        typed(pattern, true.narrow, where)

      /** Opposite to `optional`. */
      def strict[PT <: PatternTuple](pattern: PT): Match.Aux[PT, False, None.type] =
        typed(pattern, false.narrow)
      /** Opposite to `optional`. */
      def strict[PT <: PatternTuple, W <: Expr[Boolean]: CypherFragment](pattern: PT, where: W): Match.Aux[PT, False, Some[W]] =
        typed(pattern, false.narrow, where)
    }

    implicit lazy val fragment: CypherFragment[Clause] = define[Clause] {
      case Match(pattern, optional, where) =>
        val optionalStr = if (optional) "OPTIONAL " else ""
        val patternStr = pattern.toList.map(_.toCypher).mkString(", ")
        s"${optionalStr}MATCH $patternStr${whereStr(where)}"
      case With(ret, where) => s"WITH ${ret.toCypher}${whereStr(where)}"
      case Unwind(expr, as) => s"UNWIND ${expr.toCypher}${asStr(Option(as))}"
    }
  }


  sealed trait PatternTuple {
    type Patterns <: HList
    val toNel: NonEmptyList[Known[Pattern]]
    def toList: List[Known[Pattern]] = toNel.toList
  }
  object PatternTuple {
    type Aux[Patterns0 <: HList] = PatternTuple { type Patterns = Patterns0 }
    def apply(nel: NonEmptyList[Known[Pattern]]): PatternTuple =
      new PatternTuple {
        type Patterns = Nothing
        val toNel: NonEmptyList[Known[Pattern]] = nel
      }
    def apply(h: Known[Pattern], t: Known[Pattern]*): PatternTuple = apply(NonEmptyList(h, t.toList))

    def unapplySeq(pt: PatternTuple): Option[List[Known[Pattern]]] = Some(pt.toList)

    object typed extends ProductArgs {
      def applyProduct[Ps <: HList, KnownPs <: HList](patterns: Ps)(
        implicit
        nonEmpty: Cached[ops.hlist.IsHCons[Ps]],
        known: Cached[ops.hlist.Mapper.Aux[Builders.LiftKnown.type, Ps, KnownPs]],
        knownToList: Cached[ops.hlist.ToTraversable.Aux[KnownPs, List, Known[Pattern]]]
      ): PatternTuple.Aux[Ps] =
        new PatternTuple {
          type Patterns = Ps
          val toNel: NonEmptyList[Known[Pattern]] = NonEmptyList.fromListUnsafe(knownToList.value(known.value(patterns)))
        }
    }
  }

  sealed trait Pattern
  object Pattern {
    case class Let[+A](alias: String, pattern: Known[Pattern0]) extends Pattern

    sealed trait Pattern0 extends Pattern
    trait Node extends Pattern0 {
      type Alias  <: Option[String]
      type Labels <: HList
      type Values <: HList

      val alias: Option[String]
      val labels: List[String]
      val values: Map[String, Known[Expr[_]]]
    }

    case class Path[H <: Node, R <: Rel, T <: Pattern0](left: Known[H], rel: Known[R], right: Known[T]) extends Pattern0

    trait Rel extends Pattern {
      type Alias  <: Option[String]
      type Types  <: HList
      type Values <: HList
      type Length <: Option[Rel.Length]
      type Dir    <: Rel.Direction

      val alias: Option[String]
      val types: List[String]
      val values: Map[String, Known[Expr[_]]]
      val length: Option[Rel.Length]
      val dir: Rel.Direction
    }


    object Node {
      type Aux[Alias0 <: Option[String], Labels0 <: HList, Values0 <: HList] =
        Node { type Alias = Alias0; type Labels = Labels0; type Values = Values0 }

      def apply(alias: Option[String], labels: List[String], values: Map[String, Known[Expr[_]]]): Node = {
        @inline def alias0 = alias
        @inline def labels0 = labels
        @inline def values0 = values
        new Node {
          type Alias = Nothing
          type Labels = Nothing
          type Values = Nothing
          val alias: Option[String] = alias0
          val labels: List[String] = labels0
          val values: Map[String, Known[Expr[_]]] = values0
        }
      }
      def unapply(pattern: Pattern): Option[(Option[String], List[String], Map[String, Known[Expr[_]]])] =
        PartialFunction.condOpt(pattern) { case node: Node => (node.alias, node.labels, node.values) }

      def typed: Builder[None.type, Nothing, HNil] = new Builder[None.type, Nothing, HNil](None, Nil, Map())

      protected class Builder[Alias <: Option[String], Labels <: HList, Values <: HList](
        alias: Option[String],
        labels: List[String],
        values: Map[String, Known[Expr[_]]]
      ) {
        builder =>

        def withAlias(alias: String): Builder[Some[alias.type], Labels, Values] = copy(alias0 = Some(alias))
        def withoutAlias: Builder[None.type, Labels, Values] = copy(alias0 = None)

        object withLabels extends SingletonProductArgs {
          def applyProduct[Labels0 <: HList](labels: Labels0)(
            implicit toList: Cached[ops.hlist.ToTraversable.Aux[Labels0, List, String]]
          ): Builder[Alias, Labels0, Values] =
            copy(labels0 = toList.value(labels))
        }

        object withValues extends RecordArgs {
          def applyRecord[Args <: HList, KnownRecords <: HList, KnownPairs <: HList](args: Args)(
            implicit
            map: Cached[ops.record.MapValues.Aux[Builders.LiftKnown.type, Args, KnownRecords]],
            fields: Cached[ops.record.Fields.Aux[KnownRecords, KnownPairs]],
            toList: Cached[ops.hlist.ToTraversable.Aux[KnownPairs, List, (Symbol, Known[Expr[_]])]]
            // I couldn't make it with `ops.record.ToMap`
          ): Builder[Alias, Labels, Args] = {
            val l = toList.value(fields.value(map.value(args)))
            val m = l.map(Arrow[Function1].first(_.name)).toMap
            copy(values0 = m)
          }
        }

        def build(
          implicit defAlias: Alias =:!= Nothing, defLabels: Labels =:!= Nothing, defValues: Values =:!= Nothing
        ): Node.Aux[Alias, Labels, Values] = {
          type Alias0 = Alias
          type Labels0 = Labels
          type Values0 = Values
          new Node {
            type Alias = Alias0
            type Labels = Labels0
            type Values = Values0
            val alias: Option[String] = builder.alias
            val labels: List[String] = builder.labels
            val values: Map[String, Known[Expr[_]]] = builder.values
          }
        }

        private def copy[Alias0 <: Option[String], Labels0 <: HList, Values0 <: HList](
          alias0: Option[String] = alias, labels0: List[String] = labels, values0: Map[String, Known[Expr[_]]] = values
        ): Builder[Alias0, Labels0, Values0] = new Builder[Alias0, Labels0, Values0](alias0, labels0, values0)
      }
    }

    object Rel {
      type Aux[Alias0 <: Option[String], Types0 <: HList, Values0 <: HList, Length0 <: Option[Rel.Length], Dir0 <: Rel.Direction] =
        Rel { type Alias = Alias0; type Types = Types0; type Values = Values0; type Length = Length0; type Dir = Dir0 }

      def apply(alias: Option[String], types: List[String], values: Map[String, Known[Expr[_]]], length: Option[Rel.Length], dir: Rel.Direction): Rel = {
        @inline def alias0 = alias
        @inline def types0 = types
        @inline def values0 = values
        @inline def length0 = length
        @inline def dir0 = dir
        new Rel {
          type Alias  = Nothing
          type Types  = Nothing
          type Values = Nothing
          type Length = Nothing
          type Dir    = Nothing

          val alias: Option[String] = alias0
          val types: List[String] = types0
          val values: Map[String, Known[Expr[_]]] = values0
          val length: Option[Rel.Length] = length0
          val dir: Direction = dir0
        }
      }
      def unapply(pattern: Pattern): Option[(Option[String], List[String], Map[String, Known[Expr[_]]], Option[Rel.Length], Rel.Direction)] =
        PartialFunction.condOpt(pattern) { case rel: Rel => (rel.alias, rel.types, rel.values, rel.length, rel.dir) }

      def typed: Builder[None.type, Nothing, HNil, None.type, Nothing] = new Builder[None.type, Nothing, HNil, None.type, Nothing](None, Nil, Map(), None, null)

      // TODO: partial duplication of `Node.Builder`
      protected class Builder[Alias <: Option[String], Types <: HList, Values <: HList, Length <: Option[Rel.Length], Dir <: Rel.Direction](
        alias: Option[String],
        types: List[String],
        values: Map[String, Known[Expr[_]]],
        length: Option[Rel.Length],
        dir: Direction
      ) {
        builder =>

        def withAlias(alias: String): Builder[Some[alias.type], Types, Values, Length, Dir] = copy(alias0 = Some(alias))
        def withoutAlias: Builder[None.type, Types, Values, Length, Dir] = copy(alias0 = None)

        object withTypes extends SingletonProductArgs {
          def applyProduct[Labels0 <: HList](labels: Labels0)(implicit toList: Cached[ops.hlist.ToTraversable.Aux[Labels0, List, String]]): Builder[Alias, Labels0, Values, Length, Dir] =
            copy(types0 = toList.value(labels))
        }

        object withValues extends RecordArgs {
          def applyRecord[Args <: HList, KnownRecords <: HList, KnownPairs <: HList](args: Args)(
            implicit
            map: Cached[ops.record.MapValues.Aux[Builders.LiftKnown.type, Args, KnownRecords]],
            fields: Cached[ops.record.Fields.Aux[KnownRecords, KnownPairs]],
            toList: Cached[ops.hlist.ToTraversable.Aux[KnownPairs, List, (Symbol, Known[Expr[_]])]]
            // I couldn't make it with `ops.record.ToMap`
          ): Builder[Alias, Types, Args, Length, Dir] = {
            val l = toList.value(fields.value(map.value(args)))
            val m = l.map(Arrow[Function1].first(_.name)).toMap
            copy(values0 = m)
          }
        }

        def withLength[Rng <: Ior[Long, Long]](range: Rng)(implicit len: Cached[Range.IorToLength[Rng]]): Builder[Alias, Types, Values, Some[len.value.Length], Dir] =
          copy(length0 = Some(len.value(range)))
        def withLength[Len <: Rel.Length](len: Len): Builder[Alias, Types, Values, Some[Len], Dir] = copy(length0 = Some(len))
        def anyLength: Builder[Alias, Types, Values, Some[Rel.All.type], Dir] = copy(length0 = Some(Rel.All))

        def withDirection[Dir0 <: Rel.Direction](dir: Dir0): Builder[Alias, Types, Values, Length, Dir0] = copy(dir0 = dir)

        def build(
          implicit
          defAlias: Alias =:!= Nothing, defLabels: Types =:!= Nothing, defValues: Values =:!= Nothing,
          defLength: Length =:!= Nothing, defDir: Dir =:!= Nothing
        ): Rel.Aux[Alias, Types, Values, Length, Dir] = {
          type Alias0  = Alias
          type Types0  = Types
          type Values0 = Values
          type Length0 = Length
          type Dir0    = Dir
          new Rel {
            type Alias  = Alias0
            type Types  = Types0
            type Values = Values0
            type Length = Length0
            type Dir    = Dir0
            val alias: Option[String] = builder.alias
            val types: List[String] = builder.types
            val values: Map[String, Known[Expr[_]]] = builder.values
            val length: Option[Rel.Length] = builder.length
            val dir: Direction = builder.dir
          }
        }

        private def copy[Alias0 <: Option[String], Types0 <: HList, Values0 <: HList, Length0 <: Option[Rel.Length], Dir0 <: Rel.Direction](
          alias0: Option[String] = alias, types0: List[String] = types, values0: Map[String, Known[Expr[_]]] = values,
          length0: Option[Rel.Length] = length, dir0: Direction = dir
        ): Builder[Alias0, Types0, Values0, Length0, Dir0] = new Builder[Alias0, Types0, Values0, Length0, Dir0](alias0, types0, values0, length0, dir0)
      }



      sealed trait Length
      case object All extends Length
      trait Range extends Length {
        type Min <: Option[Long]
        type Max <: Option[Long]
        val limits: Ior[Long, Long]
      }
      object Range {
        type Aux[Min0 <: Option[Long], Max0 <: Option[Long]] = Range { type Min = Min0; type Max = Max0 }
        def apply(limits: Ior[Long, Long]): Range = {
          @inline val limits0 = limits
          new Range {
            type Min = Nothing
            type Max = Nothing
            val limits: Ior[Long, Long] = limits0
          }
        }
        def unapply(len: Length): Option[Ior[Long, Long]] = PartialFunction.condOpt(len) { case rng: Range => rng.limits }

        def typed[Rng <: Ior[_, _]](rng: Rng)(implicit len: Cached[IorToLength[Rng]]): len.value.Length = len.value(rng)

        trait IorToLength[Rng <: Ior[_, _]] {
          type Length <: Rel.Length
          def apply(rng: Rng): Length
        }
        object IorToLength {
          type Aux[Rng <: Ior[_, _], Length0 <: Rel.Length] = IorToLength[Rng] { type Length = Length0 }
          implicit def leftToLength[N: Integral]: Aux[Ior.Left[N], Rel.Range.Aux[Some[Long], None.type]] =
            new IorToLength[Ior.Left[N]] {
              type Length = Rel.Range.Aux[Some[Long], None.type]
              def apply(rng: Ior.Left[N]): Range.Aux[Some[Long], None.type] =
                new Range {
                  type Min = Some[Long]
                  type Max = None.type
                  val limits: Ior[Long, Long] = (rng: Ior[N, Long]).leftMap(implicitly[Numeric[N]].toLong)
                }
            }
          implicit def rightToLength[N: Integral]: Aux[Ior.Right[N], Rel.Range.Aux[None.type, Some[Long]]] =
            new IorToLength[Ior.Right[N]] {
              type Length = Rel.Range.Aux[None.type, Some[Long]]
              def apply(rng: Ior.Right[N]): Range.Aux[None.type, Some[Long]] =
                new Range {
                  type Min = None.type
                  type Max = Some[Long]
                  val limits: Ior[Long, Long] = (rng: Ior[Long, N]).map(implicitly[Numeric[N]].toLong)
                }
            }
          implicit def bothToLength[L: Integral, R: Integral]: Aux[Ior.Both[L, R], Rel.Range.Aux[Some[Long], Some[Long]]] =
            new IorToLength[Ior.Both[L, R]] {
              type Length = Rel.Range.Aux[Some[Long], Some[Long]]
              def apply(rng: Ior.Both[L, R]): Range.Aux[Some[Long], Some[Long]] =
                new Range {
                  type Min = Some[Long]
                  type Max = Some[Long]
                  val limits: Ior[Long, Long] = (rng: Ior[L, R]).bimap(implicitly[Numeric[L]].toLong, implicitly[Numeric[R]].toLong)
                }
            }
        }
      }

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
  }

  object Builders {
    object LiftKnown extends Poly1 {
      implicit def impl[E: CypherFragment]: Case.Aux[E, Known[E]] = at[E](Known(_))
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
