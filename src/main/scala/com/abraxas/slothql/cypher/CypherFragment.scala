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

    sealed trait Var[+A] extends Expr[A] {
      type Name <: String
      val name: Name
    }

    sealed trait Call[+A] extends Expr[A] {
      type Name <: String
      val name: Name

      type Params <: HList
      val params: Params
      val paramsList: scala.List[Known[Expr[_]]]
    }


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
      type Aux[+A, Name0 <: String] = Var[A] { type Name = Name0 }
      def apply[A](name0: String): Var.Aux[A, name0.type] =
        new Var[A] {
          type Name = name0.type
          val name: Name = name0
        }
      def unapply(expr: Expr[_]): Option[String] = PartialFunction.condOpt(expr) {
        case v: Var[_] => v.name
      }

      implicit def fragment[A]: CypherFragment[Var[A]] = instance.asInstanceOf[CypherFragment[Var[A]]]
      private lazy val instance = define[Var[_]](v => escapeName(v.name))
    }

    object Call {
      type Aux[+A, Name0 <: String, Params0 <: HList] = Call[A] { type Name = Name0; type Params = Params0 }
      def apply[A]: Builder[A] = Builder.asInstanceOf[Builder[A]]
      def unapply(expr: Expr[_]): Option[(String, scala.List[Known[Expr[_]]])] = PartialFunction.condOpt(expr) {
        case call: Call[_] => call.name -> call.paramsList
      }

      protected class Builder[A] {
        def apply[Params0 <: HList](name0: String, params0: Params0)(
          implicit
          toList: ops.hlist.ToTraversable.Aux[Params0, scala.List, Known[Expr[_]]]
        ): Call.Aux[A, name0.type, Params0] =
          new Call[A] {
            type Name = name0.type
            val name: name0.type = name0
            type Params = Params0
            val params: Params0 = params0
            lazy val paramsList: scala.List[Known[Expr[_]]] = params0.toList
          }
      }
      private object Builder extends Builder[Any]

      implicit def fragment[A]: CypherFragment[Call[A]] = instance.asInstanceOf[CypherFragment[Call[A]]]
      private lazy val instance = define[Call[_]] {
        case Call(func, params) => s"${escapeName(func)}(${params.map(_.toCypher).mkString(", ")})"
      }
    }

    // // // Maps // // //
    sealed trait Map extends Expr[Predef.Map[String, Expr[_]]] {
      type Records <: HList
      val records: Records
      val asMap: Predef.Map[String, Known[Expr[_]]]
    }
    type MapExpr = Expr[MapExpr0]
    type MapExpr0 = Predef.Map[String, Known[Expr[_]]]

    sealed trait Key[+A] extends Expr[A] {
      type Key <: String
      val key: Key

      type Map <: MapExpr
      val target: Map
      val targetKnown: Known[Map]
    }


    object Map {
      type Aux[Recs <: HList] = Map { type Records = Recs }
      def apply[Recs <: HList](recs: Recs)(
        implicit toMap: ops.record.ToMap.Aux[Recs, String, Known[Expr[_]]]
      ): Map.Aux[Recs] =
        new Map {
          type Records = Recs
          val records: Recs = recs
          lazy val asMap: Predef.Map[String, Known[Expr[_]]] = toMap(recs)
        }
      def unapply(expr: Expr[_]): Option[Predef.Map[String, Known[Expr[_]]]] = PartialFunction.condOpt(expr) {
        case map: Map => map.asMap
      }

      implicit lazy val fragment: CypherFragment[Map] = define(m => mapStr(m.asMap))
    }
    object Key {
      type Aux[+A, K <: String, M <: MapExpr] = Key[A] { type Key = K; type Map = M }
      def apply[A]: Builder[A] = Builder.asInstanceOf[Builder[A]]
      def unapply(expr: Expr[_]): Option[(Known[MapExpr], String)] = PartialFunction.condOpt(expr) {
        case k: Key[_] => k.targetKnown -> k.key
      }

      protected class Builder[A] {
        // explicitly specifying the `Aux` return type breaks IDE compatibility
        def apply[M <: MapExpr](m: M, k: String)(implicit frag: CypherFragment[M]) =
          new Key[A] {
            type Key = k.type
            val key: k.type = k
            type Map = M
            val target: M = m
            val targetKnown: Known[M] = Known(m)
          }
      }
      private object Builder extends Builder[Any]

      implicit def fragment[A]: CypherFragment[Key[A]] = instance.asInstanceOf[CypherFragment[Key[A]]]
      implicit lazy val instance: CypherFragment[Key[_]] = define {
        case Key(m, k) => s"${m.toCypher}.${escapeName(k)}"
      }
    }

    // // // Lists // // //
    case class List(get: scala.List[Known[Expr[_]]]) extends Expr[scala.List[Expr[_]]]
    type ListExpr = Expr[scala.List[_]] // TODO
    type IndexExpr = Expr[Long]         // TODO
    case class In(elem: Known[Expr[_]], list: Known[ListExpr]) extends Expr[Boolean]
    case class AtIndex[A](list: Known[ListExpr], index: Known[IndexExpr])(implicit val m: Manifest[A]) extends Expr[A]
    case class AtRange[A](list: Known[ListExpr], limits: Ior[Known[IndexExpr], Known[IndexExpr]])(implicit val m: Manifest[A]) extends Expr[scala.List[A]]

    object List {
      implicit lazy val fragment: CypherFragment[List] = define {
        _.get.map(_.toCypher).mkString("[ ", ", ", " ]")
      }
    }
    object In {
      implicit lazy val fragment: CypherFragment[In] = define {
        case In(elem, list) => s"${elem.toCypher} IN ${list.toCypher}"
      }
    }
    private def atIndex(list: Known[ListExpr], index: String) = s"${list.toCypher}[$index]"
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

    // // // Strings // // //
    sealed trait StringExpr extends Expr[Boolean] {
      type Left  <: Expr[String]
      type Right <: Expr[String]
      type Op    <: StringExpr.Op
      val left:  Known[Left]
      val right: Known[Right]
      val op: Op
    }
    object StringExpr {
      type Aux[L <: Expr[String], P <: Op, R <: Expr[String]] = StringExpr { type Left = L; type Right = R; type Op = P }
      def apply[L <: Expr[String], P <: Op, R <: Expr[String]](l: L, p: P, r: R)(
        implicit lFragment: CypherFragment[L], rFragment: CypherFragment[R]
      ): Aux[L, P, R] =
        new StringExpr {
          type Op = P
          type Left = L
          type Right = R
          val left: Known[L] = l
          val right: Known[R] = r
          val op: P = p
        }
      def unapply(expr: Expr[Boolean]): Option[(Known[Expr[String]], Op, Known[Expr[String]])] = PartialFunction.condOpt(expr) {
        case str: StringExpr => (str.left, str.op, str.right)
      }

      sealed trait Op
      case object StartsWith extends Op
      case object EndsWith   extends Op
      case object Contains   extends Op

      implicit lazy val fragment: CypherFragment[StringExpr] = define {
        case StringExpr(left, op, right) =>
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

    sealed trait LogicBinaryExpr extends LogicExpr {
      type Left  <: Expr[Boolean]
      type Right <: Expr[Boolean]
      type Op    <: LogicExpr.BinaryOp
      val left:  Known[Left]
      val right: Known[Right]
      val op: Op
    }

    sealed trait LogicUnaryExpr extends LogicExpr {
      type Arg <: Expr[Boolean]
      type Op  <: LogicExpr.UnaryOp
      val arg: Known[Arg]
      val op: Op
    }

    object LogicExpr {
      sealed trait BinaryOp
      case object Or  extends BinaryOp
      case object And extends BinaryOp
      case object Xor extends BinaryOp

      sealed trait UnaryOp
      case object Not     extends UnaryOp
      case object IsNull  extends UnaryOp
      case object NotNull extends UnaryOp

      implicit lazy val fragment: CypherFragment[LogicExpr] = define {
        case LogicBinaryExpr(left, op, right) =>
          val opStr = op match {
            case Or  => "OR"
            case And => "AND"
            case Xor => "XOR"
          }
          s"${left.toCypher} $opStr ${right.toCypher}"
        case LogicUnaryExpr(expr, Not) =>
          s"NOT ${expr.toCypher}"
        case LogicUnaryExpr(expr, op) =>
          val opStr = (op: @unchecked) match {
            case IsNull  => "IS NULL"
            case NotNull => "IS NOT NULL"
          }
          s"${expr.toCypher} $opStr"
      }
    }

    object LogicBinaryExpr {
      type Op = LogicExpr.BinaryOp
      type Aux[L <: Expr[Boolean], P <: Op, R <: Expr[Boolean]] = LogicBinaryExpr { type Left = L; type Right = R; type Op = P }
      def apply[L <: Expr[Boolean], P <: Op, R <: Expr[Boolean]](l: L, p: P, r: R)(
        implicit lFragment: CypherFragment[L], rFragment: CypherFragment[R]
      ): Aux[L, P, R] =
        new LogicBinaryExpr {
          type Op = P
          type Left = L
          type Right = R
          val left: Known[L] = l
          val right: Known[R] = r
          val op: P = p
        }
      def unapply(expr: Expr[Boolean]): Option[(Known[Expr[Boolean]], Op, Known[Expr[Boolean]])] = PartialFunction.condOpt(expr) {
        case b: LogicBinaryExpr => (b.left, b.op, b.right)
      }
    }

    object LogicUnaryExpr {
      type Op = LogicExpr.UnaryOp
      type Aux[E <: Expr[Boolean], P <: Op] = LogicUnaryExpr { type Arg = E; type Op = P }
      def apply[E <: Expr[Boolean], P <: Op](e: E, p: P)(implicit fragment: CypherFragment[E]): Aux[E, P] =
        new LogicUnaryExpr {
          type Op = P
          type Arg = E
          val arg: Known[E] = e
          val op: P = p
        }
      def unapply(expr: Expr[Boolean]): Option[(Known[Expr[Boolean]], Op)] = PartialFunction.condOpt(expr) {
        case u: LogicUnaryExpr => (u.arg, u.op)
      }

    }

    // // // Compare // // //
    case class CompareExpr(left: Known[Expr[Any]], right: Known[Expr[Any]], op: CompareExpr.Op) extends Expr[Boolean]
    object CompareExpr {
      sealed trait Op
      case object Lt  extends Op
      case object Lte extends Op
      case object Gte extends Op
      case object Gt  extends Op
      case object Eq  extends Op
      case object Neq extends Op

      implicit lazy val fragment: CypherFragment[CompareExpr] = define {
        case CompareExpr(left, right, op) =>
          val opStr = op match {
            case Lt  => "<"
            case Lte => "<="
            case Gte => ">="
            case Gt  => ">"
            case Eq  => "="
            case Neq => "<>"
          }
          s"${left.toCypher} $opStr ${right.toCypher}"
      }
    }
  }

  sealed trait Query[+A]
  object Query {
    case class Union[+A, +B](left: Known[Query[A]], right: Known[Query[B]], all: Boolean) extends Query[(A, B)] // TODO

    sealed trait Query0[+A] extends Query[A]

    sealed trait Return[+A] extends Query0[A] {
      type Expr <: CypherFragment.Return[_]
      val ret: Known[CypherFragment.Return[A]]
    }

    sealed trait Clause[+A] extends Query0[A] {
      type Clause <: CypherFragment.Clause
      type Query <: Query0[_]
      val clause: Known[Clause]
      val query: Known[Query0[A]]
    }


    object Return {
      type Aux[+A, R <: CypherFragment.Return[_]] = Return[A]{ type Expr = R }
      def apply[R <: CypherFragment.Return[_]](ret: R)(implicit build: Build[R]): build.Out = build(ret)

      sealed trait Build[R <: CypherFragment.Return[_]] extends DepFn1[R] { type Out <: Return[_] }
      object Build {
        type Aux[R <: CypherFragment.Return[_], Out0 <: Return[_]] = Build[R]{ type Out = Out0 }

        implicit def all: Aux[CypherFragment.Return.All, Return.Aux[Any, CypherFragment.Return.All]] =
          new Build[CypherFragment.Return.All] {
            type Out = Return.Aux[Any, CypherFragment.Return.All]
            def apply(t: CypherFragment.Return.All): Return.Aux[Any, CypherFragment.Return.All] = new Return[Any] {
              type Expr = CypherFragment.Return.All
              val ret: Known[Expr] = Known(CypherFragment.Return.All)
            }
          }

        implicit def expr[A, E <: CypherFragment.Expr[_], As <: Option[String]](
          implicit
          ev: E <:< CypherFragment.Expr[A],
          fragment: CypherFragment[CypherFragment.Return.Expr.Aux[A, E, As]]
        ): Aux[CypherFragment.Return.Expr.Aux[A, E, As], Return.Aux[A, CypherFragment.Return.Expr.Aux[A, E, As]]] =
          new Build[CypherFragment.Return.Expr.Aux[A, E, As]] {
            type Out = Return.Aux[A, CypherFragment.Return.Expr.Aux[A, E, As]]
            def apply(t: CypherFragment.Return.Expr.Aux[A, E, As]): Return.Aux[A, CypherFragment.Return.Expr.Aux[A, E, As]] =
              new Return[A] {
                type Expr = CypherFragment.Return.Expr.Aux[A, E, As]
                val ret: Known[Expr] = t
              }
          }

        implicit def list[H, T <: HList, HE <: CypherFragment.Return.Return0[_], TE <: HList](
          implicit
          ev: HE <:< CypherFragment.Return.Return0[H],
          fragment: CypherFragment[CypherFragment.Return.List.Aux[H, T, HE, TE]]
        ): Aux[CypherFragment.Return.List.Aux[H, T, HE, TE], Return.Aux[H #: T, CypherFragment.Return.List.Aux[H, T, HE, TE]]] =
          new Build[CypherFragment.Return.List.Aux[H, T, HE, TE]] {
            type Out = Return.Aux[H #: T, CypherFragment.Return.List.Aux[H, T, HE, TE]]
            def apply(t: CypherFragment.Return.List.Aux[H, T, HE, TE]): Return.Aux[H #: T, CypherFragment.Return.List.Aux[H, T, HE, TE]] =
              new Return[H #: T] {
                type Expr = CypherFragment.Return.List.Aux[H, T, HE, TE]
                val ret: Known[CypherFragment.Return.List.Aux[H, T, HE, TE]] = t
              }
          }
      }
      def unapply[A](query: Query[A]): Option[Known[CypherFragment.Return[A]]] = PartialFunction.condOpt(query) {
        case ret: Return[A @unchecked] => ret.ret
      }
    }

    object Clause {
      type Aux[+A, C <: CypherFragment.Clause, Q <: Query0[_]] = Clause[A] { type Clause = C; type Query = Q }
      def apply[C <: CypherFragment.Clause, Q <: Query0[_]](clause: C, query: Q)(implicit build: Build[C, Q]): build.Out = build(clause, query)
      def unapply[A](query: Query[A]): Option[(Known[CypherFragment.Clause], Known[Query0[A]])] = PartialFunction.condOpt(query) {
        case clause: Clause[_] => clause.clause -> clause.query
      }

      sealed trait Build[C <: CypherFragment.Clause, Q <: Query0[_]] extends DepFn2[C, Q]{ type Out <: Clause[_] }
      object Build {
        type Aux[C <: CypherFragment.Clause, Q <: Query0[_], Out0 <: Clause[_]] = Build[C, Q] { type Out = Out0 }

        implicit def ret[C <: CypherFragment.Clause, A, R <: CypherFragment.Return[_]](
          implicit
          ev: R <:< CypherFragment.Return[A],
          cFragment: CypherFragment[C],
          qFragment: CypherFragment[Query.Return.Aux[A, R]]
        ): Aux[C, Query.Return.Aux[A, R], Clause.Aux[A, C, Query.Return.Aux[A, R]]] =
          new Build[C, Query.Return.Aux[A, R]] {
            type Out = Clause.Aux[A, C, Query.Return.Aux[A, R]]
            def apply(t: C, u: Query.Return.Aux[A, R]): Clause.Aux[A, C, Return.Aux[A, R]] =
              new Clause[A] {
                type Clause = C
                type Query = Return.Aux[A, R]
                val clause: Known[C] = Known(t)(cFragment)
                val query: Known[Return.Aux[A, R]] = Known(u)(qFragment)
              }
          }

// TODO ================================================================================================================
//        implicit def clause[C <: CypherFragment.Clause, A, Q <: Query.Clause[A]]: Aux[C, Q, Clause.Aux[A, C, Q]] = ???

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

    case object All extends Return0[Any]
    type All = All.type

    sealed trait Expr[+A] extends Return0[A] {
      type Expr <: CypherFragment.Expr[_]
      type Alias <: Option[String]
      val expr: Known[CypherFragment.Expr[A]]
      val alias: Alias
    }

    sealed trait List[Head, Tail <: HList] extends Return[Head #: Tail] {
      type HeadExpr <: Return0[_]
      type KnownTailExpr <: HList

      type Cons = Head #: Tail
      type ConsExpr = HeadExpr #: KnownTailExpr

      val head: Known[Return0[Head]]
      val tail: KnownTailExpr
      val listTail: scala.List[Known[Expr[_]]]
    }

    object Expr {
      type Aux[+A, E <: CypherFragment.Expr[_], As <: Option[String]] = Expr[A] { type Expr = E; type Alias = As }
      def apply[E <: CypherFragment.Expr[_]](e: E)(implicit build: Build[E, None.type]): build.Out = build(e, None)
      def apply[E <: CypherFragment.Expr[_], As <: Option[String]](e: E, as: As)(implicit build: Build[E, As]): build.Out = build(e, as)
      def unapply[A](ret: Return[A]): Option[(Known[CypherFragment.Expr[A]], Option[String])] = PartialFunction.condOpt(ret) {
        case expr: Expr[A @unchecked] => expr.expr -> expr.alias
      }

      sealed trait Build[E <: CypherFragment.Expr[_], As <: Option[String]] extends DepFn2[E, As] { type Out <: Expr[_] }
      object Build {
        type Aux[E <: CypherFragment.Expr[_], As <: Option[String], Out0 <: Expr[_]] = Build[E, As] { type Out = Out0 }

        implicit def impl[A, E <: CypherFragment.Expr[_], As <: Option[String]](
          implicit ev: E <:< CypherFragment.Expr.Inv[A], fragment: CypherFragment[E]
        ): Aux[E, As, Expr.Aux[A, E, As]] =
          new Build[E, As] {
            type Out = Expr.Aux[A, E, As]
            def apply(t: E, u: As): Expr.Aux[A, E, As] =
              new Expr[A] {
                type Expr = E
                type Alias = As
                val expr: Known[CypherFragment.Expr[A]] = Known(t).widen
                val alias: As = u
              }
          }
      }
    }

    object List {
      private object KnownHList extends Poly1 {
        implicit def impl[A: CypherFragment]: Case.Aux[A, Known[A]] = at[A](Known(_))
      }

      type Aux[H, T <: HList, HE <: Return0[_], TE <: HList] = List[H, T] { type HeadExpr = HE; type KnownTailExpr = TE }

      def apply[H, HE <: Return0[_], TE <: HList, T <: HList, TKnown <: HList](h: HE, t: TE)(
        implicit
        ev: HE <:< Return0[H],
        headFragment: CypherFragment[HE],
        stripExpr: ComappedCov.Aux[TE, Expr, T],
        tKnown: ops.hlist.Mapper.Aux[KnownHList.type, TE, TKnown],
        toList: ops.hlist.ToTraversable.Aux[TKnown, scala.List, Known[Expr[_]]]
      ): List.Aux[H, T, HE, TKnown] =
        new List[H, T] {
          type HeadExpr = HE
          type KnownTailExpr = TKnown
          val head: Known[Return0[H]] = Known(h)(headFragment).widen
          val tail: TKnown = tKnown(t)
          lazy val listTail: scala.List[Known[Expr[_]]] = tail.toList
        }

      def unapply(ret: Return[_]): Option[(Known[Return0[_]], scala.List[Known[Expr[_]]])] = PartialFunction.condOpt(ret) {
        case list: List[_, _] => list.head -> list.listTail
      }
    }

    implicit def fragment[A]: CypherFragment[Return[A]] = instance.asInstanceOf[CypherFragment[Return[A]]]
    private lazy val instance = define[Return[Any]] {
      case All => "*"
      case Expr(expr, as) => expr.toCypher + asStr(as)
      case List(head, Nil) => head.toCypher
      case List(head, tail) => s"${head.toCypher}, ${tail.map(_.toCypher).mkString(", ")}"
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
