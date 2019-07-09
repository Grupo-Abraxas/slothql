package com.abraxas.slothql.cypher

import scala.language.{ higherKinds, implicitConversions }

import cats.{ Bifunctor, Contravariant, Functor }
import cats.data.{ Ior, NonEmptyList }
import cats.syntax.bifunctor._
import cats.syntax.functor._
import shapeless.{ :: => #:, _ }


trait CypherFragment[-A] {
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

  case class Statement(template: String, params: Map[String, Any])

  sealed trait Parameterized[Params <: HList, A] {
    def fragment: CypherFragment[A]
    def apply(a: A): Parameterized.Prepared[Params, A] = new Parameterized.Prepared(fragment.toCypher(a))
  }
  object Parameterized {
    def apply[Params <: HList, A](implicit frag: CypherFragment[A]): Parameterized[Params, A] = // TODO: some param types should probably be mapped to java
      new Parameterized[Params, A] { def fragment: CypherFragment[A] = frag }

    final class Prepared[Params <: HList, +A](template: String) extends RecordArgs {
      def applyRecord(params: Params)(implicit toMap: ops.record.ToMap.Aux[Params, _ <: Symbol, _ <: Any]): Statement =
        Statement(template, toMap(params).map{ case (k, v) => k.name -> v })

      protected[cypher] def changeParams[PF <: Poly1](implicit mapper: ops.record.MapValues[PF, Params]): Prepared[mapper.Out, A] =
        this.asInstanceOf[Prepared[mapper.Out, A]]

      override def toString: String = s"Prepared($template)"
    }
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


    override def hashCode(): Int = fragment.##
    override def equals(obj: Any): Boolean = PartialFunction.cond(obj) { case that: Known[_] => this.fragment == that.fragment }

    override def toString: String = s"Known$fragment{ $toCypher }"
  }
  object Known {
    implicit def apply[A](f: A)(implicit cypherFragment: CypherFragment[A]): Known[A] =
      new Known[A] {
        val fragment: A = f
        lazy val toCypher: String = cypherFragment.toCypher(f)
      }
    def unapply[A](arg: Known[A]): Option[A] = Some(arg.fragment)

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
    sealed trait Input[+A] extends Expr[A]
    case class Param[+A](name: String) extends Input[A]
    case class Lit[+A](value: A) extends Input[A]

    sealed trait Null[+A] extends Expr[A]
    trait Var[+A] extends Expr[A] {
      val name: String


      override def hashCode(): Int = name.hashCode
      override def equals(obj: Any): Boolean = PartialFunction.cond(obj) { case that: Var[_] => this.name == that.name }

      override def toString: String = s"Var($name)"
    }
    case class Call[+A](func: String, params: scala.List[Known[Expr[_]]]) extends Expr[A]

    object Param {
      implicit def fragment[A]: CypherFragment[Param[A]] = instance.asInstanceOf[CypherFragment[Param[A]]]
      private lazy val instance = define[Param[_]](p => s"$$${escapeName(p.name)}")
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
      implicit def literalIterableFragment[A](implicit frag: CypherFragment[Lit[A]]): CypherFragment[Lit[Iterable[A]]] = define {
        case Lit(xs) => xs.map(frag.toCypher _ compose Lit.apply).mkString("[ ", ", ", " ]")
      }

      private lazy val literalToString = define[Lit[_]](_.value.toString)
    }
    object Null {
      def apply[A]: Null[A] = instance.asInstanceOf[Null[A]]
      def unapply(expr: Expr[_]): Boolean = expr == instance
      private lazy val instance = new Null[Any] { override def toString: String = "Null" }

      implicit lazy val fragment: CypherFragment[Null[_]] = define(_ => "null")
    }
    object Var {
      def apply[A](nme: String): Var[A] = new Var[A] { val name: String = nme }
      def unapply(expr: Expr[_]): Option[String] = PartialFunction.condOpt(expr) { case v: Var[_] => v.name }

      implicit def fragment[A]: CypherFragment[Var[A]] = instance.asInstanceOf[CypherFragment[Var[A]]]
      private lazy val instance = define[Var[_]](v => escapeName(v.name))
    }
    object Call {
      implicit def fragment[A]: CypherFragment[Call[A]] = instance.asInstanceOf[CypherFragment[Call[A]]]
      private lazy val instance = define[Call[_]] {
        case Call(func, params) => s"${func.split('.').map(escapeName).mkString(".")}(${params.map(_.toCypher).mkString(", ")})"
      }
    }

    // // // Maps // // //
    sealed trait MapExpr[+A] extends Expr[A]
    case class Map[+A](get: Predef.Map[String, Known[Expr[A]]]) extends MapExpr[Predef.Map[String, A]]
    case class MapKey[+A](map: Known[Expr[Predef.Map[String, _]]], key: String) extends MapExpr[A]
    case class MapAdd[+A](map: Known[Expr[Predef.Map[String, _]]], values: Predef.Map[String, Known[Expr[A]]]) extends MapExpr[Predef.Map[String, A]]

    object MapExpr {
      implicit lazy val fragmentMap: CypherFragment[Map[_]] = define {
        m => mapStr(m.get)
      }
      implicit lazy val fragmentMapKey: CypherFragment[MapKey[_]] = define {
        case MapKey(m, k) => s"${m.toCypher}.${escapeName(k)}"
      }
      implicit lazy val fragmentMapAdd: CypherFragment[MapAdd[_]] = define {
        case MapAdd(m, vs) if vs.isEmpty => m.toCypher
        case MapAdd(m, vs) =>
          val add = vs.map{ case (k, v) => s"${escapeName(k)}: ${v.toCypher}" }.mkString(", ")
          s"${m.toCypher}{.*, $add}"
      }
    }

    // // // Lists // // //
    sealed trait ListExpr[+A] extends Expr[A]
    case class List[+A](get: scala.List[Known[Expr[A]]]) extends ListExpr[scala.List[A]]
    case class In[A](elem: Known[Expr[A]], list: Known[Expr[scala.List[A]]]) extends ListExpr[Boolean]
    case class AtIndex[A](list: Known[Expr[scala.List[A]]], index: Known[Expr[Long]]) extends ListExpr[A]
    case class AtRange[A](list: Known[Expr[scala.List[A]]], limits: Ior[Known[Expr[Long]], Known[Expr[Long]]]) extends ListExpr[scala.List[A]]
    case class Concat[A](list0: Known[Expr[scala.List[A]]], list1: Known[Expr[scala.List[A]]]) extends ListExpr[scala.List[A]]
    case class ReduceList[A, B](list: Known[Expr[scala.List[A]]], elemAlias: String, initial: Known[Expr[B]], accAlias: String, reduce: Known[Expr[B]]) extends ListExpr[B]
    case class ListComprehension[A, B](list: Known[Expr[scala.List[A]]], elemAlias: String, filter: Option[Known[Expr[Boolean]]], map: Option[Known[Expr[B]]]) extends ListExpr[scala.List[B]]
    case class ListPredicate[A](list: Known[Expr[scala.List[A]]], elemAlias: String, predicate: ListPredicate.Predicate, expr: Known[Expr[Boolean]]) extends ListExpr[Boolean]

    object ListExpr {
      implicit lazy val fragmentList: CypherFragment[List[_]] = define {
        _.get.map(_.toCypher).mkString("[ ", ", ", " ]")
      }
      implicit lazy val fragmentIn: CypherFragment[In[_]] = define {
        case In(elem, list) => s"${elem.toCypher} IN ${list.toCypher}"
      }
      implicit lazy val fragmentAtIndex: CypherFragment[AtIndex[_]] = define {
        case AtIndex(list, index) => atIndex(list, index.toCypher)
      }
      implicit lazy val fragmentAtRange: CypherFragment[AtRange[_]] = define {
        case AtRange(list, range) => atIndex(list, rangeStr(range))
      }
      implicit lazy val fragmentConcat: CypherFragment[Concat[_]] = define {
        case Concat(list0, list1) => s"${list0.toCypher} + ${list1.toCypher}"
      }
      implicit lazy val fragmentReduceList: CypherFragment[ReduceList[_, _]] = define {
        case ReduceList(list, elemAlias, initial, accAlias, reduce) =>
          s"reduce(${escapeName(accAlias)} = ${initial.toCypher}, ${escapeName(elemAlias)} IN ${list.toCypher} | ${reduce.toCypher})"
      }
      implicit lazy val fragmentListComprehension: CypherFragment[ListComprehension[_, _]] = define {
        case ListComprehension(list, _, None, None) =>
          list.toCypher
        case ListComprehension(list, elemAlias, None, Some(Known(map))) if map == Expr.Var[Any](elemAlias) =>
          list.toCypher
        case ListComprehension(list, elemAlias, filter0, map0) =>
          val filter = filter0.map(f => s" WHERE ${f.toCypher}").getOrElse("")
          val map = map0.map(f => s" | ${f.toCypher}").getOrElse("")
          s"[${escapeName(elemAlias)} IN ${list.toCypher}$filter$map]"
      }
      implicit lazy val fragmentListPredicate: CypherFragment[ListPredicate[_]] = define {
        case ListPredicate(list, alias, pred, expr) =>
          s"${pred.toCypher}(${escapeName(alias)} IN ${list.toCypher} WHERE ${expr.toCypher})"
      }

      private def atIndex(list: Known[Expr[scala.List[_]]], index: String) = s"${list.toCypher}[$index]"
    }

    object ListPredicate {
      sealed abstract class Predicate(protected[Expr] val toCypher: String)

      case object All     extends Predicate("all")
      case object Any     extends Predicate("any")
      case object Exists  extends Predicate("exists")
      case object None    extends Predicate("none")
      case object Single  extends Predicate("single")
    }


    // // // Strings // // //
    case class StringExpr(left: Known[Expr[String]], right: Known[Expr[String]], op: StringExpr.Op) extends Expr[Boolean]
    object StringExpr {
      sealed trait Op
      case object StartsWith extends Op
      case object EndsWith   extends Op
      case object Contains   extends Op
      case object Regex      extends Op

      implicit lazy val fragment: CypherFragment[StringExpr] = define {
        case StringExpr(left, right, op) =>
          val opStr = op match {
            case StartsWith => "STARTS WITH"
            case EndsWith   => "ENDS WITH"
            case Contains   => "CONTAINS"
            case Regex      => "=~"
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
        case LogicNegationExpr(expr) => s"NOT ${exprToCypher(expr)}"
        case LogicBinaryExpr(left, right, op) =>
          val opStr = op match {
            case Or  => "OR"
            case And => "AND"
            case Xor => "XOR"
          }
          s"${exprToCypher(left)} $opStr ${exprToCypher(right)}"
      }
      private def exprToCypher(expr: Known[Expr[Boolean]]) = expr.fragment match {
        case _: LogicExpr => s"(${expr.toCypher})"
        case _ => expr.toCypher
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


    // // // Mathematical // // //
    sealed trait MathematicalExpr[N] extends Expr[N] {
      val numeric: Numeric[N]
    }
    case class MathematicalBinaryExpr[N](left: Known[Expr[N]], right: Known[Expr[N]], op: MathematicalExpr.BinaryOp)
                                        (implicit val numeric: Numeric[N])
                                      extends MathematicalExpr[N]
    case class MathematicalUnaryExpr[N](expr: Known[Expr[N]], op: MathematicalExpr.UnaryOp)
                                       (implicit val numeric: Numeric[N])
                                      extends MathematicalExpr[N]

    object MathematicalExpr {
      sealed trait BinaryOp
      case object Addition        extends BinaryOp
      case object Subtraction     extends BinaryOp
      case object Multiplication  extends BinaryOp
      case object Division        extends BinaryOp
      case object ModuloDivision  extends BinaryOp
      case object Exponentiation  extends BinaryOp

      sealed trait UnaryOp
      case object Negation extends UnaryOp

      implicit lazy val fragment: CypherFragment[MathematicalExpr[_]] = define {
        case MathematicalBinaryExpr(left, right, op) =>
          val opStr = op match {
            case Addition       => "+"
            case Subtraction    => "-"
            case Multiplication => "*"
            case Division       => "/"
            case ModuloDivision => "%"
            case Exponentiation => "^"
          }
          s"${left.toCypher} $opStr ${right.toCypher}"
        case MathematicalUnaryExpr(expr, Negation) =>
          s"-${expr.toCypher}"
      }
    }


    case class Distinct[A](expr: Known[Expr[A]]) extends Expr[A]
    object Distinct {
      implicit def fragment[A]: CypherFragment[Distinct[A]] = define {
        case Distinct(expr) => s"DISTINCT ${expr.toCypher}"
      }
    }


    sealed trait CaseExpr[A] extends Expr[A]
    case class SimpleCaseExpr[V, A](value: Known[Expr[V]], cases: Predef.Map[Known[Expr[V]], Known[Expr[A]]], default: Option[Known[Expr[A]]]) extends CaseExpr[A]
    case class GenericCaseExpr[A](cases: Predef.Map[Known[Expr[Boolean]], Known[Expr[A]]], default: Option[Known[Expr[A]]]) extends CaseExpr[A]

    object CaseExpr {
      implicit lazy val fragmentSimpleCaseExpr: CypherFragment[SimpleCaseExpr[_, _]] = define {
        case SimpleCaseExpr(value, cases, default) =>
          s"CASE ${value.toCypher}${casesCypher(cases)}${defaultCypher(default)} END"
      }
      implicit lazy val fragmentGenericCaseExpr: CypherFragment[GenericCaseExpr[_]] = define {
        case GenericCaseExpr(cases, default) =>
          s"CASE${casesCypher(cases)}${defaultCypher(default)} END"
      }

      private def casesCypher[K](cases: Predef.Map[Known[Expr[K]], Known[Expr[_]]]) =
        if (cases.nonEmpty) cases.map{ case (k, v) => s"WHEN ${k.toCypher} THEN ${v.toCypher}" }.mkString(" ", " ", "")
        else ""
      private def defaultCypher(default: Option[Known[Expr[_]]]) =
        default.map(d => s" ELSE ${d.toCypher}").getOrElse("")
    }
  }

  sealed trait Query[+A]
  object Query {
    case class Union[+A](left: Known[Query[A]], right: Known[Query[A]], all: Boolean) extends Query[A]

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
    sealed trait ReturnE[+A] extends Return0[A] { val expressions: List[Known[Expr[_]]] }

    type Ascending = Boolean
    type Order = List[(Known[CypherFragment.Expr[_]], Ascending)]

    trait Options[+A] extends Return[A] {
      val ret: Known[Return0[A]]
      val distinct: Boolean
      val order: Order
      val skip: Option[Known[CypherFragment.Expr.Input[Long]]]
      val limit: Option[Known[CypherFragment.Expr.Input[Long]]]
    }
    object Options {
      type Inv[A] = Options[A]

      def apply[A](ret0: Known[Return0[A]], distinct0: Boolean, order0: Order, skip0: Option[Known[CypherFragment.Expr.Input[Long]]], limit0: Option[Known[CypherFragment.Expr.Input[Long]]]): Options[A] =
        new Options[A] {
          val ret: Known[Return0[A]] = ret0
          val distinct: Boolean = distinct0
          val order: Order = order0
          val skip: Option[Known[CypherFragment.Expr.Input[Long]]] = skip0
          val limit: Option[Known[CypherFragment.Expr.Input[Long]]] = limit0
        }

      def unapply[A](ret: Return[A]): Option[(Known[Return0[A]], Boolean, Order, Option[Known[CypherFragment.Expr.Input[Long]]], Option[Known[CypherFragment.Expr.Input[Long]]])] = PartialFunction.condOpt(ret) {
        case ops: Options[A @unchecked] => (ops.ret, ops.distinct, ops.order, ops.skip, ops.limit)
      }
    }

    case object Wildcard extends Return1[Any] {
      def as[A]: Return1[A] = this.asInstanceOf[Return1[A]]
    }
    type Wildcard = Wildcard.type

    case class Expr[+A](expr: Known[CypherFragment.Expr[A]], as: Option[String]) extends Return1[A] with ReturnE[A] {
      val expressions: List[Known[Expr[_]]] = this.known :: Nil
    }

    sealed trait Untyped extends Return0[scala.List[Any]] {
      val expressions: scala.List[Known[Expr[_]]]
      val wildcard: Boolean

      override def toString: String = s"Untyped(${expressions.mkString(", ")})"
    }
    object Untyped {
      def apply(exprs: Iterable[Known[CypherFragment.Expr[_]]], wildcard: Boolean = false): Untyped = {
        def all = wildcard
        new Untyped {
          val expressions: scala.List[Known[Expr[_]]] = exprs.toList.map(Return.Expr[Any](_, as = None).known)
          val wildcard: Boolean = all
        }
      }
      def returns(exprs: Known[Return.Expr[_]]*): Untyped = returns(wildcard = false, exprs: _*)
      def returns(wildcard: Boolean, exprs: Known[Return.Expr[_]]*): Untyped = {
        def all = wildcard
        new Untyped {
          val expressions: scala.List[Known[Expr[_]]] = exprs.toList
          val wildcard: Boolean = all
        }
      }

      def unapply(arg: Untyped): Option[(scala.List[Known[Expr[_]]], Boolean)] = Some(arg.expressions -> arg.wildcard)
    }

    /** Isn't completely compatible with cypher syntax since it doesn't permit to return ∗ as first element of a list. */
    sealed trait Tuple[Repr <: HList] extends Return0[Repr] with ReturnE[Repr] {
      val expressions: scala.List[Known[Expr[_]]]

      override def toString: String = s"Tuple(${expressions.mkString(", ")})"
    }
    object Tuple extends ProductArgs {

      sealed trait FromHList[Exprs <: HList] { build =>
        type Ret <: HList
        type Out = Tuple[Ret]
        def expressions(exprs: Exprs): scala.List[Known[Expr[_]]]
        def apply(l: Exprs): Tuple[Ret] =
          new Tuple[Ret] {
            val expressions: scala.List[Known[Return.Expr[_]]] = build.expressions(l)
          }
      }
      object FromHList {
        type Aux[L <: HList, R] = FromHList[L] { type Ret = R }

        private object UnpackKnownReturnExprPoly extends Poly1 {
          implicit def impl[KE, E, A](implicit unpackKnown: Unpack1[KE, Known, E], unpackExpr: Unpack1[E, Return, A]): Case.Aux[KE, A] = at(_ => null.asInstanceOf[A])
        }

        object KnownReturnExprPoly extends Poly1 {
          implicit def expr[A, E](implicit isExpr: Unpack1[E, CypherFragment.Expr, A], fragment: CypherFragment[E]): Case.Aux[E, Known[Return.Expr[A]]] =
            at[E](e => Return.Expr(e.known.asInstanceOf[Known[CypherFragment.Expr[A]]], None).known.widen)
          implicit def knownExpr[A, E](implicit isExpr: Unpack1[E, CypherFragment.Expr, A]): Case.Aux[Known[E], Known[Return.Expr[A]]] =
            at[Known[E]](ke => Return.Expr(ke.asInstanceOf[Known[CypherFragment.Expr[A]]], None).known.widen)
          implicit def returnExpr[A, E](implicit isReturnExpr: Unpack1[E, CypherFragment.Return.Expr, A], fragment: CypherFragment[E]): Case.Aux[E, Known[Return.Expr[A]]] =
            at[E](_.known.asInstanceOf[Known[Return.Expr[A]]])
          implicit def knownReturnExpr[A, E](implicit isReturnExpr: Unpack1[E, CypherFragment.Return.Expr, A]): Case.Aux[Known[E], Known[Return.Expr[A]]] =
            at[Known[E]](_.asInstanceOf[Known[Return.Expr[A]]])
          implicit def boolean[E](implicit isBoolean: E <:< CypherFragment.Expr[Boolean], fragment: CypherFragment[E], lowPriority: LowPriority): Case.Aux[E, Known[Return.Expr[Boolean]]] =
            at[E](e => Return.Expr[Boolean](e.known.widen, None).known)
          implicit def list[E, L, A](implicit isExpr: E <:< CypherFragment.Expr[L], unpack: Unpack1[L, scala.List, A], fragment: CypherFragment[E], lowPriority: LowPriority): Case.Aux[E, Known[Return.Expr[scala.List[A]]]] =
            at[E](e => Return.Expr(e.known.asInstanceOf[Known[CypherFragment.Expr[scala.List[A]]]], None).known)
          implicit def product[P <: Product, Repr <: HList, R0 <: HList, R <: HList](
            implicit
            notExpr: P <:!< Expr[_],
            gen: Generic.Aux[P, Repr],
            fromHList: FromHList.Aux[Repr, R]
          ): Case.Aux[P, Known[Return.Tuple[R]]] =
            at[P](p => fromHList(gen.to(p)))
        }

        implicit def listOfExpressions[Exprs <: HList, R0 <: HList, R <: HList](
          implicit
          nonEmpty: ops.hlist.IsHCons[Exprs],
          known: ops.hlist.Mapper.Aux[KnownReturnExprPoly.type, Exprs, R0],
          extract: ops.hlist.Mapper.Aux[UnpackKnownReturnExprPoly.type, R0, R],
          list: ops.hlist.ToTraversable.Aux[R0, scala.List, Known[Return.ReturnE[_]]]
        ): FromHList.Aux[Exprs, R] =
          new FromHList[Exprs] {
            type Ret = R
            def expressions(exprs: Exprs): scala.List[Known[Expr[_]]] = list(known(exprs)).flatMap(_.fragment.expressions)
          }
      }

      def applyProduct[L <: HList](l: L)(implicit build: FromHList[L]): build.Out = build(l)
      def fromHList[L <: HList](l: L)(implicit build: FromHList[L]): build.Out = build(l)

      def unapply(arg: Tuple[_]): Option[scala.List[Known[Expr[_]]]] = Some(arg.expressions)
    }

    implicit def fragment[A]: CypherFragment[Return[A]] = instance.asInstanceOf[CypherFragment[Return[A]]]
    private lazy val instance = define[Return[Any]] {
      case Wildcard | Untyped(Nil, true) => "*"
      case Expr(expr, as) => expr.toCypher + asStr(as)
      case Tuple(exprs) if exprs.nonEmpty => s"${exprs.map(_.toCypher).mkString(", ")}"
      case Untyped(exprs, w) if exprs.nonEmpty => s"${withWildcard(w)}${exprs.map(_.toCypher).mkString(", ")}"
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
        val skipFrag = skip map (" SKIP " + _.toCypher) getOrElse ""
        val limitFrag = limit map (" LIMIT " + _.toCypher) getOrElse ""
        s"$distinctFrag${expr.toCypher}$orderFrag$skipFrag$limitFrag"
    }
    private def withWildcard(wildcard: Boolean): String = if (wildcard) "*, " else ""
  }

  sealed trait Clause
  object Clause {
    case class Match(pattern: PatternTuple, optional: Boolean, where: Option[Known[Expr[Boolean]]]) extends Clause
    case class With(ret: Known[Return[_]], where: Option[Known[Expr[Boolean]]]) extends Clause
    case class Unwind(expr: Known[Expr[Seq[_]]], as: String) extends Clause

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
    case class Let(alias: String, pattern: Known[Pattern0]) extends Pattern

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

  private def escapeName(name: String) = name match {
    case "_" => "_"
    case _ => escapeName0(name)
  }
  private def escapeName0(name: String) = "`" + name.replaceAll("`", "``") + "`"
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
