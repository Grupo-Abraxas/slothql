package com.abraxas.slothql.cypher

import java.util.UUID

import scala.collection.convert.decorateAsJava.seqAsJavaListConverter
import scala.collection.mutable
import scala.language.{ higherKinds, implicitConversions }
import scala.util.hashing.MurmurHash3

import cats.{ Bifunctor, Contravariant, Functor }
import cats.data.{ Ior, NonEmptyList }
import cats.syntax.bifunctor._
import cats.syntax.functor._
import shapeless.{ :: => #:, _ }


trait CypherFragment[-A] {
  def mkCypher(f: A): CypherFragment.MkStatement
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
  case class Statement(template: Statement.Template, params: Map[Statement.Param, Any])
  object Statement {
    type Template = String
    type Param = String
  }

  trait ParamCollector {
    def apply(id: Any): Statement.Param
  }
  object ParamCollector {
    implicit def defaultThreadUnsafeParamCollector: ParamCollector =
      new ParamCollector {
        private var count = -1
        private val values = mutable.HashMap.empty[Any, String]
        private def nextParam() = {
          count += 1
          count.toString
        }
        def apply(id: Any): Statement.Param = values.getOrElseUpdate(id, nextParam())
      }
  }

  class MkStatement(mk: ParamCollector => Statement) {
    def result(implicit pc: ParamCollector): Statement = mk(pc)
    def mapT(f: Statement.Template => Statement.Template): MkStatement =
      new MkStatement(pc => {
        val statement = mk(pc)
        Statement(f(statement.template), statement.params)
      })
    def flatMapT(f: Statement.Template => MkStatement): MkStatement =
      new MkStatement(pc => {
        val statement0 = mk(pc)
        val statement = f(statement0.template).result(pc)
        statement.copy(params = statement.params ++ statement0.params)
      })
  }
  object MkStatement {
    def apply(template: Statement.Template): MkStatement = new MkStatement(_ => Statement(template, Map()))
    def apply(statement: Statement): MkStatement = new MkStatement(_ => statement)
    def apply(id: Any, template: Statement.Param => Statement.Template, value: Any): MkStatement = new MkStatement(pc => {
      val p = pc(id)
      Statement(template(p), Map(p -> value))
    })

    implicit class MkStatementsOps(mks: Iterable[MkStatement]) {
      def mergeStatements(mergeTemplates: Iterable[Statement.Template] => Statement.Template): MkStatement = new MkStatement(pc => {
        val (templates, params0) = mks.map(_.result(pc) match { case Statement(template, params) => (template, params) }).unzip
        Statement(mergeTemplates(templates), params0.flatten.toMap)
      })
    }

    implicit class MkStatementPairOps(p: (MkStatement, MkStatement)) {
      def map2Statements(f: (Statement.Template, Statement.Template) => Statement.Template): MkStatement =
        p._1.flatMapT(t1 => p._2.mapT(t2 => f(t1, t2)))
    }
  }

  @inline def apply[A](f: A)(implicit fragment: CypherFragment[A]): MkStatement = fragment.mkCypher(f)
  def define[A](toCypher0: A => MkStatement): CypherFragment[A] =
    new CypherFragment[A]{
      def mkCypher(f: A): MkStatement = toCypher0(f)
    }
  def define0[A](toCypher: A => Statement.Template): CypherFragment[A] = define(toCypher andThen MkStatement.apply)

  implicit lazy val CypherFragmentIsContravariant: Contravariant[CypherFragment] =
    new Contravariant[CypherFragment] {
      def contramap[A, B](fa: CypherFragment[A])(f: B => A): CypherFragment[B] =
        define(fa.mkCypher _ compose f)
    }

  sealed trait Known[+A] {
    self =>

    val fragment: A
    def mkCypher: MkStatement
    def toCypher(implicit pc: ParamCollector): Statement = mkCypher.result

    def map[B](f: A => B): Known[B] = new Known[B] {
      val fragment: B = f(self.fragment)
      def mkCypher: MkStatement = self.mkCypher
    }

    def widen[B](implicit ev: A <:< B): Known[B] = this.asInstanceOf[Known[B]]

    override def toString: String = {
      val Statement(template, params) = toCypher(ParamCollector.defaultThreadUnsafeParamCollector)
      val paramsStr = if (params.nonEmpty) params.map{ case (k, v) => s"$k: $v" }.mkString("; ", ", ", "") else ""
      s"Known$fragment{ $template$paramsStr }"
    }
  }
  object Known {
    implicit def apply[A](f: A)(implicit cypherFragment: CypherFragment[A]): Known[A] =
      new Known[A] {
        val fragment: A = f
        lazy val mkCypher: MkStatement = cypherFragment.mkCypher(f)
      }
    def unapply[A](arg: Known[A]): Option[A] = Some(arg.fragment)

    implicit def functor[F[_]: Functor, A: CypherFragment](fa: F[A]): F[Known[A]] = fa.map(apply[A])
    implicit def bifunctor[F[_, _]: Bifunctor, A: CypherFragment, B: CypherFragment](fab: F[A, B]): F[Known[A], Known[B]] =
      fab.bimap(apply[A], apply[B])

    implicit class KnownIterableOps(mks: Iterable[Known[_]]) {
      def mergeStatements(mergeTemplates: Iterable[Statement.Template] => Statement.Template): MkStatement = new MkStatement(pc => {
        val (templates, params0) = mks.map(_.mkCypher.result(pc) match { case Statement(t, ps) => (t, ps) }).unzip
        Statement(mergeTemplates(templates), params0.flatten.toMap)
      })
    }

    implicit class KnownPairOps(p: (Known[_], Known[_])) {
      def map2Statements(f: (Statement.Template, Statement.Template) => Statement.Template): MkStatement =
        p._1.mkCypher.flatMapT(t1 => p._2.mkCypher.mapT(t2 => f(t1, t2)))

      def join2Statements(op: String): MkStatement = map2Statements(_ + s" $op " + _)
    }
  }
  // Explicit analog of implicit `Known.apply`
  implicit class KnownOps[A: CypherFragment](f: A) {
    @inline def known: Known[A] = Known(f)
  }


  sealed trait Expr[+T]
  object Expr {
    type Inv[T] = Expr[T]

    // // // Values and Variables // // //
    case class Lit[+A](value: A, id: UUID = UUID.randomUUID()) extends Expr[A]
    trait Var[A] extends Expr[A] {
      val name: String

      override def toString: String = s"Var($name)"
    }
    case class Call[+A](func: String, params: scala.List[Known[Expr[_]]]) extends Expr[A]

    object Lit {
      implicit lazy val literalStringFragment: CypherFragment[Lit[String]] =
        defineLiteral()
      /** Warning: it does nothing to check whether the number can be represented in cypher (~Long~ equivalent). */
      implicit def literalNumericFragment[N: Numeric]: CypherFragment[Lit[N]] =
        defineLiteral()
      implicit lazy val literalBooleanFragment: CypherFragment[Lit[Boolean]] =
        defineLiteral()
      implicit def literalSeqFragment[A](implicit frag: CypherFragment[Lit[A]]): CypherFragment[Lit[Seq[A]]] =
        defineLiteral(_.asJava)

      private def defineLiteral[A](mapV: A => Any = locally[A] _) =
        define[Lit[A]](lit => MkStatement(lit.id, paramTemplate, mapV(lit.value)))
    }

    private def paramTemplate: Statement.Param => Statement.Template = p => s"$$$p"

    object Var {
      def apply[A](nme: String): Var[A] = new Var[A] { val name: String = nme }
      def unapply(expr: Expr[_]): Option[String] = PartialFunction.condOpt(expr) { case v: Var[_] => v.name }

      implicit def fragment[A]: CypherFragment[Var[A]] = instance.asInstanceOf[CypherFragment[Var[A]]]
      private lazy val instance = define0[Var[_]](v => escapeName(v.name))
    }
    object Call {
      implicit def fragment[A]: CypherFragment[Call[A]] = instance.asInstanceOf[CypherFragment[Call[A]]]
      private lazy val instance = define[Call[_]] {
        case Call(func, params) => params.mergeStatements(_.mkString(", ")).mapT(t => s"${escapeName(func)}($t)")
      }
    }

    // // // Maps // // //
    case class Map[+A](get: Predef.Map[String, Known[Expr[A]]]) extends Expr[Predef.Map[String, A]]
    case class MapKey[+A](map: Known[Expr[Predef.Map[String, _]]], key: String) extends Expr[A]
    case class MapAdd[+A](map: Known[Expr[Predef.Map[String, _]]], values: Predef.Map[String, Known[Expr[A]]]) extends Expr[Predef.Map[String, A]]

    object Map {
      implicit def fragment[A]: CypherFragment[Map[A]] = instance.asInstanceOf[CypherFragment[Map[A]]]
      lazy val instance: CypherFragment[Map[_]] = define(m => mapMkS(m.get))
    }
    object MapKey {
      implicit def fragment[A]: CypherFragment[MapKey[A]] = instance.asInstanceOf[CypherFragment[MapKey[A]]]
      lazy val instance: CypherFragment[MapKey[_]] = define {
        case MapKey(m, k) => m.mkCypher.mapT(t => s"$t.${escapeName(k)}")
      }
    }
    object MapAdd {
      implicit def fragment[A]: CypherFragment[MapAdd[A]] = instance.asInstanceOf[CypherFragment[MapAdd[A]]]
      lazy val instance: CypherFragment[MapAdd[_]] = define {
        case MapAdd(m, vs) if vs.isEmpty => m.mkCypher
        case MapAdd(m, vs) =>
          val add = vs.map{ case (k, v) => v.mkCypher.mapT(t => s"${escapeName(k)}: $t") }.mergeStatements(_.mkString(", "))
          (add, m.mkCypher).map2Statements((addT, mt) => s"$mt{.*, $addT}")
      }
    }

    // // // Lists // // //
    case class List[+A](get: scala.List[Known[Expr[A]]]) extends Expr[scala.List[A]]
    case class In[A](elem: Known[Expr[A]], list: Known[Expr[scala.List[A]]]) extends Expr[Boolean]
    case class AtIndex[A](list: Known[Expr[scala.List[A]]], index: Known[Expr[Long]]) extends Expr[A]
    case class AtRange[A](list: Known[Expr[scala.List[A]]], limits: Ior[Known[Expr[Long]], Known[Expr[Long]]]) extends Expr[scala.List[A]]
    case class Concat[A](list0: Known[Expr[scala.List[A]]], list1: Known[Expr[scala.List[A]]]) extends Expr[scala.List[A]]
    case class FilterList[A](list: Known[Expr[scala.List[A]]], elemAlias: String, filter: Known[Expr[Boolean]]) extends Expr[scala.List[A]]

    object List {
      implicit def fragment[A]: CypherFragment[List[A]] = instance.asInstanceOf[CypherFragment[List[A]]]
      private lazy val instance: CypherFragment[List[_]] = define {
        list =>
          if (list.get.forall(_.fragment.isInstanceOf[Lit[_]])) {
            val (values, ids) = list.get.map(l => (l.fragment: @unchecked) match { case Lit(v, id) => v -> id }).unzip
            val listId = MurmurHash3.seqHash(ids)
            MkStatement(listId, paramTemplate, values.asJava)
          }
          else
            list.get.mergeStatements(_.mkString("[ ", ", ", " ]"))
      }
    }
    object In {
      implicit def fragment[A]: CypherFragment[In[A]] = instance.asInstanceOf[CypherFragment[In[A]]]
      private lazy val instance: CypherFragment[In[_]] = define {
        case In(elem, list) =>
          (elem, list).map2Statements((et, lt) => s"$et IN $lt")
      }
    }
    private def atIndex(list: Known[Expr[scala.List[_]]], index: MkStatement) =
      (list.mkCypher, index).map2Statements((lt, it) => s"$lt[$it]")
    object AtIndex {
      implicit def fragment[A]: CypherFragment[AtIndex[A]] = instance.asInstanceOf[CypherFragment[AtIndex[A]]]
      private lazy val instance = define[AtIndex[_]] {
        case AtIndex(list, index) => atIndex(list, index.mkCypher)
      }
    }
    object AtRange {
      implicit def fragment[A]: CypherFragment[AtRange[A]] = instance.asInstanceOf[CypherFragment[AtRange[A]]]
      private lazy val instance = define[AtRange[_]] {
        case AtRange(list, range) => atIndex(list, rangeMkS(range))
      }
    }
    object Concat {
      implicit def fragment[A]: CypherFragment[Concat[A]] = instance.asInstanceOf[CypherFragment[Concat[A]]]
      private lazy val instance = define[Concat[_]] {
        case Concat(list0, list1) =>
          (list0, list1).join2Statements("+")
      }
    }
    object FilterList {
      implicit def fragment[A]: CypherFragment[FilterList[A]] = instance.asInstanceOf[CypherFragment[FilterList[A]]]
      private lazy val instance = define[FilterList[_]] {
        case FilterList(list, elemAlias, filter) =>
          (list, filter).map2Statements((lt, ft) => s"filter(${escapeName(elemAlias)} in $lt WHERE $ft)")
      }
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
          (left, right).join2Statements(opStr)
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
        case LogicNegationExpr(expr) => expr.mkCypher.mapT(t => s"NOT $t")
        case LogicBinaryExpr(left, right, op) =>
          val opStr = op match {
            case Or  => "OR"
            case And => "AND"
            case Xor => "XOR"
          }
          (left, right).join2Statements(opStr)
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
          (left, right).join2Statements(opStr)
        case CompareBinaryAnyExpr(left, right, op) =>
          val opStr = op match {
            case Eq  => "="
            case Neq => "<>"
          }
          (left, right).join2Statements(opStr)
        case CompareUnaryExpr(expr, op) =>
          val opStr = op match {
            case IsNull  => "IS NULL"
            case NotNull => "IS NOT NULL"
          }
          expr.mkCypher.mapT(_ + s" $opStr")
      }
    }

    case class Distinct[A](expr: Known[Expr[A]]) extends Expr[A]
    object Distinct {
      implicit def fragment[A]: CypherFragment[Distinct[A]] = define {
        case Distinct(expr) => expr.mkCypher.mapT("DISTINCT " + _)
      }
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
      case Union(left, right, all) => (left, right).join2Statements(if (all) "UNION ALL" else "UNION")
      case Clause(clause, query) => (clause, query).map2Statements(_ + " " + _)
      case Return(ret) => ret.mkCypher.mapT("RETURN " + _)
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


        object KnownHListTypes extends Poly1 {
          implicit def expr[A, E](implicit isExpr: Unpack1[E, CypherFragment.Expr, A], fragment: CypherFragment[E]): Case.Aux[E, (Known[Return.Expr[A]], A)] =
            at[E](e => Return.Expr(e.known.asInstanceOf[Known[CypherFragment.Expr[A]]], None).known.widen -> null.asInstanceOf[A])
          implicit def returnExpr[A, E](implicit isReturnExpr: Unpack1[E, CypherFragment.Return.Expr, A], fragment: CypherFragment[E]): Case.Aux[E, (Known[Return.Expr[A]], A)] =
            at[E](_.known.asInstanceOf[Known[Return.Expr[A]]] -> null.asInstanceOf[A])
          implicit def boolean[E](implicit isBoolean: E <:< CypherFragment.Expr[Boolean], fragment: CypherFragment[E], lowPriority: LowPriority): Case.Aux[E, (Known[Return.Expr[Boolean]], Boolean)] =
            at[E](e => Return.Expr[Boolean](e.known.widen, None).known -> false)
          implicit def list[E, L, A](implicit isExpr: E <:< CypherFragment.Expr[L], unpack: Unpack1[L, scala.List, A], fragment: CypherFragment[E], lowPriority: LowPriority): Case.Aux[E, (Known[Return.Expr[scala.List[A]]], scala.List[A])] =
            at[E](e => Return.Expr(e.known.asInstanceOf[Known[CypherFragment.Expr[scala.List[A]]]], None).known -> null.asInstanceOf[scala.List[A]])
        }

        implicit def listOfExpressions[Exprs <: HList, R0 <: HList, U, R <: HList, RT <: HList](
          implicit
          nonEmpty: ops.hlist.IsHCons[Exprs],
          known: ops.hlist.Mapper.Aux[KnownHListTypes.type, Exprs, R0],
          unzip: ops.hlist.Unzip.Aux[R0, U],
          extract: Unpack2[U, Tuple2, R, RT],
          list: ops.hlist.ToTraversable.Aux[R, scala.List, Known[Return.Expr[_]]]
        ): Build.Aux[Exprs, RT, R] =
          new Build[Exprs] {
            type Ret = RT
            type Expr = R
            def apply(exprs: Exprs): List.Aux[RT, R] =
              new List[RT] {
                type Expressions = R
                val expressions: R = unzip(known(exprs)).asInstanceOf[(R, RT)]._1
                val toList: scala.List[Known[Return.Expr[_]]] = list(expressions)
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
      case All => MkStatement("*")
      case Expr(expr, as) => expr.mkCypher.mapT(_ + asStr(as))
      case List(head :: Nil) => head.mkCypher
      case List(head :: tail) => (head.mkCypher, tail.mergeStatements(_.mkString(", "))).map2Statements(_ + ", " + _)
      case Options(expr, distinct, order, skip, limit) =>
        val distinctFrag = if (distinct) "DISTINCT " else ""
        val orderMkS =
          if (order.isEmpty) MkStatement("")
          else {
            val by = order.map{
              case (e, true)  => e.mkCypher
              case (e, false) => e.mkCypher.mapT(_ + " DESC")
            }
            by.mergeStatements(_.mkString(", ")).mapT(" ORDER BY " + _)
          }
        val skipFrag = skip map (" SKIP " + _) getOrElse ""
        val limitFrag = limit map (" LIMIT " + _) getOrElse ""
        (expr.mkCypher, orderMkS).map2Statements((exprT, orderT) => s"$distinctFrag$exprT$orderT$skipFrag$limitFrag")
    }
  }

  sealed trait Clause
  object Clause {
    case class Match(pattern: PatternTuple, optional: Boolean, where: Option[Known[Expr[Boolean]]]) extends Clause
    case class With(ret: Known[Return[_]], where: Option[Known[Expr[Boolean]]]) extends Clause
    case class Unwind(expr: Known[Expr[Seq[_]]], as: String) extends Clause

    implicit lazy val fragment: CypherFragment[Clause] = define[Clause] {
      case Match(pattern, optional, where) =>
        val optionalStr = if (optional) "OPTIONAL " else ""
        val patternMkS = pattern.toList.mergeStatements(_.mkString(", "))
        (patternMkS, whereMkS(where)).map2Statements((patternT, whereT) => s"${optionalStr}MATCH $patternT$whereT")
      case With(ret, where) =>
        (ret.mkCypher, whereMkS(where)).map2Statements((returnT, whereT) => s"WITH $returnT$whereT")
      case Unwind(expr, as) =>
        expr.mkCypher.mapT(exprT => s"UNWIND $exprT${asStr(Option(as))}")
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
      case Let(alias, pattern) => pattern.mkCypher.mapT(patternT => s"${escapeName(alias)} = $patternT")
      case Node(alias, labels, map) => mapMkS(map).mapT(mapT => s"(${aliasStr(alias)}${labelsStr(labels)}$mapT)")
      case Path(left, rel, right) => rel.mkCypher.flatMapT((left, right).join2Statements)
      case Rel(alias, types, map, len, dir) =>
        val lenMkS = len match {
          case None => MkStatement("")
          case Some(Rel.All) => MkStatement("*")
          case Some(Rel.Range(range)) => rangeMkS(range.bimap(Expr.Lit(_), Expr.Lit(_))).mapT("*" + _)
        }
        val paramsMkS = (mapMkS(map), lenMkS).map2Statements((mapT, lenT) => s"[${aliasStr(alias)}${typesStr(types)}$lenT$mapT]")
        dir match {
          case Rel.Outgoing => paramsMkS.mapT(t => s"-$t->")
          case Rel.Incoming => paramsMkS.mapT(t => s"<-$t-")
          case Rel.Any      => paramsMkS.mapT(t => s"-$t-")
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
  private def whereMkS(where: Option[Known[Expr[Boolean]]]) =
    where.map(_.mkCypher.mapT(" WHERE " + _)).getOrElse(MkStatement(""))
  private def asStr(as: Option[String]) = as.map(escapeName).map(" AS " + _).getOrElse("")
  private def mapMkS(map: Map[String, Known[Expr[_]]]) =
    if (map.isEmpty) MkStatement("")
    else map.map{ case (k, v) => v.mkCypher.mapT(s"${escapeName(k)}: " + _) }.mergeStatements(_.mkString("{ ", ", ", " }"))
  private def rangeMkS(ior: Ior[Known[_], Known[_]]) = ior match {
      case Ior.Left(min)      => min.mkCypher.mapT(_ + "..")
      case Ior.Right(max)     => max.mkCypher.mapT(".." + _)
      case Ior.Both(min, max) => (min, max).map2Statements(_ + ".." + _)
  }
}
