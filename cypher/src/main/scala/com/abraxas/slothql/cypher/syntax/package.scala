package com.abraxas.slothql.cypher

import scala.annotation.compileTimeOnly
import scala.language.experimental.macros
import scala.language.{ higherKinds, implicitConversions }
import scala.util.Random

import cats.data.Ior
import shapeless.{ <:!<, Generic, HList, Unpack1, |∨| }

import com.abraxas.slothql.cypher.CypherFragment.{ Expr, Known, Query, Return }
import com.abraxas.slothql.util.raiseCompilationError

package object syntax extends LowPriorityImplicits {

  sealed trait Graph extends Expr.Var[Graph] with Graph.Path with Graph.Vertex
  object Graph{
    private[syntax] def apply(): Graph = new Impl[Root] with Graph {}
    private[syntax] def path(): syntax.Path = new Impl[Path] with Path {}


    sealed trait Pattern
    sealed trait Atom   extends Pattern
    sealed trait Root   extends Pattern

    sealed trait Vertex extends Atom with Root
    sealed trait Edge   extends Atom
    sealed trait Path   extends Root


    private[syntax] sealed abstract class Impl[P <: Pattern] extends Expr.Var[P] {
      self: P =>

      private[syntax] var _alias: String = _ // This `var` should be set only once by a macro
      lazy val name: String = _alias
    }
  }

  type Vertex    = Expr.Var[Graph.Vertex]
  type Edge      = Expr.Var[Graph.Edge]
  type Path      = Expr.Var[Graph.Path]


  final implicit class GraphPatternOps(e: Expr.Var[Graph.Pattern]) {
    /** Call built-in function `func` passing `this` expression as first argument. */
    def func[R](func: String, args: Known[Expr[_]]*): Expr.Func[R] =
      Expr.Func(func, e.known :: args.toList)
  }

  final implicit class GraphAtomOps(e: Expr.Var[Graph.Atom]) {
    def props: Expr.Var[Map[String, Any]] = e.asInstanceOf[Expr.Var[Map[String, Any]]]

    /** Select vertex/edge property. */
    def prop[A](k: String): Expr.MapKey[A] = Expr.MapKey[A](props, k)
    /** Select vertex/edge property as [[Option]]. */
    def propOpt[A](k: String): Expr.MapKey[Option[A]] = prop[Option[A]](k)

    /** Alias for [[prop]]. */
    @deprecated("seems to break query type resolution", since = "03.06.18")
    def apply[A](k: String): Expr.MapKey[A] = prop(k)
    /** Alias for [[propOpt]]. */
    def opt[A](k: String): Expr.MapKey[Option[A]] = propOpt(k)

    /** Call built-in `id` function. */
    def id: Expr.Func[Long] = e.func("id")
    /** Call built-in `count` function. */
    def count: Expr.Func[Long] = e.func("count")
    /** Call built-in `keys` function. */
    def keys: Expr.Func[List[String]] = e.func("keys")

  }

  final implicit class VertexOps(v: Vertex) {
    /** Call built-in `labels` function. */
    def labels: Expr.Func[List[String]] = v.func("labels")
  }

  final implicit class EdgeOps(e: Edge) {
    /** Call built-in `type` function. */
    def tpe: Expr.Func[String] = e.func("type")
    /** Call built-in `type` function. */
    def `type`: Expr.Func[String] = tpe
  }

  final implicit class PathOps(e: Path) {
    /** Call built-in `nodes` function. */
    def nodes: Expr.Func[List[Graph.Vertex]] = e.func("nodes")
    /** Call built-in `relationships` function. */
    def edges: Expr.Func[List[Graph.Edge]] = e.func("relationships")
    /** Call built-in `nodes` function. */
    def length: Expr.Func[Long] = e.func("length")
  }


  object Vertex {
    @inline private[syntax] def apply(): Vertex = new Graph.Impl[Graph.Vertex] with Graph.Vertex
    def unapplySeq(v: Vertex): Option[Seq[AnyRef]] = Some(???)
  }
  object Edge {
    @inline private[syntax] def apply(): Edge = new Graph.Impl[Graph.Edge] with Graph.Edge
    def unapplySeq(v: Edge): Option[Seq[AnyRef]] = Some(???)
  }


  object := {
    def unapply(arg: Any): Option[(String, Any)] = Some(???)
  }
  object :?= {
    def unapply(arg: Any): Option[(String, Option[Any])] = Some(???)
  }

  object ::= {
    def unapply(arg: Graph): Option[(Path, Graph)] = Some(Graph.path() -> Graph())
  }
  object *: {
    // TODO: Binding relationships to a list in a variable length pattern is deprecated by neo4j
    def unapply(edge: Edge): Option[(Expr.Var[List[Graph.Edge]], -[Int, Int], Edge)] = Some(???)
  }

  // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // //

  implicit class FuncSymbolOps(func: Symbol) {
    def func[R](args: Known[Expr[_]]*): Expr.Func[R] = Expr.Func(func.name, args.toList)
  }
  implicit class FuncStringOps(func: String) {
    def func[R](args: Known[Expr[_]]*): Expr.Func[R] = Expr.Func(func, args.toList)
  }

  type Var[+A] = Expr.Var[A]

  implicit class ProcedureSymbolOps(procedure: Symbol) {
    def call(args: Known[Expr[_]]*): ProcedureOps.CallBuilder = new ProcedureOps.CallBuilder(procedure.name, args.toList)
  }
  implicit class ProcedureStringOps(procedure: String) {
    def call(args: Known[Expr[_]]*): ProcedureOps.CallBuilder = new ProcedureOps.CallBuilder(procedure, args.toList)
  }
  // specifying WHERE condition is not supported by this syntax helper
  // assigning aliases to procedure outputs is not supported by this syntax helper
  protected[syntax] object ProcedureOps {
    class CallBuilder(procedure: String, params: List[Known[Expr[_]]]) {
      // def void[R](res: Match.Result[R]): Match.Result[R] = ???
      def yielding(f: Any): Match.Result[_] = macro Match.Result.Call.impl
    }
  }


  protected sealed abstract class AsStringKnownExprOps(expr: Known[Expr[_]]) {
    def asString: Expr.Func[String] = Expr.Func("toString", List(expr))
  }

  implicit class NumberAsStringKnownExprOps   [A: Numeric]               (expr: Known[Expr[A]]) extends AsStringKnownExprOps(expr)
  implicit class NotNumberAsStringKnownExprOps[A: (String |∨| Boolean)#λ](expr: Known[Expr[A]]) extends AsStringKnownExprOps(expr)

  implicit class NumberAsStringExprOps   [E <: Expr[_], A](expr: E)(implicit unpack: Unpack1[E, Expr, A], frag: CypherFragment[E], ev: Numeric[A])                extends AsStringKnownExprOps(expr)
  implicit class NotNumberAsStringExprOps[E <: Expr[_], A](expr: E)(implicit unpack: Unpack1[E, Expr, A], frag: CypherFragment[E], ev: (String |∨| Boolean)#λ[A]) extends AsStringKnownExprOps(expr)


  implicit class BooleanKnownExprOps(expr0: Known[Expr[Boolean]]) {
    def unary_! : Expr.LogicNegationExpr = Expr.LogicNegationExpr(expr0)

    def and(expr1: Known[Expr[Boolean]]): Expr.LogicBinaryExpr                  = binary(expr1, Expr.LogicExpr.And)
    def and[E <: Expr[Boolean]: CypherFragment](expr1: E): Expr.LogicBinaryExpr = binary(expr1, Expr.LogicExpr.And)

    def or (expr1: Known[Expr[Boolean]]): Expr.LogicBinaryExpr                  = binary(expr1, Expr.LogicExpr.Or)
    def or [E <: Expr[Boolean]: CypherFragment](expr1: E): Expr.LogicBinaryExpr = binary(expr1, Expr.LogicExpr.Or)

    def xor(expr1: Known[Expr[Boolean]]): Expr.LogicBinaryExpr                  = binary(expr1, Expr.LogicExpr.Xor)
    def xor[E <: Expr[Boolean]: CypherFragment](expr1: E): Expr.LogicBinaryExpr = binary(expr1, Expr.LogicExpr.Xor)

    def &&(expr1: Known[Expr[Boolean]]): Expr.LogicBinaryExpr                   = and(expr1)
    def &&[E <: Expr[Boolean]: CypherFragment](expr1: E): Expr.LogicBinaryExpr  = and(expr1)

    def ||(expr1: Known[Expr[Boolean]]): Expr.LogicBinaryExpr                   = or(expr1)
    def ||[E <: Expr[Boolean]: CypherFragment](expr1: E): Expr.LogicBinaryExpr  = or(expr1)

    private def binary[E1 <: Expr[Boolean]](expr1: Known[E1], op: Expr.LogicExpr.BinaryOp) =
      Expr.LogicBinaryExpr(expr0, expr1, op)
  }
  implicit class BooleanExprOps[E0 <: Expr[Boolean]: CypherFragment](expr0: E0) extends BooleanKnownExprOps(expr0)

  implicit class CompareAnyKnownOps(expr0: Known[Expr[_]]) {
    def eq (expr1: Known[Expr[_]]): Expr.CompareBinaryAnyExpr                   = binary(expr1, Expr.CompareExpr.Eq)
    def eq [E <: Expr[_]: CypherFragment](expr1: E): Expr.CompareBinaryAnyExpr  = binary(expr1, Expr.CompareExpr.Eq)

    def neq(expr1: Known[Expr[_]]): Expr.CompareBinaryAnyExpr                   = binary(expr1, Expr.CompareExpr.Neq)
    def neq[E <: Expr[_]: CypherFragment](expr1: E): Expr.CompareBinaryAnyExpr  = binary(expr1, Expr.CompareExpr.Neq)

    def ===(expr1: Known[Expr[_]]): Expr.CompareBinaryAnyExpr                   = eq(expr1)
    def ===[E <: Expr[_]: CypherFragment](expr1: E): Expr.CompareBinaryAnyExpr  = eq(expr1)

    def <> (expr1: Known[Expr[_]]): Expr.CompareBinaryAnyExpr                   = neq(expr1)
    def <> [E <: Expr[_]: CypherFragment](expr1: E): Expr.CompareBinaryAnyExpr  = neq(expr1)

    def isNull  : Expr.CompareUnaryExpr = unary(Expr.CompareExpr.IsNull)
    def notNull : Expr.CompareUnaryExpr = unary(Expr.CompareExpr.NotNull)

    private def unary(op: Expr.CompareExpr.UnaryOp) = Expr.CompareUnaryExpr(expr0, op)
    private def binary[E1 <: Expr[_]](expr1: Known[E1], op: Expr.CompareExpr.BinaryAnyOp) =
      Expr.CompareBinaryAnyExpr(expr0, expr1, op)
  }
  implicit class CompareAnyOps[E0 <: Expr[_]: CypherFragment](expr0: E0) extends CompareAnyKnownOps(expr0)

  implicit class CompareOps[A, E0[x] <: Expr[x]](expr0: E0[A])(implicit frag0: CypherFragment[E0[A]]) {
    def lt [E1 <: Expr[A]: CypherFragment](expr1: E1): Expr.CompareBinaryExpr[A] = binary(expr1, Expr.CompareExpr.Lt)
    def lte[E1 <: Expr[A]: CypherFragment](expr1: E1): Expr.CompareBinaryExpr[A] = binary(expr1, Expr.CompareExpr.Lte)
    def gte[E1 <: Expr[A]: CypherFragment](expr1: E1): Expr.CompareBinaryExpr[A] = binary(expr1, Expr.CompareExpr.Gte)
    def gt [E1 <: Expr[A]: CypherFragment](expr1: E1): Expr.CompareBinaryExpr[A] = binary(expr1, Expr.CompareExpr.Gt)

    def < [E1 <: Expr[A]: CypherFragment](expr1: E1): Expr.CompareBinaryExpr[A] = binary(expr1, Expr.CompareExpr.Lt)
    def <=[E1 <: Expr[A]: CypherFragment](expr1: E1): Expr.CompareBinaryExpr[A] = binary(expr1, Expr.CompareExpr.Lte)
    def >=[E1 <: Expr[A]: CypherFragment](expr1: E1): Expr.CompareBinaryExpr[A] = binary(expr1, Expr.CompareExpr.Gte)
    def > [E1 <: Expr[A]: CypherFragment](expr1: E1): Expr.CompareBinaryExpr[A] = binary(expr1, Expr.CompareExpr.Gt)

    def in[E1 <: Expr[List[A]]: CypherFragment](expr1: E1): Expr.In[A] = Expr.In(expr0.known, expr1.known)

    private def binary[E1 <: Expr[A]: CypherFragment](expr1: E1, op: Expr.CompareExpr.BinaryOp) =
      Expr.CompareBinaryExpr(expr0.known, expr1.known, op)
  }

  implicit class StringKnownExprOps(expr0: Known[Expr[String]]) {
    def contains(expr1: Known[Expr[String]]): Expr.StringExpr                     = binary(expr1, Expr.StringExpr.Contains)
    def contains[E <: Expr[String]: CypherFragment](expr1: E): Expr.StringExpr    = binary(expr1, Expr.StringExpr.Contains)

    def startsWith(expr1: Known[Expr[String]]): Expr.StringExpr                   = binary(expr1, Expr.StringExpr.StartsWith)
    def startsWith[E <: Expr[String]: CypherFragment](expr1: E): Expr.StringExpr  = binary(expr1, Expr.StringExpr.StartsWith)

    def endsWith(expr1: Known[Expr[String]]): Expr.StringExpr                     = binary(expr1, Expr.StringExpr.EndsWith)
    def endsWith[E <: Expr[String]: CypherFragment](expr1: E): Expr.StringExpr    = binary(expr1, Expr.StringExpr.EndsWith)

    /** Regular expression match */
    def matches(expr1: Known[Expr[String]]): Expr.StringExpr                      = binary(expr1, Expr.StringExpr.Regex)
    /** Regular expression match */
    def matches[E <: Expr[String]: CypherFragment](expr1: E): Expr.StringExpr     = binary(expr1, Expr.StringExpr.Regex)


    def toLower: Expr.Func[String] = 'toLower.func(expr0)
    def toUpper: Expr.Func[String] = 'toUpper.func(expr0)
    def size:    Expr.Func[Long]   = 'size.func(expr0)

    def toBoolean: Expr.Func[Boolean] = 'toBoolean.func(expr0)
    def toDouble:  Expr.Func[Double]  = 'toFloat.func(expr0)
    def toLong:    Expr.Func[Long]    = 'toInteger.func(expr0)

    /** Returns a string containing the specified number of leftmost characters of the original string. */
    def takeLeft(n: Known[Expr[Long]]): Expr.Func[String]                   = 'left.func(expr0, n)
    def takeLeft[E <: Expr[Long]: CypherFragment](n: E): Expr.Func[String]  = 'left.func(expr0, n)

    /** Returns a string containing the specified number of rightmost characters of the original string. */
    def takeRight(n: Known[Expr[Long]]): Expr.Func[String]                  = 'right.func(expr0, n)
    def takeRight[E <: Expr[Long]: CypherFragment](n: E): Expr.Func[String] = 'right.func(expr0, n)

    /** Returns a string in which all occurrences of a specified string in the original string have been replaced by another (specified) string. */
    def replace(search: Known[Expr[String]], replace: Known[Expr[String]]): Expr.Func[String]                                       = 'replace.func(expr0, search, replace)
    def replace[E1 <: Expr[String]: CypherFragment, E2 <: Expr[String]: CypherFragment](search: E1, replace: E2): Expr.Func[String] = 'replace.func(expr0, search, replace)

    def reverse: Expr.Func[String] = 'reverse.func(expr0)

    /** Returns a list of strings resulting from the splitting of the original string around matches of the given delimiter. */
    def split(delimiter: Known[Expr[String]]): Expr.Func[List[String]]                  = 'split.func(expr0, delimiter)
    def split[E <: Expr[String]: CypherFragment](delimiter: E): Expr.Func[List[String]] = 'split.func(expr0, delimiter)

    /** Returns a substring of the original string, beginning with a 0-based index start. */
    def substring(start: Known[Expr[Long]]): Expr.Func[String]                  = 'substring.func(expr0, start)
    def substring[E <: Expr[Long]: CypherFragment](start: E): Expr.Func[String] = 'substring.func(expr0, start)

    /** Returns a substring of the original string, beginning with a 0-based index start and length. */
    def substring(start: Known[Expr[Long]], length: Known[Expr[Long]]): Expr.Func[String]                                       = 'substring.func(expr0, start, length)
    def substring[E1 <: Expr[Long]: CypherFragment, E2 <: Expr[Long]: CypherFragment](start: E1, length: E2): Expr.Func[String] = 'substring.func(expr0, start, length)

    /** Returns the original string with leading and trailing whitespace removed. */
    def trim: Expr.Func[String] = 'trim.func(expr0)
    /** Returns the original string with leading whitespace removed. */
    def trimLeft: Expr.Func[String] = 'lTrim.func(expr0)
    /** Returns the original string with trailing whitespace removed. */
    def trimRight: Expr.Func[String] = 'rTrim.func(expr0)

    private def binary(expr1: Known[Expr[String]], op: Expr.StringExpr.Op) = Expr.StringExpr(expr0, expr1, op)
  }

  implicit class StringExprOps[E0 <: Expr[String]: CypherFragment](expr0: E0) extends StringKnownExprOps(expr0)


  implicit class NumericKnownExprOps[N: Numeric](expr0: Known[Expr[N]]) {
    def +(expr1: Known[Expr[N]]): Expr.MathematicalBinaryExpr[N]                  = binary(expr1, Expr.MathematicalExpr.Addition)
    def +[E <: Expr[N]: CypherFragment](expr1: E): Expr.MathematicalBinaryExpr[N] = binary(expr1, Expr.MathematicalExpr.Addition)

    def -(expr1: Known[Expr[N]]): Expr.MathematicalBinaryExpr[N]                  = binary(expr1, Expr.MathematicalExpr.Subtraction)
    def -[E <: Expr[N]: CypherFragment](expr1: E): Expr.MathematicalBinaryExpr[N] = binary(expr1, Expr.MathematicalExpr.Subtraction)

    def *(expr1: Known[Expr[N]]): Expr.MathematicalBinaryExpr[N]                  = binary(expr1, Expr.MathematicalExpr.Multiplication)
    def *[E <: Expr[N]: CypherFragment](expr1: E): Expr.MathematicalBinaryExpr[N] = binary(expr1, Expr.MathematicalExpr.Multiplication)

    def /(expr1: Known[Expr[N]]): Expr.MathematicalBinaryExpr[N]                  = binary(expr1, Expr.MathematicalExpr.Division)
    def /[E <: Expr[N]: CypherFragment](expr1: E): Expr.MathematicalBinaryExpr[N] = binary(expr1, Expr.MathematicalExpr.Division)

    def %(expr1: Known[Expr[N]]): Expr.MathematicalBinaryExpr[N]                  = binary(expr1, Expr.MathematicalExpr.ModuloDivision)
    def %[E <: Expr[N]: CypherFragment](expr1: E): Expr.MathematicalBinaryExpr[N] = binary(expr1, Expr.MathematicalExpr.ModuloDivision)

    def ^(expr1: Known[Expr[N]]): Expr.MathematicalBinaryExpr[N]                  = binary(expr1, Expr.MathematicalExpr.Exponentiation)
    def ^[E <: Expr[N]: CypherFragment](expr1: E): Expr.MathematicalBinaryExpr[N] = binary(expr1, Expr.MathematicalExpr.Exponentiation)

    def unary_- : Expr.MathematicalUnaryExpr[N] = Expr.MathematicalUnaryExpr(expr0, Expr.MathematicalExpr.Negation)

    private def binary(expr1: Known[Expr[N]], op: Expr.MathematicalExpr.BinaryOp) = Expr.MathematicalBinaryExpr(expr0, expr1, op)
  }

  implicit class NumericExprOps[E0 <: Expr[_], N](expr0: E0)
                                                 (implicit
                                                  unpack: Unpack1[E0, Expr, N],
                                                  frag: CypherFragment[E0],
                                                  numeric: Numeric[N]
                                                 ) extends NumericKnownExprOps[N](Known(expr0).asInstanceOf[Known[Expr[N]]])


  protected sealed class ListMapOps[A](list: => Known[Expr[List[A]]], filter: Option[Expr.Var[A] => Known[Expr[Boolean]]]) {
    def map[B](f: Expr.Var[A] => Known[Expr[B]]): Expr.ListComprehension[A, B] = {
      val (arg, expr) = ListOps.applyExprVar(f)
      Expr.ListComprehension(list, arg.name, filter = filter.map(_(arg)), map = Some(expr))
    }
  }
  implicit class ListOps[A, E0 <: Expr[_]](expr0: E0)(implicit frag0: CypherFragment[E0], ev: E0 <:< Expr[List[A]])
    extends ListMapOps[A](expr0.known.widen, None)
  {
    private lazy val listExpr: Known[Expr[List[A]]] = expr0.known.widen

    def concat[E1 <: Expr[List[A]]: CypherFragment](expr1: E1): Expr.Concat[A] =
      Expr.Concat(listExpr, expr1.known)
    def ++[E1 <: Expr[List[A]]: CypherFragment](expr1: E1): Expr.Concat[A] = concat(expr1)

    def at[I: (Int |∨| Long)#λ, E1[x] <: Expr[x]](i: E1[I])(implicit frag1: CypherFragment[E1[Long]]): Expr.AtIndex[A] =
      Expr.AtIndex(listExpr, i.asInstanceOf[E1[Long]].known)

    def at[I1: (Int |∨| Long)#λ, I2: (Int |∨| Long)#λ, E1[x] <: Expr[x], E2[x] <: Expr[x]](
      range: Ior[E1[I1], E2[I2]]
    )(
      implicit frag1: CypherFragment[E1[I1]], frag2: CypherFragment[E2[I2]]
    ): Expr.AtRange[A] =
      Expr.AtRange(listExpr, range.bimap(_.known.asInstanceOf[Known[Expr[Long]]], _.known.asInstanceOf[Known[Expr[Long]]]))

    def at[I1: (Int |∨| Long)#λ, I2: (Int |∨| Long)#λ](range: Ior[Known[Expr[I1]], Known[Expr[I2]]]): Expr.AtRange[A] =
      Expr.AtRange(listExpr, range.bimap(_.asInstanceOf[Known[Expr[Long]]], _.asInstanceOf[Known[Expr[Long]]]))

    def at[I1: (Int |∨| Long)#λ, I2: (Int |∨| Long)#λ, E1[x] <: Expr[x], E2[x] <: Expr[x]](l: E1[I1], r: E2[I2])(
      implicit frag1: CypherFragment[E1[I1]], frag2: CypherFragment[E2[I2]]
    ): Expr.AtRange[A] = at(Ior.Both(l, r))

    def at[I1: (Int |∨| Long)#λ, I2: (Int |∨| Long)#λ](l: Known[Expr[I1]], r: Known[Expr[I2]]): Expr.AtRange[A] = at(Ior.Both(l, r))

    def from[I: (Int |∨| Long)#λ, E1[x] <: Expr[x]](i: E1[I])(implicit frag1: CypherFragment[E1[I]]): Expr.AtRange[A] = at[I, I, E1, E1](Ior.Left(i))
    def from[I: (Int |∨| Long)#λ](known: Known[Expr[I]]): Expr.AtRange[A] = at[I, I](Ior.Left(known))

    def to[I: (Int |∨| Long)#λ, E1[x] <: Expr[x]](i: E1[I])(implicit frag1: CypherFragment[E1[I]]): Expr.AtRange[A] = at[I, I, E1, E1](Ior.Right(i))
    def to[I: (Int |∨| Long)#λ](i: Known[Expr[I]]): Expr.AtRange[A] = at[I, I](Ior.Right(i))

    def slice[I1: (Int |∨| Long)#λ, I2: (Int |∨| Long)#λ, E1[x] <: Expr[x], E2[x] <: Expr[x]](
      rangeOpt: Option[Ior[E1[I1], E2[I2]]]
    )(
      implicit frag1: CypherFragment[E1[I1]], frag2: CypherFragment[E2[I2]]
    ): Known[Expr[List[A]]] = rangeOpt.map(at(_).known) getOrElse listExpr

    def size: Expr.Func[Long] = Expr.Func("size", List(expr0))

    def withFilter(f: Expr.Var[A] => Known[Expr[Boolean]]): ListOps.WithFilter[A] = new ListOps.WithFilter(listExpr, f)

    def filter(f: Expr.Var[A] => Known[Expr[Boolean]]): Expr.ListComprehension[A, A] = {
      val (arg, expr) = ListOps.applyExprVar(f)
      Expr.ListComprehension(listExpr, arg.name, filter = Some(expr), map = None)
    }
    def filter0(expr: Known[Expr[Boolean]]): Expr.ListComprehension[A, A] =
      Expr.ListComprehension(listExpr, "_", filter = Some(expr), map = None)

    def reduce[B](b: Known[Expr[B]])(f: (Expr.Var[A], Expr.Var[B]) => Known[Expr[B]]): Expr.ReduceList[A, B] = {
      val elemAlias, accAlias = randomAlias()
      val expr = f(Expr.Var[A](elemAlias), Expr.Var[B](accAlias))
      Expr.ReduceList(listExpr, elemAlias, b, accAlias, expr)
    }

    // predicates
    def all   (f: Expr.Var[A] => Known[Expr[Boolean]]): Expr.ListPredicate[A] = predicate(Expr.ListPredicate.All, f)
    def any   (f: Expr.Var[A] => Known[Expr[Boolean]]): Expr.ListPredicate[A] = predicate(Expr.ListPredicate.Any, f)
    def exists(f: Expr.Var[A] => Known[Expr[Boolean]]): Expr.ListPredicate[A] = predicate(Expr.ListPredicate.Exists, f)
    def none  (f: Expr.Var[A] => Known[Expr[Boolean]]): Expr.ListPredicate[A] = predicate(Expr.ListPredicate.None, f)
    def single(f: Expr.Var[A] => Known[Expr[Boolean]]): Expr.ListPredicate[A] = predicate(Expr.ListPredicate.Single, f)

    private def predicate(pred: Expr.ListPredicate.Predicate, f: Expr.Var[A] => Known[Expr[Boolean]]) = {
      val (arg, expr) = ListOps.applyExprVar(f)
      Expr.ListPredicate(listExpr, arg.name, pred, expr)
    }
  }
  object ListOps {
    protected class WithFilter[A](list: Known[Expr[List[A]]], filter: Expr.Var[A] => Known[Expr[Boolean]]) extends ListMapOps(list, Some(filter))

    protected[syntax] def applyExprVar[A, B](f: Expr.Var[A] => Known[Expr[B]]): (Expr.Var[A], Known[Expr[B]]) = {
      val arg = Expr.Var[A](randomAlias())
      val expr = f(arg)
      arg -> expr
    }
  }

  implicit class MapOps[A, E0 <: Expr[_]](expr0: E0)(implicit frag0: CypherFragment[E0], ev: E0 <:< Expr[Map[String, A]]) {
    def value[V](key: String): Expr.MapKey[V] = Expr.MapKey(expr0.known.widen, key)

    def add(entries: MapEntry[A]*): Expr.MapAdd[A] = Expr.MapAdd(expr0.known.widen, entries.map(_.toPair).toMap)
    def add(map: Map[String, Known[Expr[A]]]): Expr.MapAdd[A] = Expr.MapAdd(expr0.known.widen, map)
  }


  implicit class SimpleCaseKnownExprOps[A](expr: Known[Expr[A]]) {
    /** Simple case expression. */
    def whenUnsafe[B](case0: SimpleCaseSyntax.Case[A, B], cases: SimpleCaseSyntax.Case[A, B]*): Expr.SimpleCaseExpr[A, B] = SimpleCaseSyntax.make(expr, case0 +: cases, None)
    /** Simple case expression. */
    def when[B](case0: SimpleCaseSyntax.Case[A, B], cases: SimpleCaseSyntax.Case[A, B]*): SimpleCaseSyntax.Builder[A, B] = new SimpleCaseSyntax.Builder(expr, case0 +: cases)
  }
  implicit class SimpleCaseExprOps[E <: Expr[_], A](expr: E)(implicit unpack: Unpack1[E, Expr, A], frag: CypherFragment[E])
    extends SimpleCaseKnownExprOps[A](expr.known.asInstanceOf[Known[Expr[A]]])

  object SimpleCaseSyntax {
    protected[syntax] class Builder[A, B](value: Known[Expr[A]], cases: Seq[Case[A, B]]) {
      def otherwise(default: Known[Expr[B]]): Expr.SimpleCaseExpr[A, B] = make(value, cases, Some(default))
    }
    protected[syntax] def make[A, B](value: Known[Expr[A]], cases: Seq[Case[A, B]], default: Option[Known[Expr[B]]]): Expr.SimpleCaseExpr[A, B] =
      Expr.SimpleCaseExpr(value, cases.map(_.toPair).toMap, default)

    protected[syntax] case class Case[A, B](value: Known[Expr[A]], result: Known[Expr[B]]) {
      def toPair: (Known[Expr[A]], Known[Expr[B]]) = value -> result
    }
    object Case {
      implicit def pairKnownToCase[A, B](pair: (Known[Expr[A]], Known[Expr[B]])): Case[A, B] = Case(pair._1, pair._2)
      implicit def pairToCase[A, EA[_] <: Expr[_], B, EB[_] <: Expr[_]](pair: (EA[A], EB[B]))
                                                                       (implicit fragA: CypherFragment[EA[A]], fragB: CypherFragment[EB[B]]): Case[A, B] =
        Case(Known(pair._1)(fragA).asInstanceOf[Known[Expr[A]]], Known(pair._2)(fragB).asInstanceOf[Known[Expr[B]]])
    }
  }

  /** Generic case expression. */
  def whenUnsafe[A](case0: GenericCaseSyntax.Case[A], cases: GenericCaseSyntax.Case[A]*): Expr.GenericCaseExpr[A] = GenericCaseSyntax.make(case0 +: cases, None)
  /** Generic case expression. */
  def when[A](case0: GenericCaseSyntax.Case[A], cases: GenericCaseSyntax.Case[A]*): GenericCaseSyntax.Builder[A] = new GenericCaseSyntax.Builder(case0 +: cases)

  object GenericCaseSyntax {
    protected[syntax] class Builder[A](cases: Seq[Case[A]]) {
      def otherwise(default: Known[Expr[A]]): Expr.GenericCaseExpr[A] = make(cases, Some(default))
    }
    protected[syntax] def make[A](cases: Seq[Case[A]], default: Option[Known[Expr[A]]]): Expr.GenericCaseExpr[A] =
      Expr.GenericCaseExpr(cases.map(_.toPair).toMap, default)

    protected[syntax] case class Case[A](value: Known[Expr[Boolean]], result: Known[Expr[A]]) {
      def toPair: (Known[Expr[Boolean]], Known[Expr[A]]) = value -> result
    }
    object Case {
      implicit def pairKnownToCase[A](pair: (Known[Expr[Boolean]], Known[Expr[A]])): Case[A] = Case(pair._1, pair._2)
      implicit def pairToCase[EB <: Expr[Boolean], A, EA[_] <: Expr[_]](pair: (EB, EA[A]))
                                                                       (implicit fragA: CypherFragment[EA[A]], fragB: CypherFragment[EB]): Case[A] =
        Case(Known(pair._1)(fragB).widen, Known(pair._2)(fragA).asInstanceOf[Known[Expr[A]]])
    }
  }


  private def randomAlias(): String = Random.alphanumeric.take(20).mkString

  // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // //


  object -> {
    def unapply(g: Graph): Option[(Vertex, Vertex)] = Some(Vertex() -> Vertex())
    def unapply(v: Vertex): Option[(Vertex, Vertex)] = Some(Vertex() -> Vertex())
  }
  object `<-` {
    def unapply(g: Graph): Option[(Vertex, Vertex)] = Some(Vertex() -> Vertex())
    def unapply(v: Vertex): Option[(Vertex, Vertex)] = Some(Vertex() -> Vertex())
  }

  case class -[A, B](a: A, b: B)
//  object - {
//    def unapply(g: Graph): Option[(Vertex - Edge, Vertex)] = ???
//    def unapply(v: Vertex): Option[(Vertex - Edge, Vertex)] = ???
//  }

  object > {
    def unapply(g: Graph): Option[(Vertex - Edge, Vertex)] = Some(new -(Vertex(), Edge()) -> Vertex())
    def unapply(v: Vertex): Option[(Vertex - Edge, Vertex)] = Some(new -(Vertex(), Edge()) -> Vertex())

    def unapply(path: Vertex - Edge): Option[(Vertex - Edge, Vertex - Edge)] = Some(new -(Vertex(), Edge()) -> new -(Vertex(), Edge()))
  }
  object < {
    def unapply(g: Graph): Option[(Vertex, Edge - Vertex)] = Some(Vertex() -> new -(Edge(), Vertex()))
    def unapply(v: Vertex): Option[(Vertex, Edge - Vertex)] = Some(Vertex() -> new -(Edge(), Vertex()))
  }

  object -- {
    def unapply(g: Graph): Option[(Vertex, Vertex)] = Some(Vertex() -> Vertex())
    def unapply(v: Vertex): Option[(Vertex, Vertex)] = Some(Vertex() -> Vertex())
  }


  lazy val ⟶ : ->.type = ->
  lazy val ⟵ : `<-`.type = `<-`
  lazy val ⟷ : --.type = --


  implicit def lit[A](a: A): Expr.Lit[A] = Expr.Lit[A](a)
  implicit def knownLit[A](a: A)(implicit frag: CypherFragment[Expr.Lit[A]]): Known[Expr.Lit[A]] = Expr.Lit[A](a).known

  def cypherNull[A]: Expr.Null[A] = Expr.Null[A]

  def list[A](exprs: Known[Expr[A]]*): Expr.List[A] = Expr.List[A](exprs.toList)

  def distinct[A](expr: Known[Expr[A]]): Known[Expr[A]] = Expr.Distinct(expr)
  def collect[A](expr: Known[Expr[A]]): Expr.Func[List[A]] = 'collect.func[List[A]](expr)

  def dict(entries: MapEntry[Any]*): Expr.Map[Any] = Expr.Map(entries.map(_.toPair).toMap)
  def dict(map: Map[String, Known[Expr[Any]]]): Expr.Map[Any] = Expr.Map(map)

  case class MapEntry[+A](key: String, value: Known[Expr[A]]) { def toPair: (String, Known[Expr[A]]) = key -> value }
  object MapEntry {
    implicit def pairKnownToMapEntry[A](pair: (String, Known[Expr[A]])): MapEntry[A] = MapEntry(pair._1, pair._2)
    implicit def pairToMapEntry[A, E[_] <: Expr[_]](pair: (String, E[A]))(implicit frag: CypherFragment[E[A]]): MapEntry[A] =
      MapEntry(pair._1, Known(pair._2)(frag).asInstanceOf[Known[Expr[A]]])
  }


  implicit class ReturnAsOps[A, E <: Expr[_]](expr: E)(implicit frag: CypherFragment[E], tpe: E <:< Expr[A]) {
    def as(alias: String): Return.Expr[A] = Return.Expr(expr.known.widen[Expr[A]], as = Option(alias))
  }

  implicit class ReturnAsKnownOps[A](expr: Known[Expr[A]]) {
    def as(alias: String): Return.Expr[A] = Return.Expr(expr, as = Option(alias))
  }

  sealed trait QueryReturn[T]{
    type Ret
    type Out <: Return[Ret]
    def apply(t: T): Out
  }
  object QueryReturn {
    type Aux   [T, R]                 = QueryReturn[T] { type Ret = R; type Out <: Return.Return0[R] }
    type AuxOut[T, R, O <: Return[R]] = QueryReturn[T] { type Ret = R; type Out = O }

    implicit def returnExpr[A, E <: Expr[_]](
      implicit
      ev: E <:< Expr.Inv[A],
      fragment: CypherFragment[E]
    ): AuxOut[E, A, Return.Expr[A]] =
      new QueryReturn[E] {
        type Ret = A
        type Out = Return.Expr[A]
        def apply(e: E): Return.Expr[A] = Return.Expr(Known(e).widen, as = None)
      }

    implicit def returnReturnExpr[A, E <: Return.Expr[_]](
      implicit
      unpack: Unpack1[E, Return.Expr, A],
      fragment: CypherFragment[E]
    ): AuxOut[E, A, Return.Expr[A]] =
      new QueryReturn[E] {
        type Ret = A
        type Out = Return.Expr[A]
        def apply(e: E): Return.Expr[A] = e.asInstanceOf[Return.Expr[A]]
      }

    implicit def returnTuple[P <: Product, L <: HList, R <: HList](
      implicit
      ev: P <:!< Expr[_],
      gen: Generic.Aux[P, L],
      build: Return.Tuple.FromHList.Aux[L, R]
    ): AuxOut[P, R, Return.Tuple[R]] =
      new QueryReturn[P] {
        type Ret = R
        type Out = Return.Tuple[R]
        def apply(p: P): Return.Tuple[R] = build(gen.to(p))
      }

    implicit lazy val returnUntyped: AuxOut[Return.Untyped, List[Any], Return.Untyped] =
      new QueryReturn[Return.Untyped] {
        type Ret = List[Any]
        type Out = Return.Untyped
        @inline def apply(t: Return.Untyped): Return.Untyped = t
      }

    implicit def returnOptions[A, E <: Return.Options[_]](implicit ev: E <:< Return.Options.Inv[A]): AuxOut[E, A, Return.Options[A]] =
      _retOptions.asInstanceOf[AuxOut[E, A, Return.Options[A]]]
    private lazy val _retOptions = new QueryReturn[Return.Options[_]] {
      type Ret = Any
      type Out = Return.Options[_]
      def apply(t: Return.Options[_]): Return.Options[_] = t
    }
  }


  def returnTuple(exprs: Iterable[Known[Expr[_]]]): Return.Untyped = CypherFragment.Return.Untyped(exprs)

  implicit def toReturnOps[E, A](e: E)(implicit rq: QueryReturn.Aux[E, A]): ReturnOps[A] = ReturnOps(rq(e))
  implicit def toQueryMatchResult[R](q: Query.Clause[R]): Match.Result.Clause[R] = new Match.Result.Clause[R]{ protected[syntax] def clause: Query.Clause[R] = q }


  def unwind[A, R](expr: Known[Expr[Seq[A]]])(f: Expr.Var[A] => Match.Result[R]): Match.Result.Unwind[A, R] =
    macro Match.Result.Unwind.instanceImpl[A, R]

  /** Generates {{{ WITH * WHERE ... }}} */
  def filter[R](f: Known[Expr[Boolean]])(res: Match.Result[R]): Match.Result[R] =
    new Match.Result.With[R](Return.Wildcard.as[R], res.result, Some(f))

  object `with` {
    def apply[R](wildcard: Boolean, ops: syntax.ReturnOps[Any] => syntax.ReturnOps[Any], vars: Match.Result.With.Var*)
                (res: Match.Result[R]): Match.Result[R] =
      macro Match.Result.With.WithMacros.implWOV[R]

    def apply[R](wildcard: Boolean, vars: Match.Result.With.Var*)(res: Match.Result[R]): Match.Result[R] =
      macro Match.Result.With.WithMacros.implWV[R]

    /** wildcard = false */
    def apply[R](ops: syntax.ReturnOps[Any] => syntax.ReturnOps[Any], vars: Match.Result.With.Var*)
                (res: Match.Result[R]): Match.Result[R] =
      macro Match.Result.With.WithMacros.implFOV[R]

    /** wildcard = false */
    def apply[R](vars: Match.Result.With.Var*)(res: Match.Result[R]): Match.Result[R] =
      macro Match.Result.With.WithMacros.implFV[R]
  }

  object withWildcard {
    /** wildcard = true */
    def apply[R](ops: syntax.ReturnOps[Any] => syntax.ReturnOps[Any])
                (res: Match.Result[R]): Match.Result[R] =
      macro Match.Result.With.WithMacros.implTO[R]

    /** wildcard = true */
    def apply[R](ops: syntax.ReturnOps[Any] => syntax.ReturnOps[Any], var0: Match.Result.With.Var, vars: Match.Result.With.Var*)
                (res: Match.Result[R]): Match.Result[R] =
      macro Match.Result.With.WithMacros.implTOV[R]

    /** wildcard = true */
    def apply[R](vars: Match.Result.With.Var*)(res: Match.Result[R]): Match.Result[R] =
      macro Match.Result.With.WithMacros.implTV[R]
  }


  type Param[+A] = Expr.Param[A]
  def parameterized(f: Any): Match.ParameterizedQuery[_, _] = macro Match.ParameterizedQuery.impl

  final case class ReturnOps[A] protected (
      private val _ret: Known[Return.Return0[A]],
      private val _distinct: Boolean    = false,
      private val _order: Return.Order  = Nil,
      private val _skip: Option[Known[CypherFragment.Expr.Input[Long]]]   = None,
      private val _limit: Option[Known[CypherFragment.Expr.Input[Long]]]  = None
  ) extends Match.Result.Ret[A]
  {
    protected[syntax] def ret: Known[Return[A]] = Return.Options(_ret, _distinct, _order, _skip, _limit).known

    def orderBy(by: ReturnOps.OrderBy*): ReturnOps[A] = copy(_order = _order ++ by.map(_.asPair).toMap)
    def skip(n: Long): ReturnOps[A] = copy(_skip = Some(n))
    def skip(n: Option[Long]): ReturnOps[A] = copy(_skip = n.map(Expr.Lit(_)))
    def skip(n: Param[Long]): ReturnOps[A] = copy(_skip = Some(n))
    def limit(n: Long): ReturnOps[A] = copy(_limit = Some(n))
    def limit(n: Option[Long]): ReturnOps[A] = copy(_limit = n.map(Expr.Lit(_)))
    def limit(n: Param[Long]): ReturnOps[A] = copy(_limit = Some(n))
    def distinct: ReturnOps[A] = copy(_distinct = true)
    def distinct(b: Boolean): ReturnOps[A] = copy(_distinct = b)
  }

  object ReturnOps {
    sealed trait OrderBy{
      val expr: Known[Expr[_]]
      def isAscending: Boolean = this.isInstanceOf[Ascending]
      def asPair: (Known[Expr[_]], Boolean) = expr -> isAscending
    }
    case class Ascending(expr: Known[Expr[_]]) extends OrderBy
    case class Descending(expr: Known[Expr[_]]) extends OrderBy

    implicit def defaultOrderingIsAscending[E <: Expr[_]](e: E)(implicit frag: CypherFragment[E]): Ascending = Ascending(e)
  }

  implicit class OrderDescendingOps[E <: Expr[_]: CypherFragment](e: E) {
    def desc: ReturnOps.Descending = ReturnOps.Descending(e)
  }


  implicit class QueryOps[R0](q0: Query[R0]) {
    def union   [R1 >: R0](query: Query[R1]): Query[R1] = Query.Union(q0, query, all = false)
    def unionAll[R1 >: R0](query: Query[R1]): Query[R1] = Query.Union(q0, query, all = true)
  }
}

trait LowPriorityImplicits {
  @compileTimeOnly("`unwrapBooleanExprInIfGuard` is being used outside of `slothql.cypher.syntax.Match` macro")
  implicit def unwrapBooleanExprInIfGuard(e: Expr[Boolean]): Boolean = unexpected
  @compileTimeOnly("`unwrapKnownBooleanExprInIfGuard` is being used outside of `slothql.cypher.syntax.Match` macro")
  implicit def unwrapKnownBooleanExprInIfGuard(e: Known[Expr[Boolean]]): Boolean = unexpected

  @compileTimeOnly("`unwrapBooleanOptionExprInIfGuard` is being used outside of `slothql.cypher.syntax.Match` macro")
  @raiseCompilationError("Cannot use unknown optional expressions in `if` guard, make it Option[Known[Expr[Boolean]]]")
  implicit def unwrapBooleanOptionExprInIfGuard(e: Option[Expr[Boolean]]): Boolean = unexpected
  @compileTimeOnly("`unwrapKnownBooleanOptionExprInIfGuard` is being used outside of `slothql.cypher.syntax.Match` macro")
  implicit def unwrapKnownBooleanOptionExprInIfGuard(e: Option[Known[Expr[Boolean]]]): Boolean = unexpected

  private def unexpected: Nothing = sys.error("This call should have been replaced by macro")
}
