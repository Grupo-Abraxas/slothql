package com.arkondata.slothql.newcypher

import scala.annotation.compileTimeOnly
import scala.language.experimental.macros
import scala.language.implicitConversions

import cats.data.Ior
import shapeless.tag.@@
import shapeless.{ =:!=, |∨| }

import com.arkondata.slothql.newcypher.{ CypherFragment => CF }

package object syntax {

  object Match {
    def apply[R]              (query: Node => CF.Query.Query0[R]): CF.Query.Query0[R] = macro CypherSyntaxPatternMacros.match_[R]
//  def optional[R]           (query: Node => CF.Query[R]): CF.Query[R] = macro CypherSyntaxPatternMacros.optional[R]
//  def maybe[R](opt: Boolean)(query: Node => CF.Query[R]): CF.Query[R] = macro CypherSyntaxPatternMacros.maybe[R]
  }

  // TODO: support *
  // TODO: support WHERE
  // TODO: support DISTINCT
  // TODO: support ORDER BY
  // TODO: support LIMIT/OFFSET
  object With {
    def apply[T1, R](t1: CF.Expr[T1])
                    (query: CF.Expr[T1] => CF.Query.Query0[R]): CF.Query.Query0[R] =
      macro CypherSyntaxWithMacros.with1[T1, R]

    def apply[T1, T2, R](t1: CF.Expr[T1], t2: CF.Expr[T2])
                        (query: (CF.Expr[T1], CF.Expr[T2]) => CF.Query.Query0[R]): CF.Query.Query0[R] =
      macro CypherSyntaxWithMacros.with2[T1, T2, R]
  }

  object Unwind {
    // def apply[R](query: Node => CF.Query[R]): CF.Query[R] = macro CypherSyntaxPatternMacros.unwind[R]
  }

  object Call {
    // def apply[R](query: Node => CF.Query[R]): CF.Query[R] = macro CypherSyntaxPatternMacros.call[R]
  }

  object Create {
    // def apply[R](query: Node => CF.Query[R]): CF.Query[R] = macro CypherSyntaxPatternMacros.create[R]
  }

  object Merge {
    // def apply[R](query: Node => CF.Query[R]): CF.Query[R] = macro CypherSyntaxPatternMacros.merge[R]
  }

  object Delete {
    // def apply[R](query: Node => CF.Query[R]): CF.Query[R] = macro CypherSyntaxPatternMacros.delete[R]
  }

  object Foreach {
    // def apply[R](query: Node => CF.Query[R]): CF.Query[R] = macro CypherSyntaxPatternMacros.foreach[R]
  }

  // // // // // // // // // // // // // // // // //
  // // // // // Matching Graph Paths // // // // //
  // // // // // // // // // // // // // // // // //

  sealed trait GraphElem extends CypherStatement.Alias
  object GraphElem {
    sealed trait Node extends GraphElem
    sealed trait Rel  extends GraphElem { type Dir <: Rel.Direction }

    type NodeElem = Map[String, Any] @@ GraphElem.Node
    type RelElem  = Map[String, Any] @@ GraphElem.Rel
  }
  sealed trait GraphPath

  type Node = CF.Expr[GraphElem.NodeElem] with CypherStatement.Alias
  type Rel  = CF.Expr[GraphElem.RelElem]  with CypherStatement.Alias
  type Path = CF.Expr[GraphPath]          with CypherStatement.Alias

  object CypherSyntaxFromMacro {
    def mkNode(name: String): Node                          = new CF.Expr.Alias(name) with GraphElem.Node
    def mkRel[D <: Rel.Direction](name: String): Rel.Aux[D] = new CF.Expr.Alias(name) with GraphElem.Rel { type Dir = D }
    def mkPath(name: String): Path                          = new CF.Expr.Alias(name) with GraphPath
  }

  // // // // // //
  // N = Node
  // R = Rel
  // N   => N < RN
  // NR  => N < RNX
  // N   => NR > N
  // NR  => NR > NR
  // NR  => N - R
  // RN  => R - N
  // RNX => RN - R
  // RNX => RN

  protected object CypherSyntaxInternal {
    sealed trait NR {
      type Dir <: Rel.Direction
    }
    sealed trait NRI extends NR { type Dir = Rel.Incoming }
    sealed trait NRO extends NR { type Dir = Rel.Outgoing }

    sealed trait RN{
      type Dir <: Rel.Direction
    }
    sealed trait RNI extends RN { type Dir = Rel.Incoming }
    sealed trait RNO extends RN { type Dir = Rel.Outgoing }

    sealed trait RNX extends RN {
      type Dir = DirRight
      type DirLeft  <: Rel.Direction
      type DirRight <: Rel.Direction
    }
    sealed trait RNXII { type DirLeft = Rel.Incoming; type DirRight = Rel.Incoming }
    sealed trait RNXIO { type DirLeft = Rel.Incoming; type DirRight = Rel.Outgoing }
//    sealed trait RNXOI { type DirLeft = Rel.Outgoing; type DirRight = Rel.Incoming }
//    sealed trait RNXOO { type DirLeft = Rel.Outgoing; type DirRight = Rel.Outgoing }
  }
  import CypherSyntaxInternal._

  @compileTimeOnly("Con only be used inside `Match` / `Create`")
  object < {
    def unapply(n: Node): Option[(Node, RNI)] = ???
    def unapply(n: NRI): Option[(Node, RNXII)] = ???
    def unapply(n: NRO): Option[(Node, RNXIO)] = ???
  }

  @compileTimeOnly("Con only be used inside `Match` / `Create`")
  object > {
    def unapply(n: Node): Option[(NRO, Node)] = ???
    def unapply(n: NRI): Option[(NRO, NRI)] = ???
    def unapply(n: NRO): Option[(NRO, NRO)] = ???
  }

  @compileTimeOnly("Con only be used inside `Match` / `Create`")
  object - {
    def unapply(r: NRI): Option[(Node, Rel.Aux[Rel.Incoming])] = ???
    def unapply(r: NRO): Option[(Node, Rel.Aux[Rel.Outgoing])] = ???
    def unapply(l: RNI): Option[(Rel.Aux[Rel.Incoming], Node)] = ???
    def unapply(l: RNO): Option[(Rel.Aux[Rel.Outgoing], Node)] = ???
    def unapply(l: RNXII): Option[(RNI, Rel.Aux[Rel.Incoming])] = ???
    def unapply(l: RNXIO): Option[(RNI, Rel.Aux[Rel.Outgoing])] = ???
//    def unapply(l: RNXOI): Option[(RNO, Rel[Rel.Incoming])] = ???
//    def unapply(l: RNXOO): Option[(RNO, Rel[Rel.Outgoing])] = ???
  }

  @compileTimeOnly("Con only be used inside `Match` / `Create`")
  object Node {
    /** Supported params: {{{String}}}, {{{Iterable[String]}}}, {{{:=[_]}}}. */
    def unapplySeq(n: Node): Option[Seq[Any]] = ???
  }
  @compileTimeOnly("Con only be used inside `Match` / `Create`")
  object Rel {
    type Aux[D <: Rel.Direction]  = Rel { type Dir = D }

    type Direction = CF.Pattern.Rel.Direction
    type Incoming  = CF.Pattern.Rel.Incoming.type
    type Outgoing  = CF.Pattern.Rel.Outgoing.type

    /** Supported params: {{{String}}}, {{{Iterable[String]}}}, {{{:=[_]}}}, {{{**}}}. */
    def unapplySeq(r: Rel): Option[Seq[Any]] = ???
  }

  @compileTimeOnly("Con only be used inside `Match` / `Create`")
  object := {
    def unapply(any: Any): Option[(String, Any)] = ???
  }
  @compileTimeOnly("Con only be used inside `Match`")
  object ** {
    def unapply(any: Any): Option[(Int, Int)] = ???
  }
  @compileTimeOnly("Con only be used inside `Match`")
  object ::= {
    def unapply(any: Any): Option[(Path, Node)] = ???
  }

  // // // // // // // // // // // // // // // // //
  // // // // //  Return Expressions  // // // // //
  // // // // // // // // // // // // // // // // //

  implicit def cypherSyntaxExprToReturn[A](expr: CF.Expr[A]): CF.Return[A] = CF.Return.Expr(expr, as = None)
  implicit def cypherSyntaxExprToQueryReturn[A](expr: CF.Expr[A]): CF.Query.Return[A] = CF.Query.Return(expr)
  implicit def cypherSyntaxReturnToQueryReturn[A](ret: CF.Return[A]): CF.Query.Return[A] = CF.Query.Return(ret)

  implicit final class CypherSyntaxReturnAsOps[A](expr: CF.Expr[A]) {
    def as(alias: CypherStatement.Alias): CF.Return.Expr[A] = CF.Return.Expr(expr, as = Option(alias))
    def as(alias: String): CF.Return.Expr[A] = as(CF.Expr.Alias(alias))
  }

  implicit def cypherSyntaxTupleToReturn[T <: Product](tuple: T)(implicit ret: CypherSyntaxReturnTuple[T]): CF.Return[ret.Out] = ret(tuple)
  implicit def cypherSyntaxTupleToQueryReturn[T <: Product](tuple: T)(implicit ret: CypherSyntaxReturnTuple[T]): CF.Query.Return[ret.Out] = CF.Query.Return(ret(tuple))

  trait CypherSyntaxReturnTuple[T <: Product] {
    type Out
    def apply(t: T): CF.Return[Out]
  }
  object CypherSyntaxReturnTuple {
    implicit def cypherSyntaxReturnTuple[T <: Product]: CypherSyntaxReturnTuple[T] = macro CypherSyntaxMacros.returnTuple[T]
  }

  // // // // // // // // // // // // // // // // // //
  // // // // // Ops: Node & Rel & Path  // // // // //
  // // // // // // // // // // // // // // // // // //

  implicit final class CypherSyntaxGraphElemOps[E <: GraphElem](g: CF.Expr[Map[String, Any] @@ E]) {
    /** Select all vertex/edge properties */
    def props: CF.Expr[Map[String, Any]] = g.asInstanceOf[CF.Expr[Map[String, Any]]]

    /** Select vertex/edge property. */
    def prop[A](k: String): CF.Expr[A] = CF.Expr.MapKey[A](props, k)

    /** Select vertex/edge property as [[Option]]. */
    def propOpt[A](k: String): CF.Expr[Option[A]] = prop[Option[A]](k)

    /** Call built-in function `func` passing `this` expression as first argument. */
    def func[R](func: String, args: CF.Expr[_]*): CF.Expr[R] = CF.Expr.Func(func, props :: args.toList)

    /** Call built-in `id` function. */
    def id: CF.Expr[Long] = func("id")
    /** Call built-in `count` function. */
    def count: CF.Expr[Long] = func("count")
    /** Call built-in `keys` function. */
    def keys: CF.Expr[List[String]] = func("keys")
  }

  implicit final class CypherSyntaxNodeOps(n: Node) {
    /** Call built-in `labels` function. */
    def labels: CF.Expr[List[String]] = n.func("labels")
  }

  implicit final class CypherSyntaxRelOps(r: Rel) {
    /** Call built-in `type` function. */
    def tpe: CF.Expr[String] = r.func("type")

    /** Call built-in `type` function. */
    def `type`: CF.Expr[String] = tpe
  }

  implicit final class CypherSyntaxPathOps(p: Path) {
    def nodes: CF.Expr[List[GraphElem.NodeElem]] = "nodes".func(p)
    def relationships: CF.Expr[List[GraphElem.RelElem]] = "relationships".func(p)
    def length: CF.Expr[Long] = "length".func(p)
  }

  // // // // // // // // // // // // // // // // // // // // // // // // //
  // // // // // // Ops: Function & Procedure + common functions // // // //
  // // // // // // // // // // // // // // // // // // // // // // // // //

  implicit final class CypherSyntaxFuncOps(func: String) {
    def func[R](args: CF.Expr[_]*): CF.Expr[R] = CF.Expr.Func(func, args.toList)
  }

  implicit final class CypherSyntaxProcedureOps(procedure: String) {
    def call(args: CF.Expr[_]*): CypherSyntaxProcedureOps.CallBuilder = new CypherSyntaxProcedureOps.CallBuilder(procedure, args.toList)
  }
  // specifying WHERE condition is not supported by this syntax helper
  // assigning aliases to procedure outputs is not supported by this syntax helper
  protected[syntax] object CypherSyntaxProcedureOps {
    class CallBuilder(procedure: String, params: List[CF.Expr[_]]) {
      // TODO ==========================================================================================================
//      def void[R](res: Match.Result[R]): Match.Result[R] = impl.Call.void(procedure, params, res)
//      def yielding(f: Any): Match.Result[_] = macro impl.Call.impl
//      def yieldingAs(outputs: Symbol*)(f: Any): Match.Result[_] = macro impl.Call.implAs
    }
  }

  implicit final class CypherSyntaxAsStringOps[A: CypherSyntaxAsString.Can](expr: CF.Expr[A]) {
    /** Call built-in `toString` function. */
    def asString: CF.Expr[String] = CF.Expr.Func("toString", List(expr))
  }

  object CypherSyntaxAsString {
    trait Can[A]
    private object Can extends Can[Any]

    implicit lazy val canString: Can[String] = Can.asInstanceOf[Can[String]]
    implicit lazy val canBoolean: Can[Boolean] = Can.asInstanceOf[Can[Boolean]]
    implicit def canNumeric[N: Numeric]: Can[N] = Can.asInstanceOf[Can[N]]
  }

  // // // // // // // // // // // // // // // // //
  // // // // Ops:  Logic and Comparison // // // //
  // // // // // // // // // // // // // // // // //

  implicit final class CypherSyntaxLogicExprOps(expr0: CF.Expr[Boolean]) {
    def unary_! : CF.Expr[Boolean] = CF.Expr.LogicUnaryExpr(expr0, CF.Expr.LogicExpr.Negate)

    def and(expr1: CF.Expr[Boolean]): CF.Expr[Boolean] = binary(expr1, CF.Expr.LogicExpr.And)
    def && (expr1: CF.Expr[Boolean]): CF.Expr[Boolean]  = and(expr1)

    def or (expr1: CF.Expr[Boolean]): CF.Expr[Boolean] = binary(expr1, CF.Expr.LogicExpr.Or)
    def || (expr1: CF.Expr[Boolean]): CF.Expr[Boolean]  = or(expr1)

    def xor(expr1: CF.Expr[Boolean]): CF.Expr[Boolean] = binary(expr1, CF.Expr.LogicExpr.Xor)

    private def binary(expr1: CF.Expr[Boolean], op: CF.Expr.LogicExpr.BinaryOp) = CF.Expr.LogicBinaryExpr(expr0, expr1, op)
  }

  implicit final class CypherSyntaxCompareExprOps[A](expr0: CF.Expr[A]) {
    def eq (expr1: CF.Expr[_]): CF.Expr[Boolean] = binary(expr1, CF.Expr.CompareExpr.Eq)
    def ===(expr1: CF.Expr[_]): CF.Expr[Boolean] = eq(expr1)

    def neq(expr1: CF.Expr[_]): CF.Expr[Boolean] = binary(expr1, CF.Expr.CompareExpr.Neq)
    def <> (expr1: CF.Expr[_]): CF.Expr[Boolean] = neq(expr1)

    def isNull : CF.Expr[Boolean] = unary(CF.Expr.CompareExpr.IsNull)
    def notNull: CF.Expr[Boolean] = unary(CF.Expr.CompareExpr.NotNull)

    def lt (expr1: CF.Expr[A]): CF.Expr[Boolean] = binary(expr1, CF.Expr.CompareExpr.Lt)
    def <  (expr1: CF.Expr[A]): CF.Expr[Boolean] = binary(expr1, CF.Expr.CompareExpr.Lt)

    def lte(expr1: CF.Expr[A]): CF.Expr[Boolean] = binary(expr1, CF.Expr.CompareExpr.Lte)
    def <= (expr1: CF.Expr[A]): CF.Expr[Boolean] = binary(expr1, CF.Expr.CompareExpr.Lte)

    def gte(expr1: CF.Expr[A]): CF.Expr[Boolean] = binary(expr1, CF.Expr.CompareExpr.Gte)
    def >= (expr1: CF.Expr[A]): CF.Expr[Boolean] = binary(expr1, CF.Expr.CompareExpr.Gte)

    def gt (expr1: CF.Expr[A]): CF.Expr[Boolean] = binary(expr1, CF.Expr.CompareExpr.Gt)
    def > (expr1: CF.Expr[A]): CF.Expr[Boolean] = binary(expr1, CF.Expr.CompareExpr.Gt)

    def in(expr1: CF.Expr[List[A]]): CF.Expr[Boolean] = CF.Expr.InList(expr1, expr0)

    private def unary(op: CF.Expr.CompareExpr.UnaryOp) = CF.Expr.CompareUnaryExpr(expr0, op)
    private def binary(expr1: CF.Expr[_], op: CF.Expr.CompareExpr.BinaryOp) = CF.Expr.CompareBinaryExpr(expr0, expr1, op)
  }

  // // // // // // // // // // // // // // // //
  // // // // Ops:  Strings and Numeric  // // //
  // // // // // // // // // // // // // // // //

  implicit final class CypherSyntaxStringExprOps(expr0: CF.Expr[String]) {
    /** Regular expression match */
    def matches   (expr1: CF.Expr[String]): CF.Expr[Boolean] = binary(expr1, CF.Expr.StringExpr.Regex)
    def contains  (expr1: CF.Expr[String]): CF.Expr[Boolean] = binary(expr1, CF.Expr.StringExpr.Contains)
    def startsWith(expr1: CF.Expr[String]): CF.Expr[Boolean] = binary(expr1, CF.Expr.StringExpr.StartsWith)
    def endsWith  (expr1: CF.Expr[String]): CF.Expr[Boolean] = binary(expr1, CF.Expr.StringExpr.EndsWith)

    def toLower: CF.Expr[String] = "toLower".func(expr0)
    def toUpper: CF.Expr[String] = "toUpper".func(expr0)
    def size:    CF.Expr[Long]   = "size".func(expr0)

    def toBoolean: CF.Expr[Boolean] = "toBoolean".func(expr0)
    def toDouble:  CF.Expr[Double]  = "toFloat".func(expr0)
    def toLong:    CF.Expr[Long]    = "toInteger".func(expr0)

    /** Returns a string containing the specified number of leftmost characters of the original string. */
    def takeLeft(n: CF.Expr[Long]): CF.Expr[String] = "left".func(expr0, n)

    /** Returns a string containing the specified number of rightmost characters of the original string. */
    def takeRight(n: CF.Expr[Long]): CF.Expr[String] = "right".func(expr0, n)

    /** Returns a string in which all occurrences of a specified string in the original string have been replaced by another (specified) string. */
    def replace(search: CF.Expr[String], replace: CF.Expr[String]): CF.Expr[String] = "replace".func(expr0, search, replace)

    def reverse: CF.Expr[String] = "reverse".func(expr0)

    /** Returns a list of strings resulting from the splitting of the original string around matches of the given delimiter. */
    def split(delimiter: CF.Expr[String]): CF.Expr[List[String]] = "split".func(expr0, delimiter)

    /** Returns a substring of the original string, beginning with a 0-based index start. */
    def substring(start: CF.Expr[Long]): CF.Expr[String] = "substring".func(expr0, start)

    /** Returns a substring of the original string, beginning with a 0-based index start and length. */
    def substring(start: CF.Expr[Long], length: CF.Expr[Long]): CF.Expr[String] = "substring".func(expr0, start, length)

    /** Returns the original string with leading and trailing whitespace removed. */
    def trim: CF.Expr[String] = "trim".func(expr0)
    /** Returns the original string with leading whitespace removed. */
    def trimLeft: CF.Expr[String] = "lTrim".func(expr0)
    /** Returns the original string with trailing whitespace removed. */
    def trimRight: CF.Expr[String] = "rTrim".func(expr0)

    private def binary(expr1: CF.Expr[String], op: CF.Expr.StringExpr.Op) = CF.Expr.StringExpr(expr0, expr1, op)
  }

  implicit final class CypherSyntaxMathematicalExprOps[N: Numeric](expr0: CF.Expr[N]) {
    def unary_- : CF.Expr[N] = CF.Expr.MathematicalUnaryExpr(expr0, CF.Expr.MathematicalExpr.Negation)

    def +(expr1: CF.Expr[N]): CF.Expr[N] = binary(expr1, CF.Expr.MathematicalExpr.Addition)
    def -(expr1: CF.Expr[N]): CF.Expr[N] = binary(expr1, CF.Expr.MathematicalExpr.Subtraction)
    def *(expr1: CF.Expr[N]): CF.Expr[N] = binary(expr1, CF.Expr.MathematicalExpr.Multiplication)
    def /(expr1: CF.Expr[N]): CF.Expr[N] = binary(expr1, CF.Expr.MathematicalExpr.Division)
    def %(expr1: CF.Expr[N]): CF.Expr[N] = binary(expr1, CF.Expr.MathematicalExpr.ModuloDivision)
    def ^(expr1: CF.Expr[N]): CF.Expr[N] = binary(expr1, CF.Expr.MathematicalExpr.Exponentiation)

    private def binary(expr1: CF.Expr[N], op: CF.Expr.MathematicalExpr.BinaryOp) = CF.Expr.MathematicalBinaryExpr(expr0, expr1, op)
  }

  // // // // // // // // // // // // // // // //
  // // // //  //  Lists and Maps  // // // // //
  // // // // // // // // // // // // // // // //

  implicit final class ListOps[A](list: CF.Expr[List[A]]) {
    def concat(that: CF.Expr[List[A]]): CF.Expr[List[A]] = CF.Expr.Concat(list, that)
    def ++(that: CF.Expr[List[A]]): CF.Expr[List[A]] = concat(that)

    def at[I: (Int |∨| Long)#λ](i: CF.Expr[I]): CF.Expr[A] = CF.Expr.AtIndex(list, i.asInstanceOf[CF.Expr[Long]])

    def slice[I1: (Int |∨| Long)#λ, I2: (Int |∨| Long)#λ](l: CF.Expr[I1], r: CF.Expr[I2]): CF.Expr[List[A]] = slice(Ior.Both(l, r))
    def slice[I1: (Int |∨| Long)#λ, I2: (Int |∨| Long)#λ](range: Ior[CF.Expr[I1], CF.Expr[I2]]): CF.Expr[List[A]] =
      CF.Expr.AtRange(list, range.asInstanceOf[Ior[CF.Expr[Long], CF.Expr[Long]]])

    def from[I: (Int |∨| Long)#λ](i: CF.Expr[I]): CF.Expr[List[A]] = slice[I, I](Ior.Left(i))
    def to  [I: (Int |∨| Long)#λ](i: CF.Expr[I]): CF.Expr[List[A]] = slice[I, I](Ior.Right(i))

    def head: CF.Expr[A]       = "head".func(list)
    def tail: CF.Expr[List[A]] = "tail".func(list)
    def size: CF.Expr[Long]    = "size".func(list)

    def withFilter(f: CF.Expr[A] => CF.Expr[Boolean]): ListOps.WithFilter[A] = new ListOps.WithFilter(list, f)
    def filter(f: CF.Expr[A] => CF.Expr[Boolean]): CF.Expr[List[A]] = CF.Expr.ListComprehension(list, Some(f), None)

    def map[B](f: CF.Expr[A] => CF.Expr[B]): CF.Expr[List[B]] = CF.Expr.ListComprehension(list, None, Some(f))

    def reduce[B](b: CF.Expr[B])(f: (CF.Expr[B], CF.Expr[A]) => CF.Expr[B]): CF.Expr[B] = CF.Expr.Reduce(list, b, f)

    // predicates
    def all   (f: CF.Expr[A] => CF.Expr[Boolean]): CF.Expr[Boolean] = predicate(CF.Expr.ListPredicate.All, f)
    def any   (f: CF.Expr[A] => CF.Expr[Boolean]): CF.Expr[Boolean] = predicate(CF.Expr.ListPredicate.Any, f)
    def none  (f: CF.Expr[A] => CF.Expr[Boolean]): CF.Expr[Boolean] = predicate(CF.Expr.ListPredicate.None, f)
    def single(f: CF.Expr[A] => CF.Expr[Boolean]): CF.Expr[Boolean] = predicate(CF.Expr.ListPredicate.Single, f)

    private def predicate(pred: CF.Expr.ListPredicate.Predicate, f: CF.Expr[A] => CF.Expr[Boolean]) =
      CF.Expr.ListPredicate(list, pred, f)
  }

  object ListOps {
    protected final class WithFilter[A](list: CF.Expr[List[A]], filter: CF.Expr[A] => CF.Expr[Boolean]) {
      def map[B](f: CF.Expr[A] => CF.Expr[B]): CF.Expr[List[B]] = CF.Expr.ListComprehension(list, Some(filter), Some(f))
    }
  }

  implicit final class MapOps[A](map: CF.Expr[Map[String, A]]) {
    def keys: CF.Expr[List[String]] = "keys".func(map)
  }

  implicit final class MapNotAnyOps[A](map: CF.Expr[Map[String, A]])(implicit ev: A =:!= Any) {
    def value(key: String): CF.Expr[A] = CF.Expr.MapKey(map, key)
    def value(key: CF.Expr[String]): CF.Expr[A] = CF.Expr.MapDynKey(map, key)

    def add(entries: (String, CF.Expr[A])*): CF.Expr[Map[String, A]] = CF.Expr.MapAdd(map, entries.toMap)
    def add(map: Map[String, CF.Expr[A]]): CF.Expr[Map[String, A]] = CF.Expr.MapAdd(this.map, map)
  }

  implicit final class MapAnyOps[A](map: CF.Expr[Map[String, A]])(implicit ev: A =:= Any) {
    def value[V](key: String): CF.Expr[V] = CF.Expr.MapKey(map, key)
    def value[V](key: CF.Expr[String]): CF.Expr[V] = CF.Expr.MapDynKey(map, key)

    def add(entries: (String, CF.Expr[Any])*): CF.Expr[Map[String, Any]] = CF.Expr.MapAdd(map, entries.toMap)
    def add(map: Map[String, CF.Expr[Any]]): CF.Expr[Map[String, Any]] = CF.Expr.MapAdd(this.map, map)
  }

  def list[A](elems: CF.Expr[A]*): CF.Expr[List[A]] = CF.Expr.ListDef(elems.toList)

  /** Literal map constructor */
  def dict[A](entries: (String, CF.Expr[A])*): CF.Expr[Map[String, A]] = CF.Expr.MapDef(entries.toMap)
  /** Literal map constructor */
  def dict[A](map: Map[String, CF.Expr[A]]): CF.Expr[Map[String, A]] = CF.Expr.MapDef(map)

  // // // // // // // // // // // // // // // //
  // // // // Literals and Parameters // // // //
  // // // // // // // // // // // // // // // //

  def lit[A](a: A)(implicit lift: CypherStatement.LiftValue[A]): CF.Expr.Lit[A] = CF.Expr.Lit[A](a, lift)

  // // // // // // // // // // // // // // // //
  // // // // Aggregation Functions // // // //
  // // // // // // // // // // // // // // // //

  def collect[A](a: CF.Expr[A]): CF.Expr[List[A]] = "collect".func(a)
}
