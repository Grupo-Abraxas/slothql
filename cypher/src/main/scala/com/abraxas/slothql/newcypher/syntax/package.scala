package com.abraxas.slothql.newcypher

import scala.annotation.compileTimeOnly
import scala.language.experimental.macros
import scala.language.implicitConversions

import com.abraxas.slothql.newcypher.{ CypherFragment => CF }

package object syntax {

  object Match {
    def apply[R]              (query: Node => CF.Query[R]): CF.Query[R] = macro CypherSyntaxMacros.match_[R]
//  def optional[R]           (query: Node => CF.Query[R]): CF.Query[R] = macro CypherSyntaxMacros.optional[R]
//  def maybe[R](opt: Boolean)(query: Node => CF.Query[R]): CF.Query[R] = macro CypherSyntaxMacros.maybe[R]
  }

  object With {
    // def apply[R](query: Node => CF.Query[R]): CF.Query[R] = macro CypherSyntaxMacros.with_[R]
  }

  object Unwind {
    // def apply[R](query: Node => CF.Query[R]): CF.Query[R] = macro CypherSyntaxMacros.unwind[R]
  }

  object Call {
    // def apply[R](query: Node => CF.Query[R]): CF.Query[R] = macro CypherSyntaxMacros.call[R]
  }

  object Create {
    // def apply[R](query: Node => CF.Query[R]): CF.Query[R] = macro CypherSyntaxMacros.create[R]
  }

  object Merge {
    // def apply[R](query: Node => CF.Query[R]): CF.Query[R] = macro CypherSyntaxMacros.merge[R]
  }

  object Delete {
    // def apply[R](query: Node => CF.Query[R]): CF.Query[R] = macro CypherSyntaxMacros.delete[R]
  }

  object Foreach {
    // def apply[R](query: Node => CF.Query[R]): CF.Query[R] = macro CypherSyntaxMacros.foreach[R]
  }

  // // // // // // // // // // // // // // // // //
  // // // // // Matching Graph Paths // // // // //
  // // // // // // // // // // // // // // // // //

  sealed trait GraphElem {
    // TODO
    val alias: String
  }
  sealed trait Node extends GraphElem { val alias: String }
  sealed trait Rel  extends GraphElem { val alias: String; type Dir <: Rel.Direction }

  // TODO: path!

  object CypherSyntaxFromMacro {
    def mkNode(name: String): Node                          = new Node { val alias = name }
    def mkRel[D <: Rel.Direction](name: String): Rel.Aux[D] = new Rel  { val alias = name; type Dir = D }
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
    def unapply[A](any: Any): Option[(String, CF.Expr[A])] = ???
  }
  @compileTimeOnly("Con only be used inside `Match` / `Create`")
  object ** {
    def unapply(any: Any): Option[(CF.Expr[Long], CF.Expr[Long])] = ???
  }

  // // // // // // // // // // // // // // // // //
  // // // // //  Return Expressions  // // // // //
  // // // // // // // // // // // // // // // // //

  implicit def cypherSyntaxExprToReturn[A](expr: CF.Expr[A]): CF.Return[A] = CF.Return.Expr(expr, as = None)
  implicit def cypherSyntaxExprToQueryReturn[A](expr: CF.Expr[A]): CF.Query.Return[A] = CF.Query.Return(expr)
  implicit def cypherSyntaxReturnToQueryReturn[A](ret: CF.Return[A]): CF.Query.Return[A] = CF.Query.Return(ret)

  implicit final class CypherSyntaxReturnAsOps[A](expr: CF.Expr[A]) {
    def as(alias: String): CF.Return.Expr[A] = CF.Return.Expr(expr, as = Option(alias))
  }

  // // // // // // // // // // // // // // // // // //
  // // // // // Ops: Node & Rel & Path  // // // // //
  // // // // // // // // // // // // // // // // // //

  implicit final class CypherSyntaxGraphElemOps(g: GraphElem) {
    /** Select all vertex/edge properties */
    def props: CF.Expr[Map[String, Any]] = CF.Expr.Var(g.alias)

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

  // TODO ==============================================================================================================
  // implicit final class CypherSyntaxPathOps(p: Path)

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

}
