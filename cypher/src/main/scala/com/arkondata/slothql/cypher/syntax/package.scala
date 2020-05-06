package com.arkondata.slothql.cypher

import scala.annotation.{ compileTimeOnly, implicitNotFound }
import scala.language.experimental.macros
import scala.language.{ dynamics, implicitConversions }

import cats.data.{ Ior, NonEmptyList }
import shapeless.tag.@@
import shapeless.{ ::, =:!=, HList, HNil, Refute, Unpack1, ops, |∨| }

import com.arkondata.slothql.cypher.{ CypherFragment => CF }

package object syntax extends CypherSyntaxLowPriorityImplicits {

  type Expr[+A] = CF.Expr[A]
  type Query[+A] = CF.Query.Query0[A]

  object Match {
    def apply[R]              (query: Node => CF.Query.Query0[R]): CF.Query.Query0[R] = macro CypherSyntaxPatternMacros.match_[R]
    def optional[R]           (query: Node => CF.Query.Query0[R]): CF.Query.Query0[R] = macro CypherSyntaxPatternMacros.optional[R]
    def maybe[R](opt: Boolean)(query: Node => CF.Query.Query0[R]): CF.Query.Query0[R] = macro CypherSyntaxPatternMacros.maybe[R]
  }

  object With extends {
    @compileTimeOnly("would have been replaced at With.apply")
    def where(cond: CF.Expr[Boolean]): Nothing = ???

    @compileTimeOnly("would have been replaced at With.apply")
    def distinct: Nothing = ???
    @compileTimeOnly("would have been replaced at With.apply")
    def distinct(boolean: Boolean): Nothing = ???

    @compileTimeOnly("would have been replaced at With.apply")
    def orderBy(expr: CF.Expr[_], ord: CF.Return.Order = CF.Return.Order.Ascending): Nothing = ???
    @compileTimeOnly("would have been replaced at With.apply")
    def orderBy(expr: CF.Expr[_], ord: CF.Return.Order.type => CF.Return.Order): Nothing = ???
    @compileTimeOnly("would have been replaced at With.apply")
    def orderBy(seq: Seq[(CF.Expr[_], CF.Return.Order)]): Nothing = ???

    @compileTimeOnly("would have been replaced at With.apply")
    def skip(n: CF.Expr.Input[Long]): Nothing = ???
    @compileTimeOnly("would have been replaced at With.apply")
    def limit(n: CF.Expr.Input[Long]): Nothing = ???

    def apply[R](wildcard: **.type)(query: CF.Query.Query0[R]): CF.Query.Query0[R] =
      macro CypherSyntaxWithMacros.withWild0[R]

    def apply[T1, R](t1: CF.Expr[T1])
                    (query: CF.Expr[T1] => CF.Query.Query0[R]): CF.Query.Query0[R] =
      macro CypherSyntaxWithMacros.with1[T1, R]

    def apply[T1, R](wildcard: **.type, t1: CF.Expr[T1])
                    (query: CF.Expr[T1] => CF.Query.Query0[R]): CF.Query.Query0[R] =
      macro CypherSyntaxWithMacros.withWild1[T1, R]

    def apply[T1, T2, R](t1: CF.Expr[T1], t2: CF.Expr[T2])
                        (query: (CF.Expr[T1], CF.Expr[T2]) => CF.Query.Query0[R]): CF.Query.Query0[R] =
      macro CypherSyntaxWithMacros.with2[T1, T2, R]

    def apply[T1, T2, R](wildcard: **.type, t1: CF.Expr[T1], t2: CF.Expr[T2])
                        (query: (CF.Expr[T1], CF.Expr[T2]) => CF.Query.Query0[R]): CF.Query.Query0[R] =
      macro CypherSyntaxWithMacros.withWild2[T1, T2, R]
  }

  object Unwind {
    def apply[A, R](list: CF.Expr[List[A]])(func: CF.Expr[A] => CF.Query.Query0[R]): CF.Query.Query0[R] = macro CypherSyntaxUnwindMacros.unwind[A, R]
  }

  object Call {

    def apply(procedure: String, params: CF.Expr[_]*): CallOps = new CallOps

    class CallOps {
      def yielding[A1, R](yields1: String)(res: CF.Expr[A1] => CF.Query.Query0[R]): CF.Query.Query0[R] = macro CypherSyntaxCallMacros.yield1[A1, R]
      def yielding[A1, A2, R](yields1: String, yields2: String)(res:( CF.Expr[A1],  CF.Expr[A2]) => CF.Query.Query0[R]): CF.Query.Query0[R] = macro CypherSyntaxCallMacros.yield2[A1, A2, R]
      def yielding[A1, A2, A3, R](yields1: String, yields2: String, yields3: String)(res:( CF.Expr[A1],  CF.Expr[A2],  CF.Expr[A3]) => CF.Query.Query0[R]): CF.Query.Query0[R] = macro CypherSyntaxCallMacros.yield3[A1, A2, A3, R]
    }

    // @compileTimeOnly("would have been replaced at Call.apply")
    // def where(cond: CF.Expr[Boolean]): Nothing = ???
  }

  object Create {
    def apply[R](query: Node => Query[R]): Query[R] = macro CypherSyntaxPatternMacros.create[R]
  }

  object Merge {
    // def apply[R](query: Node => CF.Query[R]): CF.Query[R] = macro CypherSyntaxPatternMacros.merge[R]
  }

  object Update {
    type Prop = CF.Clause.SetProps.One

    def apply[R](prop: Prop, props: Prop*)(res: Query[R]): Query[R] =
      CF.Query.Clause(
        CF.Clause.SetProps(NonEmptyList(prop, props.toList)),
        res
      )
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

  type Node = CF.Expr[GraphElem.Node] with CypherStatement.Alias
  type Rel  = CF.Expr[GraphElem.Rel]  with CypherStatement.Alias
  type Path = CF.Expr[GraphPath]      with CypherStatement.Alias

  object CypherSyntaxFromMacro {
    def mkNode(name: String): Node                          =     CF.Expr.Alias[GraphElem.Node](name)
    def mkRel[D <: Rel.Direction](name: String): Rel.Aux[D] = new CF.Expr.Alias[GraphElem.Rel](name) { type Dir = D }
    def mkPath(name: String): Path                          =     CF.Expr.Alias[GraphPath](name)
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

  def `return`[A](expr: CF.Expr[A]): CF.Query.Return[A] = cypherSyntaxExprToQueryReturn(expr)
  def `return`[T <: Product](tuple: T)(implicit ret: CypherSyntaxReturnTuple[T]): CF.Query.Return[ret.Out] = cypherSyntaxTupleToQueryReturn(tuple)

  implicit def cypherSyntaxExprToReturn[A](expr: CF.Expr[A]): CF.Return[A] = CF.Return.Expr(expr, as = None)
  implicit def cypherSyntaxExprToQueryReturn[A](expr: CF.Expr[A]): CF.Query.Return[A] = CF.Query.Return(expr)
  implicit def cypherSyntaxReturnToQueryReturn[A](ret: CF.Return[A]): CF.Query.Return[A] = CF.Query.Return(ret)

  implicit final class CypherSyntaxReturnAsOps[A](expr: CF.Expr[A]) {
    def as(alias: CypherStatement.Alias): CF.Return.Expr[A] = CF.Return.Expr(expr, as = Option(alias))
    def as(alias: String): CF.Return.Expr[A] = as(CF.Expr.Alias.Fixed(alias))
  }

  implicit def cypherSyntaxTupleToReturn[T <: Product](tuple: T)(implicit ret: CypherSyntaxReturnTuple[T]): CF.Return[ret.Out] = ret(tuple)
  implicit def cypherSyntaxTupleToQueryReturn[T <: Product](tuple: T)(implicit ret: CypherSyntaxReturnTuple[T]): CF.Query.Return[ret.Out] = CF.Query.Return(ret(tuple))

  trait CypherSyntaxReturnTuple[T <: Product] {
    type Out
    def apply(t: T): CF.Return.Return0[Out]
  }
  object CypherSyntaxReturnTuple {
    implicit def cypherSyntaxReturnTuple[T <: Product]: CypherSyntaxReturnTuple[T] = macro CypherSyntaxMacros.returnTuple[T]
  }

  implicit def toCypherSyntaxReturnOps[T](t: T)(implicit canReturn: CypherSyntaxReturnOps.CanReturn[T]): CypherSyntaxReturnOps[T, HNil, canReturn.Out] =
    new CypherSyntaxReturnOps(t, false, Nil, None, None)(canReturn)

  final class CypherSyntaxReturnOps[T, Done <: HList, R](
    t: T,
    distinct0: Boolean,
    orderBy0: CF.Return.OrderBy,
    skip0: Option[CF.Expr.Input[Long]],
    limit0: Option[CF.Expr.Input[Long]]
  )(implicit canReturn: CypherSyntaxReturnOps.CanReturn.Aux[T, R]) {
    import CypherSyntaxReturnOps.{ NotYet, Opt }

    def distinct(implicit notYet: NotYet[Done, Opt.Distinct]): CypherSyntaxReturnOps[T, Opt.Distinct :: Done, R] =
      copy(distinct = true)

    def orderBy(expr: CF.Expr[_], ord: CF.Return.Order = CF.Return.Order.Ascending): CypherSyntaxReturnOps[T, Done, R] =
      copy(orderBy = orderBy0 :+ (expr -> ord))
    def orderBy(expr: CF.Expr[_], ord: CF.Return.Order.type => CF.Return.Order): CypherSyntaxReturnOps[T, Done, R] =
      orderBy(expr, ord(CF.Return.Order))

    def skip[N: (Int |∨| Long)#λ](inp: CF.Expr.Input[N])
                                 (implicit notYet: NotYet[Done, Opt.Skip]): CypherSyntaxReturnOps[T, Opt.Skip :: Done, R] =
      copy(skip = Some(inp.asInstanceOf[CF.Expr.Input[Long]]))

    def limit[N: (Int |∨| Long)#λ](inp: CF.Expr.Input[N])
                                  (implicit notYet: NotYet[Done, Opt.Limit]): CypherSyntaxReturnOps[T, Opt.Limit :: Done, R] =
      copy(limit = Some(inp.asInstanceOf[CF.Expr.Input[Long]]))

    private def copy[Steps <: HList](
      distinct: Boolean                  = distinct0,
      orderBy: CF.Return.OrderBy         = orderBy0,
      skip: Option[CF.Expr.Input[Long]]  = skip0,
      limit: Option[CF.Expr.Input[Long]] = limit0
    ): CypherSyntaxReturnOps[T, Steps, R] =
      new CypherSyntaxReturnOps(t, distinct, orderBy, skip, limit)

    def `return`: CF.Return[R] = {
      val options = distinct0 || orderBy0.nonEmpty || skip0.nonEmpty || limit0.nonEmpty
      val ret = canReturn(t)
      if (options) CF.Return.Options(ret, distinct0, orderBy0, skip0, limit0) else ret
    }
  }

  object CypherSyntaxReturnOps {
    implicit def returnClauseFromCypherSyntaxReturnOps[T, L <: HList, R](ops: CypherSyntaxReturnOps[T, L, R]): CF.Return[R] = ops.`return`
    implicit def query0FromCypherSyntaxReturnOps[T, L <: HList, R](ops: CypherSyntaxReturnOps[T, L, R]): CF.Query.Query0[R] = CF.Query.Return(ops.`return`)

    sealed trait CanReturn[T] {
      type Out
      def apply(t: T): CF.Return.Return0[Out]
    }
    object CanReturn {
      type Aux[T, R] = CanReturn[T] { type Out = R }

      implicit def canReturnSingle[T, A](implicit unpack: Unpack1[T, CF.Expr, A]): CanReturn.Aux[T, A] =
        new CanReturn[T] {
          type Out = A
          def apply(t: T): CF.Return.Return0[A] = CF.Return.Expr(t.asInstanceOf[CF.Expr[A]], None)
        }

      implicit def canReturnTuple[T <: Product](implicit ev: CypherSyntaxReturnTuple[T]): CanReturn.Aux[T, ev.Out] =
        new CanReturn[T] {
          type Out = ev.Out
          def apply(t: T): CF.Return.Return0[ev.Out] = ev(t)
        }
    }

    sealed trait Opt
    object Opt {
      case object Distinct extends Opt
      case object Skip     extends Opt
      case object Limit    extends Opt

      type Distinct = Distinct.type
      type Skip     = Skip.type
      type Limit    = Limit.type
    }

    @implicitNotFound("${S} has already been configured")
    trait NotYet[Done <: HList, S <: Opt]
    object NotYet {
      implicit def notYet[Done <: HList, S <: Opt](
        implicit not: Refute[ops.hlist.Selector[Done, S]]
      ): NotYet[Done, S] = instance.asInstanceOf[NotYet[Done, S]]

      private lazy val instance = new NotYet[HList, Opt] {}
    }
  }

  // // // // // // // // // // // // // // // // // //
  // // // // // Ops: Node & Rel & Path  // // // // //
  // // // // // // // // // // // // // // // // // //

  implicit final class CypherSyntaxGraphElemOps(g: CF.Expr[GraphElem]) {
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

  implicit final class CypherSyntaxNodeOps(n: CF.Expr[GraphElem.Node]) {
    /** Call built-in `labels` function. */
    def labels: CF.Expr[List[String]] = n.func("labels")
  }

  implicit final class CypherSyntaxRelOps(r: CF.Expr[GraphElem.Rel]) {
    /** Call built-in `type` function. */
    def tpe: CF.Expr[String] = r.func("type")

    /** Call built-in `type` function. */
    def `type`: CF.Expr[String] = tpe
  }

  implicit final class CypherSyntaxPathOps(p: Path) {
    def nodes: CF.Expr[List[GraphElem.Node]] = "nodes".func(p)
    def relationships: CF.Expr[List[GraphElem.Rel]] = "relationships".func(p)
    def length: CF.Expr[Long] = "length".func(p)
  }

  // // // // // // // // // // // //
  // // // //  Functions  // // // //
  // // // // // // // // // // // //

  implicit final class CypherSyntaxFuncOps(func: String) {
    def func[R](args: CF.Expr[_]*): CF.Expr[R] = CF.Expr.Func(func, args.toList)
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

  // // // // // // // // // // // // // // //
  // // // //  Built-in functions  // // // //
  // // // // // // // // // // // // // // //

  /** Alias for [[Neo4jBuiltIn]]. */
  lazy val neo4j: Neo4jBuiltIn.type = Neo4jBuiltIn

  object Neo4jBuiltIn {
    lazy val randomUUID: Expr[String] = "randomUUID".func[String]()
    lazy val timestamp: Expr[Long]    = "timestamp".func[Long]()
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
    def last: CF.Expr[A]       = "last".func(list)
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

  type Param[A] = CF.Expr.Param[A]
  type MapParam = Param[Map[String, CypherStatement.LiftedValue]]

  object parameterized extends ParameterizedCypherQuery.Build

  // // // // // // // // // // // // // // // //
  // // // // Aggregation Functions // // // //
  // // // // // // // // // // // // // // // //

  def collect[A](a: CF.Expr[A]): CF.Expr[List[A]] = "collect".func(a)

  // // // // // // // // // //
  // // // // Union // // // //
  // // // // // // // // // //

  implicit final class UnionOps[A](q1: CF.Query.Query0[A]) {
    def union(q2: CF.Query.Query0[A]): CF.Query[A] = CF.Query.Union(q1, q2, all = false)
    def unionAll(q2: CF.Query.Query0[A]): CF.Query[A] = CF.Query.Union(q1, q2, all = true)
  }

  // // // // // // // // // // //
  // // // // Set  Ops // // // //
  // // // // // // // // // // //

  implicit final class SetPropOps(e: CF.Expr[GraphElem]) {
    object set extends Dynamic {
      def updateDynamic[V](prop: String)(value: Expr[V]): Update.Prop =
        CF.Clause.SetProps.One(e.asInstanceOf[Expr[Map[String, Any]]], prop, value)
    }
    def setProp: set.type = set
  }

}
