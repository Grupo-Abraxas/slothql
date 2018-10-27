package com.abraxas.slothql.cypher

import scala.language.{ higherKinds, implicitConversions }
import scala.util.Random

import cats.data.Ior
import shapeless.{ <:!<, Generic, HList, |∨| }

import com.abraxas.slothql.cypher.CypherFragment.{ Expr, Known, Query, Return }

package object syntax extends LowPriorityImplicits {

  sealed trait Graph extends Expr.Var[Map[String, Any]] with Graph.Vertex

  type GraphElem = Expr.Var[Map[String, Any]] with Graph.Elem
  type Vertex    = Expr.Var[Map[String, Any]] with Graph.Vertex
  type Edge      = Expr.Var[Map[String, Any]] with Graph.Edge

  object GraphElem {
    private[syntax] class Impl extends Expr.Var[Map[String, Any]] {
      private[syntax] var _alias: String = _ // This `var` should be set only once by a macro

      lazy val name: String = _alias
      lazy val m: Manifest[Map[String, Any]] = manifest[Map[String, Any]]
    }
  }

  final implicit class GraphElemOps(e: GraphElem) {
    /** Select vertex/edge property. */
    def prop[A](k: String): Expr.Key[A] = Expr.Key[A](e, k)
    /** Select vertex/edge property as [[Option]]. */
    def propOpt[A](k: String): Expr.Key[Option[A]] = prop[Option[A]](k)

    /** Alias for [[prop]]. */
    @deprecated("seems to break query type resolution", since = "03.06.18")
    def apply[A](k: String): Expr.Key[A] = prop(k)
    /** Alias for [[propOpt]]. */
    def opt[A](k: String): Expr.Key[Option[A]] = propOpt(k)

    /** Call built-in function `func` passing `this` expression as first argument. */
    def call[R](func: String, args: Known[Expr[_]]*): Expr.Call[R] =
      Expr.Call(func, e.known :: args.toList)

    /** Call built-in `id` function. */
    def id: Expr.Call[Long] = call("id")
    /** Call built-in `count` function. */
    def count: Expr.Call[Long] = call("count")
    /** Call built-in `keys` function. */
    def keys: Expr.Call[List[String]] = call("keys")

  }

  final implicit class VertexOps(v: Vertex) {
    /** Call built-in `labels` function. */
    def labels: Expr.Call[List[String]] = v.call("labels")
  }

  final implicit class EdgeOps(e: Edge) {
    /** Call built-in `type` function. */
    def tpe: Expr.Call[String] = e.call("type")
    /** Call built-in `type` function. */
    def `type`: Expr.Call[String] = tpe
  }

  private[syntax] object Graph{
    sealed trait Elem
    sealed trait Vertex extends Elem
    sealed trait Edge   extends Elem

    val instance: Graph = new GraphElem.Impl with Graph {}
  }

  object Vertex {
    @inline private[syntax] def apply(): Vertex = (new GraphElem.Impl).asInstanceOf[Vertex]
    def unapplySeq(v: Vertex): Option[Seq[AnyRef]] = Some(???)
  }
  object Edge {
    @inline private[syntax] def apply(): Edge = (new GraphElem.Impl).asInstanceOf[Edge]
    def unapplySeq(v: Edge): Option[Seq[AnyRef]] = Some(???)
  }


  object := {
    def unapply(arg: Any): Option[(String, Any)] = Some(???)
  }
  object :?= {
    def unapply(arg: Any): Option[(String, Option[Any])] = Some(???)
  }

  object *: {
    def unapply(edge: Edge): Option[(Expr.Var[List[Map[String, Any]]], -[Int, Int], Edge)] = Some(???)
  }

  // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // //

  implicit class CallExprOps(func: Symbol) {
    def call[R](args: Known[Expr[_]]*): Expr.Call[R] = Expr.Call(func.name, args.toList)
  }

  implicit class BooleanExprOps[E0 <: Expr[Boolean]: CypherFragment](expr0: E0) {
    def unary_! : Expr.LogicNegationExpr = Expr.LogicNegationExpr(expr0.known)

    def and[E1 <: Expr[Boolean]: CypherFragment](expr1: E1): Expr.LogicBinaryExpr = binary(expr1, Expr.LogicExpr.And)
    def or [E1 <: Expr[Boolean]: CypherFragment](expr1: E1): Expr.LogicBinaryExpr = binary(expr1, Expr.LogicExpr.Or)
    def xor[E1 <: Expr[Boolean]: CypherFragment](expr1: E1): Expr.LogicBinaryExpr = binary(expr1, Expr.LogicExpr.Xor)

    def &&[E1 <: Expr[Boolean]: CypherFragment](expr1: E1): Expr.LogicBinaryExpr = and(expr1)
    def ||[E1 <: Expr[Boolean]: CypherFragment](expr1: E1): Expr.LogicBinaryExpr = or (expr1)

    private def binary[E1 <: Expr[Boolean]: CypherFragment](expr1: E1, op: Expr.LogicExpr.BinaryOp) =
      Expr.LogicBinaryExpr(expr0.known, expr1.known, op)
  }

  implicit class CompareAnyOps[E0 <: Expr[_]: CypherFragment](expr0: E0) {
    def eq [E1 <: Expr[_]: CypherFragment](expr1: E1): Expr.CompareBinaryAnyExpr = binary(expr1, Expr.CompareExpr.Eq)
    def neq[E1 <: Expr[_]: CypherFragment](expr1: E1): Expr.CompareBinaryAnyExpr = binary(expr1, Expr.CompareExpr.Neq)

    def ===[E1 <: Expr[_]: CypherFragment](expr1: E1): Expr.CompareBinaryAnyExpr = eq(expr1)
    def <> [E1 <: Expr[_]: CypherFragment](expr1: E1): Expr.CompareBinaryAnyExpr = neq(expr1)

    def isNull  : Expr.CompareUnaryExpr = unary(Expr.CompareExpr.IsNull)
    def notNull : Expr.CompareUnaryExpr = unary(Expr.CompareExpr.NotNull)

    private def unary(op: Expr.CompareExpr.UnaryOp) = Expr.CompareUnaryExpr(expr0.known, op)
    private def binary[E1 <: Expr[_]: CypherFragment](expr1: E1, op: Expr.CompareExpr.BinaryAnyOp) =
      Expr.CompareBinaryAnyExpr(expr0.known, expr1.known, op)
  }

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

  implicit class ListOps[A, E0 <: Expr[_]](expr0: E0)(implicit frag0: CypherFragment[E0], ev: E0 <:< Expr[List[A]]) {
    def concat[E1 <: Expr[List[A]]: CypherFragment](expr1: E1): Expr.Concat[A] =
      Expr.Concat(expr0.known.widen, expr1.known)
    def ++[E1 <: Expr[List[A]]: CypherFragment](expr1: E1): Expr.Concat[A] = concat(expr1)

    def at[I: (Int |∨| Long)#λ, E1[x] <: Expr[x]](i: E1[I])(implicit frag1: CypherFragment[E1[Long]]): Expr.AtIndex[A] =
      Expr.AtIndex(expr0.known.widen, i.asInstanceOf[E1[Long]].known)

    def at[E1 <: Expr[Long]: CypherFragment, E2 <: Expr[Long]: CypherFragment](range: Ior[E1, E2]): Expr.AtRange[A] =
      Expr.AtRange(expr0.known.widen, range.bimap(_.known, _.known))

    def at[I1: (Int |∨| Long)#λ, I2: (Int |∨| Long)#λ, E1[x] <: Expr[x], E2[x] <: Expr[x]](l: E1[I1], r: E2[I2])(
      implicit frag1: CypherFragment[E1[Long]], frag2: CypherFragment[E2[Long]]
    ): Expr.AtRange[A] =
      at(Ior.Both(l.asInstanceOf[E1[Long]], r.asInstanceOf[E2[Long]]))

    def from[I: (Int |∨| Long)#λ, E1[x] <: Expr[x]](i: E1[I])(implicit frag1: CypherFragment[E1[Long]]): Expr.AtRange[A] =
      at[E1[Long], E1[Long]](Ior.Left(i.asInstanceOf[E1[Long]]))

    def to[I: (Int |∨| Long)#λ, E1[x] <: Expr[x]](i: E1[I])(implicit frag1: CypherFragment[E1[Long]]): Expr.AtRange[A] =
      at[E1[Long], E1[Long]](Ior.Right(i.asInstanceOf[E1[Long]]))

    def filter(f: Expr.Var[A] => Known[Expr[Boolean]])(implicit mf: Manifest[A]): Expr.FilterList[A] = {
      val alias = randomAlias()
      val expr = f(Expr.Var[A](alias))
      Expr.FilterList(expr0.known.widen, alias, expr)
    }
    def filter0(expr: Known[Expr[Boolean]])(implicit mf: Manifest[A]): Expr.FilterList[A] = Expr.FilterList(expr0.known.widen, "_", expr)
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

  def list[A](exprs: Known[Expr[A]]*): Expr.List[A] = Expr.List[A](exprs.toList)
  def litList[A](exprs: A*)(implicit frag: CypherFragment[Expr.Lit[A]]): Expr.List[A] =
    Expr.List[A](exprs.map(Expr.Lit(_).known).toList)

  def distinct[A](expr: Known[Expr[A]]): Known[Expr[A]] = Expr.Distinct(expr)
  def collect[A](expr: Known[Expr[A]]): Expr.Call[List[A]] = 'collect.call[List[A]](expr)

  def dict(entries: MapEntry[Any]*): Expr.Map[Any] = Expr.Map(entries.map(_.toPair).toMap)
  def dict(map: Map[String, Known[Expr[Any]]]): Expr.Map[Any] = Expr.Map(map)

  case class MapEntry[+A](key: String, value: Known[Expr[A]]) { def toPair: (String, Known[Expr[A]]) = key -> value }
  object MapEntry {
    implicit def pairKnownToMapEntry[A](pair: (String, Known[Expr[A]])): MapEntry[A] = MapEntry(pair._1, pair._2)
    implicit def pairToMapEntry[A, E[_] <: Expr[_]](pair: (String, E[A]))(implicit frag: CypherFragment[E[A]]): MapEntry[A] =
      MapEntry(pair._1, Known(pair._2)(frag).asInstanceOf[Known[Expr[A]]])
  }

  sealed trait QueryReturn[T]{
    type Ret
    type Out <: Return[Ret]
    def apply(t: T): Out
  }
  object QueryReturn {
    type Aux[T, R, O <: Return[R]] = QueryReturn[T] { type Ret = R; type Out = O }

    implicit def returnExpr[A, E <: Expr[_]](
      implicit
      ev: E <:< Expr.Inv[A],
      fragment: CypherFragment[E]
    ): Aux[E, A, Return.Expr[A]] =
      new QueryReturn[E] {
        type Ret = A
        type Out = Return.Expr[A]
        def apply(e: E): Return.Expr[A] = Return.Expr(Known(e).widen, as = None)
      }

    implicit def returnTuple[P <: Product, L <: HList, R <: HList, E <: HList](
      implicit
      ev: P <:!< Expr[_],
      gen: Generic.Aux[P, L],
      build: Return.List.Build.Aux[L, R, E]
    ): Aux[P, R, Return.List.Aux[R, E]] =
      new QueryReturn[P] {
        type Ret = R
        type Out = Return.List.Aux[R, E]
        def apply(p: P): Return.List.Aux[R, E] = build(gen.to(p))
      }

    implicit lazy val returnUntypedList: Aux[Return.UntypedList, List[Any], Return.UntypedList] =
      new QueryReturn[Return.UntypedList] {
        type Ret = List[Any]
        type Out = Return.UntypedList
        @inline def apply(t: Return.UntypedList): Return.UntypedList = t
      }

    implicit def returnOptions[A, E <: Return.Options[_]](implicit ev: E <:< Return.Options.Inv[A]): Aux[E, A, Return.Options[A]] =
      _retOptions.asInstanceOf[Aux[E, A, Return.Options[A]]]
    private lazy val _retOptions = new QueryReturn[Return.Options[_]] {
      type Ret = Any
      type Out = Return.Options[_]
      def apply(t: Return.Options[_]): Return.Options[_] = t
    }
  }


  def returnTuple(exprs: Iterable[Known[Expr[_]]]): Return.UntypedList = CypherFragment.Return.List.untyped(exprs)

  implicit def toReturnOps[E, A, R <: Return.Return0[A]](e: E)(implicit rq: QueryReturn.Aux[E, A, R]): ReturnOps[A] = ReturnOps(rq(e))
  implicit def toQueryMatchResult[R](q: Query.Clause[R]): Match.Result.Clause[R] = new Match.Result.Clause[R]{ protected[syntax] def clause: Query.Clause[R] = q }


  def `with`[R](ops: ReturnOps[Any] => ReturnOps[Any])(res: Match.Result[R]): Match.Result.With[R] =
    new Match.Result.With[R] {
      protected[syntax] def ret: Known[Return[R]] = ops(ReturnOps(Return.All)).copy(_ret = Return.All.as[R]).ret
      protected[syntax] def query: Known[Query.Query0[R]] = res.result
    }

  protected final case class ReturnOps[A](
      private val _ret: Known[Return.Return0[A]],
      private val _distinct: Boolean    = false,
      private val _order: Return.Order  = Map(),
      private val _skip: Option[Long]   = None,
      private val _limit: Option[Long]  = None
  ) extends Match.Result.Ret[A]
  {
    protected[syntax] def ret: Known[Return[A]] = Return.Options(_ret, _distinct, _order, _skip, _limit).known

    def orderBy(by: ReturnOps.OrderBy*): ReturnOps[A] = copy(_order = _order ++ by.map(_.asPair).toMap)
    def skip(n: Long): ReturnOps[A] = copy(_skip = Some(n))
    def skip(n: Option[Long]): ReturnOps[A] = copy(_skip = n)
    def limit(n: Long): ReturnOps[A] = copy(_limit = Some(n))
    def limit(n: Option[Long]): ReturnOps[A] = copy(_limit = n)
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

  def conditionOpt[T](cond: Option[T => CypherFragment.Known[CypherFragment.Expr[Boolean]]])(t: T): Known[Expr[Boolean]] =
    cond.map(_(t)).getOrElse(lit(true))
}

trait LowPriorityImplicits {
  implicit def unwrapBooleanExprInIfGuard(e: Expr[Boolean]): Boolean = ???
  implicit def unwrapKnownBooleanExprInIfGuard(e: Known[Expr[Boolean]]): Boolean = ???
}
