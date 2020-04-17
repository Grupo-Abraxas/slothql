package com.arkondata.slothql02.cypher.syntax

import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.reflect.macros.{ blackbox, whitebox }

import cats.data.NonEmptyList
import shapeless.{ CaseClassMacros, HList }

import com.arkondata.slothql02.cypher.CypherFragment
import com.arkondata.slothql02.cypher.CypherFragment.Pattern.Rel
import com.arkondata.slothql02.cypher.CypherFragment._
import com.arkondata.slothql02.util.raiseCompilationError

object Match { MatchObj =>
  def apply[R]   (f: Graph => Match.Result[R]): Query[R] = macro Internal.implApply[R]
  def optional[R](f: Graph => Match.Result[R]): Query[R] = macro Internal.implOptional[R]
  def maybeOptional[R](opt: Boolean)(f: Graph => Match.Result[R]): Query[R] = macro Internal.implMaybeOptional[R]

  sealed trait OptionalResult[R] {
    def maybeResult: Option[Query.Query0[R]]
    final def resultOrNothing: Query.Query0[R] = maybeResult getOrElse Query.Return(CypherFragment.Return.Nothing.as[R])
  }
  sealed trait Result[R] extends OptionalResult[R] {
    def result: Query.Query0[R]
    final def maybeResult: Option[Query.Query0[R]] = Some(result)
  }
  object Result{
    implicit def resultToQuery[R](result: Result[R]): Query[R] = result.result

    def manually[R](res: Query.Query0[R]): Result[R] = new Custom[R] { def result: Query.Query0[R] = res }

    protected[syntax] trait Custom[R] extends Result[R]

    protected[syntax] trait Ret[R] extends Result[R] {
      protected[syntax] def ret: Known[Return[R]]
      def result: Query.Query0[R] = Query.Return(ret)
    }

    protected[syntax] trait Clause[R] extends Result[R] {
      protected[syntax] def clause: Query.Clause[R]
      def result: Query.Query0[R] = clause
    }

    protected[syntax] case object None extends OptionalResult[Unit] {
      def maybeResult: Option[Query.Query0[Unit]] = scala.None
    }
    // TODO =========================================================
    // TODO: rename clause's aliases to avoid collision!?
  }

  final class ParameterizedQuery[Params <: HList, +R](val prepared: Parameterized.Prepared[Params, Query[R]]) extends AnyVal {
    override def toString: String = prepared.toString
  }


  object ParameterizedQuery {
    implicit def extractPrepared[Params <: HList, R](pq: ParameterizedQuery[Params, R]): Parameterized.Prepared[Params, Query[R]] = pq.prepared

    def impl(c: whitebox.Context)(f: c.Tree): c.Tree = {
      lazy val c0: c.type = c
      val helper = new CaseClassMacros { val c: c0.type = c0 }

      import c.universe._

      val ParamSymbol = symbolOf[Param[_]]
      val QueryClauseSymbol = symbolOf[CypherFragment.Query.Clause[_]]
      val QuerySymbol       = symbolOf[CypherFragment.Query[_]]
      val MatchResultType   = typeOf[MatchObj.Result[_]]

      f match {
        case Function(params, body) =>
          val (retType, isMatchResult) = body.tpe match {
            case TypeRef(_, QueryClauseSymbol, List(t)) => t -> false
            case TypeRef(_, QuerySymbol, List(t))       => t -> false
            case tpe@TypeRef(_, _, List(t)) if tpe <:< MatchResultType => t -> true
            case other => c.abort(body.pos, s"Not a query: $other")
          }
          val (paramTrees, recTpes) = params.map{ p =>
            val tpe = p.tpt.tpe match {
              case TypeRef(_, ParamSymbol, List(t)) => t
              case other => c.abort(p.pos, s"`parameterized` arguments must be of type `Param[?]`, got $other")
            }
            val argTree = q"_root_.com.arkondata.slothql02.cypher.CypherFragment.Expr.Param[$tpe](${p.name.decodedName.toString})"
            val recEntry = helper.mkFieldTpe(p.name, tpe)
            argTree -> recEntry
          }.unzip
          val recTpe = helper.mkHListTpe(recTpes)
          val outTypeTree = tq"_root_.com.arkondata.slothql02.cypher.CypherFragment.Query[$retType]"

          val query0 = q"$f(..$paramTrees)"
          val query = if (isMatchResult) q"$query0.result" else query0
          q"""
            new _root_.com.arkondata.slothql02.cypher.syntax.Match.ParameterizedQuery(
              _root_.com.arkondata.slothql02.cypher.CypherFragment.Parameterized[$recTpe, $outTypeTree].apply($query)
            )
           """
        case _ =>
          c.abort(c.enclosingPosition, "Expecting a function (Param[A1], Param[A2], ...) => CypherFragment.Query.Clause[R]")
      }
    }
  }

  private type VE = Vertex - Edge
  private type EV = Edge - Vertex
  private type II = Int - Int

  // The definitions that should be package-private but cannot be
  object Internal {
    @inline def setAlias(e: CypherFragment.Expr.Var[_], alias: String): Unit = e.asInstanceOf[Graph.Impl[_]]._alias = alias
    @inline def graph: Graph = Graph()
  }

  private[syntax] object MacrosInternal {
    var CheckForUnboundUsage: Seq[(Any, Any)] = Nil
  }

  class Internal(override val c: whitebox.Context) extends InternalImpl(c)

  class InternalImpl[C <: whitebox.Context](val c: C) {
    import c.universe._

    def implApply[R: c.WeakTypeTag](f: c.Expr[Graph => MatchObj.Result[R]]): c.Expr[Query[R]] =
      impl[R](optional = c.universe.reify(false), f)
    def implOptional[R: c.WeakTypeTag](f: c.Expr[Graph => MatchObj.Result[R]]): c.Expr[Query[R]] =
      impl[R](optional = c.universe.reify(true), f)
    def implMaybeOptional[R: c.WeakTypeTag](opt: c.Expr[Boolean])(f: c.Expr[Graph => MatchObj.Result[R]]): c.Expr[Query[R]] =
      impl[R](optional = opt, f)

    val `syntax pkg` = c.typeOf[com.arkondata.slothql02.cypher.syntax.`package`.type]
    val `syntax <-` = typeOf[`<-`.type]
    val `syntax ->` = typeOf[->.type]
    val `syntax <` = typeOf[<.type]
    val `syntax >` = typeOf[>.type]
    val `syntax V-E` = typeOf[VE]
    val `syntax E-V` = typeOf[EV]
    val `syntax Int-Int` = typeOf[II]
    val `syntax :=` = typeOf[:=.type]
    val `syntax :?=` = typeOf[:?=.type]
    val `syntax ::=` = typeOf[::=.type]
    val `syntax *:` = typeOf[*:.type]
    val GraphType         = typeOf[Graph]
    val GraphVertexType   = typeOf[Vertex]
    val GraphEdgeType     = typeOf[Edge]
    val GraphVertexCTType = weakTypeOf[ClassTag[Vertex]]
    val GraphEdgeCTType   = weakTypeOf[ClassTag[Edge]]
    val ExprType          = weakTypeOf[CypherFragment.Expr[_]]
    val KnownExprType     = weakTypeOf[Known[CypherFragment.Expr[_]]]
    val KnownExprSMapType = weakTypeOf[Map[String, Known[CypherFragment.Expr[_]]]]

    object SomeCypherExpr {
      def unapply(tree: Tree): Option[Tree] = if (tree.tpe <:< ExprType || tree.tpe <:< KnownExprType) Some(tree) else None
    }

    sealed trait KnownExpr[+A] {
      def symbol: Symbol
      def name: Option[Name]
      def expr: c.Expr[Known[A]]
      def underlying: List[KnownPattern]
    }
    sealed trait KnownPattern extends KnownExpr[Pattern]
    case class NodeKP(symbol: Symbol, name: Option[Name], expr: c.Expr[Known[Pattern.Node]])
        extends KnownPattern with KnownExpr[Pattern.Node] { def underlying: List[KnownPattern] = Nil }
    case class RelKP(symbol: Symbol, name: Option[Name], expr: c.Expr[Known[Pattern.Rel]])
        extends KnownPattern with KnownExpr[Pattern.Rel] { def underlying: List[KnownPattern] = Nil }
    case class LetKP(symbol: Symbol, name: Option[Name], expr: c.Expr[Known[Pattern.Let]], underlying: List[KnownPattern])
        extends KnownPattern with KnownExpr[Pattern.Let]

    object KnownExpr {
      def node(name: Name, body: Tree): c.Expr[Known[Pattern.Node]] = PatternMatch.extractBindParams(body) match {
        case PatternMatch.NewBindParams(labels, values) =>
          node0(aliasTree(name), labels, valuesTree(values))
        case PatternMatch.ReuseBindParams(reuse) =>
          if (name != termNames.WILDCARD) c.abort(body.pos, s"Cannot re-bind graph vertices, use the old ones")
          nodeT(q"_root_.scala.Some($reuse.name)")
      }
      def node1(name: Name): c.Expr[Known[Pattern.Node]] =
        nodeT(aliasTree(name))
      def nodeT(tree: Tree): c.Expr[Known[Pattern.Node]] =
        node0(tree, q"_root_.scala.Nil", q"_root_.scala.collection.immutable.Map.empty")

      def node0(alias: Tree, labels: Tree, values: Tree): c.Expr[Known[Pattern.Node]] = c.Expr[Known[Pattern.Node]](
        mkKnown(
          q"_root_.com.arkondata.slothql02.cypher.CypherFragment.Pattern.Node(alias = $alias, labels = $labels, map = $values)"
        )
      )

      def rel(name: Option[Name], body: Tree, length: Option[Tree], dir: c.Expr[Rel.Direction]): c.Expr[Known[Pattern.Rel]] =
        PatternMatch.extractBindParams(body) match {
          case PatternMatch.NewBindParams(labels, values) =>
            rel0(
              alias = name.map(aliasTree(_)).getOrElse(q"_root_.scala.None"),
              types = labels,
              map = valuesTree(values),
              length = length.map(len => q"_root_.scala.Some($len)").getOrElse(q"_root_.scala.None"),
              dir = dir.tree
            )
          case PatternMatch.ReuseBindParams(reuse) =>
            if (name.exists(_ != termNames.WILDCARD)) c.abort(body.pos, s"Cannot re-bind graph edges, use the old ones")
            rel0(
              alias = q"_root_.scala.Some($reuse.name)",
              types = q"_root_.scala.Nil",
              map = q"""_root_.scala.collection.Map.empty[
                          String,
                           ${ttKnown(tq"_root_.com.arkondata.slothql02.cypher.CypherFragment.Expr[_]")}
                        ]""",
              length = q"_root_.scala.None",
              dir = dir.tree
            )
      }

      // simple
      def rel(dir: c.Expr[Rel.Direction]): RelKP = RelKP(NoSymbol, None, KnownExpr.rel(None, EmptyTree, None, dir))

      def rel0(alias: Tree, types: Tree, map: Tree, length: Tree, dir: Tree): c.Expr[Known[Pattern.Rel]] = {
        val tree = mkKnown(q"""
          _root_.com.arkondata.slothql02.cypher.CypherFragment.Pattern.Rel(
            alias = $alias,
            types = $types,
            map = $map,
            length = $length ,
            dir = $dir
          )
        """)
        c.Expr[Known[Pattern.Rel]](tree)
      }

      def let(name: Name, pattern: Tree): c.Expr[Known[Pattern.Let]] = c.Expr[Known[Pattern.Let]](
        mkKnown(q"""
          _root_.com.arkondata.slothql02.cypher.CypherFragment.Pattern.Let(
            alias = ${name.decodedName.toString},
            pattern = $pattern
          )
        """)
      )

      def let(name: Name, patterns: List[KnownPattern]): c.Expr[Known[Pattern.Let]] =
        KnownExpr.let(name, PatternMatch.toFragment(patterns).left.get.tree) // TODO: get

      private def valuesTree(values: List[Tree]) =
        q"""
          _root_.scala.collection.Seq[
            _root_.scala.collection.Seq[(_root_.java.lang.String, ${ttKnown(tq"_root_.com.arkondata.slothql02.cypher.CypherFragment.Expr[_]")})]
          ](..$values).flatten.toMap
        """

      private def aliasTree(name: Name): Tree = name match {
        case termNames.WILDCARD => q"_root_.scala.None"
        case _ => q"_root_.scala.Some(${name.decodedName.toString})"
      }
    }

    object ExtractNode {
      def unapply(tree: Tree): Option[NodeKP] = tree match {
        case Ident(termNames.WILDCARD) =>
          Some{ NodeKP(tree.symbol, None, KnownExpr.node1(termNames.WILDCARD)) }
        case i@Ident(_) =>
          c.abort(i.pos, "Graph element matching: `Vertex(?)` / `Edge(?)`")
        case Bind(name, body) =>
          Some{ NodeKP(tree.symbol, Some(name), KnownExpr.node(name, body)) }
        case ua@UnApply(fun, _) if fun.tpe =:= typeOf[Option[Seq[AnyRef]]] =>
          Some{ NodeKP(tree.symbol, None, KnownExpr.node(termNames.WILDCARD, ua)) }
        case _ => None
      }
    }

    object ExtractRel {
      type Build = c.Expr[Rel.Direction] => RelKP

      def unapply(tree: Tree): Option[Build] = tree match {
        case Ident(termNames.WILDCARD) =>
          Some{ dir => RelKP(tree.symbol, None, KnownExpr.rel(None, EmptyTree, None, dir)) }
        case Bind(name, body) =>
          Some{ dir => RelKP(tree.symbol, Some(name), KnownExpr.rel(Some(name), body, None, dir)) }
        case ua@UnApply(fun, _) if fun.tpe =:= typeOf[Option[Seq[AnyRef]]] =>
          Some{ dir => RelKP(tree.symbol, None, KnownExpr.rel(None, ua, None, dir)) }
        case UnApply(Apply(Select(sel, TermName("unapply")), _), List(bind, limits, edgeT)) if sel.tpe =:= `syntax *:` =>
          val name = DeepRel.name(bind)
          if (name.nonEmpty) c.warning(bind.pos,
            s"Binding a variable length relationship pattern to a variable ('${name.get}') is deprecated and will be unsupported in a future version of neo4j")
          val length = DeepRel.length(limits)
          val edge = (edgeT: @unchecked) match {
            case Ident(termNames.WILDCARD)                                     => EmptyTree
            case ua@UnApply(fun, _) if fun.tpe =:= typeOf[Option[Seq[AnyRef]]] => ua
          }
          Some { dir: c.Expr[Rel.Direction] => RelKP(bind.symbol, name, KnownExpr.rel(name, edge, Some(length), dir)) }
        case _ => None
      }

      object DeepRel {
        def name(tree: Tree): Option[Name] = (tree: @unchecked) match {
          case Ident(termNames.WILDCARD)            => None
          case Bind(nme, Ident(termNames.WILDCARD)) => Some(nme)
        }

        def length(tree: Tree): Tree = (tree: @unchecked) match {
          case Ident(termNames.WILDCARD) => relAll
          case Apply(tt, List(minT, maxT)) if tt.tpe.resultType =:= `syntax Int-Int` =>
            def extractInt(tree: Tree) = (tree: @unchecked) match {
              case Literal(Constant(i: Int)) => Some(i)
              case Ident(termNames.WILDCARD) => None
            }
            def range(ior: Tree) = q"_root_.com.arkondata.slothql02.cypher.CypherFragment.Pattern.Rel.Range($ior)"

            extractInt(minT) -> extractInt(maxT) match {
              case (Some(l), Some(r)) => range(q"_root_.cats.data.Ior.Both($l, $r)")
              case (Some(l), None)    => range(q"_root_.cats.data.Ior.Left($l)")
              case (None,    Some(r)) => range(q"_root_.cats.data.Ior.Right($r)")
              case (None, None)       => relAll
            }
        }

        private lazy val relAll = q"_root_.com.arkondata.slothql02.cypher.CypherFragment.Pattern.Rel.All"
      }
    }


    def impl[R: c.WeakTypeTag](optional: c.Expr[Boolean], f: c.Expr[Graph => MatchObj.Result[R]]): c.Expr[Query[R]] = mkClause(f) {
      (pattern, _, where) =>
        reify {
          Clause.Match(
            NonEmptyList(pattern.splice, Nil),
            optional = optional.splice,
            where = where.splice
          )
        }
    }

    def mkClause[R: c.WeakTypeTag](f: c.Expr[Graph => MatchObj.OptionalResult[R]])(
      mk: (c.Expr[Known[Pattern]], Option[Position], c.Expr[Option[Known[CypherFragment.Expr[Boolean]]]]) => c.Expr[Known[Clause]]
    ): c.Expr[Query[R]] = {
      val (pattern, kps, guard) = matchPattern(f)
        def extractBindSymbols(kps: List[KnownPattern]): List[(Symbol, Option[Name])] =
          kps.flatMap(ke => (ke.symbol, ke.name) :: extractBindSymbols(ke.underlying))
        val bindSymbols = extractBindSymbols(kps).toSet

        val setAliases = bindSymbols.withFilter(_._2.isDefined).map{
          case (symbol, name) =>
            q"_root_.com.arkondata.slothql02.cypher.syntax.Match.Internal.setAlias($symbol, ${name.get.decodedName.toString})"
        }.toList


        val whereVarName = c.internal.reificationSupport.freshTermName("where")
        val whereVarExpr0 = c.Expr[Unit](
          q"""
            var $whereVarName: _root_.scala.Option[
              ${ttKnown(tq"_root_.com.arkondata.slothql02.cypher.CypherFragment.Expr[_root_.scala.Boolean]")}] = null
           """)
        val whereVarDefTree = c.typecheck{ whereVarExpr0.tree }
        val whereVarSymbol  = whereVarDefTree.symbol
        val whereIdent      = Ident(whereVarSymbol)
        val whereIdentExpr  = c.Expr[Option[Known[CypherFragment.Expr[Boolean]]]](whereIdent)
        val whereVarExpr    = c.Expr[Unit](Block(whereVarDefTree :: Nil, EmptyTree))

        val whereClause = guard map (_.tree) map {
          case Apply(Select(pkg, TermName("unwrapBooleanExprInIfGuard")), List(cond)) if pkg.tpe =:= `syntax pkg` =>
            q"_root_.scala.Some(${mkKnown(cond)})"
          case Apply(Select(pkg, TermName("unwrapKnownBooleanExprInIfGuard")), List(cond)) if pkg.tpe =:= `syntax pkg` =>
            q"_root_.scala.Some($cond)"
          case tree@Apply(Select(pkg, TermName("unwrapBooleanOptionExprInIfGuard")), _) if pkg.tpe =:= `syntax pkg` =>
            raiseCompilationError(c)(tree)
          case Apply(Select(pkg, TermName("unwrapKnownBooleanOptionExprInIfGuard")), List(cond)) if pkg.tpe =:= `syntax pkg` =>
            cond
          case g => c.abort(g.pos, "`if` contents cannot be transformed to WHERE clause:\n" + showRaw(g))
        } getOrElse
          q"_root_.scala.None"

        val setWhereVar = Assign(whereIdent, whereClause)


        object fTransormer extends Transformer {
          override def transform(tree: Tree): Tree = tree match {
            case b@Bind(name, _) if name != typeNames.WILDCARD =>
              if (!name.toString.contains("$")) bound += b.symbol
              val newBind = Bind(name, Ident(termNames.WILDCARD))
              c.internal.setSymbol(newBind, b.symbol)
            case UnApply(fun, _) if fun.tpe =:= typeOf[Option[Seq[AnyRef]]] =>
              Bind(termNames.WILDCARD, Ident(termNames.WILDCARD))
            case UnApply(Apply(Select(sel, TermName("unapply")), _), List(bind, _, _)) if sel.tpe =:= `syntax *:` =>
              bind
            case _ =>
              val forbiddenCheck = unboundUsage.find(_._1 equalsStructure tree)
              forbiddenCheck.foreach(_._2(tree, bound))
              super.transform(tree)
          }

          override def transformCaseDefs(trees: List[CaseDef]): List[CaseDef] =
            super.transformCaseDefs(trees).map {
              case CaseDef(pat, _, b0) =>
                val b = b0 match {
                  case Block(stats, expr) => Block(setAliases ::: setWhereVar :: stats, expr)
                  case expr               => Block(setAliases ::: setWhereVar :: Nil,   expr)
                }
                CaseDef(patTransformer.transform(pat), EmptyTree, b)
            }

          private lazy val unboundUsage = MatchObj.MacrosInternal.CheckForUnboundUsage
            .asInstanceOf[Seq[(Tree, (Tree, Set[Symbol]) => Unit)]]

          private var bound = Set.empty[Symbol]
          private object patTransformer extends Transformer {
            private lazy val boundNames = bound.map(_.name: c.universe.Name)
            override def transform(tree: Tree): Tree = tree match {
              case Ident(name) if boundNames contains name =>
                Bind(termNames.WILDCARD, Ident(typeNames.WILDCARD))
              case _ => super.transform(tree)
            }
          }
        }

        val retName = c.internal.reificationSupport.freshTermName("return")
        val retValExpr = c.Expr[Unit](
          q"""
             val $retName = ${fTransormer.transform(f.tree)}(_root_.com.arkondata.slothql02.cypher.syntax.Match.Internal.graph)
           """)
        val retExpr = c.Expr[MatchObj.OptionalResult[R]](q"$retName")

        val res = reify {
          whereVarExpr.splice
          retValExpr.splice
          Query.Clause(
            mk(pattern, guard.map(_.tree.pos), whereIdentExpr).splice,
            retExpr.splice.resultOrNothing.known
          )
        }

//        c.info(c.enclosingPosition, showCode(res.tree), force = true)
//        c.info(c.enclosingPosition, showRaw(res.tree), force = true)

        res
    }

    def matchPattern(f: c.Expr[Graph => _]): (c.Expr[Known[Pattern]], List[KnownPattern], Option[c.Expr[Known[CypherFragment.Expr[Boolean]]]]) =
      (f.tree: @unchecked) match {
        case Function(
              List(ValDef(_, arg0Name1, _, EmptyTree)),
              c.universe.Match(Ident(arg0Name2), cases)
            ) if arg0Name1 == arg0Name2
          =>
            cases match {
              case List(CaseDef(pattern, guard0, _)) =>
                val kps = PatternMatch.extractPatternRev(pattern)
                val guard = if (guard0.isEmpty) None else Some(c.Expr[Known[CypherFragment.Expr[Boolean]]](guard0))
                (PatternMatch.toFragment(kps).merge, kps, guard)
              case _ =>
                c.abort(c.enclosingPosition, "Only one case is permitted in `Match`.")
            }
      }

    object PatternMatch {
      sealed trait BindParams
      case class NewBindParams(labelsTree: Tree, valueTrees: List[Tree]) extends BindParams
      case class ReuseBindParams(reuse: Tree) extends BindParams

      def extractBindParams(body: Tree): BindParams = body match {
        case UnApply(Apply(Select(sel, TermName("unapplySeq")), _), List(arg))
          if (sel.tpe =:= typeOf[Vertex.type] && (arg.tpe =:= GraphVertexType || arg.tpe <:< GraphType)) ||
             (sel.tpe =:= typeOf[Edge.type]   && arg.tpe =:= GraphEdgeType) =>
          ReuseBindParams(arg)
        case UnApply(Apply(Select(sel, TermName("unapplySeq")), _), args) if sel.tpe =:= typeOf[Vertex.type] || sel.tpe =:= typeOf[Edge.type] =>
          if (args.exists(arg => arg.tpe =:= GraphVertexType || arg.tpe =:= GraphEdgeType))
            c.abort(body.pos, "Graph element matching: `Vertex(?)` / `Edge(?)`")
          val labels = args.collect{
            case one if one.tpe <:< typeOf[String] => q"_root_.scala.List($one)"
            case many if many.tpe <:< typeOf[Iterable[String]] => many
          }.reduceLeftOption((acc, next) => q"$acc ++ $next")
           .map(acc => q"$acc.toList")
           .getOrElse(q"_root_.scala.Nil")
          val values = args.collect{
            case UnApply(Apply(Select(sel2, TermName("unapply")), _), args2) if sel2.tpe =:= `syntax :=` =>
              (args2: @unchecked) match {
                case List(Literal(Constant(k: String)), e) if e.tpe <:< ExprType =>
                  q"Seq($k -> $e)"
                case List(Literal(Constant(k: String)), e) if e.tpe <:< KnownExprType =>
                  q"Seq($k -> $e)"
                case List(Literal(Constant(k: String)), v) =>
                  q"Seq($k -> _root_.com.arkondata.slothql02.cypher.CypherFragment.Expr.Lit($v).known)"
              }
            case UnApply(Apply(Select(sel2, TermName("unapply")), _), args2) if sel2.tpe =:= `syntax :?=` =>
              (args2: @unchecked) match {
                case List(Literal(Constant(k: String)), v) =>
                  q"$v.map(_root_.com.arkondata.slothql02.cypher.CypherFragment.Expr.Lit(_).known).map($k -> _).toSeq"
              }
            case map if map.tpe <:< KnownExprSMapType => q"$map.toSeq"
            case map if map.tpe <:< typeOf[Map[String, Any]] => c.abort(map.pos, "Expecting Map[String, Known[Expr[_]]]")
          }
          NewBindParams(labels, values)
        case _ => NewBindParams(q"_root_.scala.Nil", Nil)
      }

      def extractPatternRev(tree: Tree): List[KnownPattern] = tree match {
        case ua@UnApply(Apply(Select(arrow, TermName("unapply")), _), args) =>
          import KnownExpr.rel
          (args: @unchecked) match {
            case List(V1(v1),              V1(v2))              if arrow.tpe =:= `syntax <-` => v2 :: rel(reify(Rel.Incoming)) :: v1 :: Nil
            case List(l,                   V1(v))               if arrow.tpe =:= `syntax <-` => v  :: rel(reify(Rel.Incoming)) :: extractPatternRev(l)
            case List(V1(v1),              V1(v2))              if arrow.tpe =:= `syntax ->` => v2 :: rel(reify(Rel.Outgoing)) :: v1 :: Nil
            case List(l,                   V1(v))               if arrow.tpe =:= `syntax ->` => v  :: rel(reify(Rel.Outgoing)) :: extractPatternRev(l)
            case List(DashListE(l, e),     V1(v))               if arrow.tpe =:= `syntax >`  => v  :: e  (reify(Rel.Outgoing)) :: extractPatternRev(l)
            case List(VOrDashVE(revHead1), VOrDashVE(revHead2)) if arrow.tpe =:= `syntax >`  => revHead2 ::: revHead1
            case List(l,                   VOrDashVE(revHead))  if arrow.tpe =:= `syntax >`  => revHead  ::: extractPatternRev(l)
            case List(V(v),                VOrDashEV(revHead))  if arrow.tpe =:= `syntax <`  => revHead  ::: v
            case List(l,                   VOrDashEV(revHead))  if arrow.tpe =:= `syntax <`  => revHead  ::: extractPatternRev(l)
            case List(b@Bind(name, _), pat)                     if arrow.tpe =:= `syntax ::=` =>
              val patRev = extractPatternRev(pat)
              LetKP(b.symbol, Some(name), KnownExpr.let(name, patRev), patRev) :: Nil // TODO: handle Wildcard name
            case List(V(v)) if arrow.tpe <:< GraphVertexCTType => v
            case _ => failedToParse(ua)
          }
        case V(v) => v
        case _ => failedToParse(tree)
      }
      private def failedToParse(tree: Tree) =
        c.abort(c.enclosingPosition, s"Failed to parse pattern of ${showCode(tree)}\n\n${showRaw(tree)}")


      object V1 {
        def unapply(tree: Tree): Option[KnownPattern] = PartialFunction.condOpt(tree) {
          case ExtractNode(node) => node
        }
      }

      object V {
        def unapply(tree: Tree): Option[List[KnownPattern]] = PartialFunction.condOpt(tree) {
          case ExtractNode(node) => node :: Nil
        }
      }

      object VOrDashEV {
        def unapply(tree: Tree): Option[List[KnownPattern]] = PartialFunction.condOpt(tree) {
          case Apply(tt, List(ExtractRel(rel), ExtractNode(node))) if tt.tpe.resultType =:= `syntax E-V` =>
            node :: rel(reify(Rel.Incoming)) :: Nil
          case ExtractNode(node) =>
            node :: Nil
        }
      }
      object VOrDashVE {
        def unapply(tree: Tree): Option[List[KnownPattern]] = PartialFunction.condOpt(tree) {
          case Apply(tt: TypeTree, List(ExtractNode(node), ExtractRel(rel))) if tt.tpe.resultType =:= `syntax V-E` =>
            rel(reify(Rel.Outgoing)) :: node :: Nil
          case ExtractNode(node) =>
            node :: Nil
        }
      }

      object DashListE {
        def unapply(tree: Tree): Option[(Tree, c.Expr[Rel.Direction] => KnownPattern)] = PartialFunction.condOpt(tree) {
          case Apply(tt: TypeTree, List(t, ExtractRel(rel))) if tt.tpe.resultType =:= `syntax V-E` =>
            t -> (dir => rel(dir))
        }
      }

      def toFragment(patternRev: List[KnownPattern]): Either[c.Expr[Known[Pattern.Pattern0]], c.Expr[Known[Pattern.Let]]] = (patternRev: @unchecked) match {
        case LetKP(_, _, let, _) :: Nil  => Right(let)
        case NodeKP(_, _, node)  :: tail => toFragment0(tail, node)
      }

      private def toFragment0(patternRev: List[KnownPattern], acc: c.Expr[Known[Pattern.Pattern0]]): Either[c.Expr[Known[Pattern.Pattern0]], c.Expr[Known[Pattern.Let]]] =
        (patternRev: @unchecked) match {
          case Nil => Left(acc)
          case RelKP(_, _, rel) :: NodeKP(_, _, node) :: tail =>
            val newAcc = reify {
              Known(
                Pattern.Path(node.splice, rel.splice, acc.splice)
              )
            }
            toFragment0(tail, newAcc)
        }
    }

    private def mkKnown(tpe: Type, tree: Tree) = q"_root_.com.arkondata.slothql02.cypher.CypherFragment.Known[$tpe]($tree)"
    private def mkKnown(tree: Tree) = q"_root_.com.arkondata.slothql02.cypher.CypherFragment.Known($tree)"
    private def ttKnown(tt: Tree) = tq"_root_.com.arkondata.slothql02.cypher.CypherFragment.Known[$tt]"
  }

}

/* SyntaxTest3:

case user <(role)- x <(y)- group =>

(user < (role - x)) < (y - group)


             [<]        |
             / \        |
            /   \       |
          [<] (y-group) | right of [<] => -[Edge, Vertex] => y: Edge, group: Vertex
          / \           |
         /   \          |
        /  (role-x)     | right of [<] => -[Edge, Vertex] => role: Edge, x: Vertex
    (user)              | left  of [<] => Vertex

*/





/* SyntaxTest2:

case a -(b)> c -(d)> e <(f)- g =>

(((a - b) > (c - d)) > e) < (f - g)

             [<]      |
             / \      |
            /   \     |
          [>]  (f-g)  | right of [<] => -[Edge, Vertex] => f: Edge, g: Vertex
          / \         |
         /   \        |
       [>]   (e)      | right of [>] => Vertex
       / \            |
      /   \           |
     /   (c-d)        | right of [>] => -[Vertex, Edge] => c: Vertex, d: Edge
  (a-b)               | left  of [>] => -[Vertex, Edge] => a: Vertex, b: Edge
                      |



Function(
  List(ValDef(Modifiers(PARAM | SYNTHETIC), TermName("x0$1"), TypeTree(), EmptyTree)),
  Match(
    Ident(TermName("x0$1")),
    List(
      CaseDef(
        UnApply(
          Apply(
            Select(
              Select(
                Select(This(TypeName("syntax")), com.arkondata.slothql.cypher.syntax.package),
                com.arkondata.slothql.cypher.syntax.$less
              ),
              TermName("unapply")
            ),
            List(Ident(TermName("<unapply-selector>")))
          ),
          List(
            UnApply(
              Apply(Select(Select(Select(This(TypeName("syntax")), com.arkondata.slothql.cypher.syntax.package), com.arkondata.slothql.cypher.syntax.$greater), TermName("unapply")), List(Ident(TermName("<unapply-selector>")))),
              List(
                UnApply(
                  Apply(Select(Select(Select(This(TypeName("syntax")), com.arkondata.slothql.cypher.syntax.package), com.arkondata.slothql.cypher.syntax.$greater), TermName("unapply")), List(Ident(TermName("<unapply-selector>")))),
                  List(
                    Apply(
                      TypeTree().setOriginal(
                        Select(Select(This(TypeName("syntax")), com.arkondata.slothql.cypher.syntax.package), com.arkondata.slothql.cypher.syntax.$minus)
                      ),
                      List(
                        Bind(TermName("a"), Ident(termNames.WILDCARD)),
                        Bind(TermName("b"), Ident(termNames.WILDCARD))
                      )
                    ),
                    Apply(TypeTree().setOriginal(Select(Select(This(TypeName("syntax")), com.arkondata.slothql.cypher.syntax.package), com.arkondata.slothql.cypher.syntax.$minus)), List(Bind(TermName("c"), Ident(termNames.WILDCARD)), Bind(TermName("d"), Ident(termNames.WILDCARD))))
                  )
                ),
                Bind(TermName("e"), Ident(termNames.WILDCARD))
              )
            ),
            Apply(
              TypeTree().setOriginal(
                Select(Select(This(TypeName("syntax")), com.arkondata.slothql.cypher.syntax.package), com.arkondata.slothql.cypher.syntax.$minus)
              ),
              List(
                Bind(TermName("f"), Ident(termNames.WILDCARD)),
                Bind(TermName("g"), Ident(termNames.WILDCARD))
              )
            )
          )
        ),
        EmptyTree,
        Apply(
          Apply(
            TypeApply(Select(Select(This(TypeName("syntax")), com.arkondata.slothql.cypher.syntax.package), TermName("returnExpr")), List(TypeTree(), TypeTree())),
            List(
              Apply(
                Select(TypeApply(Select(Ident(TermName("a")), TermName("prop")), List(TypeTree().setOriginal(Select(Ident(scala), scala.Int)))), TermName("apply")),
                List(Literal(Constant("count")))
              )
            )
          ),
          List(
            Apply(
              TypeApply(
                Select(Select(This(TypeName("Expr")), com.arkondata.slothql.cypher.Return.Expr.Build), TermName("impl")), List(TypeTree(), TypeTree(), TypeTree())
              ),
              List(
                TypeApply(
                  Select(Select(This(TypeName("scala")), scala.Predef), TermName("$conforms")),
                  List(TypeTree())
                ),
                Select(
                  Select(This(TypeName("Expr")), com.arkondata.slothql.cypher.Expr.Key),
                  TermName("instance")
                )
              )
            )
          )
        )
      )
    )
  )
)
*/