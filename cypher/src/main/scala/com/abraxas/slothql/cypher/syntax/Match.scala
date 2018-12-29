package com.abraxas.slothql.cypher.syntax

import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.reflect.macros.{ blackbox, whitebox }

import cats.data.NonEmptyList

import com.abraxas.slothql.cypher.CypherFragment
import com.abraxas.slothql.cypher.CypherFragment.Pattern.Rel
import com.abraxas.slothql.cypher.CypherFragment._
import com.abraxas.slothql.util.raiseCompilationError

object Match { MatchObj =>
  def apply[R]   (f: Graph => Match.Result[R]): Query[R] = macro implApply[R]
  def optional[R](f: Graph => Match.Result[R]): Query[R] = macro implOptional[R]

  sealed trait Result[R] { def result: Query.Query0[R] }
  object Result{
    implicit def resultToQuery[R](result: Result[R]): Query[R] = result.result

    protected[syntax] trait Ret[R] extends Result[R] {
      protected[syntax] def ret: Known[Return[R]]
      def result: Query.Query0[R] = Query.Return(ret)
    }
    protected[syntax] trait Clause[R] extends Result[R] {
      protected[syntax] def clause: Query.Clause[R]
      def result: Query.Query0[R] = clause
    }
    protected[syntax] trait With[R] extends Result[R] {
      protected[syntax] def ret: Known[Return[R]]
      protected[syntax] def query: Known[Query.Query0[R]]
      def result: Query.Query0[R] = Query.Clause(Clause.With(ret, where = None), query)
    }
    protected[syntax] trait Unwind[A, R] extends Result[R] {
      protected[syntax] def exprs: Known[Expr[List[A]]]
      protected[syntax] def alias: String
      protected[syntax] def query: Known[Query.Query0[R]]
      def result: Query.Query0[R] = Query.Clause(Clause.Unwind(exprs, as = alias), query)
    }
    object Unwind {
      def apply[A, R](listExpr: Known[Expr[List[A]]], unwindAlias: String, next: Known[Query.Query0[R]]): Unwind[A, R] =
        new Unwind[A, R] {
          protected[syntax] def exprs: Known[Expr[List[A]]] = listExpr
          protected[syntax] def alias: String = unwindAlias
          protected[syntax] def query: Known[Query.Query0[R]] = next
        }

      def instanceImpl[A: c.WeakTypeTag, R: c.WeakTypeTag]
                      (c: blackbox.Context)
                      (list: c.Expr[Known[Expr[List[A]]]])
                      (f: c.Expr[Expr.Var[A] => Match.Result[R]])
                      : c.Expr[Match.Result.Unwind[A, R]] = {
        import c.universe._
        val paramName = (f.tree: @unchecked) match {
          case Function(ValDef(_, name, _, _) :: Nil, _) => name.toString
        }
        val rowExprTree = q"_root_.com.abraxas.slothql.cypher.CypherFragment.Expr.Var[${weakTypeOf[A]}]($paramName)"
        val rowExpr = c.Expr[CypherFragment.Expr.Var[A]](rowExprTree)
        reify {
          Unwind[A, R](
            list.splice,
            c.Expr[String](Literal(Constant(paramName))).splice,
            f.splice(rowExpr.splice).result
          )
        }
      }
    }
    // TODO =========================================================
    // TODO: rename clause's aliases to avoid collision!?
  }

  private type VE = Vertex - Edge
  private type EV = Edge - Vertex
  private type II = Int - Int

  // The definitions that should be package-private but cannot be
  object Internal {
    @inline def setAlias(e: CypherFragment.Expr.Var[_], alias: String): Unit = e.asInstanceOf[GraphElem.Impl]._alias = alias
    @inline def graph: Graph = Graph()
  }

  def implApply[R: c.WeakTypeTag](c: whitebox.Context)(f: c.Expr[Graph => Match.Result[R]]): c.Expr[Query[R]] = impl[R](optional = false, c)(f)
  def implOptional[R: c.WeakTypeTag](c: whitebox.Context)(f: c.Expr[Graph => Match.Result[R]]): c.Expr[Query[R]] = impl[R](optional = true, c)(f)
  def impl[R: c.WeakTypeTag](optional: Boolean, c: whitebox.Context)(f: c.Expr[Graph => MatchObj.Result[R]]): c.Expr[Query[R]] = {
    import c.universe._

    val `syntax pkg` = c.typeOf[com.abraxas.slothql.cypher.syntax.`package`.type]
    val `syntax <-` = typeOf[`<-`.type]
    val `syntax ->` = typeOf[->.type]
    val `syntax <` = typeOf[<.type]
    val `syntax >` = typeOf[>.type]
    val `syntax V-E` = typeOf[VE]
    val `syntax E-V` = typeOf[EV]
    val `syntax Int-Int` = typeOf[II]
    val `syntax :=` = typeOf[:=.type]
    val `syntax :?=` = typeOf[:?=.type]
    val `syntax *:` = typeOf[*:.type]
    val GraphVertexType = weakTypeOf[ClassTag[CypherFragment.Expr.Var[Map[String, Any]] with Graph.Vertex]]
    val GraphEdgeType   = weakTypeOf[ClassTag[CypherFragment.Expr.Var[Map[String, Any]] with Graph.Edge]]
    val ExprType        = weakTypeOf[CypherFragment.Expr[_]]
    val KnownExprType   = weakTypeOf[Known[CypherFragment.Expr[_]]]

    object SomeCypherExpr {
      def unapply(tree: Tree): Option[Tree] = if (tree.tpe <:< ExprType || tree.tpe <:< KnownExprType) Some(tree) else None
    }

    object UnApplyClassTag {
      def unapply(tree :Tree): Option[(Type, Tree)] = PartialFunction.condOpt(tree) {
        case UnApply(Apply(Select(Typed(_, tpt), TermName("unapply")), _), List(arg)) => tpt.tpe -> arg
      }
    }


    def extractBindParams(body: Tree): (Tree, List[Tree]) = body match {
      case UnApplyClassTag(_, arg) => extractBindParams(arg)
      case UnApply(Apply(Select(sel, TermName("unapplySeq")), _), args) if sel.tpe =:= typeOf[Vertex.type] || sel.tpe =:= typeOf[Edge.type] =>
        val labels = args.collect{
          case one if one.tpe <:< typeOf[String] => q"_root_.scala.List($one)"
          case many if many.tpe <:< typeOf[Iterable[String]] => many
        }.reduceLeftOption((acc, next) => q"$acc ++ $next")
         .map(acc => q"$acc.toList")
         .getOrElse(q"_root_.scala.Nil")
        val values = args.collect{
          case UnApply(Apply(Select(sel2, TermName("unapply")), _), args2) if sel2.tpe =:= `syntax :=` =>
            (args2: @unchecked) match {
              case List(Literal(Constant(k: String)), SomeCypherExpr(v)) =>
                q"Some($k -> $v)"
              case List(Literal(Constant(k: String)), v) =>
                q"Some($k -> _root_.com.abraxas.slothql.cypher.CypherFragment.Expr.Lit($v))"
            }
          case UnApply(Apply(Select(sel2, TermName("unapply")), _), args2) if sel2.tpe =:= `syntax :?=` =>
            (args2: @unchecked) match {
              case List(Literal(Constant(k: String)), v) =>
                q"$v.map(_root_.com.abraxas.slothql.cypher.CypherFragment.Expr.Lit(_)).map($k -> _)"
            }
        }
        (labels, values)
      case _ => (q"_root_.scala.Nil", Nil)
    }


    def aliasTree(name: Name): Tree = name match {
      case termNames.WILDCARD => q"_root_.scala.None"
      case _ => q"_root_.scala.Some(${name.decodedName.toString})"
    }

    def knownNodeExpr(name: Name, body: Tree): c.Expr[Known[Pattern.Node]] = {
      val (labels, values) = extractBindParams(body)
      val tree = q"""
        _root_.com.abraxas.slothql.cypher.CypherFragment.Known(
          _root_.com.abraxas.slothql.cypher.CypherFragment.Pattern.Node(
            alias = ${aliasTree(name)},
            labels = $labels,
            map = _root_.scala.collection.Seq[Option[(String, _root_.com.abraxas.slothql.cypher.CypherFragment.Known[_root_.com.abraxas.slothql.cypher.CypherFragment.Expr[_]])]]
                                             (..$values).flatten.toMap
          )
        )
      """
      c.Expr[Known[Pattern.Node]](tree)
    }

    def knownRelExpr(name: Option[Name], body: Tree, length: Option[Tree], dir: c.Expr[Rel.Direction]): c.Expr[Known[Pattern.Rel]] = {
      val (labels, values) = extractBindParams(body)
      val tree = q"""
        _root_.com.abraxas.slothql.cypher.CypherFragment.Known(
          _root_.com.abraxas.slothql.cypher.CypherFragment.Pattern.Rel(
            alias = ${name.map(aliasTree(_)).getOrElse(q"_root_.scala.None")},
            types = $labels,
            map = _root_.scala.collection.Seq[Option[(String, _root_.com.abraxas.slothql.cypher.CypherFragment.Known[_root_.com.abraxas.slothql.cypher.CypherFragment.Expr[_]])]]
                                             (..$values).flatten.toMap,
            length = ${length.map(len => q"_root_.scala.Some($len)").getOrElse(q"_root_.scala.None")},
            dir = ${dir.tree}
          )
        )
      """
      c.Expr[Known[Pattern.Rel]](tree)
    }

    object ExtractNode {
      def unapply(tree: Tree): Option[NamedKnownExpr[Pattern.Node]] = tree match {
        case UnApplyClassTag(tpe, arg) if tpe <:< GraphVertexType => unapply(arg)
        case Ident(termNames.WILDCARD) =>
          Some{ (tree.symbol, None) -> reify { Known{ Pattern.Node(alias = None, labels = Nil, map = Map()) } } }
        case Bind(name, body) =>
          Some{ (tree.symbol, Some(name)) -> knownNodeExpr(name, body) }
        case ua@UnApply(fun, _) if fun.tpe =:= typeOf[Option[Seq[AnyRef]]] =>
          Some{ (tree.symbol, None) -> knownNodeExpr(termNames.WILDCARD, ua) }
        case _ => None
      }
    }

    object ExtractRel {
      type Build = c.Expr[Rel.Direction] => NamedKnownExpr[Pattern.Rel]

      def unapply(tree: Tree): Option[Build] = tree match {
        case UnApplyClassTag(tpe, arg) if tpe <:< GraphEdgeType => unapply(arg)
        case Ident(termNames.WILDCARD) =>
          Some{ dir => (tree.symbol, None) -> knownRelExpr(None, EmptyTree, None, dir) }
        case Bind(name, body) =>
          Some{ dir => (tree.symbol, Some(name)) -> knownRelExpr(Some(name), body, None, dir) }
        case ua@UnApply(fun, _) if fun.tpe =:= typeOf[Option[Seq[AnyRef]]] =>
          Some{ dir => (tree.symbol, None) -> knownRelExpr(None, ua, None, dir) }
        case UnApply(Apply(Select(sel, TermName("unapply")), _), List(bind, limits, edgeT)) if sel.tpe =:= `syntax *:` =>
          val name = DeepRel.name(bind)
          val length = DeepRel.length(limits)
          val edge = (edgeT: @unchecked) match { case UnApplyClassTag(tpe, arg) if tpe <:< GraphEdgeType => arg }
          Some { dir: c.Expr[Rel.Direction] => ((bind.symbol, name), knownRelExpr(name, edge, Some(length), dir)) }
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
            def range(ior: Tree) = q"_root_.com.abraxas.slothql.cypher.CypherFragment.Pattern.Rel.Range($ior)"

            extractInt(minT) -> extractInt(maxT) match {
              case (Some(l), Some(r)) => range(q"_root_.cats.data.Ior.Both($l, $r)")
              case (Some(l), None)    => range(q"_root_.cats.data.Ior.Left($l)")
              case (None,    Some(r)) => range(q"_root_.cats.data.Ior.Right($r)")
              case (None, None)       => relAll
            }
        }

        private lazy val relAll = q"_root_.com.abraxas.slothql.cypher.CypherFragment.Pattern.Rel.All"
      }
    }

    type NamedKnownExpr[A] = ((Symbol, Option[Name]), c.Expr[Known[A]])
    type KnownVertexEdgeExpr = Either[NamedKnownExpr[Pattern.Node], NamedKnownExpr[Pattern.Rel]]

    object V1 {
      def unapply(tree: Tree): Option[KnownVertexEdgeExpr] = PartialFunction.condOpt(tree) {
        case ExtractNode(node) => Left(node)
      }
    }

    object V {
      def unapply(tree: Tree): Option[List[KnownVertexEdgeExpr]] = PartialFunction.condOpt(tree) {
        case ExtractNode(node) => Left(node) :: Nil
      }
    }

    object VOrDashEV {
      def unapply(tree: Tree): Option[List[KnownVertexEdgeExpr]] = PartialFunction.condOpt(tree) {
        case Apply(tt, List(ExtractRel(rel), ExtractNode(node))) if tt.tpe.resultType =:= `syntax E-V` =>
          Left(node) :: Right(rel(reify(Rel.Incoming))) :: Nil
        case ExtractNode(node) =>
          Left(node) :: Nil
      }
    }
    object VOrDashVE {
      def unapply(tree: Tree): Option[List[KnownVertexEdgeExpr]] = PartialFunction.condOpt(tree) {
        case Apply(tt: TypeTree, List(ExtractNode(node), ExtractRel(rel))) if tt.tpe.resultType =:= `syntax V-E` =>
          Right(rel(reify(Rel.Outgoing))) :: Left(node) :: Nil
        case ExtractNode(node) =>
          Left(node) :: Nil
      }
    }

    object DashListE {
      def unapply(tree: Tree): Option[(Tree, c.Expr[Rel.Direction] => KnownVertexEdgeExpr)] = PartialFunction.condOpt(tree) {
        case Apply(tt: TypeTree, List(t, ExtractRel(rel))) if tt.tpe.resultType =:= `syntax V-E` =>
          t -> (dir => Right(rel(dir)))
      }
    }

    def simpleRel(dir: c.Expr[Rel.Direction]): KnownVertexEdgeExpr = Right {
      (NoSymbol, None) -> knownRelExpr(None, EmptyTree, None, dir)
    }

    def failedToParse(tree: Tree) =
      c.abort(c.enclosingPosition, s"Failed to parse pattern of ${showCode(tree)}\n\n${showRaw(tree)}")

    (f.tree: @unchecked) match {
      case Function(
            List(ValDef(_, arg0Name1, _, EmptyTree)),
            c.universe.Match(Ident(arg0Name2), cases)
          ) if arg0Name1 == arg0Name2 =>

        val CaseDef(pattern0, guard0, body0) = cases match {
          case List(caseDef) => caseDef
          case _ => c.abort(c.enclosingPosition, "Only one case is permitted in `Match`.")
        }

        def extractPatternRev(tree: Tree): List[KnownVertexEdgeExpr] = tree match {
          case ua@UnApply(Apply(Select(arrow, TermName("unapply")), _), args) =>
            (args: @unchecked) match {
              case List(V1(v1),              V1(v2))              if arrow.tpe =:= `syntax <-` => v2 :: simpleRel(reify(Rel.Incoming)) :: v1 :: Nil
              case List(l,                   V1(v))               if arrow.tpe =:= `syntax <-` => v  :: simpleRel(reify(Rel.Incoming)) :: extractPatternRev(l)
              case List(V1(v1),              V1(v2))              if arrow.tpe =:= `syntax ->` => v2 :: simpleRel(reify(Rel.Outgoing)) :: v1 :: Nil
              case List(l,                   V1(v))               if arrow.tpe =:= `syntax ->` => v  :: simpleRel(reify(Rel.Outgoing)) :: extractPatternRev(l)
              case List(DashListE(l, e),     V1(v))               if arrow.tpe =:= `syntax >`  => v  :: e        (reify(Rel.Outgoing)) :: extractPatternRev(l)
              case List(VOrDashVE(revHead1), VOrDashVE(revHead2)) if arrow.tpe =:= `syntax >`  => revHead2 ::: revHead1
              case List(l,                   VOrDashVE(revHead))  if arrow.tpe =:= `syntax >`  => revHead  ::: extractPatternRev(l)
              case List(V(v),                VOrDashEV(revHead))  if arrow.tpe =:= `syntax <`  => revHead  ::: v
              case List(l,                   VOrDashEV(revHead))  if arrow.tpe =:= `syntax <`  => revHead  ::: extractPatternRev(l)
              case List(V(v)) if arrow.tpe <:< GraphVertexType => v
              case _ => failedToParse(ua)
            }
          case V(v) => v
          case _ => failedToParse(tree)
        }


        def toFragment0(patternRev: List[KnownVertexEdgeExpr], acc: c.Expr[Known[Pattern.Pattern0]]): c.Expr[Known[Pattern.Pattern0]] =
          (patternRev: @unchecked) match {
            case Nil => acc
            case Right((_, relExpr)) :: Left((_, nodeExpr)) :: tail =>
              val newAcc = reify {
                Known(
                  Pattern.Path(nodeExpr.splice, relExpr.splice, acc.splice)
                )
              }
              toFragment0(tail, newAcc)
          }
        def toFragment(patternRev: List[KnownVertexEdgeExpr]): c.Expr[Known[Pattern.Pattern0]] = (patternRev: @unchecked) match {
          case Left((_, node)) :: tail => toFragment0(tail, node)
        }

        val p = extractPatternRev(pattern0)
        val pattern = toFragment(p)

        val bindSymbols = p.map(_.left.map(_._1).right.map(_._1).merge).toSet

        val setAliases = bindSymbols.withFilter(_._2.isDefined).map{
          case (symbol, name) =>
            q"_root_.com.abraxas.slothql.cypher.syntax.Match.Internal.setAlias($symbol, ${name.get.decodedName.toString})"
        }.toList


        val whereVarExpr0 = reify {
          var where: Option[Known[CypherFragment.Expr[Boolean]]] = null
        }
        val whereVarDefTree = c.typecheck{ whereVarExpr0.tree.asInstanceOf[Block].stats.head }
        val whereVarSymbol  = whereVarDefTree.symbol
        val whereIdent      = Ident(whereVarSymbol)
        val whereIdentExpr  = c.Expr[Option[Known[CypherFragment.Expr[Boolean]]]](whereIdent)
        val whereVarExpr    = c.Expr[Unit](Block(whereVarDefTree :: Nil, EmptyTree))

        val whereClause = guard0 match {
          case Apply(Select(pkg, TermName("unwrapBooleanExprInIfGuard")), List(cond)) if pkg.tpe =:= `syntax pkg` =>
            q"_root_.scala.Some(_root_.com.abraxas.slothql.cypher.CypherFragment.Known($cond))"
          case Apply(Select(pkg, TermName("unwrapKnownBooleanExprInIfGuard")), List(cond)) if pkg.tpe =:= `syntax pkg` =>
            q"_root_.scala.Some($cond)"
          case tree@Apply(Select(pkg, TermName("unwrapBooleanOptionExprInIfGuard")), _) if pkg.tpe =:= `syntax pkg` =>
            raiseCompilationError(c)(tree)
          case Apply(Select(pkg, TermName("unwrapKnownBooleanOptionExprInIfGuard")), List(cond)) if pkg.tpe =:= `syntax pkg` =>
            cond
          case EmptyTree => q"_root_.scala.None"
          case _ => c.abort(guard0.pos, "`if` contents cannot be transformed to WHERE clause:\n" + showRaw(guard0))
        }

        val setWhereVar = Assign(whereIdent, whereClause)


        object fTransormer extends Transformer {
          override def transform(tree: c.universe.Tree): c.universe.Tree = tree match {
            case b@Bind(name, _) =>
              val newBind = Bind(name, Ident(termNames.WILDCARD))
              c.internal.setSymbol(newBind, b.symbol)
            case UnApply(fun, _) if fun.tpe =:= typeOf[Option[Seq[AnyRef]]] =>
              Bind(termNames.WILDCARD, Ident(termNames.WILDCARD))
            case UnApply(Apply(Select(sel, TermName("unapply")), _), List(bind, _, _)) if sel.tpe =:= `syntax *:` =>
              bind
            case other =>
              super.transform(other)
          }

          override def transformCaseDefs(trees: List[c.universe.CaseDef]): List[c.universe.CaseDef] =
            super.transformCaseDefs(trees).map {
              case CaseDef(pat, _, b0) =>
                val b = b0 match {
                  case Block(stats, expr) => Block(setAliases ::: setWhereVar :: stats, expr)
                  case expr               => Block(setAliases ::: setWhereVar :: Nil,   expr)
                }
                CaseDef(pat, EmptyTree, b)
            }
        }

        val f2 = c.Expr[Graph => MatchObj.Result[R]](fTransormer.transform(f.tree))

        val res = reify {
          whereVarExpr.splice
          val ret = f2.splice.apply(Internal.graph)
          Query.Clause(
            Clause.Match(
              NonEmptyList(pattern.splice, Nil),
              optional = c.Expr[Boolean](Literal(Constant(optional))).splice,
              where = whereIdentExpr.splice
            ),
            ret.result.known
          )
        }

//        c.info(c.enclosingPosition, showCode(res.tree), force = true)
//        c.info(c.enclosingPosition, showRaw(res.tree), force = true)

        res
    }
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
                Select(This(TypeName("syntax")), com.abraxas.slothql.cypher.syntax.package),
                com.abraxas.slothql.cypher.syntax.$less
              ),
              TermName("unapply")
            ),
            List(Ident(TermName("<unapply-selector>")))
          ),
          List(
            UnApply(
              Apply(Select(Select(Select(This(TypeName("syntax")), com.abraxas.slothql.cypher.syntax.package), com.abraxas.slothql.cypher.syntax.$greater), TermName("unapply")), List(Ident(TermName("<unapply-selector>")))),
              List(
                UnApply(
                  Apply(Select(Select(Select(This(TypeName("syntax")), com.abraxas.slothql.cypher.syntax.package), com.abraxas.slothql.cypher.syntax.$greater), TermName("unapply")), List(Ident(TermName("<unapply-selector>")))),
                  List(
                    Apply(
                      TypeTree().setOriginal(
                        Select(Select(This(TypeName("syntax")), com.abraxas.slothql.cypher.syntax.package), com.abraxas.slothql.cypher.syntax.$minus)
                      ),
                      List(
                        Bind(TermName("a"), Ident(termNames.WILDCARD)),
                        Bind(TermName("b"), Ident(termNames.WILDCARD))
                      )
                    ),
                    Apply(TypeTree().setOriginal(Select(Select(This(TypeName("syntax")), com.abraxas.slothql.cypher.syntax.package), com.abraxas.slothql.cypher.syntax.$minus)), List(Bind(TermName("c"), Ident(termNames.WILDCARD)), Bind(TermName("d"), Ident(termNames.WILDCARD))))
                  )
                ),
                Bind(TermName("e"), Ident(termNames.WILDCARD))
              )
            ),
            Apply(
              TypeTree().setOriginal(
                Select(Select(This(TypeName("syntax")), com.abraxas.slothql.cypher.syntax.package), com.abraxas.slothql.cypher.syntax.$minus)
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
            TypeApply(Select(Select(This(TypeName("syntax")), com.abraxas.slothql.cypher.syntax.package), TermName("returnExpr")), List(TypeTree(), TypeTree())),
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
                Select(Select(This(TypeName("Expr")), com.abraxas.slothql.cypher.Return.Expr.Build), TermName("impl")), List(TypeTree(), TypeTree(), TypeTree())
              ),
              List(
                TypeApply(
                  Select(Select(This(TypeName("scala")), scala.Predef), TermName("$conforms")),
                  List(TypeTree())
                ),
                Select(
                  Select(This(TypeName("Expr")), com.abraxas.slothql.cypher.Expr.Key),
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