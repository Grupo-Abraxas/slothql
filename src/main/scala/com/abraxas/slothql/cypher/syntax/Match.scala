package com.abraxas.slothql.cypher.syntax

import scala.language.experimental.macros
import scala.reflect.ClassTag
import scala.reflect.macros.whitebox

import com.abraxas.slothql.cypher.CypherFragment
import com.abraxas.slothql.cypher.CypherFragment.Pattern.Rel
import com.abraxas.slothql.cypher.CypherFragment._

object Match {
  def apply[R](f: Graph => Return[R]): Query[R] = macro impl[R]


  private type VE = Vertex - Edge
  private type EV = Edge - Vertex
  private type II = Int - Int

  // The definitions that should be package-private but cannot be
  object Internal {
    @inline def setAlias(e: CypherFragment.Expr.Var[_], alias: String): Unit = e.asInstanceOf[GraphElem.Impl]._alias = alias
    @inline def graph: Graph = Graph.instance
  }

  def impl[R: c.WeakTypeTag](c: whitebox.Context)(f: c.Expr[Graph => Return[R]]): c.Expr[Query[R]] = {
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
    val `syntax *:` = typeOf[*:.type]
    val GraphVertexType = weakTypeOf[ClassTag[CypherFragment.Expr.Var[Map[String, Any]] with Graph.Vertex]]
    val GraphEdgeType   = weakTypeOf[ClassTag[CypherFragment.Expr.Var[Map[String, Any]] with Graph.Edge]]


    object UnApplyClassTag {
      def unapply(tree :Tree): Option[(Type, Tree)] = PartialFunction.condOpt(tree) {
        case UnApply(Apply(Select(Typed(_, tpt), TermName("unapply")), _), List(arg)) => tpt.tpe -> arg
      }
    }


    def extractBindParams(body: Tree): (List[String], List[(String, Tree)]) = body match {
      case UnApplyClassTag(_, arg) => extractBindParams(arg)
      case UnApply(Apply(Select(sel, TermName("unapplySeq")), _), args) if sel.tpe =:= typeOf[Vertex.type] || sel.tpe =:= typeOf[Edge.type] =>
        val labels = args.collect{ case Literal(Constant(label: String)) => label }
        val values = args.collect{
          case UnApply(Apply(Select(sel2, TermName("unapply")), _), args2) if sel2.tpe =:= `syntax :=` =>
            (args2: @unchecked) match {
              case List(Literal(Constant(k: String)), v) =>
                k -> q"_root_.com.abraxas.slothql.cypher.CypherFragment.Expr.Lit($v)"
            }
        }
        (labels, values)
      case _ => (Nil, Nil)
    }


    def withAliasBuilder(name: Name): Tree => Tree = builderTree => name match {
      case termNames.WILDCARD => builderTree
      case _ => q"$builderTree.withAlias(${name.decodedName.toString})"
    }

    def withValuesBuilder(values: List[(String, Tree)]): Tree => Tree = builderTree =>
      q"$builderTree.withValues(..${values.map { case (k, v) => q"${TermName(k)} = $v" }})"

    def withLengthBuilder(length: Option[Tree]): Tree => Tree = builderTree =>
      length map (l => q"$builderTree.withLength($l)") getOrElse builderTree

    def nodeExpr(name: Name, body: Tree): c.Expr[Pattern.Node] = {
      val (labels, values) = extractBindParams(body)
      val builder = withAliasBuilder(name) compose withValuesBuilder(values) apply
        q"""
          _root_.com.abraxas.slothql.cypher.CypherFragment.Pattern.Node
            .typed
            .withLabels(..$labels)
         """
      c.Expr[Pattern.Node](q"$builder.build")
    }

    def relExpr(name: Option[Name], body: Tree, length: Option[Tree], dir: c.Expr[Rel.Direction]): c.Expr[Pattern.Rel] = {
      val (types, values) = extractBindParams(body)
      val withAliasBuilder0 = name.map(withAliasBuilder(_)) getOrElse (identity(_: Tree): Tree)
      val builder = withAliasBuilder0 compose withValuesBuilder(values) compose withLengthBuilder(length) apply
        q"""
          _root_.com.abraxas.slothql.cypher.CypherFragment.Pattern.Rel
            .typed
            .withTypes(..$types)
            .withDirection($dir)
         """
      c.Expr[Pattern.Rel](q"$builder.build")
    }

    object ExtractNode {
      def unapply(tree: Tree): Option[NamedExpr[Pattern.Node]] = tree match {
        case UnApplyClassTag(tpe, arg) if tpe <:< GraphVertexType => unapply(arg)
        case Ident(termNames.WILDCARD) =>
          Some{ (tree.symbol, None) -> reify { Pattern.Node(alias = None, labels = Nil, values = Map()) } }
        case Bind(name, body) =>
          Some{ (tree.symbol, Some(name)) -> nodeExpr(name, body) }
        case ua@UnApply(fun, _) if fun.tpe =:= typeOf[Option[Seq[AnyRef]]] =>
          Some{ (tree.symbol, None) -> nodeExpr(termNames.WILDCARD, ua) }
        case _ => None
      }
    }

    object ExtractRel {
      type Build = c.Expr[Rel.Direction] => NamedExpr[Pattern.Rel]

      def unapply(tree: Tree): Option[Build] = tree match {
        case UnApplyClassTag(tpe, arg) if tpe <:< GraphEdgeType => unapply(arg)
        case Ident(termNames.WILDCARD) =>
          Some{ dir => (tree.symbol, None) -> relExpr(None, EmptyTree, None, dir) }
        case Bind(name, body) =>
          Some{ dir => (tree.symbol, Some(name)) -> relExpr(Some(name), body, None, dir) }
        case ua@UnApply(fun, _) if fun.tpe =:= typeOf[Option[Seq[AnyRef]]] =>
          Some{ dir => (tree.symbol, None) -> relExpr(None, ua, None, dir) }
        case UnApply(Apply(Select(sel, TermName("unapply")), _), List(bind, limits, edgeT)) if sel.tpe =:= `syntax *:` =>
          val name = DeepRel.name(bind)
          val length = DeepRel.length(limits)
          val edge = (edgeT: @unchecked) match { case UnApplyClassTag(tpe, arg) if tpe <:< GraphEdgeType => arg }
          Some { dir: c.Expr[Rel.Direction] => ((bind.symbol, name), relExpr(name, edge, Some(length), dir)) }
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
            def range(ior: Tree) = q"_root_.com.abraxas.slothql.cypher.CypherFragment.Pattern.Rel.Range.typed($ior)"

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

    type NamedExpr[A] = ((Symbol, Option[Name]), c.Expr[A])
    type VertexEdgeExpr = Either[NamedExpr[Pattern.Node], NamedExpr[Pattern.Rel]]

    object V1 {
      def unapply(tree: Tree): Option[VertexEdgeExpr] = PartialFunction.condOpt(tree) {
        case ExtractNode(node) => Left(node)
      }
    }

    object V {
      def unapply(tree: Tree): Option[List[VertexEdgeExpr]] = PartialFunction.condOpt(tree) {
        case ExtractNode(node) => Left(node) :: Nil
      }
    }

    object VOrDashEV {
      def unapply(tree: Tree): Option[List[VertexEdgeExpr]] = PartialFunction.condOpt(tree) {
        case Apply(tt, List(ExtractRel(rel), ExtractNode(node))) if tt.tpe.resultType =:= `syntax E-V` =>
          Left(node) :: Right(rel(reify(Rel.Incoming))) :: Nil
        case ExtractNode(node) =>
          Left(node) :: Nil
      }
    }
    object VOrDashVE {
      def unapply(tree: Tree): Option[List[VertexEdgeExpr]] = PartialFunction.condOpt(tree) {
        case Apply(tt: TypeTree, List(ExtractNode(node), ExtractRel(rel))) if tt.tpe.resultType =:= `syntax V-E` =>
          Right(rel(reify(Rel.Outgoing))) :: Left(node) :: Nil
        case ExtractNode(node) =>
          Left(node) :: Nil
      }
    }

    object DashListE {
      def unapply(tree: Tree): Option[(Tree, c.Expr[Rel.Direction] => VertexEdgeExpr)] = PartialFunction.condOpt(tree) {
        case Apply(tt: TypeTree, List(t, ExtractRel(rel))) if tt.tpe.resultType =:= `syntax V-E` =>
          t -> (dir => Right(rel(dir)))
      }
    }

    def simpleRel(dir: c.Expr[Rel.Direction]): VertexEdgeExpr = Right {
      (NoSymbol, None) -> relExpr(None, EmptyTree, None, dir)
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

        def extractPatternRev(tree: Tree): List[VertexEdgeExpr] = tree match {
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
              case _ => failedToParse(ua)
            }
          case V(v) => v
          case _ => failedToParse(tree)
        }


        def toFragment0(patternRev: List[VertexEdgeExpr], acc: c.Expr[Pattern.Pattern0]): c.Expr[Pattern.Pattern0] =
          (patternRev: @unchecked) match {
            case Nil => acc
            case Right((_, relExpr)) :: Left((_, nodeExpr)) :: tail =>
              val newAcc = reify {
                Pattern.Path(nodeExpr.splice, relExpr.splice, acc.splice)
              }
              toFragment0(tail, newAcc)
          }
        def toFragment(patternRev: List[VertexEdgeExpr]): c.Expr[Pattern.Pattern0] = (patternRev: @unchecked) match {
          case Left((_, node)) :: tail => toFragment0(tail, node)
        }

        val p = extractPatternRev(pattern0)
        val pattern = toFragment(p)

        val bindSymbols = p.map(_.left.map(_._1).right.map(_._1).merge).toSet

        val setAliases = bindSymbols.withFilter(_._2.isDefined).map{
          case (symbol, name) =>
            q"_root_.com.abraxas.slothql.cypher.syntax.Match.Internal.setAlias($symbol, ${name.get.decodedName.toString})"
        }.toList


        val (whereDefined, whereClause) = guard0 match {
          case Apply(Select(pkg, TermName("unwrapBooleanExprInIfGuard")), List(cond)) if pkg.tpe =:= `syntax pkg` =>
            true -> cond
          case EmptyTree =>
            false -> EmptyTree
          case _ => c.abort(guard0.pos, "`if` contents cannot be transformed to WHERE clause:\n" + showRaw(guard0))
        }

        lazy val whereVarTree0 = q"var where: ${whereClause.tpe} = null"
        lazy val whereVarDefTree = c.typecheck{ whereVarTree0 }
        lazy val whereIdent      = Ident(whereVarDefTree.symbol)

        val setWhereVar = if (whereDefined) Assign(whereIdent, whereClause) else EmptyTree


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
              case CaseDef(pat, _, b0) => // TODO: transform only outer `CaseDef`; right now the underlying `CaseDef`s are also modified
                val b = b0 match {
                  case Block(stats, expr) => Block(setAliases ::: setWhereVar :: stats, expr)
                  case expr               => Block(setAliases ::: setWhereVar :: Nil,   expr)
                }
                CaseDef(pat, EmptyTree, b)
            }
        }

        val f2 = c.Expr[Graph => CypherFragment.Return[R]](fTransormer.transform(f.tree))

        val patternTupleTree =
          q"_root_.com.abraxas.slothql.cypher.CypherFragment.PatternTuple.typed($pattern)"
        val tree =
          q"""
            ${if (whereDefined) whereVarDefTree else EmptyTree}
            val ret = $f2(_root_.com.abraxas.slothql.cypher.syntax.Match.Internal.graph)
            _root_.com.abraxas.slothql.cypher.CypherFragment.Query.Clause.typed(
              ${
                if (whereDefined)
                  q"""
                    _root_.com.abraxas.slothql.cypher.CypherFragment.Clause.Match.strict(
                      $patternTupleTree,
                      $whereIdent
                    )
                   """
                else
                  q"_root_.com.abraxas.slothql.cypher.CypherFragment.Clause.Match.strict($patternTupleTree)"
              },
              _root_.com.abraxas.slothql.cypher.CypherFragment.Query.Return(ret)
            )
           """

        // TODO: most of the `Expr` are no longer needed

//        c.info(c.enclosingPosition, showCode(res.tree), force = true)
//        c.info(c.enclosingPosition, showRaw(res.tree), force = true)

        c.Expr[Query[R]](tree)
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