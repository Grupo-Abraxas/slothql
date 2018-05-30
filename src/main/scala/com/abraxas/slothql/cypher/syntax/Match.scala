package com.abraxas.slothql.cypher.syntax

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

import cats.data.NonEmptyList
import shapeless.{ :: => #:, _ }

import com.abraxas.slothql.cypher.CypherFragment
import com.abraxas.slothql.cypher.CypherFragment.Pattern.Rel
import com.abraxas.slothql.cypher.CypherFragment._

object Match {
  def apply[R](f: Graph => Return[R]): Query[R] = macro impl[R]


  private type VE = Vertex - Edge
  private type EV = Edge - Vertex

  // The definitions that should be package-private but cannot be
  object Internal {
    @inline def setAlias(e: GraphElem, alias: String): Unit = e._alias = alias
    @inline def graph: Graph = Graph
  }

  def impl[R: c.WeakTypeTag](c: whitebox.Context)(f: c.Expr[Graph => Return[R]]): c.Expr[Query[R]] = {
    import c.universe._

    val `syntax <` = typeOf[<.type]
    val `syntax >` = typeOf[>.type]
    val `syntax V-E` = typeOf[VE]
    val `syntax E-V` = typeOf[EV]

    def extractBindParams(body: Tree): (List[String], List[Tree], Option[Tree]) = body match {
      case UnApply(Apply(Select(sel, TermName("unapplySeq")), _), args)
          if sel.tpe =:= typeOf[Vertex.type] || sel.tpe =:= typeOf[Edge.type] =>
        val labels = args.collect{ case Literal(Constant(label: String)) => label }
        val values = args.collect{
          case UnApply(Apply(Select(sel2, TermName("unapply")), _), args2) if sel2.tpe =:= typeOf[:=.type] =>
            (args2: @unchecked) match {
              case List(Literal(Constant(k: String)), v) =>
                q"($k, _root_.com.abraxas.slothql.cypher.CypherFragment.Expr.Lit($v))"
            }
        }
        val length = None // TODO
        (labels, values, length)
      case _ => (Nil, Nil, None)
    }


    def aliasTree(name: Name): Tree = name match {
      case termNames.WILDCARD => q"_root_.scala.None"
      case _ => q"_root_.scala.Some(${name.decodedName.toString})"
    }

    def knownNodeExpr(name: Name, body: Tree): c.Expr[Known[Pattern.Node]] = {
      val (labelsTrees, valuesTrees, lengthTree) = extractBindParams(body)
      if (lengthTree.isDefined) c.abort(body.pos, s"Vertex cannot have length!")

      val tree = q"""
        _root_.com.abraxas.slothql.cypher.CypherFragment.Known(
          _root_.com.abraxas.slothql.cypher.CypherFragment.Pattern.Node(
            alias = ${aliasTree(name)},
            labels = _root_.scala.List(..$labelsTrees),
            map = _root_.scala.Predef.Map(..$valuesTrees)
          )
        )
      """
      c.Expr[Known[Pattern.Node]](tree)
    }

    def knownRelExpr(name: Option[Name], body: Tree, dir: c.Expr[Rel.Direction]): c.Expr[Known[Pattern.Rel]] = {
      val (labelsTrees, valuesTrees, lengthTree) = extractBindParams(body)
      val tree = q"""
        _root_.com.abraxas.slothql.cypher.CypherFragment.Known(
          _root_.com.abraxas.slothql.cypher.CypherFragment.Pattern.Rel(
            alias = ${name.map(aliasTree(_)).getOrElse(q"_root_.scala.None")},
            types = _root_.scala.List(..$labelsTrees),
            map = _root_.scala.Predef.Map(..$valuesTrees),
            length = ${lengthTree.map(len => q"_root_.scala.Some($len)").getOrElse(q"_root_.scala.None")},
            dir = ${dir.tree}
          )
        )
      """
      c.Expr[Known[Pattern.Rel]](tree)
    }

    object ExtractNode {
      def unapply(tree: Tree): Option[NamedKnownExpr[Pattern.Node]] = PartialFunction.condOpt(tree) {
        case Ident(termNames.WILDCARD) =>
          (tree.symbol, None) -> reify { Known{ Pattern.Node(alias = None, labels = Nil, map = Map()) } }
        case Bind(name, body) =>
          (tree.symbol, Some(name)) -> knownNodeExpr(name, body)
        case ua@UnApply(fun, _) if fun.tpe =:= typeOf[Option[Seq[AnyRef]]] =>
          (tree.symbol, None) -> knownNodeExpr(termNames.WILDCARD, ua)
      }
    }

    object ExtractRel {
      type Build = c.Expr[Rel.Direction] => NamedKnownExpr[Pattern.Rel]

      def unapply(tree: Tree): Option[Build] = PartialFunction.condOpt(tree) {
        case Ident(termNames.WILDCARD) =>
          dir => (tree.symbol, None) -> knownRelExpr(None, EmptyTree, dir)
        case Bind(name, body) =>
          dir => (tree.symbol, Some(name)) -> knownRelExpr(Some(name), body, dir)
        case ua@UnApply(fun, _) if fun.tpe =:= typeOf[Option[Seq[AnyRef]]] =>
          dir => (tree.symbol, None) -> knownRelExpr(None, ua, dir)
      }
    }

    type NamedKnownExpr[A] = ((Symbol, Option[Name]), c.Expr[Known[A]])
    type KnownVertexEdgeExpr = Either[NamedKnownExpr[Pattern.Node], NamedKnownExpr[Pattern.Rel]]

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
              case List(V(v),                VOrDashEV(revHead))  if arrow.tpe =:= `syntax <` => revHead ::: v
              case List(l,                   VOrDashEV(revHead))  if arrow.tpe =:= `syntax <` => revHead ::: extractPatternRev(l)
              case List(VOrDashVE(revHead1), VOrDashVE(revHead2)) if arrow.tpe =:= `syntax >` => revHead2 ::: revHead1
              case List(l,                   VOrDashVE(revHead))  if arrow.tpe =:= `syntax >` => revHead ::: extractPatternRev(l)
              case _ => failedToParse(ua)
            }
          case _ => failedToParse(tree)
        }


        def toFragment0(patternRev: List[KnownVertexEdgeExpr], acc: c.Expr[Known[Pattern.Pattern0]]): c.Expr[Known[Pattern.Pattern0]] = patternRev match {
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

        object fTransormer extends Transformer {
          override def transform(tree: c.universe.Tree): c.universe.Tree = tree match {
            case b@Bind(name, _) =>
              val newBind = Bind(name, Ident(termNames.WILDCARD))
              c.internal.setSymbol(newBind, b.symbol)
            case UnApply(fun, _) if fun.tpe =:= typeOf[Option[Seq[AnyRef]]] =>
              Bind(termNames.WILDCARD, Ident(termNames.WILDCARD))
            case other =>
              super.transform(other)
          }

          override def transformCaseDefs(trees: List[c.universe.CaseDef]): List[c.universe.CaseDef] =
            super.transformCaseDefs(trees).map {
              case CaseDef(pat, guard, b0) =>
                val b = b0 match {
                  case Block(stats, expr) => Block(setAliases ::: stats, expr)
                  case expr               => Block(setAliases, expr)
                }
                CaseDef(pat, guard, b)
            }
        }

        val f2 = c.Expr[Graph => CypherFragment.Return[R]](fTransormer.transform(f.tree))

        val res = reify {
          Query.Clause(
            Clause.Match(
              NonEmptyList(pattern.splice, Nil),
              optional = false,
              where = None
            ),
            Query.Return(f2.splice.apply(Internal.graph))
          )
        }

//        c.info(NoPosition, showCode(res.tree), force = true)
//        c.info(NoPosition, showRaw(res.tree), force = true)

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