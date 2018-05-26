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

  def impl[R: c.WeakTypeTag](c: whitebox.Context)(f: c.Expr[Graph => Return[R]]): c.Expr[Query[R]] = {
    import c.universe._

    val `syntax <` = typeOf[<.type]
    val `syntax >` = typeOf[>.type]
    val `syntax V-E` = typeOf[VE]
    val `syntax E-V` = typeOf[EV]

    sealed trait Pattern
    case class Node(tree: Tree) extends Pattern
    case class Edge(dir: c.Expr[Rel.Direction], tree: Option[Tree]) extends Pattern

    (f.tree: @unchecked) match {
      case Function(
            List(ValDef(_, arg0Name1, _, EmptyTree)),
            c.universe.Match(Ident(arg0Name2), cases)
          ) if arg0Name1 == arg0Name2 =>

        val CaseDef(pattern0, guard0, body0) = cases match {
          case List(caseDef) => caseDef
          case _ => c.abort(c.enclosingPosition, "Only one case is permitted in `Match`.")
        }

        object VOrDashEV {
          def unapply(tree: Tree): Option[List[Pattern]] = PartialFunction.condOpt(tree) {
            case Apply(tt, List(l, r)) if tt.tpe.resultType =:= `syntax E-V` =>
              Node(r) :: Edge(reify(Rel.Incoming), Some(l)) :: Nil
            case _ if tree.tpe <:< typeOf[Vertex] =>
              Node(tree) :: Nil
          }
        }
        object VOrDashVE {
          def unapply(tree: Tree): Option[List[Pattern]] = PartialFunction.condOpt(tree) {
            case Apply(tt: TypeTree, List(l, r)) if tt.tpe.resultType =:= `syntax V-E` =>
              Edge(reify(Rel.Outgoing), Some(r)) :: Node(l) :: Nil
            case _ if tree.tpe <:< typeOf[Vertex] =>
              Node(tree) :: Nil
          }
        }

        def extractPatternRev(tree: Tree): List[Pattern] = tree match {
          case UnApply(Apply(Select(arrow, TermName("unapply")), _), args) =>
            (arrow.tpe: @unchecked) match {
              case t if t =:= `syntax <` =>
                (args: @unchecked) match {
                  case List(l, VOrDashEV(revHead)) => revHead ::: extractPatternRev(l)
                }
              case t if t =:= `syntax >` =>
                (args: @unchecked) match {
                  case List(VOrDashVE(revHead1), VOrDashVE(revHead2)) => revHead2 ::: revHead1
                  case List(l,                   VOrDashVE(revHead))  => revHead ::: extractPatternRev(l)
                }
            }
        }

        def extractNode(tree: Tree): c.Expr[Known[Pattern.Node]] = tree match {
          case Bind(termNames.EMPTY, _) => reify {
            Pattern.Node(alias = None, labels = Nil, map = Map()) // TODO: alias
          }
          case Bind(name, _) => reify {
            val nme = c.Expr[String](Literal(Constant(name.decodedName.toString))).splice
            Pattern.Node(alias = Some(nme), labels = Nil, map = Map()) // TODO: alias
          }
        }

        def extractRel(dir: c.Expr[Rel.Direction], tree: Option[Tree]): c.Expr[Known[Pattern.Rel]] = tree match {
          case Some(Bind(name, body)) => reify {
            val nme = c.Expr[String](Literal(Constant(name.decodedName.toString))).splice
            Rel(alias = Some(nme), types = Nil, map = Map(), length = None, dir = dir.splice)
          }
          case None => reify {
            Rel(alias = None, types = Nil, map = Map(), length = None, dir = dir.splice)
          }
        }

        def toFragment0(patternRev: List[Pattern], acc: c.Expr[Known[Pattern.Pattern0]]): c.Expr[Known[Pattern.Pattern0]] = patternRev match {
          case Nil => acc
          case Edge(dir, edge) :: Node(node) :: tail =>
            val newAcc = reify {
              Known(
                Pattern.Path(
                  extractNode(node).splice,
                  extractRel(dir, edge).splice,
                  acc.splice
                )
              )
            }
            toFragment0(tail, newAcc)
        }
        def toFragment(patternRev: List[Pattern]): c.Expr[Known[Pattern.Pattern0]] = (patternRev: @unchecked) match {
          case Node(node) :: tail => toFragment0(tail, extractNode(node))
        }

        val p = extractPatternRev(pattern0)
        val pattern = toFragment(p)

        val bindNames = p.collect {
          case Node(Bind(name, _)) => name
          case Edge(_, Some(Bind(name, _))) => name
        }.toSet

        object fTransormer extends Transformer {
          override def transform(tree0: c.universe.Tree): c.universe.Tree = {
            val tree = tree0 match {
              case i@Ident(name) if i.tpe <:< typeOf[GraphElem] && bindNames.contains(name) =>
                c.typecheck(q"""$i.setAlias(${name.decodedName.toString})""")
              case other =>
                super.transform(other)
            }
            tree
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
            Query.Return(f2.splice.apply(Graph))
          )
        }

        c.info(NoPosition, showCode(res.tree), force = true)
        c.info(NoPosition, showRaw(res.tree), force = true)

        res
    }
  }
}


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