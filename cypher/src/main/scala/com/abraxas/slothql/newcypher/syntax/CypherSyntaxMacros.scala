package com.abraxas.slothql.newcypher.syntax

import scala.annotation.tailrec
import scala.reflect.macros.blackbox

import cats.data.NonEmptyList

import com.abraxas.slothql.newcypher.CypherFragment.{ Pattern => P }
import com.abraxas.slothql.newcypher.{ syntax, CypherFragment => CF }

class CypherSyntaxMacros(val c: blackbox.Context) {
  import c.universe._

  def match_[R: WeakTypeTag](query: c.Expr[Node => CF.Query[R]]): c.Expr[CF.Query[R]] = qImpl(query) {
    (guard, pattern) => reify {
      CF.Clause.Match(
        NonEmptyList.one[P](pattern.splice),
        optional = false,
        where = guard.splice
      )
    }
  }

  protected type Guard     = c.Expr[Option[CF.Expr[Boolean]]]
  protected type QPattern  = c.Expr[P]
  protected type QInner[R] = c.Expr[CF.Query.Query0[R]]

  protected def impl[R: WeakTypeTag](query: c.Expr[Node => CF.Query[R]]): (Guard, QPattern, QInner[R]) = query.tree match {
    case Function(List(_), Match(_, cases)) =>
      cases match {
        case List(CaseDef(pattern, guard, body)) =>
          val Pattern(elems) = pattern
          val bindDefs = elems.collect{
            case (Some(nme), Pattern.Tpe.Node, _) =>
              q"val ${TermName(nme)} = _root_.com.abraxas.slothql.newcypher.syntax.CypherSyntaxFromMacro.mkNode($nme)"
            case (Some(nme), Pattern.Tpe.Rel(dir), _) =>
              q"val ${TermName(nme)} = _root_.com.abraxas.slothql.newcypher.syntax.CypherSyntaxFromMacro.mkRel[$dir]($nme)"
          }
          val newBody = transformBody(elems, body)
          val qBody = c.Expr[CF.Query.Query0[R]](q"..$bindDefs; $newBody")
          val guardExpr = c.Expr[CF.Expr[Boolean]](guard)
          val qGuard = if (guard.nonEmpty) reify{ Some(guardExpr.splice) } else reify{ None }
          (qGuard, mkPattern(elems), qBody)
        case _ => c.abort(query.tree.pos, "Query must have a single case clause")
      }
    case other => c.abort(query.tree.pos, s"Unexpected query function: $other")
  }

  protected def qImpl[R: WeakTypeTag](query0: c.Expr[Node => CF.Query[R]])
                                     (mkClause: (Guard, QPattern) => c.Expr[CF.Clause]): c.Expr[CF.Query[R]] = {
    val (guard, pattern, inner) = impl(query0)
    reify {
      CF.Query.Clause(mkClause(guard, pattern).splice, inner.splice)
    }
  }

  protected def mkPattern(elems: List[Pattern.Elem]): c.Expr[P.Pattern0] = {
    val Pattern.Elem.Node(_, head) :: tail = elems.reverse
    @tailrec
    def inner(ps: List[Pattern.Elem], acc: c.Expr[P.Pattern0]): c.Expr[P.Pattern0] =
      ps match {
        case Pattern.Elem.Rel(_, _, r) :: Pattern.Elem.Node(_, n) :: tail =>
          inner(tail, reify{ P.Path(n.splice, r.splice, acc.splice) })
        case Nil => acc
      }
    inner(tail, head)
  }
  protected def transformBody(elems: List[Pattern.Elem], body: Tree): Tree = {
    val newBody = new BodyTransformer(elems.flatMap(_._1)).transform(body)
    c.untypecheck(newBody)
  }

  protected class BodyTransformer(names: Set[String]) extends Transformer {
    def this(it: Iterable[String]) = this(it.toSet)
    override def transform(tree: Tree): Tree = tree match {
      case Ident(TermName(name)) if names contains name => q"${TermName(name)}"
      case _ => super.transform(tree)
    }
  }

  implicit lazy val liftablePatternA: Liftable[P.PatternA] = {
    case P.Node(alias, labels, map) => reify{ P.Node(alias, labels, map) }.tree
    case P.Rel(alias, types, map, length, dir) => reify{ P.Rel(alias, types, map, length, dir) }.tree
  }

  /**
   * Pat := Pat < RNX
   *      | NR > Pat
   * NR  := Pat < RNX
   *      | NR > NR
   *      | Node - Rel
   * RN  := Rel - Node
   * RNX := RN - Rel
   *      | RN
   */
  object Pattern {
    type Elem = (Option[String], Tpe, c.Expr[P])

    object Elem {
      object Node {
        def unapply(elem: Elem): Option[(Option[String], c.Expr[P.Node])] = PartialFunction.condOpt(elem) {
          case (a, Tpe.Node, e) => a -> e.asInstanceOf[c.Expr[P.Node]]
        }
      }
      object Rel {
        def unapply(elem: Elem): Option[(Option[String], Type, c.Expr[P.Rel])] = PartialFunction.condOpt(elem) {
          case (a, Tpe.Rel(dir), e) => (a, dir, e.asInstanceOf[c.Expr[P.Rel]])
        }
      }
    }

    sealed trait Tpe
    object Tpe{
      case object Node extends Tpe
      case class Rel(dir: Type) extends Tpe
    }

    /**
     * Node := Pat < RNX
     *       | NR > Pat
     */
    def unapply(tree: Tree): Option[List[Elem]] = PartialFunction.condOpt(tree) {
      case Node(n)                               => List(n)
      case UnApplyLR(Pattern(xs), Syntax_<, RNX(ys)) => xs ::: ys
      case UnApplyLR(NR(xs),  Syntax_>, Pattern(ys)) => xs ::: ys
    }

    object UnApplyLR {
      def unapply(tree: Tree): Option[(Tree, Symbol, Tree)] = PartialFunction.condOpt(tree) {
        case UnApply(Apply(Select(op, TermName("unapply")), _), List(lhs, rhs)) => (lhs, op.symbol, rhs)
      }
    }

    def stringName(name: Name): Option[String] = name match {
      case termNames.WILDCARD | typeNames.WILDCARD => None
      case _ => Some(name.decodedName.toString)
    }

    /**
     * NR := Pat < RNX
     *     | NR > NR
     *     | Node - Rel
     */
    object NR {
      def unapply(tree: Tree): Option[List[Elem]] = PartialFunction.condOpt(tree) {
        case UnApplyLR(Pattern(xs), Syntax_<, RNX(ys)) => xs ::: ys
        case UnApplyLR(NR(xs),      Syntax_>, NR(ys))  => xs ::: ys
        case UnApplyLR(Node(n),     Syntax_-, Rel(r))  => List(n, r)
      }
    }

    /**
     * RN := Rel - Node
     */
    object RN {
      def unapply(tree: Tree): Option[List[Elem]] = PartialFunction.condOpt(tree) {
        case UnApplyLR(Rel(r), Syntax_-, Node(n)) => List(r, n)
      }
    }

    /**
     * RNX  := RN - Rel
     *       | RN
     */
    object RNX {
      def unapply(tree: Tree): Option[List[Elem]] = PartialFunction.condOpt(tree) {
        case UnApplyLR(RN(xs), Syntax_-, Rel(r)) => xs ::: List(r)
        case RN(xs) => xs
      }
    }

    object Node {
      def unapply(tree: Tree): Option[Elem] = PartialFunction.condOpt(tree) {
        case pq"$name@$obj(..$args)" if obj.symbol == SyntaxNodeObj =>
          val nme  = stringName(name)
          val expr = reify{ P.Node(stringExpr(nme).splice, Nil, Map()) }
          (nme, Tpe.Node, expr)
        case pq"$name@$other" if tree.tpe <:< SyntaxNodeType =>
          val nme  = stringName(name)
          val expr = reify{ P.Node(stringExpr(nme).splice, Nil, Map()) }
          (nme, Tpe.Node, expr)
      }
    }
    object Rel {
      def unapply(tree: Tree): Option[Elem] = PartialFunction.condOpt(tree) {
        case pq"$name@$obj(..$args)" if obj.symbol == SyntaxRelObj =>
          val nme  = stringName(name)
          val dir  = dirExpr(obj)
          val expr = reify{ P.Rel(stringExpr(nme).splice, Nil, Map(), None, dir.splice) }
          (nme, Tpe.Rel(dir.staticType), expr)
        case pq"$name@$_" if tree.tpe <:< SyntaxRelType =>
          val nme  = stringName(name)
          val dir  = dirExpr(tree)
          val expr = reify{ P.Rel(stringExpr(nme).splice, Nil, Map(), None, dir.splice) }
          (nme, Tpe.Rel(dir.staticType), expr)
      }
    }

    private def dirExpr(tree: Tree) = (tree.tpe.decl(TypeName("Dir")).asType.toType: @unchecked) match {
      case tpe if tpe <:< DirectionInType  => reify{ P.Rel.Incoming }
      case tpe if tpe <:< DirectionOutType => reify{ P.Rel.Outgoing }
    }

    private def stringExpr(str: Option[String]): c.Expr[Option[String]] = c.Expr(q"$str")

    lazy val Syntax_<  = rootMirror.staticModule("com.abraxas.slothql.newcypher.syntax.$less")
    lazy val Syntax_>  = rootMirror.staticModule("com.abraxas.slothql.newcypher.syntax.$greater")
    lazy val Syntax_-  = rootMirror.staticModule("com.abraxas.slothql.newcypher.syntax.$minus")
    lazy val Syntax_:= = rootMirror.staticModule("com.abraxas.slothql.newcypher.syntax.$colons$equal")
    lazy val Syntax_** = rootMirror.staticModule("com.abraxas.slothql.newcypher.syntax.$times$times")

    lazy val SyntaxNodeObj  = symbolOf[syntax.Node.type]
    lazy val SyntaxNodeType = typeOf[syntax.Node].typeConstructor
    lazy val SyntaxRelObj   = symbolOf[syntax.Rel.type]
    lazy val SyntaxRelType  = typeOf[syntax.Rel].typeConstructor

    lazy val DirectionInType  = typeOf[syntax.Rel.Incoming]
    lazy val DirectionOutType = typeOf[syntax.Rel.Outgoing]
  }
}