package com.arkondata.slothql.newcypher.syntax

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.reflect.macros.blackbox

import cats.data.NonEmptyList

import com.arkondata.slothql.newcypher.CypherFragment.{ Pattern => P }
import com.arkondata.slothql.newcypher.CypherStatement.Alias
import com.arkondata.slothql.newcypher.{ syntax, CypherFragment => CF }

class CypherSyntaxPatternMacros(val c: blackbox.Context) {
  import c.universe._

  def match_[R: WeakTypeTag](query: c.Expr[Node => CF.Query.Query0[R]]): c.Expr[CF.Query.Query0[R]] = qImpl(query) {
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
  protected type QDefs     = List[Tree]
  protected type QInner[R] = c.Expr[CF.Query.Query0[R]]

  protected def impl[R: WeakTypeTag](query: c.Expr[Node => CF.Query[R]]): (Guard, QPattern, QDefs, QInner[R]) = query.tree match {
    case Function(List(_), Match(_, cases)) =>
      cases match {
        case List(CaseDef(pattern, guard, body)) =>
          val Pattern(elems) = pattern
          val bindDefs = elems.collect{
            case (Some(nme), Pattern.Tpe.Node, _) =>
              q"val ${TermName(nme)} = _root_.com.arkondata.slothql.newcypher.syntax.CypherSyntaxFromMacro.mkNode($nme)"
            case (Some(nme), Pattern.Tpe.Rel(dir), _) =>
              q"val ${TermName(nme)} = _root_.com.arkondata.slothql.newcypher.syntax.CypherSyntaxFromMacro.mkRel[$dir]($nme)"
          }
          val newBody = transformBody(elems, body)
          val guardExpr = c.Expr[CF.Expr[Boolean]](guard)
          val qGuard = if (guard.nonEmpty) reify{ Some(guardExpr.splice) } else reify{ None }
          (qGuard, mkPattern(elems), bindDefs, c.Expr[CF.Query.Query0[R]](newBody))
        case _ => c.abort(query.tree.pos, "Query must have a single case clause")
      }
    case other => c.abort(query.tree.pos, s"Unexpected query function: $other")
  }

  protected def qImpl[R: WeakTypeTag](query0: c.Expr[Node => CF.Query.Query0[R]])
                                     (mkClause: (Guard, QPattern) => c.Expr[CF.Clause]): c.Expr[CF.Query.Query0[R]] = {
    val (guard, pattern, defs, inner) = impl(query0)
    val clause = reify {
      CF.Query.Clause(mkClause(guard, pattern).splice, inner.splice)
    }
    c.Expr[CF.Query.Query0[R]](q"..$defs; $clause")
  }

  protected def mkPattern(elems: List[Pattern.Elem]): c.Expr[P.Pattern0] = {
    val Pattern.Elem.Node(nme0, head) :: tail = elems.reverse
    @tailrec
    def inner(ps: List[Pattern.Elem], acc: c.Expr[P.Pattern0]): c.Expr[P.Pattern0] =
      ps match {
        case Pattern.Elem.Rel(nmeR, _, r) :: Pattern.Elem.Node(nmeN, n) :: tail =>
          val aliasN0 = nmeN.map{ nme => c.Expr[Alias](q"${TermName(nme)}") }
          val node = aliasN0.map{ a => n(reify{ Some(a.splice) }) }
                            .getOrElse(n(reify{ None }))
          val aliasR0 = nmeR.map{ nme => c.Expr[Alias](q"${TermName(nme)}") }
          val rel = aliasR0.map{ a => r(reify{ Some(a.splice) }) }
                           .getOrElse(r(reify{ None }))
          inner(tail, reify{ P.Path(node.splice, rel.splice, acc.splice) })
        case Nil => acc
      }
    val alias0 = nme0.map{ nme => c.Expr[Alias](q"${TermName(nme)}") }
    val first = alias0.map{ a => head(reify{ Some(a.splice) }) }.getOrElse(head(reify{ None }))
    inner(tail, first)
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
    type Elem = (Option[String], Tpe, c.Expr[Option[Alias]] => c.Expr[P])

    object Elem {
      object Node {
        def unapply(elem: Elem): Option[(Option[String], c.Expr[Option[Alias]] => c.Expr[P.Node])] = PartialFunction.condOpt(elem) {
          case (a, Tpe.Node, e) => a -> e.asInstanceOf[c.Expr[Option[Alias]] => c.Expr[P.Node]]
        }
      }
      object Rel {
        def unapply(elem: Elem): Option[(Option[String], Type, c.Expr[Option[Alias]] => c.Expr[P.Rel])] = PartialFunction.condOpt(elem) {
          case (a, Tpe.Rel(dir), e) => (a, dir, e.asInstanceOf[c.Expr[Option[Alias]] => c.Expr[P.Rel]])
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
        case pq"$obj0(${UnApply(m, args)})" if obj0.tpe <:< typeOf[ClassTag[syntax.Node]]
                                            && m.symbol == SyntaxNodeUnapplySeqSymbol =>
          (None, Tpe.Node, nodeExpr(args))
        case pq"$name@$obj0(${UnApply(m, args)})" if obj0.tpe <:< typeOf[ClassTag[syntax.Node]]
                                                  && m.symbol == SyntaxNodeUnapplySeqSymbol =>
          (stringName(name), Tpe.Node, nodeExpr(args))
        case pq"$name@$_" if tree.tpe <:< SyntaxNodeType =>
          (stringName(name), Tpe.Node, nodeExpr(Nil))
        case pq"${Ident(nme.WILDCARD)}" if tree.tpe <:< SyntaxNodeType =>
          (None, Tpe.Node, nodeExpr(Nil))
        case pq"${Ident(name)}" if tree.tpe <:< SyntaxNodeType =>
          val node = c.Expr[syntax.Node](q"${name.toTermName}")
          def expr(alias: c.Expr[Option[Alias]]) = reify{ P.Node(Some(node.splice), Nil, Map()) }
          (None, Tpe.Node, expr)
      }

      private def nodeExpr(args: List[Tree])(alias: c.Expr[Option[Alias]]) = reify {
        P.Node(alias.splice, collectLabels(args).splice, collectProps(args).splice)
      }
    }
    object Rel {
      def unapply(tree: Tree): Option[Elem] = PartialFunction.condOpt(tree) {
        case pq"$obj0(${UnApply(m, args)})" if obj0.tpe <:< typeOf[ClassTag[syntax.Rel]]
                                            && m.symbol == SyntaxRelUnapplySeqSymbol =>
          val (expr, relTpe) = relExprAndDirType(tree, args)
          (None, relTpe, expr)
        case pq"$name@$obj0(${UnApply(m, args)})" if obj0.tpe <:< typeOf[ClassTag[syntax.Rel]]
                                                  && m.symbol == SyntaxRelUnapplySeqSymbol =>
          val (expr, relTpe) = relExprAndDirType(tree, args)
          (stringName(name), relTpe, expr)
        case pq"$name@$_" if tree.tpe <:< SyntaxRelType =>
          val (expr, relTpe) = relExprAndDirType(tree, Nil)
          (stringName(name), relTpe, expr)
        case pq"${Ident(nme.WILDCARD)}" if tree.tpe <:< SyntaxRelType =>
          val (expr, relTpe) = relExprAndDirType(tree, Nil)
          (None, relTpe, expr)
      }

      private def relExprAndDirType(tree: Tree, args: List[Tree]) = {
        val dir = dirExpr(tree)
        def expr(alias: c.Expr[Option[Alias]]) = reify {
          P.Rel(alias.splice, collectLabels(args).splice, collectProps(args).splice, None, dir.splice)
        }
        expr _ -> Tpe.Rel(dir.staticType)
      }
    }

    private def dirExpr(tree: Tree) = (tree.tpe.decl(TypeName("Dir")).asType.toType: @unchecked) match {
      case tpe if tpe <:< DirectionInType  => reify{ P.Rel.Incoming }
      case tpe if tpe <:< DirectionOutType => reify{ P.Rel.Outgoing }
    }

    private def collectLabels(args: Seq[Tree]): c.Expr[List[String]] = {
      val labels0 = args.collect {
        case tree if tree.tpe <:< typeOf[String] => q"_root_.scala.List($tree)"
        case tree if tree.tpe <:< typeOf[Iterable[String]] => tree
      }
      if (labels0.isEmpty) reify { Nil }
      else c.Expr[List[String]](q"${labels0.reduce((i1, i2) => q"$i1 ++ $i2")}.toList")
    }

    private def collectProps(args: Seq[Tree]): c.Expr[Map[String, CF.Expr[_]]] = {
      val props0 = args.collect {
        case UnApply(m,  List(lhs, rhs)) if m.symbol == Syntax_UnapplySeqSymbol_:= =>
          val key = lhs match {
            case Literal(Constant(s: String)) => s
            case _ => c.abort(lhs.pos, "Property key must be a literal string")
          }
          val value = if (rhs.tpe <:< CypherExprType) rhs
          else q"_root_.com.arkondata.slothql.newcypher.syntax.lit($rhs)"
          key -> value
      }
      c.Expr[Map[String, CF.Expr[_]]](q"_root_.scala.Predef.Map(..$props0)")
    }

    lazy val CypherExprType = typeOf[CF.Expr[_]].typeConstructor

    lazy val Syntax_<  = rootMirror.staticModule("com.arkondata.slothql.newcypher.syntax.$less")
    lazy val Syntax_>  = rootMirror.staticModule("com.arkondata.slothql.newcypher.syntax.$greater")
    lazy val Syntax_-  = rootMirror.staticModule("com.arkondata.slothql.newcypher.syntax.$minus")
    lazy val Syntax_:= = rootMirror.staticModule("com.arkondata.slothql.newcypher.syntax.$colon$eq")
    lazy val Syntax_** = rootMirror.staticModule("com.arkondata.slothql.newcypher.syntax.$times$times")

    lazy val SyntaxNodeUnapplySeqSymbol = typeOf[syntax.Node.type].decl(TermName("unapplySeq")).asMethod
    lazy val SyntaxRelUnapplySeqSymbol  = typeOf[syntax.Rel.type] .decl(TermName("unapplySeq")).asMethod
    lazy val Syntax_UnapplySeqSymbol_:= = Syntax_:=.typeSignature.decl(TermName("unapply")).asMethod
    lazy val SyntaxNodeType = typeOf[syntax.Node].typeConstructor
    lazy val SyntaxRelObj   = symbolOf[syntax.Rel.type]
    lazy val SyntaxRelType  = typeOf[syntax.Rel].typeConstructor

    lazy val DirectionInType  = typeOf[syntax.Rel.Incoming]
    lazy val DirectionOutType = typeOf[syntax.Rel.Outgoing]
  }
}