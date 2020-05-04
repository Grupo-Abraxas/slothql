package com.arkondata.slothql.cypher.syntax

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.reflect.macros.blackbox

import cats.data.{ Ior, NonEmptyList }

import com.arkondata.slothql.cypher.CypherFragment.{ Pattern => P }
import com.arkondata.slothql.cypher.CypherStatement.Alias
import com.arkondata.slothql.cypher.{ syntax, CypherFragment => CF }

class CypherSyntaxPatternMacros(val c: blackbox.Context) {
  import c.universe._

  def match_[R: WeakTypeTag](query: c.Expr[Node => CF.Query.Query0[R]]): c.Expr[CF.Query.Query0[R]] = matchImpl(query, reify(false))
  def optional[R: WeakTypeTag](query: c.Expr[Node => CF.Query.Query0[R]]): c.Expr[CF.Query.Query0[R]] = matchImpl(query, reify(true))
  def maybe[R: WeakTypeTag](opt: c.Expr[Boolean])(query: c.Expr[Node => CF.Query.Query0[R]]): c.Expr[CF.Query.Query0[R]] = matchImpl(query, opt)

  protected def matchImpl[R: WeakTypeTag](query: c.Expr[Node => CF.Query.Query0[R]], optional: c.Expr[Boolean]): c.Expr[CF.Query.Query0[R]] =
    qImpl(query) {
      (guard, pattern) => reify {
        CF.Clause.Match(
          NonEmptyList.one[P](pattern.splice),
          optional = optional.splice,
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
        case List(CaseDef(pattern, guard0, body)) =>
          val PatternPlus(letAliasOpt, elems0) = pattern
          val (rebind0, bindDefsNodeRel) = elems0.collect{
            case (Some(nme), Pattern.Tpe.Node, _) =>
              val name = c.freshName(nme)
              val tree = q"val ${TermName(name)} = _root_.com.arkondata.slothql.cypher.syntax.CypherSyntaxFromMacro.mkNode($nme)"
              (nme, name) -> tree
            case (Some(nme), Pattern.Tpe.Rel(dir), _) =>
              val name = c.freshName(nme)
              val tree = q"val ${TermName(name)} = _root_.com.arkondata.slothql.cypher.syntax.CypherSyntaxFromMacro.mkRel[$dir]($nme)"
              (nme, name) -> tree
          }.unzip
          val bindDefOptLet = letAliasOpt.map{ nme =>
            val name = c.freshName(nme)
            val tree = q"val ${TermName(name)} = _root_.com.arkondata.slothql.cypher.syntax.CypherSyntaxFromMacro.mkPath($nme)"
            (nme, name) -> tree
          }
          val letAliasExprOpt = bindDefOptLet.map{ case ((_, name), _) => c.Expr[Alias](q"${TermName(name)}") }

          val rebind = (rebind0 ++ bindDefOptLet.map(_._1).toSeq).toMap
          val newBody = transformBody(rebind, body)

          val guardTree = guard0 match {
            case q"$m($expr)" if m.symbol == SyntaxIfGuardUnwrap    => q"_root_.scala.Some($expr)"
            case q"$m($expr)" if m.symbol == SyntaxIfGuardUnwrapOpt => expr
            case _                                                  => q"_root_.scala.None"
          }
          val guard = c.Expr[Option[CF.Expr[Boolean]]](transformBody(rebind, guardTree))
          val elems = elems0.map{ case (nme, tpe, expr) => (nme.map(rebind), tpe, expr) }
          val patternExpr = maybeLetPattern(letAliasExprOpt, elems)
          val bindDefs = bindDefsNodeRel ++ bindDefOptLet.map(_._2).toSeq
          (guard, patternExpr, bindDefs, c.Expr[CF.Query.Query0[R]](newBody))
        case _ => c.abort(query.tree.pos, "Query must have a single case clause")
      }
    case other => c.abort(query.tree.pos, s"Unexpected query function: $other")
  }

  private lazy val SyntaxPackageTypeSignature = c.mirror.staticPackage("com.arkondata.slothql.cypher.syntax").typeSignature
  private lazy val SyntaxIfGuardUnwrap = SyntaxPackageTypeSignature.decl(TermName("booleanCypherExprToBooleanForIfGuard"))
  private lazy val SyntaxIfGuardUnwrapOpt = SyntaxPackageTypeSignature.decl(TermName("optionalBooleanCypherExprToBooleanForIfGuard"))

  protected def qImpl[R: WeakTypeTag](query0: c.Expr[Node => CF.Query.Query0[R]])
                                     (mkClause: (Guard, QPattern) => c.Expr[CF.Clause]): c.Expr[CF.Query.Query0[R]] = {
    val (guard, pattern, defs, inner) = impl(query0)
    val clause = reify {
      CF.Query.Clause(mkClause(guard, pattern).splice, inner.splice)
    }
    c.Expr[CF.Query.Query0[R]](q"..$defs; $clause")
  }

  protected def maybeLetPattern(letOpt: Option[c.Expr[Alias]], elems: List[Pattern.Elem]): c.Expr[P] =
    letOpt.map(let => reify{ P.Let(let.splice, mkPattern(elems).splice) })
          .getOrElse(mkPattern(elems))

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

  protected def transformBody(names: Map[String, String], body: Tree): Tree = {
    val newBody = new BodyTransformer(names).transform(body)
    c.untypecheck(newBody)
  }

  protected class BodyTransformer(names: Map[String, String]) extends Transformer {
    override def transform(tree: Tree): Tree = tree match {
      case Ident(TermName(name)) if names contains name => q"${TermName(names(name))}"
      case _ => super.transform(tree)
    }
  }

  implicit lazy val liftablePatternA: Liftable[P.PatternA] = {
    case P.Node(alias, labels, map) => reify{ P.Node(alias, labels, map) }.tree
    case P.Rel(alias, types, map, length, dir) => reify{ P.Rel(alias, types, map, length, dir) }.tree
  }

  object PatternPlus {
    type LetAlias = String

    def unapply(tree: Tree): Option[(Option[LetAlias], List[Pattern.Elem])] = (tree: @unchecked) match {
      case UnApply(m, List(lhs, rhs)) if m.symbol == SyntaxUnapply_::= =>
        val alias = lhs match {
          case pq"$name@$_" => Pattern.stringName(name)
          case _ => None
        }
        if (alias.isEmpty) c.abort(tree.pos, "Left hand side of `::=` match must be a binding.")
        val Pattern(pattern) = rhs
        Some(alias -> pattern)
      case Pattern(pattern) => Some(None -> pattern)
    }

    lazy val SyntaxUnapply_::= = rootMirror.staticModule("com.arkondata.slothql.cypher.syntax.$colon$colon$eq")
                                           .typeSignature.decl(TermName("unapply"))
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
      case Node(n@(_, _, _))                         => List(n)
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
        case UnApplyLR(Pattern(xs),       Syntax_<, RNX(ys))          => xs ::: ys
        case UnApplyLR(NR(xs),            Syntax_>, NR(ys))           => xs ::: ys
        case UnApplyLR(Node(n@(_, _, _)), Syntax_-, Rel(r@(_, _, _))) => List(n, r)
      }
    }

    /**
     * RN := Rel - Node
     */
    object RN {
      def unapply(tree: Tree): Option[List[Elem]] = PartialFunction.condOpt(tree) {
        case UnApplyLR(Rel(r@(_, _, _)), Syntax_-, Node(n@(_, _, _))) => List(r, n)
      }
    }

    /**
     * RNX  := RN - Rel
     *       | RN
     */
    object RNX {
      def unapply(tree: Tree): Option[List[Elem]] = PartialFunction.condOpt(tree) {
        case UnApplyLR(RN(xs), Syntax_-, Rel(r@(_, _, _))) => xs ::: List(r)
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
        case pq"${Ident(termNames.WILDCARD)}" if tree.tpe <:< SyntaxNodeType =>
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
        case pq"${Ident(termNames.WILDCARD)}" if tree.tpe <:< SyntaxRelType =>
          val (expr, relTpe) = relExprAndDirType(tree, Nil)
          (None, relTpe, expr)
      }

      private def relExprAndDirType(tree: Tree, args: List[Tree]) = {
        val dir = dirExpr(tree)
        val varLength0 = args.collect {
          case t if t.symbol == Syntax_** =>
            reify { P.Rel.All }
          case pq"$obj($lhs, $rhs)" if obj.symbol == Syntax_** =>
            (lhs, rhs) match {
              case (IW(),         IW())         => reify { P.Rel.All }
              case (IntExpr(min), IW())         => reify { P.Rel.Range(Ior.left(min.splice)) }
              case (IW(),         IntExpr(max)) => reify { P.Rel.Range(Ior.right(max.splice)) }
              case (IntExpr(min), IntExpr(max)) => reify { P.Rel.Range(Ior.both(min.splice, max.splice)) }
            }
        }
        if (varLength0.length > 1) c.abort(tree.pos, "Multiple ** used at same relationship")
        val varLength = varLength0.headOption.map(len => reify { Some(len.splice) }).getOrElse(reify{ None })
        def expr(alias: c.Expr[Option[Alias]]) = reify {
          P.Rel(alias.splice, collectLabels(args).splice, collectProps(args).splice, varLength.splice, dir.splice)
        }
        expr _ -> Tpe.Rel(dir.staticType)
      }

      private object IW {
        def unapply(tree: Tree): Boolean = PartialFunction.cond(tree) { case Ident(termNames.WILDCARD) => true }
      }

      private object IntExpr {
        def unapply(tree: Tree): Option[c.Expr[Int]] = if (tree.tpe <:< typeOf[Int]) Some(c.Expr[Int](tree)) else None
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
                      else q"_root_.com.arkondata.slothql.cypher.syntax.lit($rhs)"
          key -> value
      }
      c.Expr[Map[String, CF.Expr[_]]](q"_root_.scala.Predef.Map(..$props0)")
    }

    lazy val CypherExprType = typeOf[CF.Expr[_]]

    lazy val Syntax_<  = rootMirror.staticModule("com.arkondata.slothql.cypher.syntax.$less")
    lazy val Syntax_>  = rootMirror.staticModule("com.arkondata.slothql.cypher.syntax.$greater")
    lazy val Syntax_-  = rootMirror.staticModule("com.arkondata.slothql.cypher.syntax.$minus")
    lazy val Syntax_:= = rootMirror.staticModule("com.arkondata.slothql.cypher.syntax.$colon$eq")
    lazy val Syntax_** = rootMirror.staticModule("com.arkondata.slothql.cypher.syntax.$times$times")

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