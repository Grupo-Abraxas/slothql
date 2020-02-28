package com.abraxas.slothql.newcypher.syntax

import scala.reflect.macros.blackbox

import com.abraxas.slothql.cypher.CypherFragment.{ Pattern => P }
import com.abraxas.slothql.newcypher.syntax

class CypherSyntaxMacros(val c: blackbox.Context) {
  import c.universe._

  def matchImpl[R: WeakTypeTag](query: Tree): Tree = query match {
    case Function(List(_), Match(_, cases)) =>
      cases match {
        case List(CaseDef(pattern, guard, body)) =>
          val Pattern(binds) = pattern
          println(s"binds = ${binds.mkString("\n        ")}")
          val bindDefs = binds.collect{
            case P.Node(Some(a), _, _) =>
              q"val ${TermName(a)} = _root_.com.abraxas.slothql.newcypher.syntax.CypherSyntaxFromMacro.mkNode($a)"
            case P.Rel(Some(a), _, _, _, dir) =>
              q"val ${TermName(a)} = _root_.com.abraxas.slothql.newcypher.syntax.CypherSyntaxFromMacro.mkRel[${dirType{dir}}]($a)"
          }
          val newBody = transformBody(binds, body)
          // TODO ======================================================================================================
          val res =
            q"""
              ..$bindDefs
              $newBody
              ??? : _root_.com.abraxas.slothql.cypher.CypherFragment.Query[${weakTypeOf[R]}]
             """
          println(s"res = $res")
          res
        case _ => c.abort(query.pos, "Query must have a single case clause")
      }
    case other => c.abort(query.pos, s"Unexpected query function: $other")
  }

  private def dirType(dir: P.Rel.Direction): Type = (dir: @unchecked) match {
    case P.Rel.Incoming => typeOf[P.Rel.Incoming.type]
    case P.Rel.Outgoing => typeOf[P.Rel.Outgoing.type]
  }

  protected def transformBody(binds: List[P.PatternA], body: Tree): Tree = {
    val newBody = new BodyTransformer(binds).transform(body)
    c.untypecheck(newBody)
  }

  protected class BodyTransformer(binds: List[P.PatternA]) extends Transformer {
    private val names = binds.flatMap(_.alias).toSet
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
    /**
     * Node := Pat < RNX
     *       | NR > Pat
     */
    def unapply(tree: Tree): Option[List[P.PatternA]] = PartialFunction.condOpt(tree) {
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
      def unapply(tree: Tree): Option[List[P.PatternA]] = PartialFunction.condOpt(tree) {
        case UnApplyLR(Pattern(xs), Syntax_<, RNX(ys)) => xs ::: ys
        case UnApplyLR(NR(xs),      Syntax_>, NR(ys))  => xs ::: ys
        case UnApplyLR(Node(n),     Syntax_-, Rel(r))  => List(n, r)
      }
    }

    /**
     * RN := Rel - Node
     */
    object RN {
      def unapply(tree: Tree): Option[List[P.PatternA]] = PartialFunction.condOpt(tree) {
        case UnApplyLR(Rel(r), Syntax_-, Node(n)) => List(r, n)
      }
    }

    /**
     * RNX  := RN - Rel
     *       | RN
     */
    object RNX {
      def unapply(tree: Tree): Option[List[P.PatternA]] = PartialFunction.condOpt(tree) {
        case UnApplyLR(RN(xs), Syntax_-, Rel(r)) => xs ::: List(r)
        case RN(xs) => xs
      }
    }

    object Node {
      def unapply(tree: Tree): Option[P.Node] = PartialFunction.condOpt(tree) {
        case pq"$name@$obj(..$args)" if obj.symbol == SyntaxNodeObj =>
          println(s"args = $args")
          P.Node(stringName(name), Nil, Map())
        case pq"$name@$other" if tree.tpe <:< SyntaxNodeType =>
          P.Node(stringName(name), Nil, Map())
      }
    }
    object Rel {
      def unapply(tree: Tree): Option[P.Rel] = PartialFunction.condOpt(tree) {
        case pq"$name@$obj(..$args)" if obj.symbol == SyntaxRelObj =>
          println(s"args = $args")
          P.Rel(stringName(name), Nil, Map(), None, dir(obj))
        case pq"$name@$_" if tree.tpe <:< SyntaxRelType =>
          P.Rel(stringName(name), Nil, Map(), None, dir(tree))
      }
    }

    private def dir(tree: Tree) = (tree.tpe.decl(TypeName("Dir")).asType.toType: @unchecked) match {
      case tpe if tpe <:< DirectionInType  => P.Rel.Incoming
      case tpe if tpe <:< DirectionOutType => P.Rel.Outgoing
    }

    lazy val Syntax_<  = symbolOf[<.type].companionSymbol
    lazy val Syntax_>  = symbolOf[>.type].companionSymbol
    lazy val Syntax_-  = symbolOf[-.type].companionSymbol
    lazy val Syntax_:= = symbolOf[:=.type].companionSymbol
    lazy val Syntax_** = symbolOf[**.type].companionSymbol

    lazy val SyntaxNodeObj  = symbolOf[syntax.Node.type].companionSymbol
    lazy val SyntaxNodeType = typeOf[syntax.Node].typeConstructor
    lazy val SyntaxRelObj   = symbolOf[syntax.Rel.type].companionSymbol
    lazy val SyntaxRelType  = typeOf[syntax.Rel].typeConstructor

    lazy val DirectionInType  = typeOf[syntax.Rel.Incoming]
    lazy val DirectionOutType = typeOf[syntax.Rel.Outgoing]
  }
}