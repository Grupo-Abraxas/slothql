package com.abraxas.slothql.arrow.util.show

import java.util.UUID

import scala.annotation.tailrec
import scala.language.higherKinds
import scala.reflect.runtime.{ universe => ru }

import cats.Functor
import qq.droste
import qq.droste.data.{ :<, Attr, Coattr }
import qq.droste.{ CVAlgebra, CVCoalgebra, Gather }

import com.abraxas.slothql.arrow.ScalaExpr


object ScalaExprToDot {
  def apply(expr: ScalaExpr): String = {
    val dot = droste.scheme.ghylo(
      ExprDot.exprDotToString.gather(Gather.histo),
      ExprDot.wrappedScalaExprToExprDot.scatter(DrosteExtra.Scatter.futu)
    ).apply(RootScalaExpr(expr))
    s"""digraph {
       |${dot.map("\t" + _).mkString("\n")}
       |}""".stripMargin
  }

  sealed trait WrappedScalaExpr
  case class RootScalaExpr(expr: ScalaExpr) extends WrappedScalaExpr
  case class JustScalaExpr(expr: ScalaExpr) extends WrappedScalaExpr

  sealed trait ExprDot[A]
  object ExprDot {
    type Id = String

    case class Node     [A](edgeLabel: String,         nodeLabel: String,          id: Id = randomId())          extends ExprDot[A]
    case class RootLike [A](edgeLabel: Option[String], nodeLabel: String, next: A, id: Id = randomId())          extends ExprDot[A]
    case class Compose  [A](prev: A, next: A)                                                                    extends ExprDot[A]
    case class Cluster  [A](edgeLabel: String,         inside: A,         tgtLabel: String, id: Id = randomId()) extends ExprDot[A]
    case class Branching[A](branching: Branching.Type, branches: List[A], tgtLabel: String, id: Id = randomId()) extends ExprDot[A]
    object Branching{
      sealed trait Type
      case object Split  extends Type
      case object Choose extends Type
    }

    implicit val exprDotFunctor: Functor[ExprDot] = new Functor[ExprDot] {
      def map[A, B](fa: ExprDot[A])(f: A => B): ExprDot[B] = fa match {
        case c: Node[_]      => c.asInstanceOf[Node[B]]
        case r: RootLike[A]  => r.copy(next = f(r.next))
        case Compose(p, n)   => Compose(f(p), f(n))
        case b: Branching[A] => b.copy(branches = b.branches.map(f))
        case c: Cluster[A]   => c.copy(inside = f(c.inside))
      }
    }

    val wrappedScalaExprToExprDot: CVCoalgebra[ExprDot, WrappedScalaExpr] = CVCoalgebra {
      case JustScalaExpr(e) => scalaExprToDot(e)
      case RootScalaExpr(e) => RootLike(None, showType(e.src), Coattr.roll(scalaExprToDot(e)))
    }

    private def scalaExprToDot(expr0: ScalaExpr): ExprDot[Coattr[ExprDot, WrappedScalaExpr]] = {
      def just = Coattr.pure[ExprDot, WrappedScalaExpr] _ compose JustScalaExpr.apply
      def root = Coattr.pure[ExprDot, WrappedScalaExpr] _ compose RootScalaExpr.apply

      expr0 match {
        case e@ScalaExpr.Split(exprs)      => Branching(Branching.Split, exprs.map(just), showType(e.tgt))
        case e@ScalaExpr.Choose(exprs)     => Branching(Branching.Choose, exprs.map(root), showType(e.tgt))
        case e@ScalaExpr.Composition(f, g) => Compose(just(g), just(f))
        case e@ScalaExpr.FMap(expr)        => Cluster("map", root(expr), showType(e.tgt))
        case e@ScalaExpr.MBind(expr)       => Cluster("flatMap", root(expr), showType(e.tgt))
        case e@ScalaExpr.Const(expr)       => Cluster("const", root(expr), showType(e.tgt))
        case ScalaExpr.Id(tag)             => Node("id", showType(tag))
        case ScalaExpr.Literal(lit)        => Node("literal", lit.toString)
        case e@ScalaExpr.SelectField(name) => Node(name, showType(e.tgt))
        // TODO: more cases
      }
    }

    private def showType(tag: ru.TypeTag[_]): String = showType(tag.tpe, parentIsInfix = false)
    private def showType(tpe0: ru.Type, parentIsInfix: Boolean): String = {
      def wrap(str: String) = if (parentIsInfix) s"($str)" else str
      tpe0 match {
        case ShowType.Tuple(tpes)  => tpes.map(showType(_, parentIsInfix = false)).mkString("(", ", ", ")")
        case ShowType.HList(tpes)  => wrap(tpes.foldLeft("HNil") { case (acc, t) => s"${showType(t, parentIsInfix = true)} :: $acc" })
        case ShowType.Coprod(tpes) => wrap(tpes.foldLeft("CNil") { case (acc, t) => s"${showType(t, parentIsInfix = true)} :+: $acc" })
        case _ =>
          val tpe = tpe0.dealias
          val args = if (tpe.typeArgs.nonEmpty) tpe.typeArgs.map(showType(_, parentIsInfix = false)).mkString("[", ", ", "]") else ""
          tpe.typeSymbol.name.toString + args
      }
    }
    private object ShowType {
      object Tuple {
        def unapply(tpe: ru.Type): Option[List[ru.Type]] =
          if (tpe.typeSymbol.fullName matches """^scala\.Tuple\d+$""") Some(tpe.typeArgs)
          else None
      }
      object HList {
        def unapply(tpe: ru.Type): Option[List[ru.Type]] = inner(tpe, Nil)

        def inner(tpe: ru.Type, accRev: List[ru.Type]): Option[List[ru.Type]] = tpe match {
          case ru.TypeRef(_, HConsSymb, List(h, t)) => inner(t, h :: accRev)
          case ru.TypeRef(_, HNilSymb, Nil)         => Some(accRev.reverse)
          case _                                    => None
        }
        private val HConsSymb = ru.symbolOf[shapeless.::[_, _]]
        private val HNilSymb  = ru.symbolOf[shapeless.HNil]
      }
      object Coprod {
        def unapply(tpe: ru.Type): Option[List[ru.Type]] = inner(tpe, Nil)

        def inner(tpe: ru.Type, accRev: List[ru.Type]): Option[List[ru.Type]] = tpe match {
          case ru.TypeRef(_, CConsSymb, List(h, t)) => inner(t, h :: accRev)
          case ru.TypeRef(_, CNilSymb, Nil)         => Some(accRev.reverse)
          case _                                    => None
        }
        private val CConsSymb = ru.symbolOf[shapeless.:+:[_, _]]
        private val CNilSymb  = ru.symbolOf[shapeless.CNil]
      }
    }

    val exprDotToString: CVAlgebra[ExprDot, List[String]] = CVAlgebra {
      case RootLike(_, label, nextDot :< nextDotExpr, id) =>
        Make.node(id, Make.Opt.label(label)) :: nextDot ::: connect(id, nextDotExpr)
      case Node(_, label, id) =>
        Make.node(id, Make.Opt.label(label)) :: Nil
      case Compose(prevDot :< prevDotExpr, nextDot :< nextDotExpr) =>
        prevDot ::: nextDot ::: connect(getLastId(prevDotExpr), nextDotExpr)
      case Branching(Branching.Split, branches, label, id) =>
        Make.node(id, Make.Opt.label(label)) :: branches.zipWithIndex.flatMap{
          case (dot0 :< exprDot, i) => dot0 ::: Make.edge(s"_${i + 1}", getLastId(exprDot), id)
        }
      case Branching(Branching.Choose, branches, label, id) =>
        Make.node(id, Make.Opt.label(label)) :: branches.flatMap{
          case dot0 :< exprDot => dot0 ::: Make.edge("", getLastId(exprDot), id)
        }
      case Cluster(_, dot0 :< dotExpr, label, id) =>
        val openCluster = s"""subgraph "cluster_${randomId()}" {"""
        val closeCluster = "}"
        val tgtNode = Make.node(id, Make.Opt.label(label))
        val tgtEdge = Make.edge("", getLastId(dotExpr), id)
        "" :: openCluster :: dot0.map("\t" + _) ::: closeCluster :: tgtNode :: tgtEdge
    }

    private def connect(parent: Id, next: ExprDot[Attr[ExprDot, List[String]]]): List[String] =
      firstEdgeLabelsAndIds(next, None).flatMap {
        case (edgeLabel, id) => Make.edge(edgeLabel, parent, id)
      }


    @tailrec private def getLastId(exprDot: ExprDot[Attr[ExprDot, List[String]]]): Id = exprDot match {
      case Node(_, _, id)               => id
      case Branching(_, _, _, id)       => id
      case Cluster(_, _, _, id)         => id
      case RootLike(_, _, _ :< next, _) => getLastId(next)
      case Compose(_, _ :< next)        => getLastId(next)
    }

    // TODO: not tailrec because of `flatMap`
    private def firstEdgeLabelsAndIds(exprDot: ExprDot[Attr[ExprDot, List[String]]], otherEdge: Option[String]): List[(String, Id)] = exprDot match {
      case Node(edge, _, id)           => List(edge -> id)
      case RootLike(edgeOpt, _, _, id) => List(edgeOpt.orElse(otherEdge).getOrElse("") -> id)
      case Cluster(edge, _ :< e, _, _) => firstEdgeLabelsAndIds(e, otherEdge = otherEdge orElse Some(edge))
      case Compose(_ :< prev, _)       => firstEdgeLabelsAndIds(prev, otherEdge)
      case Branching(_, bs, _, _)      => bs.flatMap{ case _ :< branch => firstEdgeLabelsAndIds(branch, otherEdge orElse Some("on")) } // TODO: flatMap
    }

    object Make {
      def node(id: String, options: String*): String =
        s"${nodeId(id)} [${options.mkString(",")}];"

      def nodeId(id: String): String = s""""$id""""

      def edge(label: String, from: String, to: String, options: String*): List[String] =
        s"edge [${(Opt.label(label) +: options).mkString(",")}]" :: connection(from, to) :: Nil
      private def connection(from: String, to: String): String = s"${Make.nodeId(from)} -> ${Make.nodeId(to)};"

      object Opt {
        def label(s: String): String = s"""label="$s""""
        def htmlLabel(html: String): String = s"label=<$html>"
      }
    }

    private def randomId() = UUID.randomUUID().toString
  }
}


