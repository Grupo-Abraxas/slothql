package com.abraxas.slothql.cypher.fragments

import cats.data.Ior

import com.abraxas.slothql.FragmentDefinitionHelper.{ As, fromNarrow, fragment }
import com.abraxas.slothql.cypher.CypherFragment

sealed trait Expr[+T]
object Expr {
  type Inv[T] = Expr[T]

  // // // Values and Variables // // //
  case class Lit[A](value: A)(implicit val m: Manifest[A]) extends Expr[A]

  @fragment
  trait Var[A] extends Expr[A] {
    type Name <: String

    val name: String fromNarrow Name
    implicit val m: Manifest[A]

    override def toString: String = s"Var[$m]($name)"
  }
  object Var {
    implicit def fragment[A]: CypherFragment[Var[A]] = instance.asInstanceOf[CypherFragment[Var[A]]]
    private lazy val instance = CypherFragment.define[Var[_]](v => CypherFragment.escapeName(v.name))
  }


//  case class Call[A](func: String, params: scala.List[Known[Expr[_]]]) extends Expr[A]
//
//  object Lit {
//    implicit lazy val literalStringFragment: CypherFragment[Lit[String]] = CypherFragment.define {
//      case Lit(str) => "\"" + str.replaceAll("\"", "\\\"") + "\""
//    }
//    /** Warning: it does nothing to check whether the number can be represented in cypher (~Long~ equivalent). */
//    implicit def literalNumericFragment[N: Numeric]: CypherFragment[Lit[N]] =
//      literalToString.asInstanceOf[CypherFragment[Lit[N]]]
//    implicit lazy val literalBooleanFragment: CypherFragment[Lit[Boolean]] =
//      literalToString.asInstanceOf[CypherFragment[Lit[Boolean]]]
//
//    private lazy val literalToString = CypherFragment.define[Lit[_]](_.value.toString)
//  }
//  object Call {
//    implicit def fragment[A]: CypherFragment[Call[A]] = instance.asInstanceOf[CypherFragment[Call[A]]]
//    private lazy val instance = CypherFragment.define[Call[_]] {
//      case Call(func, params) => s"${CypherFragment.escapeName(func)}(${params.map(_.toCypher).mkString(", ")})"
//    }
//  }
//
//  // // // Maps // // //
//  case class Map[A](get: Predef.Map[String, Known[Expr[A]]]) extends Expr[Predef.Map[String, A]]
//  case class Key[A](map: Known[Expr[Predef.Map[String, _]]], key: String)(implicit val m: Manifest[A]) extends Expr[A]
//
//  object Map {
//    implicit lazy val fragment: CypherFragment[Map[_]] = CypherFragment.define(m => CypherFragment.mapStr(m.get))
//  }
//  object Key {
//    implicit def fragment[A]: CypherFragment[Key[A]] = instance.asInstanceOf[CypherFragment[Key[A]]]
//    implicit lazy val instance: CypherFragment[Key[_]] = CypherFragment.define {
//      case Key(m, k) => s"${m.toCypher}.${CypherFragment.escapeName(k)}"
//    }
//  }
//
//  // // // Lists // // //
//  case class List[A](get: scala.List[Known[Expr[A]]]) extends Expr[scala.List[A]]
//  case class In[A](elem: Known[Expr[A]], list: Known[Expr[scala.List[A]]]) extends Expr[Boolean]
//  case class AtIndex[A](list: Known[Expr[scala.List[A]]], index: Known[Expr[Long]]) extends Expr[A]
//  case class AtRange[A](list: Known[Expr[scala.List[A]]], limits: Ior[Known[Expr[Long]], Known[Expr[Long]]]) extends Expr[scala.List[A]]
//  case class Concat[A](list0: Known[Expr[scala.List[A]]], list1: Known[Expr[scala.List[A]]]) extends Expr[scala.List[A]]
//
//  object List {
//    implicit def fragment[A]: CypherFragment[List[A]] = instance.asInstanceOf[CypherFragment[List[A]]]
//    private lazy val instance: CypherFragment[List[_]] = CypherFragment.define {
//      _.get.map(_.toCypher).mkString("[ ", ", ", " ]")
//    }
//  }
//  object In {
//    implicit def fragment[A]: CypherFragment[In[A]] = instance.asInstanceOf[CypherFragment[In[A]]]
//    private lazy val instance: CypherFragment[In[_]] = CypherFragment.define {
//      case In(elem, list) => s"${elem.toCypher} IN ${list.toCypher}"
//    }
//  }
//  private def atIndex(list: Known[Expr[scala.List[_]]], index: String) = s"${list.toCypher}[$index]"
//  object AtIndex {
//    implicit def fragment[A]: CypherFragment[AtIndex[A]] = instance.asInstanceOf[CypherFragment[AtIndex[A]]]
//    private lazy val instance = CypherFragment.define[AtIndex[_]] {
//      case AtIndex(list, index) => atIndex(list, index.toCypher)
//    }
//  }
//  object AtRange {
//    implicit def fragment[A]: CypherFragment[AtRange[A]] = instance.asInstanceOf[CypherFragment[AtRange[A]]]
//    private lazy val instance = CypherFragment.define[AtRange[_]] {
//      case AtRange(list, range) => atIndex(list, CypherFragment.rangeStr(range))
//    }
//  }
//  object Concat {
//    implicit def fragment[A]: CypherFragment[Concat[A]] = instance.asInstanceOf[CypherFragment[Concat[A]]]
//    private lazy val instance = CypherFragment.define[Concat[_]] {
//      case Concat(list0, list1) => s"${list0.toCypher} + ${list1.toCypher}"
//    }
//  }
//
//  // // // Strings // // //
//  case class StringExpr(left: Known[Expr[String]], right: Known[Expr[String]], op: StringExpr.Op) extends Expr[Boolean]
//  object StringExpr {
//    sealed trait Op
//    case object StartsWith extends Op
//    case object EndsWith   extends Op
//    case object Contains   extends Op
//
//    implicit lazy val fragment: CypherFragment[StringExpr] = CypherFragment.define {
//      case StringExpr(left, right, op) =>
//        val opStr = op match {
//          case StartsWith => "STARTS WITH"
//          case EndsWith   => "ENDS WITH"
//          case Contains   => "CONTAINS"
//        }
//        s"${left.toCypher} $opStr ${right.toCypher}"
//    }
//  }
//
//  // // // Logic // // //
//  sealed trait LogicExpr extends Expr[Boolean]
//  case class LogicBinaryExpr(left: Known[Expr[Boolean]], right: Known[Expr[Boolean]], op: LogicExpr.BinaryOp) extends LogicExpr
//  case class LogicNegationExpr(expr: Known[Expr[Boolean]]) extends LogicExpr
//  object LogicExpr {
//    sealed trait BinaryOp
//    case object Or  extends BinaryOp
//    case object And extends BinaryOp
//    case object Xor extends BinaryOp
//
//    implicit lazy val fragment: CypherFragment[LogicExpr] = CypherFragment.define {
//      case LogicNegationExpr(expr) => s"NOT ${expr.toCypher}"
//      case LogicBinaryExpr(left, right, op) =>
//        val opStr = op match {
//          case Or  => "OR"
//          case And => "AND"
//          case Xor => "XOR"
//        }
//        s"${left.toCypher} $opStr ${right.toCypher}"
//    }
//  }
//
//  // // // Compare // // //
//  sealed trait CompareExpr extends Expr[Boolean]
//  case class CompareBinaryExpr[A](left: Known[Expr[A]], right: Known[Expr[A]], op: CompareExpr.BinaryOp) extends CompareExpr
//  case class CompareBinaryAnyExpr(left: Known[Expr[Any]], right: Known[Expr[Any]], op: CompareExpr.BinaryAnyOp) extends CompareExpr
//  case class CompareUnaryExpr(expr: Known[Expr[Any]], op: CompareExpr.UnaryOp) extends CompareExpr
//
//  object CompareExpr {
//    sealed trait BinaryOp
//    case object Lt  extends BinaryOp
//    case object Lte extends BinaryOp
//    case object Gte extends BinaryOp
//    case object Gt  extends BinaryOp
//
//    sealed trait BinaryAnyOp
//    case object Eq  extends BinaryAnyOp
//    case object Neq extends BinaryAnyOp
//
//    sealed trait UnaryOp
//    case object IsNull  extends UnaryOp
//    case object NotNull extends UnaryOp
//
//    implicit lazy val fragment: CypherFragment[CompareExpr] = CypherFragment.define {
//      case CompareBinaryExpr(left, right, op) =>
//        val opStr = op match {
//          case Lt  => "<"
//          case Lte => "<="
//          case Gte => ">="
//          case Gt  => ">"
//        }
//        s"${left.toCypher} $opStr ${right.toCypher}"
//      case CompareBinaryAnyExpr(left, right, op) =>
//        val opStr = op match {
//          case Eq  => "="
//          case Neq => "<>"
//        }
//        s"${left.toCypher} $opStr ${right.toCypher}"
//      case CompareUnaryExpr(expr, op) =>
//        val opStr = op match {
//          case IsNull  => "IS NULL"
//          case NotNull => "IS NOT NULL"
//        }
//        s"${expr.toCypher} $opStr"
//    }
//  }
}
