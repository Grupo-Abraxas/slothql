package com.abraxas.slothql.arrow.util

import scala.reflect.runtime.{ universe => ru }
import scala.util.Random

import shapeless._

import com.abraxas.slothql.arrow.Arrow
import com.abraxas.slothql.util.{ ClassUtils, TypeUtils }


trait ShowT[T] {
  def apply(): String
  def simple: String
  def simplest: String
  def isInfix: Boolean
}
object ShowT extends ShowTLowPriorityImplicits {
  def apply[T](implicit show: ShowT[T]): String = show()
  def define[T](show: String, showSimple: String, showSimplest: String, infix: Boolean): ShowT[T] =
    new ShowT[T] {
      def apply(): String = show
      def simple: String = Option(showSimple).getOrElse(apply())
      def simplest: String = Option(showSimplest).getOrElse(simple)
      def isInfix: Boolean = infix
    }
  def define[T](show: String, showSimple: String = null, infix: Boolean)(implicit typeTag: ru.TypeTag[T]): ShowT[T] =
    define(show, showSimple, TypeUtils.Show.noTypeParams(typeTag.tpe), infix)

  implicit def defaultTupleShow[P <: Product, Repr <: HList, ShowRepr <: HList](
    implicit
    gen: Generic.Aux[P, Repr],
    isTuple: ops.hlist.Tupler.Aux[Repr, P],
    showEach: ops.hlist.LiftAll.Aux[ShowT, Repr, ShowRepr],
    toList: ops.hlist.ToTraversable.Aux[ShowRepr, List, ShowT[_]],
    tag: ru.TypeTag[P],
    low: LowPriority
  ): ShowT[P] = {
    val showL = showEach.instances.toList
    ShowT.define(
      show       = showL.map(_())     .mkString("(", ", ", ")"),
      showSimple = showL.map(_.simple).mkString("(", ", ", ")"),
      infix = false
    )
  }

  implicit def defaultHListShowT[L <: HList, ShowL <: HList](
    implicit
    showEach: ops.hlist.LiftAll.Aux[ShowT, L, ShowL],
    toList: ops.hlist.ToTraversable.Aux[ShowL, List, ShowT[_]],
    low: LowPriority
  ): ShowT[L] = {
    val showL = showEach.instances.toList
    ShowT.define[L](
      show         = showL.foldRight("HNil")((show, acc) => s"${showInfix(show, _())} :: $acc"),
      showSimple   = showL.foldRight("HNil")((show, acc) => s"${showInfix(show, _.simple)} :: $acc"),
      showSimplest = "HList",
      infix = true
    )
  }

  private def showInfix(show: ShowT[_], f: ShowT[_] => String) = if (show.isInfix) s"(${f(show)})" else f(show)
}

trait ShowTLowPriorityImplicits {
  implicit def defaultShowT[T: ru.TypeTag](implicit low: LowPriority): ShowT[T] =
    ShowT.define(TypeUtils.Show(ru.typeOf[T]), TypeUtils.Show.noTypeParams(ru.typeOf[T]), infix = false)
}


trait ToDot[A <: Arrow] { def apply(a: A): String }
object ToDot {
  def apply[A <: Arrow](a: A)(implicit toDot: ToDot[A]): String = toDot(a)
  def define[A <: Arrow](toDot: A => String): ToDot[A] = new ToDot[A] { def apply(a: A): String = toDot(a) }

  implicit def defaultToDot[A <: Arrow](
    implicit
    showSource: ShowT[A#Source],
    toDot: ArrowToDot[A]
  ): ToDot[A] = define { a =>
    val (sourceDot, source) = ArrowToDot.defaultNewTypeNode[A#Source]()
    s"""
       |digraph {
       |${sourceDot + toDot(a, source)._1}
       |}
     """.stripMargin
  }
}

trait ArrowToDot[A <: Arrow] {
  import ArrowToDot._
  def apply(a: A, source: SourceNodeId): (String, TargetNodeId)
}

object ArrowToDot {
  type NodeId = String
  type SourceNodeId = String
  type TargetNodeId = String

  def define[A <: Arrow](toDot: (A, SourceNodeId) => (String, TargetNodeId)): ArrowToDot[A] =
    new ArrowToDot[A] {
      def apply(a: A, source: SourceNodeId): (String, TargetNodeId) = toDot(a, source)
    }

  def defaultNewTypeNode[T](props: String*)(implicit show: ShowT[T]): (String, NodeId) = {
    val id = show.simplest + "_" + randInt()
    s"""$id [label="${show()}",${props.mkString(",")}]""" -> id
  }
  def defaultEdge(source: SourceNodeId, target: TargetNodeId, label: String, props: String*): String =
    s"""
       |edge [label="$label",${props.mkString(",")}];
       |$source -> $target;
     """.stripMargin

  object ArrowToDotPoly extends Poly1 {
    implicit def impl[A <: Arrow](implicit toDot: ArrowToDot[A]): Case.Aux[(A, SourceNodeId), (String, TargetNodeId)] =
      at{ case (a, source) => toDot(a, source) }
  }

  implicit def splitArrowToDot[A <: Arrow, Arrs <: HList, ArrsZ <: HList, ArrsDot <: HList](
    implicit
    split: A <:< Arrow.Split[Arrs],
    zipWithSource: ops.hlist.ZipConst.Aux[SourceNodeId, Arrs, ArrsZ],
    mapToDot: ops.hlist.Mapper.Aux[ArrowToDotPoly.type, ArrsZ, ArrsDot],
    dotToList: ops.hlist.ToTraversable.Aux[ArrsDot, List, (String, TargetNodeId)],
    showTarget: ShowT[A#Target]
  ): ArrowToDot[A] = define {
    (a, source) =>
      val (dots, targets) = dotToList(mapToDot(zipWithSource(source, a.arrows))).unzip
      val id = "split_tuple_" + randInt()
      val tupleNode = s"""$id [label="${showTarget.simple}"]"""
      val tupleArrows = targets.zipWithIndex.map{ case (id0, i) => defaultEdge(id0, id, s"_${i + 1}") }.mkString
      val dot = s"""
         |$tupleNode;
         |${dots.mkString}
         |edge[style=bold];
         |$tupleArrows
         |edge[style=solid]; // reset
       """.stripMargin
      dot -> id
  }

  implicit def compositeArrowToDot[A <: Arrow, Arrs0 <: HList, Arrs <: HList](
    implicit
    notSplit: A <:!< Arrow.Split[_],
    composition: A <:< Arrow.Composition[_, _],
    unchain: Arrow.Unchain.Aux[A, Arrs0],
    reverse: ops.hlist.Reverse.Aux[Arrs0, Arrs],
    toDot: ChainToDot[Arrs]
  ): ArrowToDot[A] = define{ (a, source) => toDot(reverse(unchain(a)), source) }

  implicit def defaultSingleArrowToDot[A <: Arrow](
    implicit
    low: LowPriority,
    showTarget: ShowT[A#Target],
    showArrow: ShowT[A] = null
  ): ArrowToDot[A] = define {
    (a, source) =>
      val (targetDot, target) = defaultNewTypeNode[A#Target]()
      val arrLabel = Option(showArrow).map(_.simple).getOrElse(ClassUtils.Show(a.getClass))
      val edgeDot = defaultEdge(source, target, arrLabel)
      val dot = s"""
         |$targetDot
         |$edgeDot
       """.stripMargin
      dot -> target
  }

  trait ChainToDot[Arrs <: HList] {
    def apply(a: Arrs, source: SourceNodeId): (String, TargetNodeId)
  }
  object ChainToDot {
    implicit def emptyChainToDot: ChainToDot[HNil] =
      new ChainToDot[HNil] { def apply(a: HNil, source: SourceNodeId): (String, TargetNodeId) = "" -> source }
    implicit def nonEmptyChainToDot[L <: HList, H <: Arrow, T <: HList](
      implicit
      hcons: ops.hlist.IsHCons.Aux[L, H, T],
      headToDot: ArrowToDot[H],
      tailToDot: ChainToDot[T]
    ): ChainToDot[L] =
      new ChainToDot[L] {
        def apply(a: L, source: SourceNodeId): (String, TargetNodeId) = {
          val (headDot, headTgt) = headToDot(a.head, source)
          val (tailDot, tailTgt) = tailToDot(a.tail, headTgt)
          (headDot + tailDot, tailTgt)
        }
      }
  }

  private def randInt() = Random.nextInt(Int.MaxValue)
}