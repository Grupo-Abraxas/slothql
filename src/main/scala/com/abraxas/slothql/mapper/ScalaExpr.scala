package com.abraxas.slothql.mapper

import scala.language.{ dynamics, higherKinds }

import shapeless.tag.@@
import shapeless.{ HList, LabelledGeneric, ops }

import com.abraxas.slothql.mapper.Arrow.Types


sealed trait ScalaExpr extends Arrow with ScalaExpr.FieldSelectionOps

object ScalaExpr {
  type Aux[-S, +T] = ScalaExpr { type Source >: S; type Target <: T }

  def apply[A]: Id[A] = Id[A]


  /** Identity arrow. (self selection) */
  sealed trait Id[A] extends ScalaExpr with Arrow.Id[A]
  object Id {
    def apply[A]: Id[A] = instance.asInstanceOf[Id[A]]
    private lazy val instance = new Id[Any]{}
  }

  /** Expression representing selection of a field of an ADT (case class). */
  case class SelectField[Obj, K <: String, V](field: K)
    extends ScalaExpr { type Source = Obj; type Target = V }

  /** Expression representing functor `map` operation. */
  case class FMap[F[_], E <: ScalaExpr](expr: E)(implicit val F: cats.Functor[F])
    extends ScalaExpr { type Source = F[expr.Source]; type Target = F[expr.Target] }
  object FMap {
    def apply[F[_]]: Builder[F] = Builder.asInstanceOf[Builder[F]]

    protected class Builder[F[_]] {
      def apply[E <: ScalaExpr](expr: E)(implicit F: cats.Functor[F]): FMap[F, E] = FMap[F, E](expr)
    }
    private object Builder extends Builder
  }
  
  /** Expression representing monadic bind / `flatMap` operation. */
  case class MBind[F[_], E <: ScalaExpr](expr: E)(implicit val M: cats.Monad[F])
    extends ScalaExpr { type Source = F[expr.Source]; type Target = expr.Target }
  object MBind {
    def apply[F[_]]: Builder[F] = Builder.asInstanceOf[Builder[F]]

    protected class Builder[F[_]] {
      def apply[E <: ScalaExpr](expr: E)(implicit M: cats.Monad[F]): MBind[F, E] = MBind[F, E](expr)
    }
    private object Builder extends Builder
  }

  // TODO
  case class SelectIn[F[_], Sel, V](sel: Sel)
    extends ScalaExpr { type Source = F[V]; type Target = V }


  // // // // // // Syntax Ops // // // // // //


  implicit class ScalaExprFMapOps[A <: ScalaExpr, F[_], TA, S0](a: A)(
    implicit
    targetA: Types.Aux[A, _, TA],
    source0: TA <:< F[S0],
    functor: cats.Functor[F]
  ) {

    def map[B <: ScalaExpr](b: B)(implicit compose: Arrow.Compose[FMap[F, B], A]): compose.Out = compose(new FMap[F, B](b), a)
    def map[B <: ScalaExpr](fb: Id[S0] => B)(implicit compose: Arrow.Compose[FMap[F, B], A]): compose.Out = compose(new FMap[F, B](fb(Id[S0])), a)
  }

  implicit class ScalaExprMBindOps[A <: ScalaExpr, F[_], TA, S0](a: A)(
    implicit
    targetA: Types.Aux[A, _, TA],
    source0: TA <:< F[S0],
    monad: cats.Monad[F]
  ) {

    def flatMap[B <: ScalaExpr.Aux[_, F[_]]](b: B)(implicit compose: Arrow.Compose[MBind[F, B], A]): compose.Out = compose(new MBind[F, B](b), a)
    def flatMap[B <: ScalaExpr.Aux[_, F[_]]](fb: Id[S0] => B)(implicit compose: Arrow.Compose[MBind[F, B], A]): compose.Out = compose(new MBind[F, B](fb(Id[S0])), a)
  }

  protected trait FieldSelectionOps extends Dynamic {
    expr: ScalaExpr =>

    def selectDynamic(k: String)(implicit ev: Syntax.HasField[Source, Symbol @@ k.type]): SelectField[Source, k.type, ev.Value] = SelectField(k)
  }

  object Syntax {
    /** Evidence that `Obj` has a field of type `V` with name `K` */
    @annotation.implicitNotFound(msg = "${Obj} doesn't have field ${K}")
    trait HasField[Obj, K] { type Value }
    object HasField {
      type Aux[Obj, K, V] = HasField[Obj, K] { type Value = V }
      implicit def evidence[Obj, K, V, Repr <: HList](
        implicit
        generic: LabelledGeneric.Aux[Obj, Repr], select: ops.record.Selector.Aux[Repr, K, V]
      ): HasField.Aux[Obj, K, V] = instance.asInstanceOf[HasField.Aux[Obj, K, V]]
      private lazy val instance = new HasField[Any, Any]{}
    }

  }

}
