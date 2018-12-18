package com.abraxas.slothql.arrow

import scala.language.higherKinds

import cats.syntax.flatMap._
import cats.syntax.functor._
import shapeless._
import shapeless.tag.@@

trait FuncArrow extends Arrow {
  def apply(src: Source): Target
}

object FuncArrow {
  type Aux[-S, +T] = FuncArrow { type Source >: S; type Target <: T }
  type Inv[ S,  T] = FuncArrow { type Source  = S; type Target  = T }

  def from[A <: Arrow](a: A)(implicit f: Functor[A, FuncArrow]): f.Out = f(a)
  def apply[S, T](func: S => T): FuncArrow.Inv[S, T] = new Impl[S, T](func)


  protected class Impl[S, T](func: S => T) extends (S => T) with FuncArrow {
    type Source = S
    type Target = T
    @inline def apply(s: S): T = func(s)
    override def toString() = "<function1>"
  }


  implicit def composeFunc1ArrowsAsFunctions[A, B, C]: Arrow.Compose.Aux[FuncArrow.Inv[B, C], FuncArrow.Inv[A, B], FuncArrow.Inv[A, C]] =
    composeInstance.asInstanceOf[Arrow.Compose.Aux[FuncArrow.Inv[B, C], FuncArrow.Inv[A, B], FuncArrow.Inv[A, C]]]
  private lazy val composeInstance = new Arrow.Compose[FuncArrow.Inv[Any, Any], FuncArrow.Inv[Any, Any]] {
    type Out = FuncArrow.Aux[Any, Any]
    def apply(f: FuncArrow.Inv[Any, Any], g: FuncArrow.Inv[Any, Any]): FuncArrow.Aux[Any, Any] = FuncArrow(f.apply _ compose g.apply)
  }

  object ApplyTupled extends Poly1 {
    implicit def applyFunc1Arrow[S, T]: Case.Aux[(FuncArrow.Inv[S, T], S), T] = at[(FuncArrow.Inv[S, T], S)]{ case (f, s) => f(s) }
  }

  implicit def mapSplitToFunc1Arrow[
    A <: Arrow, Arrows <: HList, Mapped <: HList, Arrows1 <: HList, S, T, ZL <: HList, TL <: HList
  ](
    implicit
    isSplit0: A <:< Arrow.Split[Arrows],
    fmap: Lazy[Functor.FMapHList.Aux[Arrows, FuncArrow, Mapped]],
    split: Arrow.Split.Splitter.Aux[Mapped, _ <: Arrow.Split.Aux[Arrows1, S, T]],
    zip: ops.hlist.ZipConst.Aux[S, Arrows1, ZL],
    applyArrows: ops.hlist.Mapper.Aux[ApplyTupled.type, ZL, TL],
    tupler: ops.hlist.Tupler.Aux[TL, T]
  ): Functor.Aux[A, FuncArrow, FuncArrow.Inv[S, T]] =
    Functor.define[A, FuncArrow](t =>
      FuncArrow(s => tupler(applyArrows(zip(s, split(fmap.value(t.arrows)).arrows))))
    )

  implicit def mapScalaExprIdToFunc1Arrow[A]: Functor.Aux[ScalaExpr.Id[A], FuncArrow, FuncArrow.Inv[A, A]] =
    scalaExprIdFunctor.asInstanceOf[Functor.Aux[ScalaExpr.Id[A], FuncArrow, FuncArrow.Inv[A, A]]]
  private lazy val scalaExprIdFunctor = Functor.define[Arrow, FuncArrow](_ => FuncArrow(identity[Any]))

  implicit def mapScalaExprSelectFieldToFunc1Arrow[Obj, K <: String, V, Repr <: HList](
    implicit
    generic: LabelledGeneric.Aux[Obj, Repr],
    select:  ops.record.Selector.Aux[Repr, Symbol @@ K, V]
  ): Functor.Aux[ScalaExpr.SelectField[Obj, K, V], FuncArrow, FuncArrow.Inv[Obj, V]] =
    Functor.define[ScalaExpr.SelectField[Obj, K, V], FuncArrow](_ => FuncArrow(select.apply _ compose generic.to))

  implicit def mapScalaExprFMapToFunc1Arrow[F[_], E <: ScalaExpr, S, T](
    implicit
    functor: cats.Functor[F],
    types0: Arrow.Types.Aux[E, S, T],
    mapE: Functor.Aux[E, FuncArrow, FuncArrow.Inv[S, T]]
  ): Functor.Aux[ScalaExpr.FMap[F, E], FuncArrow, FuncArrow.Inv[F[S], F[T]]] =
    Functor.define[ScalaExpr.FMap[F, E], FuncArrow](t => FuncArrow(_.map(mapE(t.expr).apply)))

  implicit def mapScalaExprMBindToFunc1Arrow[F[_], E <: ScalaExpr, S, T, T0](
    implicit
    monad: cats.Monad[F],
    types0: Arrow.Types.Aux[E, S, T],
    mapE: Functor.Aux[E, FuncArrow, FuncArrow.Inv[S, T]],
    targetIsF: T <:< F[T0]
  ): Functor.Aux[ScalaExpr.MBind[F, E], FuncArrow, FuncArrow.Inv[F[S], F[T0]]] =
    Functor.define[ScalaExpr.MBind[F, E], FuncArrow](t =>
      FuncArrow(_ flatMap (mapE(t.expr).apply _  andThen targetIsF))
    )

//  implicit def mapScalaExpr?ToFunc1Arrow: Functor.Aux[ScalaExpr.?, FuncArrow, FuncArrow.Inv[?, ?]] = ???

}

