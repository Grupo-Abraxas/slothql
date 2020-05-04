package com.arkondata.slothql.cypher

import scala.language.{ higherKinds, implicitConversions }

import cats.{ Applicative, Functor, Id, Invariant, Monad, MonoidK, Semigroupal, ~> }
import cats.free.FreeT
import cats.syntax.apply._
import cats.syntax.applicative._
import cats.syntax.functor._
import shapeless.HList

import com.arkondata.slothql.cypher.{ CypherFragment => CF }

trait CypherTransactor[F[_], Src, C[_]] {
  type Tx[R] = CypherTransactor.Tx[F, Src, C, R]
  type Out[R]

  def runRead [A](tx: Tx[A]): Out[A]
  def runWrite[A](tx: Tx[A]): Out[A]
}

object CypherTransactor {
  type Tx[F[_], Src, C[_], R] = FreeT[Operation[Src, C, *], F, R]

  // // // // // // // // // // // // //
  // // // Main Monad Constructors // //
  // // // // // // // // // // // // //

  def query[F[_]: Applicative, Src, C[_], A](s: CypherStatement.Prepared[A])(implicit read: Reader[Src, A]): Tx[F, Src, C, A] =
    FreeT.liftF(Query(s, read))

  def query[F[_]: Applicative, Src, C[_], A](s: CypherStatement.Complete[A])(implicit read: Reader[Src, A]): Tx[F, Src, C, A] =
    query(s.withParams(Map()))

  def query[F[_]: Applicative, Src, C[_], A](q: CF.Query[A])(implicit gen: CypherStatement.Gen, read: Reader[Src, A]): Tx[F, Src, C, A] =
    query(q.toCypher(gen)._1)

  def query[F[_]: Applicative, Src, C[_], A, Params <: HList](pcq: ParametrizedCypherQuery[Params, A])
                                                             (implicit read: Reader[Src, A]): ParametrizedCypherQuery.Apply[Params, A, Tx[F, Src, C, A]] =
    new ParametrizedCypherQuery.Apply(pcq, query(_))

  def unwind [F[_]: Applicative, Src, C[_]]: C ~> Tx[F, Src, C, *]        = λ[C ~> Tx[F, Src, C, *]](c => FreeT.liftF(Unwind(c)))
  def nothing[F[_]: Applicative, Src, C[_]: MonoidK, A]: Tx[F, Src, C, A] = unwind.apply(MonoidK[C].empty[A])

  type TxC[F[_], Src, C[_], A] = Tx[F, Src, C, C[A]]

  def gather[F[_]: Applicative, Src, C[_]: Applicative](implicit M: Monad[TxC[F, Src, C, *]]): Tx[F, Src, C, *] ~> TxC[F, Src, C, *] =
    λ[Tx[F, Src, C, *] ~> TxC[F, Src, C, *]](
      _.mapK[TxC[F, Src, C, *]](
        λ[F ~> TxC[F, Src, C, *]]{
          fa => FreeT.liftT(fa.map(_.pure[C]))
        }
      ).foldMap(
         λ[Operation[Src, C, *] ~> TxC[F, Src, C, *]]{
           op => FreeT.liftF(mkGather(op))
         }
       )
    )
  @inline
  private def mkGather[Src, C[_], A](op: Operation[Src, C, A]): Operation[Src, C, C[A]] = Gather(op)

  // // // // // // // // // // // // // // // // //
  // // // Underlying Transactor Operations // // //
  // // // // // // // // // // // // // // // // //

  sealed trait Operation[Src, C[_], R]
  case class Query [Src, C[_], R](query: CypherStatement.Prepared[R], read: Reader[Src, R]) extends Operation[Src, C, R]
  case class Gather[Src, C[_], R](op: Operation[Src, C, R])                          extends Operation[Src, C, C[R]]
  case class Unwind[Src, C[_], R](values: C[R])                                      extends Operation[Src, C, R]

  trait Reader[Src, A] {
    def sourceName: String
    def name: String
    def apply(src: Src): A

    override def toString: String = s"Reader[$sourceName -> $name]"
  }

  // // // // // // // // //
  // // //  Helpers // // //
  // // // // // // // // //

  def const[F[_]: Applicative, Src, C[_]]: Id ~> Tx[F, Src, C, *] = λ[Id ~> Tx[F, Src, C, *]](FreeT.pure(_))
  def liftF[F[_]: Applicative, Src, C[_]]: F  ~> Tx[F, Src, C, *] = λ[F  ~> Tx[F, Src, C, *]](FreeT.liftT(_))

  def liftTx[F[_]: Applicative, Src, C[_]]: λ[A => F[Tx[F, Src, C, A]]] ~> Tx[F, Src, C, *] =
    λ[λ[A => F[Tx[F, Src, C, A]]] ~> Tx[F, Src, C, *]](FreeT.liftT(_).flatMap(locally))

  def unwindTx[F[_]: Applicative, Src, C[_]]: λ[A => C[Tx[F, Src, C, A]]] ~> Tx[F, Src, C, *] =
    λ[λ[A => C[Tx[F, Src, C, A]]] ~> Tx[F, Src, C, *]](
      unwind[F, Src, C].apply(_).flatMap(locally)
    )

  def mapOpt[F[_]: Applicative, Src, C[_]: MonoidK, A, B](tx: Tx[F, Src, C, A])
                                                         (f: A => Option[B]): Tx[F, Src, C, B] =
    tx.flatMap { f(_).map(const[F, Src, C].apply).getOrElse(nothing) }

  def flatTraverseTx[F[_]: Applicative, Src, C[_]: Functor, A, B](c: C[A])(tx: A => Tx[F, Src, C, B]): Tx[F, Src, C, B] =
    unwindTx[F, Src, C].apply(c.map(tx))

  def traverseTx[F[_]: Applicative, Src, C[_]: Applicative, A, B](c: C[A])(tx: A => Tx[F, Src, C, B])
                                                                 (implicit M: Monad[TxC[F, Src, C, *]]): Tx[F, Src, C, C[B]] =
    flatTraverseTx(c)(tx).gather

  def filter[F[_]: Applicative, Src, C[_]: MonoidK, A](tx: Tx[F, Src, C, A])
                                                      (pred: A => Boolean): Tx[F, Src, C, A] =
    tx.flatMap { a => if (pred(a)) const.apply(a) else nothing }

  def filtering[F[_]: Applicative, Src, C[_]: MonoidK, A](tx: Tx[F, Src, C, A])
                                                         (predTx: A => Tx[F, Src, C, Boolean]): Tx[F, Src, C, A] =
    filter {
      for {
        a <- tx
        b <- predTx(a)
      } yield (a, b)
    }(_._2).map(_._1)

  def product[F[_], Src, C[_], A, B](txA: Tx[F, Src, C, A], txB: Tx[F, Src, C, B])
                                    (implicit S: Semigroupal[Tx[F, Src, C, *]],
                                              I: Invariant[Tx[F, Src, C, *]]): Tx[F, Src, C, (A, B)] = (txA, txB).tupled

// TODO ================================================================================================================

  /** Will raise [[CannotZipException]] if lengths of the results do not correspond */
  def zip[F[_], Src, C[_], A, B](txA: Tx[F, Src, C, A], txB: Tx[F, Src, C, B]): Tx[F, Src, C, (A, B)] = ???

  /** Will raise [[CannotZip3Exception]] if lengths of the results do not correspond */
  def zip3[F[_], Src, CC[_], A, B, C](txA: Tx[F, Src, CC, A], txB: Tx[F, Src, CC, B], txC: Tx[F, Src, CC, C]): Tx[F, Src, CC, (A, B, C)] = ???

//  class CannotZipException(left: Seq[Any], right: Seq[Any]) extends Exception(
//    s"Cannot zip because of different length:\n\tleft:  $left\n\tright: $right")

//  class CannotZip3Exception(seq1: Seq[Any], seq2: Seq[Any], seq3: Seq[Any]) extends Exception(
//    s"Cannot zip because of different length:\n\t1: $seq1\n\t2: $seq2\n\t3: $seq3")


  // // // // // // // // //
  // // //  Syntax  // // //
  // // // // // // // // //

  final implicit class SyntaxGatherOps[F[_]: Applicative, Src, C[_]: Applicative, A](tx: Tx[F, Src, C, A])
                                                                                    (implicit M: Monad[TxC[F, Src, C, *]]){
    def gather: TxC[F, Src, C, A] = CypherTransactor.gather[F, Src, C].apply(tx)
  }

  final implicit class SyntaxTraverseOps[F[_]: Applicative, Src, C[_]: Applicative, A](ca: C[A])
                                                                                      (implicit M: Monad[TxC[F, Src, C, *]]){
    def traverseTx    [B](tx: A => Tx[F, Src, C, B]): Tx[F, Src, C, C[B]] = CypherTransactor.traverseTx(ca)(tx)
    def flatTraverseTx[B](tx: A => Tx[F, Src, C, B]): Tx[F, Src, C, B]    = CypherTransactor.flatTraverseTx(ca)(tx)

    def unwind: Tx[F, Src, C, A] = CypherTransactor.unwind[F, Src, C].apply(ca)
  }

  final implicit class SyntaxOps[F[_]: Applicative, Src, C[_]: MonoidK, A](tx: Tx[F, Src, C, A]) {
    def filtering (pred: A => Tx[F, Src, C, Boolean]): Tx[F, Src, C, A] = CypherTransactor.filtering(tx)(pred)
    def filter    (pred: A => Boolean)               : Tx[F, Src, C, A] = CypherTransactor.filter(tx)(pred)
    def withFilter(pred: A => Boolean)               : Tx[F, Src, C, A] = filter(pred)
    def mapOpt[B] (f: A => Option[B])                : Tx[F, Src, C, B] = CypherTransactor.mapOpt(tx)(f)

    def x  [B](that: Tx[F, Src, C, B]): Tx[F, Src, C, (A, B)] = CypherTransactor.product(tx, that)
    def zip[B](that: Tx[F, Src, C, B]): Tx[F, Src, C, (A, B)] = CypherTransactor.zip(tx, that)
  }

  abstract class Syntax[F[_]: Applicative, Src, C[_]: Applicative: MonoidK](
    implicit txCMonad: Monad[TxC[F, Src, C, *]]
  ) {
    type Tx[R]     = CypherTransactor.Tx[F, Src, C, R]
    type Reader[R] = CypherTransactor.Reader[Src, R]

    def query[R](q: CF.Query[R])                (implicit gen: CypherStatement.Gen, read: Reader[R]): Tx[R] = CypherTransactor.query(q)
    def query[R](s: CypherStatement.Complete[R])(implicit read: Reader[R])                          : Tx[R] = CypherTransactor.query(s)

    def query[Params <: HList, R](q: ParametrizedCypherQuery[Params, R])
                                 (implicit read: Reader[R]): ParametrizedCypherQuery.Apply[Params, R, Tx[R]] = CypherTransactor.query(q)

    def gather  : Tx               ~> TxC[F, Src, C, *] = CypherTransactor.gather
    def unwind  : C                ~> Tx                = CypherTransactor.unwind
    def unwindTx: λ[A => C[Tx[A]]] ~> Tx                = CypherTransactor.unwindTx
    def nothing[R]: Tx[R]                               = CypherTransactor.nothing

    def const : Id               ~> Tx = CypherTransactor.const
    def liftF : F                ~> Tx = CypherTransactor.liftF
    def liftTx: λ[A => F[Tx[A]]] ~> Tx = CypherTransactor.liftTx

    def product[X, Y]   (x: Tx[X], y: Tx[Y])          : Tx[(X, Y)]    = CypherTransactor.product(x, y)
    def zip    [X, Y]   (x: Tx[X], y: Tx[Y])          : Tx[(X, Y)]    = CypherTransactor.zip(x, y)
    def zip3   [X, Y, Z](x: Tx[X], y: Tx[Y], z: Tx[Z]): Tx[(X, Y, Z)] = CypherTransactor.zip3(x, y, z)

    val ops: Ops = new Ops

    protected class Ops {
      implicit def cypherTransactorSyntaxOps[A](tx: Tx[A]): CypherTransactor.SyntaxOps[F, Src, C, A] =
        new CypherTransactor.SyntaxOps[F, Src, C, A](tx)
      implicit def cypherTransactorSyntaxGatherOps[A](tx: Tx[A]): CypherTransactor.SyntaxGatherOps[F, Src, C, A] =
        new CypherTransactor.SyntaxGatherOps[F, Src, C, A](tx)
      implicit def cypherTransactorSyntaxTraverseOps[A](ca: C[A]): CypherTransactor.SyntaxTraverseOps[F, Src, C, A] =
        new CypherTransactor.SyntaxTraverseOps[F, Src, C, A](ca)
    }
  }
}
