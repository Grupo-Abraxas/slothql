package com.abraxas.slothql.cypher

import scala.language.higherKinds

import cats.effect.IO
import cats.free.Free
import cats.free.Free.liftF
import cats.{ Functor, FunctorFilter, Monad, ~> }
import cats.instances.vector._
import cats.syntax.apply._
import cats.syntax.traverse._
import shapeless.{ DepFn1, |∨| }

import com.abraxas.slothql.cypher.CypherFragment.{ Known, Query }
import com.abraxas.slothql.util.MoreZip

trait CypherTransactor {
  type TxBuilder <: CypherTxBuilder
  val txBuilder: TxBuilder
  import txBuilder._

  type Operation[R] = txBuilder.Operation[R]
  type Read[R]      = txBuilder.Read[R]
  type ReadWrite[R] = txBuilder.ReadWrite[R]

  final lazy val Read   = txBuilder.Read

  final def read[A](query: Known[Query[A]])(implicit read: Reader[A]): ReadTx[read.Out] = txBuilder.read(query)

  def readIO[A](query: Known[Query[A]])(implicit r: Reader[A]): IO[Seq[r.Out]] =
    runRead(read(query))

  // Run transactions
  def run[A, Tx: (ReadTx[A] |∨| WriteTx[A])#λ](tx: Tx)(
    implicit isRead: Tx <:< ReadTx[_] = null, isWrite: Tx <:< WriteTx[_] = null
  ): IO[Seq[A]] =
    tx match {
      case tx: ReadTx[A]  @unchecked if isRead  != null => runRead(tx)
      case tx: WriteTx[A] @unchecked if isWrite != null => runWrite(tx)
    }
  def runRead[A](tx: ReadTx[A]): IO[Seq[A]]
  def runWrite[A](tx: WriteTx[A]): IO[Seq[A]]

}

object CypherTransactor {
  type Aux[R, RR[x] <: Reader[R, x]] = CypherTransactor { type Result = R; type Reader[A] = RR[A] }

  trait Reader[Src, A] extends DepFn1[Src]
  object Reader {
    type Aux[Src, A, R] = Reader[Src, A] { type Out = R }
    def apply[Src, A](implicit reader: Reader[Src, A]): Aux[Src, A, reader.Out] = reader
  }
}


trait CypherTxBuilder {
  type Result
  type Reader[A] <: CypherTransactor.Reader[Result, A]
  type ReaderAux[A, R] = Reader[A] { type Out = R }

  type CypherQuery[+A] = CypherFragment.Query[A]

  sealed trait Operation[R]
  sealed trait Read[R] extends Operation[Seq[R]] {
    type A
    val query: Known[Query[A]]
    val reader: CypherTransactor.Reader.Aux[Result, A, R]
  }
  sealed trait ReadWrite[R] extends Read[R] {
    override val query: Known[Query[A]] // TODO: `Query[A]`
  }

  protected[cypher] case class Gather[R](read: Read[R]) extends Read[Vector[R]] {
    type A = read.A
    lazy val query: Known[Query[read.A]] = ???
    lazy val reader: CypherTransactor.Reader.Aux[Result, read.A, Vector[R]] = ???
  }
  protected[cypher] case class Unwind[R](values: Iterable[R]) extends Read[R] {
    type A = R
    lazy val query: Known[Query[R]] = ???
    lazy val reader: CypherTransactor.Reader.Aux[Result, R, R] = ???
  }

  object Read {
    type Aux[R, A0] = Read[R] { type A = A0 }
    def apply[A0](q: Known[Query[A0]])(implicit r: Reader[A0]): Aux[r.Out, A0] =
      new Read[r.Out] {
        type A = A0
        val query: Known[Query[A]] = q
        val reader: CypherTransactor.Reader.Aux[Result, A, r.Out] =
          r.asInstanceOf[CypherTransactor.Reader.Aux[Result, A, r.Out]]
      }

    def const[A](a: A): ReadTx[A] = Free.pure(a)

    def product[A, B](txA: ReadTx[A], txB: ReadTx[B]): ReadTx[(A, B)] = (txA, txB).tupled

    /** Will throw [[CannotZipException]] if lengths of the results do not correspond */
    def zip[A, B](txA: ReadTx[A], txB: ReadTx[B]): ReadTx[(A, B)] =
      for {
        as <- txA.gather
        bs <- txB.gather
        _ = if (as.length != bs.length) throw new CannotZipException(as, bs)
        zipped <- unwind(as zip bs)
      } yield zipped

    /** Will throw [[CannotZip3Exception]] if lengths of the results do not correspond */
    def zip3[A, B, C](txA: ReadTx[A], txB: ReadTx[B], txC: ReadTx[C]): ReadTx[(A, B, C)] =
      for {
        as <- txA.gather
        bs <- txB.gather
        cs <- txC.gather
        _ = if (as.length != bs.length || bs.length != cs.length) throw new CannotZip3Exception(as, bs, cs)
        zipped <- unwind(MoreZip.zip3(as, bs, cs))
      } yield zipped

    def filterOpt[A, B](tx: ReadTx[A])(f: A => Option[B]): ReadTx[B]  = tx.flatMap { r => f(r).map(const).getOrElse(nothing) }
    def filter   [A]   (tx: ReadTx[A])(pred: A => Boolean): ReadTx[A] = tx.flatMap { r => if (pred(r)) const(r) else nothing }

    def unwind[A](iterable: Iterable[A]): ReadTx[A] = liftF(Unwind(iterable))
    def nothing[A]: ReadTx[A] = unwind(Vector.empty)

    class CannotZipException(left: Seq[Any], right: Seq[Any]) extends Exception(
      s"Cannot zip because of different length:\n\tleft:  $left\n\tright: $right")

    class CannotZip3Exception(seq1: Seq[Any], seq2: Seq[Any], seq3: Seq[Any]) extends Exception(
      s"Cannot zip because of different length:\n\t1: $seq1\n\t2: $seq2\n\t3: $seq3")


    implicit object ReadTxVectorIsMonad extends Monad[λ[A => ReadTx[Vector[A]]]] {
      def pure[A](x: A): ReadTx[Vector[A]] =
        Free.pure(Vector(x))
      override def map[A, B](fa: ReadTx[Vector[A]])(f: A => B): ReadTx[Vector[B]] =
        fa.map(_.map(f))
      def flatMap[A, B](fa: ReadTx[Vector[A]])(f: A => ReadTx[Vector[B]]): ReadTx[Vector[B]] =
        fa.flatMap(_.flatTraverse(f))

      // TODO: Is it tail-call optimised? (I doubt it)
      def tailRecM[A, B](a0: A)(f: A => ReadTx[Vector[Either[A, B]]]): ReadTx[Vector[B]] =
        f(a0) flatMap {
          _.flatTraverse[ReadTx, B] {
            case Left(a)  => tailRecM(a)(f)
            case Right(b) => Free.pure(Vector(b))
          }
        }
    }

    implicit object ReadTxIsFunctorFilter extends FunctorFilter[ReadTx] {
      val functor: Functor[ReadTx] = Functor[ReadTx]
      def mapFilter[A, B](fa: ReadTx[A])(f: A => Option[B]): ReadTx[B] = filterOpt(fa)(f)
    }


    implicit class ReadTxOps[A](tx: ReadTx[A]) {
      def gather: ReadTx[Vector[A]] = tx.foldMap[λ[A => ReadTx[Vector[A]]]](
        λ[Read ~> λ[A => ReadTx[Vector[A]]]](fa => liftF(newGather(fa)))
      )

      def filter      (pred: A => Boolean): ReadTx[A] = Read.filter(tx)(pred)
      def filterOpt[B](f: A => Option[B]): ReadTx[B]  = Read.filterOpt(tx)(f)

      def x[B](that: ReadTx[B]): ReadTx[(A, B)] = Read.product(tx, that)

      /** Will throw [[CannotZipException]] if lengths of the results do not correspond */
      def zip[B](that: ReadTx[B]): ReadTx[(A, B)] = Read.zip(tx, that)
    }

    private def newGather[R](read: Read[R]): Read[Vector[R]] = Gather(read)
  }


  type ReadTx[R] = Free[Read, R]
  type WriteTx[R] = Free[ReadWrite, R]


  def read[A](query: Known[Query[A]])(implicit read: Reader[A]): ReadTx[read.Out] =
    liftF[Read, read.Out](Read[A](query))
}